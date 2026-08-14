#######################################################################################
################### A General Tool for Living Meta-Analysis #################
#######################################################################################
# v.1.3 2026.08.14
#
################### Bayesian multilevel (three-level) model functions #################
#
# Fits the Bayesian analogue of the frequentist multilevel model (see
# fitMultilevelCHE): every effect size is its own row, with known sampling
# variance and true effects varying at two levels,
#     es_i ~ Normal(theta_i, var_i),   theta_i = mu + u_paper + w_ES,
# fit with brms/Stan (NUTS).  Priors follow the sidebar "Prior specifications":
# normal(mean, sd) on mu and Half cauchy / Half normal (with scale) on BOTH
# heterogeneity SDs (tau_paper and tau_ES).  Only those proper priors are
# supported (Stan needs proper, sample-able priors).
#
# COMPILATION STRATEGY (the expensive part of Stan): the prior CONSTANTS are
# passed to Stan as data (brms "stanvars"), so the generated Stan code -- and
# therefore the compiled model -- is identical across prior values and data
# sets.  The first fit of each model structure compiles (~30-90 s) and is kept
# as a process-wide template in bmlTemplates; every later fit reuses it via
# update() and only samples (a few seconds).  There are at most 4 structures:
# {Half cauchy, Half normal} x {no moderator, moderator}.  The moderator column
# is always renamed "moderator" in the data so one template serves any chosen
# moderator variable.
#
# Sampling settings: 4 chains x (3000 iter - 1000 warmup) = 8000 kept draws,
# adapt_delta = 0.99 (the near-zero tau_ES funnel produces divergences at the
# default 0.8), fixed seed for reproducibility.
#######################################################################################

## process-wide store for compiled template fits (shared by all sessions)
bmlTemplates <- new.env(parent = emptyenv())

## sampling settings used for every fit (kept in one place; also reported in the
## UI).  4 x (4000 - 1000) = 12000 kept draws: with 8000 the default data sat at
## ESS ~270 / Rhat ~1.015 (borderline); 12000 gives ESS ~700 / Rhat ~1.003.
## cores: sequential chains on shinyapps.io (forked parallel workers double the
## peak memory and, when the OOM killer takes one, die with the cryptic
## "Error in sink(type = \"output\"): invalid connection" seen in the server
## logs); locally two cores are safe and faster
bmlSettings <- list(chains = 4, iter = 4000, warmup = 1000,
                    cores = if (Sys.getenv("R_CONFIG_ACTIVE") == "shinyapps") 1 else 2,
                    adapt_delta = 0.99, seed = 4242)

## the two supported tau-prior families
bmlSupportedTauPriors <- c("Half cauchy", "Half normal")

## Build the data frame the model uses from a multilevel MA() table.
## moderatorName: NULL for the overall model; otherwise that factor column is
## copied into a column literally named "moderator" (one compiled template then
## serves every moderator choice).
bmlData <- function(MA, moderatorName = NULL) {
  d <- data.frame(es    = as.numeric(MA$es),
                  var   = as.numeric(MA$var),
                  Paper = factor(as.character(MA$Paper)),
                  ID    = factor(as.character(MA$ID)))
  if (!is.null(moderatorName)) {
    d$moderator <- droplevels(factor(as.character(MA[[moderatorName]])))
  }
  d
}

## Known sampling covariance matrix V for the CHE (correlated-effects) model:
## the known variances on the diagonal, and rho * sqrt(v_i * v_j) off-diagonal for
## pairs of effect sizes from the SAME paper (0 across papers).  This is the same
## metafor::vcalc() call fitMultilevelCHE() makes, so the frequentist and Bayesian
## multilevel models use an identical V by construction.
##
## rho is a working assumption (primary studies essentially never report it), so
## the app exposes it as a slider defaulting to the conventional 0.5.
##
## Stan needs a positive-definite V.  At rho = 0.5 this holds comfortably even for
## papers contributing many effect sizes, but large rho with a large within-paper
## block can push V singular, so nudge with Matrix::nearPD when needed and record
## that on the result as attribute "nudged" for the UI to report.
bmlV <- function(d, rho = 0.5) {
  V <- metafor::vcalc(vi = d$var, cluster = d$Paper, obs = seq_len(nrow(d)), rho = rho)
  ## vcalc() returns a classed "vcovmat"; brms wants a plain numeric matrix
  V <- matrix(as.numeric(V), nrow = nrow(d), ncol = nrow(d))
  ev <- min(eigen(V, symmetric = TRUE, only.values = TRUE)$values)
  nudged <- FALSE
  if (ev <= .Machine$double.eps^0.5) {
    if (!requireNamespace("Matrix", quietly = TRUE))
      stop("The sampling covariance matrix is not positive definite and the Matrix ",
           "package is not available to correct it.  Try a smaller ρ.")
    V <- as.matrix(Matrix::nearPD(V, ensureSymmetry = TRUE)$mat)
    nudged <- TRUE
  }
  attr(V, "nudged") <- nudged
  attr(V, "minEigen") <- ev
  V
}

## Fit (or refit from the compiled template) the Bayesian multilevel model.
## d must come from bmlData().  Returns a brmsfit.
##
## che = TRUE (the app's only production path, matching the always-CHE frequentist
## multilevel tab) models the within-paper correlation of SAMPLING ERRORS via
## brms fcor() with the known covariance V from bmlV().  Two constraints come with
## that construction: se() and fcor() are mutually exclusive, so the known
## variances move from se(sqrt(var)) into the diagonal of V; and sigma must be
## fixed to 1, or the model would estimate a free scale on top of the known V and
## be unidentified against the ID-level random effect.
## che = FALSE keeps the older diagonal-V model (independent sampling errors); it
## is retained for the test suite, and is reachable in the app by setting rho = 0.
fitBayesianMultilevel <- function(d, tauprior, scaletau, mupriormean, mupriorsd,
                                  rho = 0.5, che = TRUE) {
  if (!requireNamespace("brms", quietly = TRUE))
    stop("The brms package is required for the Bayesian multilevel model.")
  if (!tauprior %in% bmlSupportedTauPriors)
    stop("Unsupported tau prior for the Bayesian multilevel model: ", tauprior)

  hasModerator <- "moderator" %in% names(d)
  ## the CHE and diagonal models are different Stan programs and must not share a
  ## compiled template.  This does not increase the production template count:
  ## with CHE always on, the "che" variants replace the "diag" ones (at most 4
  ## per process, as before); "diag" is compiled only by the tests.
  templateKey  <- paste(tauprior, if (hasModerator) "moderator" else "base",
                        if (che) "che" else "diag")

  ## prior constants enter as DATA, so all values share one compiled model
  sv <- brms::stanvar(x = as.numeric(mupriormean), name = "prior_mu_mean") +
        brms::stanvar(x = as.numeric(mupriorsd),   name = "prior_mu_sd")   +
        brms::stanvar(x = as.numeric(scaletau),    name = "prior_tau_scale")
  tauPriorString <- if (tauprior == "Half cauchy") "cauchy(0, prior_tau_scale)"
                    else "normal(0, prior_tau_scale)"
  ## no moderator: normal prior on the overall mean (Intercept);
  ## moderator: cell-means coding (~ 0 + moderator), same normal prior on every
  ## group mean (class "b"), paralleling buildBmrModel()
  priors <- c(brms::set_prior("normal(prior_mu_mean, prior_mu_sd)",
                              class = if (hasModerator) "b" else "Intercept"),
              brms::set_prior(tauPriorString, class = "sd"))
  rhs <- if (hasModerator) "0 + moderator" else "1"
  ## CHE: known variances live in V, sigma fixed to 1 (see the header comment)
  formulaText <- if (che)
    paste("es ~", rhs, "+ (1 | Paper) + (1 | ID) + fcor(V)")
  else
    paste("es | se(sqrt(var)) ~", rhs, "+ (1 | Paper) + (1 | ID)")
  if (che) {
    priors <- c(priors, brms::set_prior("constant(1)", class = "sigma"))
    V <- bmlV(d, rho = rho)
    ## brms takes the known covariance through data2, not data
    extraArgs <- list(data2 = list(V = V), family = stats::gaussian())
  } else {
    V <- NULL
    extraArgs <- list()
  }

  template <- bmlTemplates[[templateKey]]
  fit <- if (is.null(template)) {
    do.call(brms::brm, c(list(
              stats::as.formula(formulaText), data = d, prior = priors, stanvars = sv,
              chains = bmlSettings$chains, iter = bmlSettings$iter,
              warmup = bmlSettings$warmup, cores = bmlSettings$cores,
              control = list(adapt_delta = bmlSettings$adapt_delta),
              seed = bmlSettings$seed, refresh = 0, silent = 2), extraArgs))
  } else {
    ## V's dimension changes with the selection, but N is Stan *data*, so the
    ## compiled template is reused without recompiling (verified by benchmark)
    do.call(stats::update, c(list(
           template, newdata = d, stanvars = sv,
           chains = bmlSettings$chains, iter = bmlSettings$iter,
           warmup = bmlSettings$warmup, cores = bmlSettings$cores,
           seed = bmlSettings$seed, refresh = 0, silent = 2),
           if (che) list(data2 = list(V = V)) else list()))
  }
  if (is.null(template)) bmlTemplates[[templateKey]] <- fit
  fit
}

## Cache lookup, mirroring checkOldBmrModels(): platform-safe comparison of the
## MA data (factors as characters) plus all prior settings, rho and the moderator.
## rho is compared with samePriorValue() rather than identical() for the same
## reason the priors are: saved caches hold integer and double spellings of the
## same number, and identical() silently misses those and forces a slow refit.
checkOldBmlModels <- function(listPrevious, MA, tauprior, mupriorsd, scaletau,
                              mupriormean, moderatorName, rho = 0.5, che = TRUE) {
  return_bml <- FALSE
  if (length(listPrevious)) {
    MA <- as.data.frame(MA)
    MA <- MA %>% mutate_if(is.factor, as.character)
    for (i in seq_along(listPrevious)) {
      ma_previous <- as.data.frame(listPrevious[[i]]$MA)
      ma_previous <- ma_previous %>% mutate_if(is.factor, as.character)
      if (identical(ma_previous, MA) &&
          identical(listPrevious[[i]]$moderatorName, moderatorName) &&
          identical(listPrevious[[i]]$tauprior, tauprior) &&
          samePriorValue(listPrevious[[i]]$mupriorsd, mupriorsd) &&
          samePriorValue(listPrevious[[i]]$scaletau, scaletau) &&
          samePriorValue(listPrevious[[i]]$mupriormean, mupriormean) &&
          ## entries cached before rho existed are treated as the 0.5 default
          ## (written out rather than using %||%, which is base R only from 4.4)
          samePriorValue(if (is.null(listPrevious[[i]]$rho)) 0.5
                         else listPrevious[[i]]$rho, rho) &&
          ## a CHE fit and a diagonal fit are different models, never interchangeable
          identical(if (is.null(listPrevious[[i]]$che)) TRUE
                    else isTRUE(listPrevious[[i]]$che), isTRUE(che))
      ) {
        return_bml <- listPrevious[[i]]$bml
        break
      }
    }
  }
  if (isTruthy(return_bml)) return_bml else FALSE
}

## ---- output builders (all work from the fitted brmsfit) ----

## posterior summary table: one row per mean parameter (overall mu, or each
## moderator group), plus the two heterogeneity SDs
bmlSummaryTable <- function(fit) {
  fx <- brms::fixef(fit)                      # Estimate, Est.Error, Q2.5, Q97.5
  rn <- rownames(fx)
  rn[rn == "Intercept"] <- "µ (overall mean)"
  rn <- sub("^moderator", "mean: ", rn)
  sds <- rbind(summary(fit)$random$Paper[1, , drop = FALSE],
               summary(fit)$random$ID[1, , drop = FALSE])
  out <- data.frame(Parameter = c(rn, "τ between papers (sd)", "τ within papers (sd)"),
                    rbind(fx[, c("Estimate", "Est.Error", "Q2.5", "Q97.5"), drop = FALSE],
                          as.matrix(sds[, c("Estimate", "Est.Error", "l-95% CI", "u-95% CI")])),
                    check.names = FALSE, row.names = NULL)
  names(out) <- c("Parameter", "Posterior mean", "SD", "2.5%", "97.5%")
  out
}

## MCMC health line for display: Rhat, effective sample size, divergences
bmlDiagnosticsText <- function(fit) {
  s <- summary(fit)
  ess <- suppressWarnings(min(s$fixed[, "Bulk_ESS"],
                              s$random$Paper[, "Bulk_ESS"],
                              s$random$ID[, "Bulk_ESS"], na.rm = TRUE))
  ndiv <- sum(subset(brms::nuts_params(fit), Parameter == "divergent__")$Value)
  sprintf(paste("MCMC diagnostics: max Rhat = %.3f (want < 1.01); min bulk ESS = %.0f",
                "(want > 400); divergent transitions = %d of %d (want 0).",
                "%d chains, %d kept draws, adapt_delta = %.2f, seed = %d."),
          max(brms::rhat(fit), na.rm = TRUE), ess, ndiv,
          bmlSettings$chains * (bmlSettings$iter - bmlSettings$warmup),
          bmlSettings$chains, bmlSettings$chains * (bmlSettings$iter - bmlSettings$warmup),
          bmlSettings$adapt_delta, bmlSettings$seed)
}

## per-paper posterior estimates (mu + u_paper) for the forest plot;
## only for the no-moderator model
bmlForestData <- function(fit) {
  co <- coef(fit)$Paper[, , "Intercept"]
  data.frame(Paper = rownames(co),
             Estimate = co[, "Estimate"], Lower = co[, "Q2.5"], Upper = co[, "Q97.5"],
             row.names = NULL)
}

## forest plot: per-paper posterior means and 95% credible intervals with the
## overall posterior mean (dashed) and zero (grey) reference lines; for the
## moderator model the rows are the per-group means instead
bmlForestPlot <- function(fit) {
  fx <- brms::fixef(fit)
  if ("Intercept" %in% rownames(fx)) {
    fd <- bmlForestData(fit)
    fd <- fd[order(fd$Estimate), ]
    fd$Paper <- factor(fd$Paper, levels = fd$Paper)
    overall <- fx["Intercept", "Estimate"]
    ggplot(fd, aes(x = Estimate, y = Paper)) +
      geom_vline(xintercept = 0, color = "grey60") +
      geom_vline(xintercept = overall, color = "#D55E00", linetype = "dashed") +
      geom_linerange(aes(xmin = Lower, xmax = Upper), color = "grey40") +
      geom_point(size = 1.6, color = "#0072B2") +
      labs(x = "Hedges' g (posterior mean and 95% CrI per paper)", y = NULL,
           caption = "dashed line: overall posterior mean µ") +
      theme_minimal_hgrid(10)
  } else {
    gd <- data.frame(Group = sub("^moderator", "", rownames(fx)),
                     Estimate = fx[, "Estimate"], Lower = fx[, "Q2.5"], Upper = fx[, "Q97.5"])
    gd <- gd[order(gd$Estimate), ]
    gd$Group <- factor(gd$Group, levels = gd$Group)
    ggplot(gd, aes(x = Estimate, y = Group)) +
      geom_vline(xintercept = 0, color = "grey60") +
      geom_linerange(aes(xmin = Lower, xmax = Upper), color = "grey40") +
      geom_point(size = 2.4, color = "#D55E00") +
      labs(x = "Hedges' g (posterior mean and 95% CrI per moderator group)", y = NULL) +
      theme_minimal_hgrid(12)
  }
}

## posterior density plots for the mean parameter(s) and both taus
bmlDensityPlot <- function(fit) {
  vars <- brms::variables(fit)
  keep <- vars[grepl("^b_|^sd_", vars)]
  brms::mcmc_plot(fit, variable = keep, type = "dens") +
    ggplot2::theme_minimal(base_size = 11)
}
