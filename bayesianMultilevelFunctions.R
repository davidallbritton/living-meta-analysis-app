#######################################################################################
################### A General Tool for Living Meta-Analysis #################
#######################################################################################
# v.1.3 2026.07.19
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

## Fit (or refit from the compiled template) the Bayesian multilevel model.
## d must come from bmlData().  Returns a brmsfit.
fitBayesianMultilevel <- function(d, tauprior, scaletau, mupriormean, mupriorsd) {
  if (!requireNamespace("brms", quietly = TRUE))
    stop("The brms package is required for the Bayesian multilevel model.")
  if (!tauprior %in% bmlSupportedTauPriors)
    stop("Unsupported tau prior for the Bayesian multilevel model: ", tauprior)

  hasModerator <- "moderator" %in% names(d)
  templateKey  <- paste(tauprior, if (hasModerator) "moderator" else "base")

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
  formulaText <- if (hasModerator)
    "es | se(sqrt(var)) ~ 0 + moderator + (1 | Paper) + (1 | ID)"
  else
    "es | se(sqrt(var)) ~ 1 + (1 | Paper) + (1 | ID)"

  template <- bmlTemplates[[templateKey]]
  fit <- if (is.null(template)) {
    brms::brm(stats::as.formula(formulaText), data = d, prior = priors, stanvars = sv,
              chains = bmlSettings$chains, iter = bmlSettings$iter,
              warmup = bmlSettings$warmup, cores = bmlSettings$cores,
              control = list(adapt_delta = bmlSettings$adapt_delta),
              seed = bmlSettings$seed, refresh = 0, silent = 2)
  } else {
    update(template, newdata = d, stanvars = sv,
           chains = bmlSettings$chains, iter = bmlSettings$iter,
           warmup = bmlSettings$warmup, cores = bmlSettings$cores,
           seed = bmlSettings$seed, refresh = 0, silent = 2)
  }
  if (is.null(template)) bmlTemplates[[templateKey]] <- fit
  fit
}

## Cache lookup, mirroring checkOldBmrModels(): platform-safe comparison of the
## MA data (factors as characters) plus all prior settings and the moderator.
checkOldBmlModels <- function(listPrevious, MA, tauprior, mupriorsd, scaletau,
                              mupriormean, moderatorName) {
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
          samePriorValue(listPrevious[[i]]$mupriormean, mupriormean)
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
