#######################################################################################
################### A General Tool for Living Meta-Analysis #################
#######################################################################################
# v.1.6 2026.11.22
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
# COMPILATION STRATEGY: the prior CONSTANTS are passed to Stan as data (brms
# "stanvars"), so the generated Stan code -- and therefore the compiled model --
# is identical across prior values and data sets.  The first fit of each model
# structure compiles (~30 s) and is kept as a process-wide template in
# bmlTemplates; every later fit reuses it via update() and only samples.  There
# are at most 4 structures: {Half cauchy, Half normal} x {no moderator,
# moderator}.  The moderator column is always renamed "moderator" in the data so
# one template serves any chosen moderator variable.
#
# SAMPLING COST: with CHE on (the production path), sampling, not compilation,
# dominates.  The fcor(V) likelihood is one multivariate normal over all N
# effect sizes (dense N x N covariance; its Cholesky factor is computed once,
# but every gradient still costs O(N^2)), and NUTS takes about twice as many
# steps per iteration as the diagonal model.  Measured 2026-10-16 on the
# default data (79 rows): ~45-80 s per chain, so ~2 min per fit locally (2
# cores) and ~4x one chain on shinyapps.io (sequential chains), versus ~3 s per
# chain for the diagonal model.  Hence the precomputed-fits strategy for
# production.
#
# Sampling settings: see bmlSettings below.  adapt_delta = 0.99 (the near-zero
# tau_ES funnel produces divergences at the default 0.8), fixed seed for
# reproducibility.
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
##
## sharing: optional sharing table (readSharingTable(), HelperFunctions.R).  Then
## only effect sizes that share data are correlated (buildSamplingV()), and a V
## that is not positive definite is REFUSED rather than nudged: it means no joint
## distribution has the stated correlations, a fact about the design and rho that
## the user needs to see.  buildSamplingV() already refuses rho at or above the
## ceiling; the eigenvalue check below catches the numerically borderline case.
bmlV <- function(d, rho = 0.5, sharing = NULL) {
  V0 <- buildSamplingV(d, rho = rho, sharing = sharing)
  ## vcalc() returns a classed "vcovmat"; brms wants a plain numeric matrix
  V <- matrix(as.numeric(V0), nrow = nrow(d), ncol = nrow(d))
  ev <- min(eigen(V, symmetric = TRUE, only.values = TRUE)$values)
  nudged <- FALSE
  if (!is.null(sharing)) {
    if (ev <= .Machine$double.eps^0.5)
      stop(sprintf(paste("the sampling covariance matrix built from the sharing table is",
                         "not positive definite at rho = %s (ceiling for this selection:",
                         "%.3f).  Lower rho."), format(rho), attr(V0, "rhoCeiling")))
    for (a in c("ids", "rhoCeiling", "nUnmatched", "nShared")) attr(V, a) <- attr(V0, a)
  } else if (ev <= .Machine$double.eps^0.5) {
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
                                  rho = 0.5, che = TRUE, sharing = NULL) {
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
    V <- bmlV(d, rho = rho, sharing = sharing)
    assertVAligned(V, d)
    ## brms takes the known covariance through data2, not data
    V <- matrix(as.numeric(V), nrow(d), nrow(d))   # plain matrix for brms
    extraArgs <- list(data2 = list(V = V), family = stats::gaussian())
  } else {
    V <- NULL
    extraArgs <- list()
  }

  template <- bmlTemplates[[templateKey]]
  fit <- if (is.null(template)) {
    ## the formula lives in the global environment, not in this function's frame:
    ## brms keeps data2 (V) in an environment whose parent is the formula's, and a
    ## frame-parented one would make every saved fit carry this frame (the
    ## compiled-template store and the previous fit, ~100 MB per saved model)
    do.call(brms::brm, c(list(
              stats::as.formula(formulaText, env = globalenv()), data = d, prior = priors,
              stanvars = sv,
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
                              mupriormean, moderatorName, rho = 0.5, che = TRUE,
                              sharingKey = NULL) {
  return_bml <- FALSE
  if (length(listPrevious)) {
    MA <- as.data.frame(MA)
    MA <- MA %>% mutate_if(is.factor, as.character)
    for (i in seq_along(listPrevious)) {
      ma_previous <- as.data.frame(listPrevious[[i]]$MA)
      ma_previous <- ma_previous %>% mutate_if(is.factor, as.character)
      if (sameMAData(ma_previous, MA) &&
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
                    else isTRUE(listPrevious[[i]]$che), isTRUE(che)) &&
          ## the part of the sharing table that applies to these rows (sharingKey(),
          ## HelperFunctions.R; NULL = flat, which is what entries cached before
          ## sharing tables existed used).  Irrelevant when the correlated-errors
          ## correction is skipped, since V is then not used at all.
          (!isTRUE(che) || identical(listPrevious[[i]]$sharingKey, sharingKey))
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

## ---- reporting extras (v1.6) ----
## All work from the fitted brmsfit and are deterministic given its draws, except
## the posterior predictive replicates, which take an explicit seed.

## The shortest interval holding `level` of the draws (highest-density interval;
## exact for a unimodal posterior).  Reported beside the equal-tailed interval.
bmlHdi <- function(x, level = 0.95) {
  x <- sort(as.numeric(x)); n <- length(x); k <- ceiling(level * n)
  w <- x[k:n] - x[seq_len(n - k + 1)]
  j <- which.min(w)
  c(lower = x[j], upper = x[j + k - 1])
}

## Prediction interval for the TRUE effect of a new effect size in a new paper:
## the mean plus both heterogeneity sources.  Given one draw it is
## Normal(mu, sqrt(tau_paper^2 + tau_es^2)); across draws it is the mixture of
## those normals, whose quantiles are found by root-finding on the mixture CDF.
## No extra random numbers are drawn, so the interval is the same every time
## for the same fit.
bmlPredictionInterval <- function(mu, tauPaper, tauEs, level = 0.95) {
  s   <- sqrt(tauPaper^2 + tauEs^2)
  p   <- c((1 - level) / 2, 1 - (1 - level) / 2)
  cdf <- function(x) mean(stats::pnorm(x, mu, s))
  brk <- c(min(mu - 6 * s), max(mu + 6 * s))
  q   <- vapply(p, function(pp)
    stats::uniroot(function(x) cdf(x) - pp, brk, tol = 1e-8)$root, numeric(1))
  c(mean = mean(mu), lower = q[1], upper = q[2])
}

## Draws of the mean parameter(s), one column per mean: "µ (overall mean)" for
## the overall model, else one column per moderator level, in level order (with
## cell-means coding brms orders the coefficients like the factor's levels, which
## avoids guessing how brms renames levels with spaces or symbols).
bmlMeanDraws <- function(fit) {
  m  <- as.matrix(fit)
  fx <- rownames(brms::fixef(fit))
  out <- m[, paste0("b_", fx), drop = FALSE]
  if (identical(fx, "Intercept")) colnames(out) <- "µ (overall mean)" else {
    lv <- levels(droplevels(factor(fit$data$moderator)))
    # brms renames levels in its coefficient names but keeps their order
    stopifnot(length(lv) == length(fx))
    colnames(out) <- lv
  }
  out
}
bmlTauDraws <- function(fit) {
  m <- as.matrix(fit)
  cbind(paper = m[, "sd_Paper__Intercept"], es = m[, "sd_ID__Intercept"])
}

## 95% highest-density intervals for the parameters of bmlSummaryTable(), in its
## row order
bmlHdiTable <- function(fit) {
  dr <- cbind(bmlMeanDraws(fit), bmlTauDraws(fit))
  h <- t(apply(dr, 2, bmlHdi))
  data.frame(Parameter = bmlSummaryTable(fit)$Parameter,
             `HDI lower` = h[, "lower"], `HDI upper` = h[, "upper"],
             check.names = FALSE, row.names = NULL)
}

## Prediction interval per mean parameter (overall, or per moderator level, each
## with the model's common heterogeneity SDs)
bmlPredictionTable <- function(fit, level = 0.95) {
  md <- bmlMeanDraws(fit); td <- bmlTauDraws(fit)
  rows <- lapply(colnames(md), function(g)
    bmlPredictionInterval(md[, g], td[, "paper"], td[, "es"], level))
  out <- data.frame(Mean = colnames(md), do.call(rbind, rows), row.names = NULL)
  names(out) <- c(if (ncol(md) == 1) "Parameter" else "Moderator level",
                  "Posterior mean", sprintf("%g%% PI lower", 100 * level),
                  sprintf("%g%% PI upper", 100 * level))
  out
}

## Difference between two moderator levels' means (levelA - levelB) from the
## draws: posterior mean, SD, equal-tailed and highest-density 95% intervals, and
## the posterior probability that the difference is above zero.  NULL if either
## level is not in the fitted model.
bmlContrast <- function(fit, levelA, levelB) {
  md <- bmlMeanDraws(fit)
  if (!all(c(levelA, levelB) %in% colnames(md)) || identical(levelA, levelB)) return(NULL)
  x <- md[, levelA] - md[, levelB]
  h <- bmlHdi(x)
  list(draws = x,
       table = data.frame(Contrast = paste(levelA, "−", levelB),
                          `Posterior mean` = mean(x), SD = stats::sd(x),
                          `2.5%` = unname(stats::quantile(x, .025)),
                          `97.5%` = unname(stats::quantile(x, .975)),
                          `HDI lower` = h[["lower"]], `HDI upper` = h[["upper"]],
                          `P(> 0)` = mean(x > 0), check.names = FALSE))
}

## Posterior predictive replicates with NEW random effects ("mixed" predictive
## replication; Gelman, Meng & Stern, 1996; Marshall & Spiegelhalter, 2003):
## every effect size gets a new effect-size-level effect, and with
## level = "paper" every paper a new paper-level effect too, plus correlated
## sampling error from the fit's own V.  Replicating at the fitted random effects
## would reproduce the data almost exactly, and a check built on that could not
## fail.  brms reuses the fit's V (data2), which is valid only because the new
## data are the fitted data in the same order with new labels.
bmlMixedReplicates <- function(fit, level = c("es", "paper"), seed = 4242, ndraws = 2000) {
  level <- match.arg(level)
  nd <- fit$data
  nd$ID <- paste0("new_es_", seq_len(nrow(nd)))
  if (level == "paper") nd$Paper <- paste0("new_paper_", as.character(nd$Paper))
  ## brms takes no seed here, so set R's for this call and restore the session's
  ## random-number state afterwards
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = globalenv())
  on.exit(if (had) assign(".Random.seed", old, envir = globalenv())
          else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
            rm(".Random.seed", envir = globalenv()))
  set.seed(seed)
  brms::posterior_predict(fit, newdata = nd, allow_new_levels = TRUE,
                          sample_new_levels = "gaussian", ndraws = ndraws)
}

## Observed statistics (mean, SD, min, max of the effect sizes) against their
## replicated distributions, overall and per moderator level.  The posterior
## predictive p is P(T(yrep) >= T(y)): values near 0 or 1 flag misfit.
bmlPpcTable <- function(fit, yrep) {
  y <- fit$data$es
  group <- if ("moderator" %in% names(fit$data)) fit$data$moderator else NULL
  stats <- list(mean = mean, SD = stats::sd, min = min, max = max)
  one <- function(idx, label) do.call(rbind, lapply(names(stats), function(s) {
    f <- stats[[s]]; obs <- f(y[idx]); rep <- apply(yrep[, idx, drop = FALSE], 1, f)
    data.frame(Group = label, Statistic = s, Observed = obs, `Replicated mean` = mean(rep),
               `2.5%` = unname(stats::quantile(rep, .025)),
               `97.5%` = unname(stats::quantile(rep, .975)),
               `ppp` = mean(rep >= obs), check.names = FALSE, row.names = NULL)
  }))
  out <- one(seq_along(y), "all effect sizes")
  if (!is.null(group))
    for (g in levels(droplevels(factor(group)))) {
      idx <- which(group == g)
      if (length(idx) >= 2) out <- rbind(out, one(idx, g))   # an SD needs two values
    }
  out
}

## the effect sizes' known sampling variances: the diagonal of V in the CHE
## model (where var is not a model variable), else the se() term's variable
bmlSamplingVar <- function(fit) {
  if (!is.null(fit$data2$V)) diag(as.matrix(fit$data2$V))
  else as.numeric(fit$data$var)
}

## Each effect size's observed value and 95% sampling interval beside its
## posterior (shrunken) true effect and 95% credible interval
bmlShrinkage <- function(fit) {
  th <- brms::posterior_epred(fit)
  d  <- fit$data
  data.frame(ID = as.character(d$ID), Paper = as.character(d$Paper),
             Group = if ("moderator" %in% names(d)) as.character(d$moderator) else "",
             observed = d$es, se = sqrt(bmlSamplingVar(fit)),
             posterior = colMeans(th),
             lower = apply(th, 2, stats::quantile, .025),
             upper = apply(th, 2, stats::quantile, .975),
             stringsAsFactors = FALSE, row.names = NULL)
}

## Forest plot of bmlShrinkage(): each effect size's observed value with its 95%
## sampling interval (open circle, grey) and its posterior estimate with its 95%
## credible interval (filled circle, black), grouped by moderator level if any
bmlShrinkageForest <- function(sh) {
  sh <- sh[order(sh$Group, sh$Paper, sh$observed), ]
  ## one row per effect size, labelled by its paper (repeats made unique with
  ## invisible trailing spaces, so a paper's rows share its label)
  lab <- ave(sh$Paper, sh$Paper, FUN = function(x) paste0(x, strrep(" ", seq_along(x) - 1)))
  sh$row <- factor(lab, levels = rev(unique(lab)))
  g <- ggplot(sh, aes(y = row)) +
    geom_vline(xintercept = 0, color = "grey70") +
    geom_linerange(aes(xmin = observed - 1.96 * se, xmax = observed + 1.96 * se),
                   color = "grey60", position = position_nudge(y = 0.2)) +
    geom_point(aes(x = observed), shape = 1, color = "grey40", size = 1.3,
               position = position_nudge(y = 0.2)) +
    geom_linerange(aes(xmin = lower, xmax = upper), color = "black",
                   position = position_nudge(y = -0.2)) +
    geom_point(aes(x = posterior), shape = 19, size = 1.3,
               position = position_nudge(y = -0.2)) +
    labs(x = "Hedges' g", y = NULL,
         caption = paste("open circle, grey: observed effect size and 95% sampling interval;",
                         "filled circle, black: posterior estimate and 95% credible interval")) +
    theme_minimal(base_size = 9) +
    theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank())
  if (any(nzchar(sh$Group)))
    g <- g + facet_grid(Group ~ ., scales = "free_y", space = "free_y") +
      theme(strip.text.y = element_text(angle = 0, hjust = 0, face = "bold"))
  g
}

## Prior, posterior and implied likelihood for one parameter with a
## Normal(priorMean, priorSd) prior.  The likelihood is the normal whose
## precision is the posterior's minus the prior's (a normal approximation; the
## same bookkeeping as a "share of the posterior precision from the prior").  If
## the posterior is no more precise than the prior there is no implied
## likelihood, and the plot says so.  Returns a ggplot with the numbers as the
## attribute "values".
bmlTriplot <- function(draws, priorMean, priorSd, xlab) {
  m <- mean(draws); s2 <- stats::var(draws)
  precLik <- 1 / s2 - 1 / priorSd^2
  likSd <- if (precLik > 0) sqrt(1 / precLik) else NA_real_
  likMean <- if (precLik > 0) (m / s2 - priorMean / priorSd^2) / precLik else NA_real_
  ## the window frames the posterior and likelihood; a wide prior then shows as
  ## the nearly flat curve it is over that range
  span <- 5 * max(sqrt(s2), if (is.finite(likSd)) likSd else 0)
  ctr <- if (is.finite(likMean)) mean(c(m, likMean)) else m
  xs <- seq(ctr - span, ctr + span, length.out = 512)
  pd <- stats::density(draws, n = 512)
  curves <- rbind(
    data.frame(x = xs, y = stats::dnorm(xs, priorMean, priorSd), Curve = "prior"),
    if (is.finite(likSd))
      data.frame(x = xs, y = stats::dnorm(xs, likMean, likSd),
                 Curve = "likelihood (normal approximation)"),
    data.frame(x = pd$x, y = pd$y, Curve = "posterior"))
  curves$Curve <- factor(curves$Curve, levels = c("prior", "likelihood (normal approximation)",
                                                  "posterior"))
  curves <- curves[curves$x >= min(xs) & curves$x <= max(xs), ]
  g <- ggplot(curves, aes(x, y, linetype = Curve, linewidth = Curve)) +
    geom_vline(xintercept = 0, color = "grey70") +
    geom_line() +
    scale_linetype_manual(values = c(prior = "dotted",
                                     "likelihood (normal approximation)" = "dashed",
                                     posterior = "solid"), drop = FALSE) +
    scale_linewidth_manual(values = c(prior = 0.7, "likelihood (normal approximation)" = 0.7,
                                      posterior = 1.1), drop = FALSE) +
    labs(x = xlab, y = "density", linetype = NULL, linewidth = NULL,
         caption = if (!is.finite(likSd))
           "The posterior is no more precise than the prior: no implied likelihood to draw.") +
    theme_minimal(base_size = 11) + theme(legend.position = "top")
  attr(g, "values") <- c(posteriorMean = m, posteriorSd = sqrt(s2), priorMean = priorMean,
                         priorSd = priorSd, likelihoodMean = likMean, likelihoodSd = likSd,
                         priorShareOfPrecision = (1 / priorSd^2) / (1 / s2))
  g
}

## The prior of a triplot target: the overall mean or a level's mean has the µ
## prior; the difference of two level means has Normal(0, sqrt(2) * prior SD),
## since every level mean has the same independent prior.
bmlTriplotPrior <- function(target, mupriormean, mupriorsd) {
  if (identical(target, "contrast")) c(mean = 0, sd = sqrt(2) * mupriorsd)
  else c(mean = mupriormean, sd = mupriorsd)
}
