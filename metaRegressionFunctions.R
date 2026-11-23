# include this along with the other helper functions

##############################################################################
################ Multilevel (CHE) model fitter ###############################
# Fits the correlated-and-hierarchical-effects (CHE) working model for a set of
# UNaggregated effect-size rows (Pustejovsky & Tipton, 2022):
#   * within-paper sampling errors are assumed correlated at `rho` -- the same
#     assumption the ID/Papers aggregation makes via agg(..., cor = rho)
#     (imputed with metafor::vcalc, since the true correlations are unknown).
#     rho is essentially never reported by primary studies, so it is a working
#     assumption, not an estimate: the app defaults it to the conventional 0.5
#     and exposes it as a sidebar slider for sensitivity analysis;
#   * random effects ~ 1 | Paper/ID estimate between-paper and within-paper
#     heterogeneity (a three-level model);
#   * coefficient tests and confidence intervals are cluster-robust (CR2 with
#     Satterthwaite degrees of freedom, via metafor::robust + clubSandwich),
#     so the inference does not lean on the imputed rho.
# The returned object keeps class rma.mv (plus robust.rma), so forest(),
# funnel(), predict(), sigma2, QE etc. all work as for a plain rma.mv fit.
# `mods` optionally adds a moderator formula (e.g. ~ moderator).
# `sharing` optionally supplies a sharing table (readSharingTable(), in
# HelperFunctions.R): only effect sizes that share data are then correlated at
# rho, instead of every within-paper pair (see buildSamplingV()).
fitMultilevelCHE <- function(d, sl = d$study, rho = 0.5, mods = NULL, sharing = NULL) {
  V <- buildSamplingV(d, rho = rho, sharing = sharing)
  if (!is.null(sharing)) {
    assertVAligned(V, d)
    V <- matrix(as.numeric(V), nrow(d), nrow(d))   # plain matrix for rma.mv
  }
  # rma.mv() rejects an explicitly NULL mods argument, so branch instead
  model <- if (is.null(mods)) {
    metafor::rma.mv(yi = es, V = V, random = ~ 1 | Paper/ID, data = d, slab = sl)
  } else {
    metafor::rma.mv(yi = es, V = V, mods = mods,
                    random = ~ 1 | Paper/ID, data = d, slab = sl)
  }
  # robust() evaluates `cluster` inside the model's data, so use the bare column name
  metafor::robust(model, cluster = Paper, clubSandwich = TRUE)
}

## The CHE model with one mean per moderator level (cell-means coding,
## ~ 0 + moderator): all levels in ONE model, sharing its heterogeneity and its
## sampling covariance, with cluster-robust (CR2) inference.  Used for the
## multilevel regression's per-level estimates (forestByGroup(), cheCellMeans())
## and contrasts (cheContrast()).  `moderator` is a vector of the rows' levels.
fitCheCellMeans <- function(d, moderator, rho = 0.5, sharing = NULL, slab = NULL) {
  d$moderator <- droplevels(factor(as.character(moderator)))
  # (robust() cannot form its omnibus test that ALL cell means are zero when there
  # are few papers per level; that test is not used here, so its warning is muffled)
  m <- withCallingHandlers(
    fitMultilevelCHE(d, sl = if (is.null(slab)) d$study else slab, rho = rho,
                     mods = ~ 0 + moderator, sharing = sharing),
    warning = function(w) if (grepl("omnibus Wald test", conditionMessage(w)))
      invokeRestart("muffleWarning"))
  # with cell-means coding the coefficients are in the factor's level order
  stopifnot(identical(rownames(m$beta), paste0("moderator", levels(d$moderator))))
  m
}

## Each level's estimated mean from fitCheCellMeans(): estimate, CR2 95% CI,
## Satterthwaite df and the 95% prediction interval for a new effect size from a
## new paper at that level (both heterogeneity components added to the estimate's
## variance), plus the numbers of papers and effect sizes.  A level whose df cannot
## be computed (a single paper) gets no intervals (NA).
cheCellMeans <- function(d, moderator, rho = 0.5, sharing = NULL, m = NULL) {
  if (is.null(m)) m <- fitCheCellMeans(d, moderator, rho, sharing)
  lv <- levels(droplevels(factor(as.character(moderator))))
  est <- as.numeric(m$beta); se <- sqrt(diag(as.matrix(m$vb)))
  df <- as.numeric(m$ddf)
  tc <- ifelse(is.finite(df) & df > 0, stats::qt(.975, pmax(df, 1e-8)), NA_real_)
  sp <- sqrt(se^2 + sum(m$sigma2))
  data.frame(Level = lv,
             Papers = vapply(lv, function(l) length(unique(d$Paper[as.character(moderator) == l])), 1L),
             `Effect sizes` = as.vector(table(factor(as.character(moderator), levels = lv))),
             Estimate = est, `95% CI lower` = est - tc * se, `95% CI upper` = est + tc * se,
             df = df, `95% PI lower` = est - tc * sp, `95% PI upper` = est + tc * sp,
             check.names = FALSE, row.names = NULL)
}

## Difference between two moderator levels' means (levelA - levelB) in the CHE
## model with cell-means coding (fitCheCellMeans()): estimate, SE, CI and test with
## cluster-robust (CR2) standard errors and Satterthwaite degrees of freedom.
## NULL if either level is absent from these rows.
cheContrast <- function(d, moderator, levelA, levelB, rho = 0.5, sharing = NULL) {
  lv <- levels(droplevels(factor(as.character(moderator))))
  if (!all(c(levelA, levelB) %in% lv) || identical(levelA, levelB)) return(NULL)
  m <- fitCheCellMeans(d, moderator, rho, sharing)
  cv <- rep(0, length(lv)); cv[match(levelA, lv)] <- 1; cv[match(levelB, lv)] <- -1
  a <- stats::anova(m, X = matrix(cv, nrow = 1))
  est <- as.numeric(a$Xb); se <- as.numeric(a$se); df <- as.numeric(a$ddf)
  data.frame(Contrast = paste(levelA, "\u2212", levelB), Estimate = est, SE = se,
             `95% CI lower` = est - stats::qt(.975, df) * se,
             `95% CI upper` = est + stats::qt(.975, df) * se,
             df = df, t = est / se, p = as.numeric(a$pval), check.names = FALSE)
}

## The standard two-level random-effects model on AGGREGATED data with one mean per
## moderator level (cell-means coding, ~ 0 + moderator) and ONE between-study
## variance tau^2 shared by all levels, with Knapp-Hartung tests and intervals
## (scaling factor truncated at 1, test = "adhoc").  The Meta-Regression tab's
## subgroup estimates (forestByGroup()), its table (rmaCellMeans()), its test of
## subgroup differences and its contrast (rmaContrast()) all come from this model.
fitRmaCellMeans <- function(MA, moderator, slab = NULL) {
  d <- data.frame(es = as.numeric(MA$es), var = as.numeric(MA$var),
                  moderator = droplevels(factor(as.character(moderator))))
  m <- metafor::rma(es, var, mods = ~ 0 + moderator, data = d, test = "adhoc",
                    slab = if (is.null(slab)) seq_len(nrow(d)) else slab)
  stopifnot(identical(rownames(m$beta), paste0("moderator", levels(d$moderator))))
  m
}

## Each level's estimated mean from fitRmaCellMeans(): estimate, Knapp-Hartung 95%
## CI and df, the 95% prediction interval for a new study at that level (with the
## shared tau^2), and, for comparison, the heterogeneity of the level's studies on
## their own (a separate random-effects fit within the level: tau^2, I^2, Q and its
## p; NA for a level with a single study).
rmaCellMeans <- function(MA, moderator, m = NULL) {
  if (is.null(m)) m <- fitRmaCellMeans(MA, moderator)
  mod <- as.character(moderator)
  lv <- levels(droplevels(factor(mod)))
  est <- as.numeric(m$beta); se <- sqrt(diag(as.matrix(m$vb)))
  df <- rep(as.numeric(m$ddf)[1], length(lv))
  tc <- ifelse(is.finite(df) & df > 0, stats::qt(.975, pmax(df, 1e-8)), stats::qnorm(.975))
  sp <- sqrt(se^2 + m$tau2)
  sep <- lapply(lv, function(l) {
    i <- mod == l
    if (sum(i) < 2) return(NULL)
    tryCatch(metafor::rma(as.numeric(MA$es[i]), as.numeric(MA$var[i])), error = function(e) NULL)
  })
  sepStat <- function(f) vapply(sep, function(x) if (is.null(x)) NA_real_ else as.numeric(f(x)), 1)
  data.frame(Level = lv, Studies = as.vector(table(factor(mod, levels = lv))),
             Estimate = est, `95% CI lower` = est - tc * se, `95% CI upper` = est + tc * se,
             df = df, `95% PI lower` = est - tc * sp, `95% PI upper` = est + tc * sp,
             `This level alone: tau^2` = sepStat(function(x) x$tau2),
             `This level alone: I^2 (%)` = sepStat(function(x) x$I2),
             `This level alone: Q` = sepStat(function(x) x$QE),
             `This level alone: p (Q)` = sepStat(function(x) x$QEp),
             check.names = FALSE, row.names = NULL)
}

## The same contrast in the standard two-level random-effects model on AGGREGATED
## data (the Meta-Regression tab's model): rma() with one mean per level
## (~ 0 + moderator) and Knapp-Hartung tests and intervals with the scaling factor
## truncated at 1 (test = "adhoc").  NULL if either level is absent.
rmaContrast <- function(MA, moderator, levelA, levelB) {
  lv <- levels(droplevels(factor(as.character(moderator))))
  if (!all(c(levelA, levelB) %in% lv) || identical(levelA, levelB)) return(NULL)
  m <- fitRmaCellMeans(MA, moderator)
  cv <- rep(0, length(lv)); cv[match(levelA, lv)] <- 1; cv[match(levelB, lv)] <- -1
  a <- stats::anova(m, X = matrix(cv, nrow = 1))
  est <- as.numeric(a$Xb); se <- as.numeric(a$se)
  df <- if (length(a$ddf) && is.finite(as.numeric(a$ddf)[1])) as.numeric(a$ddf)[1] else Inf
  data.frame(Contrast = paste(levelA, "\u2212", levelB), Estimate = est, SE = se,
             `95% CI lower` = est - stats::qt(.975, df) * se,
             `95% CI upper` = est + stats::qt(.975, df) * se,
             df = df, t = est / se, p = as.numeric(a$pval), check.names = FALSE)
}

## The same contrast from a bayesmeta bmr() fit with cell-means coding (the Bayesian
## Meta-Regression tab's model).  bmr's posterior is a mixture over a grid of tau
## values of normal distributions for the coefficients ($support: weights,
## conditional means and covariances), so a difference of two level means has an
## exact mixture-of-normals posterior: its mean and SD in closed form, its quantiles
## and P(> 0) from the mixture's CDF, with no simulation.  `moderator` is the
## moderator column of the data the model was fitted to: bmr() renames levels
## (make.names: "music+noise" becomes "music.noise"), so the levels are taken from
## the moderator with bmrLevels(), the same function buildBmrModel() uses to name the
## coefficients, and must match the fit's coefficient names exactly after make.names
## and in the same order.  NULL if they do not, or if either level is not among them.
bmrContrast <- function(fit, moderator, levelA, levelB, type = "hdi") {
  lv <- bmrLevels(moderator)
  if (!identical(make.names(lv), as.character(fit$variables)) ||
      !all(c(levelA, levelB) %in% lv) || identical(levelA, levelB)) return(NULL)
  cv <- rep(0, length(lv)); cv[match(levelA, lv)] <- 1; cv[match(levelB, lv)] <- -1
  s  <- fit$support
  w  <- s$weight / sum(s$weight)
  mu <- as.numeric(s$mean %*% cv)
  sd <- sqrt(vapply(seq_along(w), function(i) as.numeric(t(cv) %*% s$covariance[i, , ] %*% cv),
                    numeric(1)))
  m  <- sum(w * mu)
  cdf <- function(x) sum(w * stats::pnorm(x, mu, sd))
  ## the 95% interval of the chosen kind from the mixture's CDF (cdfInterval())
  iv <- cdfInterval(cdf, c(min(mu - 8 * sd), max(mu + 8 * sd)), type)
  tab <- data.frame(Contrast = paste(levelA, "\u2212", levelB), `Posterior mean` = m,
                    SD = sqrt(sum(w * (sd^2 + mu^2)) - m^2),
                    lower = iv[["lower"]], upper = iv[["upper"]],
                    `P(> 0)` = 1 - cdf(0), check.names = FALSE)
  names(tab)[4:5] <- intervalNames(type)
  tab
}

## A bmr() fit with its reported 95% intervals (tau and each coefficient, the
## summary rows "95% lower" / "95% upper") of the chosen kind; nothing refitted.
bmrIntervals <- function(fit, type = "hdi") {
  m <- intervalMethod(type)
  if (identical(fit$interval.type, m)) return(fit)
  r <- c("95% lower", "95% upper")
  fit$summary[r, "tau"] <- fit$post.interval(tau.level = .95, method = m)
  for (j in seq_along(fit$variables))
    fit$summary[r, fit$variables[j]] <- fit$post.interval(beta.level = .95, which.beta = j, method = m)
  fit$interval.type <- m
  fit
}
bmrSummaryTable <- function(fit, type = "hdi") {
  s <- bmrIntervals(fit, type)$summary
  rownames(s)[match(c("95% lower", "95% upper"), rownames(s))] <- intervalNames(type)
  s
}

##############################################################################
################ Functions for meta-regression ###############################

############## a little helper function to add Q-test, I^2, and tau^2 estimate info
# (from https://www.metafor-project.org/doku.php/plots:forest_plot_with_subgroups)
# For multilevel rma.mv models (no I^2/tau^2), the two variance components
# (between papers, within papers) are shown instead.
mlabfun <- function(text, x) {
  if (inherits(x, "rma.mv")) {
    return(list(bquote(paste(.(text),
                    " (Q = ", .(fmtx(x$QE, digits=2)),
                    ", df = ", .(x$k - x$p), ", ",
                    .(fmtp(x$QEp, digits=3, pname="p", add0=TRUE, sep=TRUE, equal=TRUE)), "; ",
                    sigma[paper]^2, " = ", .(fmtx(x$sigma2[1], digits=2)), ", ",
                    sigma[ES]^2, " = ", .(fmtx(x$sigma2[2], digits=2)), ")"))))
  }
  list(bquote(paste(.(text),
                    " (Q = ", .(fmtx(x$QE, digits=2)),
                    ", df = ", .(x$k - x$p), ", ",
                    .(fmtp(x$QEp, digits=3, pname="p", add0=TRUE, sep=TRUE, equal=TRUE)), "; ",
                    I^2, " = ", .(fmtx(x$I2, digits=1)), "%, ",
                    tau^2, " = ", .(fmtx(x$tau2, digits=2)), ")")))}


##################### function to create a forest plot with a categorical moderator
###### start of forestByGroup() function
forestByGroup <- function(MA=MA, moderator, slab=MA$study, cex=1,
                          header="Study",addpred=TRUE, caterpillar=F,
                          multilevel=FALSE, rho=0.5, sharing=NULL,
         ##                 col="black", border="black",
                          ...) {
  ## additional arguments that might be useful: xlim, psize, xlab
  ## For caterpillar plot, use caterpillar=TRUE (not slab=NA as you would in forest() )
  ## You can also add any other arguments that forest() allows
  # MA is the dataframe with yi=es, vi=var. Note it is not a model object like x is for forest()
  # moderator is a vector of the same length as MA, such as MA$moderatorFactor
  # multilevel: fit three-level rma.mv models (effect sizes nested in papers;
  # MA must then hold the UNaggregated rows with Paper and ID columns).
  # Which model each part of the plot comes from:
  #   * each study's row: the data;
  #   * the bottom diamond: a model for all studies WITHOUT the moderator;
  #   * each subgroup diamond (and its prediction interval) and the test for
  #     subgroup differences: ONE "shared" model with a mean per level and one
  #     set of heterogeneity variances for all levels -- fitCheCellMeans() (CR2
  #     inference) when multilevel, fitRmaCellMeans() (Knapp-Hartung) otherwise;
  #   * (two-level only) the line under each subgroup diamond: that subgroup's own
  #     heterogeneity, from a separate fit within the subgroup, for description.
  # rho: assumed within-paper sampling correlation, used only when multilevel
  # sharing: optional sharing table (see buildSamplingV()), used only when multilevel

  # model fitter shared by the overall model and the subgroup models
  fitOne <- function(d, sl) {
    if (multilevel) fitMultilevelCHE(d, sl, rho = rho, sharing = sharing)   # CHE + cluster-robust inference
    else rma(d$es, d$var, slab = sl, test = "adhoc")       # Knapp-Hartung, truncated at 1
  }
  overallLabel  <- if (multilevel) "Multilevel model for all studies, no moderator"
                   else "RE model for all studies, no moderator"
  #
  fma <- fitOne(MA, slab)
  #
  # if the moderator is not a factor, make it one
  if (!is.factor(moderator)) {
    moderator <- as.factor(as.character(moderator))
  }
  # drop any empty subgroups so they are left out of the plot.
  # (e.g. a moderator group whose studies were all deselected in the
  #  selection tab still lingers as an empty factor level, which would
  #  otherwise make rma() fail on a subgroup with zero data points)
  moderator <- droplevels(moderator)

  # calculate some things for the height of the plot
  #
  ### Things you might want to change:
  spaceBelow <- 2
  spaceAbove <- 2
  groupSpace <-  spaceBelow + spaceAbove     # space around each group for group summary; 2 above and 2 below
  belowGroup <- -1.5   # how far below the group data to put the group summary
  belowPlot <-  -2.5     # how far below plot to put the meta-regression summary
  topSpace  <-  2      # how much room to leave at the top for labels
  #
  ### Things you probably do not want to change:
  nDataPoints <- length(fma$yi)
  GroupNames <- levels(moderator)  
  nGroups <- length(levels(moderator)) 
  totalHeight <- nDataPoints + (nGroups * groupSpace) + (topSpace)
  # need the number in each group; top and bottom row for each group
  groupSizes <- table(moderator)          # number in each level of GroupNames

  # every subgroup's estimate, CI and prediction interval from the shared model
  # (rows in level order)
  cellMeans <- if (multilevel) {
    cheCellMeans(MA, moderator, m = fitCheCellMeans(MA, moderator, rho = rho,
                                                    sharing = sharing, slab = slab))
  } else rmaCellMeans(MA, moderator, m = fitRmaCellMeans(MA, moderator, slab = slab))
  # calculate row numbers and models for subgroups by looping over subgroups:
  # initialize values for the loop
  groupLabelRow <- integer(nGroups)
  groupModelRow <- integer(nGroups)
  lineNum <-  spaceBelow + 1 # initialize the line of the forest plot to the row for the first ES
  rowsString <- "c("    # initialize the string
  #
  for(i in 1:nGroups) {
    groupname <- GroupNames[i]
    groupsize <- groupSizes[i]
    y <- (lineNum + groupsize - 1)
    rowsString <- paste0(rowsString, lineNum, ":", y)
    if(i < nGroups) rowsString <- paste0(rowsString, ", ")
    else rowsString <- paste0(rowsString, ")")
    groupLabelRow[i] <- y + 1
    groupModelRow[i] <- lineNum + belowGroup
    # create a model for each group (a multilevel subgroup with too few papers
    # can fail to fit; fall back to the two-level model for that subgroup)
    # update for the next entry
    lineNum <- (y + groupSpace + 1)
  }
  
  rowsVector <- eval(parse(text = rowsString))
 
  # Define the list of arguments for forest()
  args_list <- list(...)  
  args_list$x <- fma
  args_list$cex <- cex
  args_list$ylim <- c(-2, totalHeight)
  args_list$order <- moderator
  args_list$rows <- rowsVector
  args_list$mlab <- mlabfun(overallLabel, fma)
  args_list$header <- header
  args_list$addpred <- addpred
  if(caterpillar) args_list$slab <- NA

  # Call the forest function with the dynamically constructed argument list
  do.call(metafor::forest, args_list)
  
  ### set font expansion factor (as in forest() above) and use a bold font
  op <- par(cex=cex, font=2)
  
  ### switch to bold italic font
  par(font=4)
  ### add text for the subgroups
  usr <- par("usr")
  text(usr[1], groupLabelRow, pos=4, GroupNames, xpd=TRUE)
  
  # restore original plot parameters
  par(op)
  #  dev.off()  resets plot parameters  
  
  # get color arguments for subgroup polygons
  argsForSubs <- list()
  # Check if col and border are in args_list
  if ("col" %in% names(args_list)) {
    argsForSubs$col <- args_list$col
  }
  if ("border" %in% names(args_list)) {
    argsForSubs$border <- args_list$border
  }

  # add models for subgroups
  for (i in 1:nGroups) {
    # Define the base arguments for addpoly
    baseArgs <- if (multilevel) {
      cm <- cellMeans[i, ]
      noCI <- is.na(cm[["95% CI lower"]])      # a single paper: no CR2 interval
      c(list(x = cm$Estimate,
             ci.lb = if (noCI) cm$Estimate else cm[["95% CI lower"]],
             ci.ub = if (noCI) cm$Estimate else cm[["95% CI upper"]],
             rows = groupModelRow[i], xpd = TRUE,
             mlab = if (noCI) "Mean in the shared model (one paper: no interval)"
                    else bquote(paste("Mean in the shared model (CR2, df = ",
                                      .(fmtx(cm$df, digits = 1)), ")"))),
        if (addpred && !noCI) list(pi.lb = cm[["95% PI lower"]], pi.ub = cm[["95% PI upper"]]))
    } else {
      cm <- cellMeans[i, ]
      c(list(x = cm$Estimate, ci.lb = cm[["95% CI lower"]], ci.ub = cm[["95% CI upper"]],
             rows = groupModelRow[i], xpd = TRUE,
             mlab = list(bquote(paste("Mean in the shared model (one ", tau^2,
                                      " for all subgroups; Knapp-Hartung, df = ", .(cm$df), ")")))),
        if (addpred) list(pi.lb = cm[["95% PI lower"]], pi.ub = cm[["95% PI upper"]]))
    }
    # Combine the base arguments with the additional arguments
    combinedArgs <- c(baseArgs, argsForSubs)
    # Call addpoly with the combined arguments
    do.call(addpoly, combinedArgs)
    # two-level: the subgroup's own heterogeneity (separate fit), for description
    if (!multilevel) {
      cm <- cellMeans[i, ]
      txt <- if (is.na(cm[["This level alone: Q"]])) "This subgroup alone: a single study"
        else bquote(paste("This subgroup alone (separate fit): Q = ",
                          .(fmtx(cm[["This level alone: Q"]], digits = 2)), ", df = ", .(cm$Studies - 1), ", ",
                          .(fmtp(cm[["This level alone: p (Q)"]], digits = 3, pname = "p", add0 = TRUE,
                                 sep = TRUE, equal = TRUE)), "; ",
                          I^2, " = ", .(fmtx(cm[["This level alone: I^2 (%)"]], digits = 1)), "%, ",
                          tau^2, " = ", .(fmtx(cm[["This level alone: tau^2"]], digits = 2))))
      text(usr[1], groupModelRow[i] - 1, pos = 4, cex = cex * 0.85, font = 3, xpd = TRUE, txt)
    }
  }
  

  # add model testing moderator (multilevel: CHE model, so QM/QMp below are the
  # cluster-robust F test of the subgroup differences; two-level: the F test
  # from the ad hoc Knapp-Hartung adjustment)
  fmaMod <- if (multilevel) {
    fitMultilevelCHE(MA, mods = ~ moderator, rho = rho, sharing = sharing)
  } else {
    rma(MA$es, MA$var, mods = ~ moderator, test = "adhoc")
  }
  #
  ### add text for the test of subgroup differences (an F test with two df
  ### unless the model used z tests, in which case QM is chi-square)
  testStat <- if (fmaMod$test == "z") {
    bquote(paste(Q[M], " = ", .(fmtx(fmaMod$QM, digits=2)), ", df = ", .(fmaMod$QMdf[1])))
  } else {
    bquote(paste(F, "(", .(fmaMod$QMdf[1]), ", ", .(fmtx(fmaMod$QMdf[2], digits=1)), ") = ",
                 .(fmtx(fmaMod$QM, digits=2))))
  }
  text(usr[1], belowPlot, pos=4, cex=cex, xpd=TRUE,
       bquote(paste("Test for subgroup differences (shared model): ", .(testStat), ", ",
                    .(fmtp(fmaMod$QMp, digits=2, pname="p", add0=TRUE, sep=TRUE, equal=TRUE)))))
}
###### end of forestByGroup() function


##################### Bayesian meta-regression with one categorical moderator
###### start of buildBmrModel() function
# Fits a Bayesian meta-regression using bmr() from the bayesmeta package.
# Uses cell-means coding (~ moderator - 1) so that each regression coefficient
# is the pooled posterior effect for one level of the moderator, giving a
# per-group estimate + 95% credible interval that parallels the frequentist
# subgroup forest plot in forestByGroup().
#
# The tau prior is mapped exactly as in the non-moderator bayesmeta() model
# (see the bma() reactive in server.R) so priors are consistent across tabs.
# The chosen mu prior (mupriormean / mupriorsd) is applied to every group's
# coefficient via bmr()'s beta prior.
## The moderator as the factor buildBmrModel() fits: empty levels dropped (same fix
## as forestByGroup()), level order kept if it already is a factor.  bmrContrast()
## uses the same function, so its levels line up with the coefficients.
bmrModeratorFactor <- function(moderator) {
  if (!is.factor(moderator)) moderator <- as.factor(as.character(moderator))
  droplevels(moderator)
}
bmrLevels <- function(moderator) levels(bmrModeratorFactor(moderator))

buildBmrModel <- function(MA, moderatorName, tauprior, scaletau, mupriormean, mupriorsd) {
  moderator <- bmrModeratorFactor(MA[[moderatorName]])

  # cell-means design matrix: one coefficient per moderator level
  X <- model.matrix(~ moderator - 1)
  colnames(X) <- levels(moderator)     # clean names to just the level labels
  betaMean <- rep(mupriormean, ncol(X))
  betaSD   <- rep(mupriorsd,   ncol(X))

  if (tauprior == "Half cauchy") {
    bmr(y = MA$es, sigma = sqrt(MA$var), labels = MA$study, X = X,
        tau.prior = function(t) dhalfcauchy(t, scale = scaletau),
        beta.prior.mean = betaMean, beta.prior.sd = betaSD)
  } else if (tauprior == "Half normal") {
    bmr(y = MA$es, sigma = sqrt(MA$var), labels = MA$study, X = X,
        tau.prior = function(t) dhalfnormal(t, scale = scaletau),
        beta.prior.mean = betaMean, beta.prior.sd = betaSD)
  } else {
    bmr(y = MA$es, sigma = sqrt(MA$var), labels = MA$study, X = X,
        tau.prior = tauprior,
        beta.prior.mean = betaMean, beta.prior.sd = betaSD)
  }
}
###### end of buildBmrModel() function


##################### Forest plot for a Bayesian meta-regression (bmr) model
###### start of forestBmrByGroup() function
# Draws a forest plot for a bmr() model that is laid out like the frequentist
# forestByGroup() plot: the individual studies are grouped WITHIN each level of
# the moderator, and each group gets a Bayesian posterior summary polygon
# (posterior mean + 95% credible interval, taken from the bmr model that was fit
# with cell-means coding, so each coefficient is that group's pooled effect).
#
#   bmr       : a model returned by buildBmrModel() (cell-means coded)
#   MA        : the data frame with es, var, study (same MA used to fit bmr)
#   moderator : the moderator vector (same length as MA), e.g. MA[[moderatorName]]
#
# Only observed study effect sizes are plotted (as in forestByGroup); no overall
# pooled polygon is drawn, because a cell-means meta-regression has a per-group
# estimate rather than a single overall effect.
#   efac      : vertical expansion factor for the CI arrow ends and the summary
#               diamonds, passed straight through to metafor.  metafor's default
#               of 1 is too tall for the close row spacing here; the Bayesian
#               meta-regression tab exposes this as a "Symbol size" slider
#               (default 0.3).  Larger = taller symbols; useful when a plot has
#               few studies, smaller when it has many.
forestBmrByGroup <- function(bmr, MA, moderator, slab = MA$study, cex = 1,
                             header = "Study", efac = 0.3, ...) {
  # match the group structure used when the model was fit
  if (!is.factor(moderator)) moderator <- as.factor(as.character(moderator))
  moderator  <- droplevels(moderator)
  GroupNames <- levels(moderator)
  nGroups    <- length(GroupNames)
  groupSizes <- table(moderator)

  # order studies by group so each group's rows are contiguous
  ord      <- order(moderator)
  es       <- MA$es[ord]
  vv       <- MA$var[ord]
  slab     <- slab[ord]

  # layout constants (same as forestByGroup)
  spaceBelow <- 2
  spaceAbove <- 2
  groupSpace <- spaceBelow + spaceAbove
  belowGroup <- -1.5
  topSpace   <- 2
  nDataPoints <- length(es)
  totalHeight <- nDataPoints + (nGroups * groupSpace) + topSpace

  # assign plot rows for each group's studies, its label, and its summary polygon
  rowsVector    <- integer(0)
  groupLabelRow <- integer(nGroups)
  groupModelRow <- integer(nGroups)
  lineNum <- spaceBelow + 1
  for (i in 1:nGroups) {
    y <- lineNum + groupSizes[i] - 1
    rowsVector       <- c(rowsVector, lineNum:y)
    groupLabelRow[i] <- y + 1
    groupModelRow[i] <- lineNum + belowGroup
    lineNum <- y + groupSpace + 1
  }

  # plot the individual observed studies, grouped by moderator level
  # (addfit is not relevant here: forest.default draws no model polygon)
  metafor::forest(x = es, vi = vv, slab = slab, rows = rowsVector,
                  ylim = c(-2, totalHeight), cex = cex, header = header,
                  efac = efac, ...)

  # add the moderator-level labels (bold italic), as in forestByGroup
  op <- par(cex = cex, font = 4)
  usr <- par("usr")
  text(usr[1], groupLabelRow, pos = 4, GroupNames, xpd = TRUE)
  par(op)

  # add a Bayesian posterior summary polygon for each group, using the bmr model's
  # per-group posterior mean and 95% credible interval.
  # bmr() sanitises the coefficient names (e.g. "a+b" -> "a.b") but keeps them in
  # the same order as the moderator levels, so reference the summary columns via
  # bmr$variables[i] while labelling the polygon with the original level GroupNames[i].
  s    <- bmr$summary
  vars <- bmr$variables
  for (i in 1:nGroups) {
    metafor::addpoly(x = s["mean", vars[i]],
                     ci.lb = s["95% lower", vars[i]],
                     ci.ub = s["95% upper", vars[i]],
                     rows = groupModelRow[i],
                     mlab = paste0("Bayesian estimate: ", GroupNames[i]),
                     efac = efac, xpd = TRUE, cex = cex, ...)
  }
}
###### end of forestBmrByGroup() function


##############################################################################
################ End of Functions for meta-regression ########################

