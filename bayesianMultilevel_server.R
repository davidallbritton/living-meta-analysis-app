#######################################################################################
################### A General Tool for Living Meta-Analysis #################
#######################################################################################
# v.1.6 2026.11.22
#
################### Bayesian Multilevel panels (server) ###############################
#
# TWO tabs share this machinery, so the overall and moderator analyses can both
# be fitted, viewed, and kept cached at the same time:
#   * "Bayesian Multilevel"            -- the overall three-level model
#   * "Bayesian Multilevel Regression" -- the same model with cell-means coding
#     for the moderator chosen in "Moderator Selection" (per-group posteriors)
# Model, priors, compilation strategy, and output builders live in
# bayesianMultilevelFunctions.R (sourced from global.R).
#
# SETTINGS SNAPSHOT: both tabs work from myrvs$bayesSnapshot -- the prior and
# moderator settings captured by MA() when "(Re)Calculate Meta-Analysis" was
# pressed.  Changing priors or the moderator therefore has NO effect on these
# tabs until the next recalculation (the app's documented convention); it can
# neither silently swap in a cached model nor pop a confirmation dialog
# mid-session.
#
# These tabs are ALWAYS available (they read the unaggregated dataset MAml(),
# which every recalculation builds alongside the aggregated one -- the
# "Aggregate over" radio does not affect them).  Requirements enforced with
# explanatory messages:
#   * the snapshotted tau prior must be Half cauchy or Half normal, and the mu
#     prior mean and SD must both be filled in (Stan needs proper priors),
#   * the Regression tab additionally needs a moderator selected (at
#     recalculation time).
#
# Computational-cost handling mirrors bma()/bmr(): each tab only builds its
# model while it is open, a NEW (uncached) model needs a confirmation (the
# modal also warns about the one-time Stan compilation), and every fit is
# cached in myrvs$previousBmlModels (with the precomputed fits that come with
# the data added at first use, precomputed_server.R; downloadable in "Saved
# Plots and Models").
#
# This file is sourced (local = T) from server.R.
######################################################################################


## session cache, seeded from the process-wide seed loaded in global.R
## (shared across sessions via copy-on-write, like the other model caches);
## entries are keyed by data + priors + moderatorName ("" = overall model),
## so both tabs' models live side by side in the one cache
myrvs$previousBmlModels <- list()   # precomputed results are added at first use (precomputed_server.R)

## triggers (TRUE only after the user confirms the modal, or on a cache hit);
## both are reset by MA() on every recalculation
myrvs$triggerBml <- FALSE
myrvs$triggerBmlReg <- FALSE

## Is the snapshot usable for the Stan model?  NULL if yes, else a message.
bmlPriorProblem <- function(snap) {
  if (is.null(snap)) return("Press \"(Re)Calculate Meta-Analysis\" first.")
  if (!isTruthy(snap$tauprior) || !snap$tauprior %in% bmlSupportedTauPriors)
    return(paste("The Bayesian multilevel model supports only the proper tau priors",
                 '"Half cauchy" and "Half normal" (with their scale).',
                 'Choose one in the "Prior specifications" tab and press',
                 '"(Re)Calculate Meta-Analysis".'))
  if (is.null(snap$mupriormean) || is.na(snap$mupriormean) ||
      is.null(snap$mupriorsd)   || is.na(snap$mupriorsd))
    return(paste("The Bayesian multilevel model needs a proper µ prior:",
                 'fill in both the µ prior mean and SD in the "Prior specifications"',
                 'tab and press "(Re)Calculate Meta-Analysis".'))
  NULL
}

## cache lookup for a snapshot + moderator choice (moderatorName "" = overall)
bmlCached <- function(MA, snap, moderatorName) {
  checkOldBmlModels(useCache("bml"), MA = MA, tauprior = snap$tauprior,
                    mupriorsd = snap$mupriorsd, scaletau = snap$scaletau,
                    mupriormean = snap$mupriormean, moderatorName = moderatorName,
                    rho = snap$rhoCHE, che = !isTRUE(snap$skipCHE),
                    sharingKey = sharingKey(snap$sharing, MA$ID))
}

## fit (or retrieve) the model for a snapshot + moderator choice, caching new fits
bmlFitOrCache <- function(MA, snap, moderatorName) {
  old_bml <- bmlCached(MA, snap, moderatorName)
  if (isTruthy(old_bml)) return(old_bml)
  d <- bmlData(MA, moderatorName = if (nzchar(moderatorName)) moderatorName else NULL)
  # Stan compilation/sampling can fail on hosted servers (no C++ toolchain or
  # not enough memory); degrade to an explanatory message instead of a raw error
  newbml <- tryCatch(
    fitBayesianMultilevel(d, tauprior = snap$tauprior, scaletau = snap$scaletau,
                          mupriormean = snap$mupriormean, mupriorsd = snap$mupriorsd,
                          ## CHE on by default, matching the frequentist
                          ## multilevel tabs; the sidebar escape hatch turns it off
                          rho = snap$rhoCHE, che = !isTRUE(snap$skipCHE),
                          sharing = snap$sharing),
    error = function(e) e)
  if (inherits(newbml, "error")) {
    ## a sharing table that cannot give a positive-definite covariance at this rho
    ## is a property of the design and the setting, not of the server
    if (grepl("positive definite", conditionMessage(newbml)))
      validate(need(FALSE, paste0("The model was not fitted: ", conditionMessage(newbml))))
    validate(need(FALSE, paste0(
      "The Bayesian multilevel model could not be fit on this server (",
      conditionMessage(newbml), ").  Stan model compilation often fails on hosted ",
      "servers with limited memory; running the app locally supports these tabs.  ",
      "The frequentist multilevel tabs are unaffected.")))
  }
  newrow <- list(MA = MA, tauprior = snap$tauprior, mupriorsd = snap$mupriorsd,
                 scaletau = snap$scaletau, mupriormean = snap$mupriormean,
                 moderatorName = moderatorName, rho = snap$rhoCHE,
                 che = !isTRUE(snap$skipCHE),
                 sharingKey = sharingKey(snap$sharing, MA$ID), bml = newbml)
  myrvs$previousBmlModels[length(myrvs$previousBmlModels) + 1] <- list(newrow)
  newbml
}

## confirmation modal shared by both tabs.  Sets the trigger FALSE first for a
## NEW (uncached) request: assigning the same value to a reactiveValues entry
## does not invalidate dependents, so without the reset a still-TRUE trigger
## would leave a stale model on display after the settings changed.
bmlConfirmModal <- function(triggerName) {
  myrvs[[triggerName]] <- FALSE
  shinyalert(
    title = paste("Are you sure you want to fit this new Bayesian multilevel model?",
                  "Each fit typically takes a few minutes (longer for large selections",
                  "and on the hosted app), and the FIRST fit of each model in a session",
                  "also compiles it (about half a minute extra)."),
    type = "warning",
    showCancelButton = TRUE,
    confirmButtonText = "Yes, continue!",
    cancelButtonText = "No, not right now.",
    callbackR = function(value) {
      myrvs[[triggerName]] <- value
    }
  )
}


## ---- overall model ("Bayesian Multilevel" tab) ----

observe({
  MAml()   # reactive dep: re-check after each recalculation
  req(input$mainTabset == "bayesian_multilevel")
  snap <- myrvs$bayesSnapshot          # reactive dep: changes only at recalculation
  req(is.null(bmlPriorProblem(snap)))
  isolate(old_bml <- bmlCached(MAml(), snap, ""))
  if (!isTruthy(old_bml)) bmlConfirmModal("triggerBml")
  else myrvs$triggerBml <- TRUE
})

bmlModel <- reactive({
  snap <- myrvs$bayesSnapshot
  validate(need(is.null(bmlPriorProblem(snap)), bmlPriorProblem(snap)))
  req(myrvs$triggerBml)
  isolate(bmlFitOrCache(MAml(), snap, ""))
})

## the plots are named outputs with height = "auto" containers, so the tall
## per-paper forest cannot overlap the content below it
output$bmlForestOut <- renderPlot({
  bmlForestPlot(bmlModel(), ciType())
}, height = function() max(400, length(unique(as.character(MAml()$Paper))) * 14 + 120))
output$bmlDensOut <- renderPlot({ bmlDensityPlot(bmlModel()) }, height = 350)

## Prominent in-tab warning when the escape hatch is on.  The sidebar popover is
## easy to miss, and a too-narrow credible interval looks perfectly normal, so the
## caveat has to travel with the results themselves (it also lands in printouts).
bmlSkipWarning <- function(snap) {
  if (!isTRUE(snap$skipCHE)) return(NULL)
  div(style = paste("border: 2px solid #c0392b; background-color: #fdecea;",
                    "padding: 10px; margin-bottom: 12px;"),
      p(tags$b(style = "color:#c0392b;",
               "Preliminary result -- the correlated-errors correction was skipped."),
        style = "margin-bottom: 6px;"),
      p(tags$small(
        "These models assume that effect sizes from the same paper have independent",
        "sampling errors.  Where a paper contributes several effect sizes from the same",
        "participants -- several outcome measures on one sample, repeated post-tests, or",
        "several treatment groups compared against one shared control -- that assumption",
        "is wrong, and the credible intervals below are",
        tags$b("too narrow"), ". The pooled estimate can shift substantially as well.",
        "Use this only for a quick look; re-run with the correction (sidebar:",
        tags$em("Dependence (\u03c1)"), ") on a local machine, or download the generated",
        "script from the Downloads tab, before reporting these numbers."),
        style = "margin-bottom: 0;"))
}

output$bmlContent <- renderUI({
  fit <- bmlModel()
  tagList(
    bmlSkipWarning(myrvs$bayesSnapshot),
    if (!isTRUE(myrvs$bayesSnapshot$skipCHE)) covarianceNote(),
    p(tags$b("Overall model (no moderator), using the priors as of the last recalculation.")),
    p(tags$small(bmlDiagnosticsText(fit))),
    h4("Posterior summary:"),
    renderTable(bmlSummaryTable(fit, ciType()), digits = 3),
    p(tags$small(intervalNote(ciType()))),
    h4("Prediction interval for a new study's true effect:"),
    renderTable(bmlPredictionTable(fit, type = ciType()), digits = 3),
    p(tags$small(bmlPredictionNote, intervalNote(ciType()))),
    h4(paste0("Per-paper posterior estimates (", intervalWords(ciType()), "):")),
    plotOutput("bmlForestOut", height = "auto"),
    h4("Posterior densities:"),
    plotOutput("bmlDensOut", height = "350px"),
    bmlExtrasUI("bml"),
    br()
  )
})


## ---- moderator model ("Bayesian Multilevel Regression" tab) ----

observe({
  MAml()   # reactive dep: re-check after each recalculation
  req(input$mainTabset == "bayesian_multilevel_regression")
  snap <- myrvs$bayesSnapshot          # reactive dep: changes only at recalculation
  req(is.null(bmlPriorProblem(snap)))
  req(nzchar(snap$moderatorName))    # needs a moderator (as of recalculation)
  isolate(old_bml <- bmlCached(MAml(), snap, snap$moderatorName))
  if (!isTruthy(old_bml)) bmlConfirmModal("triggerBmlReg")
  else myrvs$triggerBmlReg <- TRUE
})

bmlRegModel <- reactive({
  snap <- myrvs$bayesSnapshot
  validate(need(is.null(bmlPriorProblem(snap)), bmlPriorProblem(snap)))
  validate(need(nzchar(snap$moderatorName),
                paste('No moderator was selected at the last recalculation.',
                      'Choose "Yes" and a moderator in the "Moderator Selection" tab,',
                      'then press "(Re)Calculate Meta-Analysis".')))
  req(myrvs$triggerBmlReg)
  isolate(bmlFitOrCache(MAml(), snap, snap$moderatorName))
})

output$bmlRegForestOut <- renderPlot({ bmlForestPlot(bmlRegModel(), ciType()) }, height = 300)
output$bmlRegDensOut <- renderPlot({ bmlDensityPlot(bmlRegModel()) }, height = 350)

output$bmlRegContent <- renderUI({
  fit <- bmlRegModel()
  moderatorName <- isolate(myrvs$bayesSnapshot$moderatorName)
  tagList(
    bmlSkipWarning(myrvs$bayesSnapshot),
    if (!isTRUE(myrvs$bayesSnapshot$skipCHE)) covarianceNote(),
    p(tags$b(paste0("Moderator: ", moderatorName,
                    " (per-group posterior means), using the settings as of the last recalculation."))),
    p(tags$small(bmlDiagnosticsText(fit))),
    h4("Posterior summary:"),
    renderTable(bmlSummaryTable(fit, ciType()), digits = 3),
    p(tags$small(intervalNote(ciType()))),
    h4("Contrast between two moderator levels:"),
    uiOutput("bmlRegContrastOut"),
    h4("Prediction intervals for a new study's true effect, per level:"),
    renderTable(bmlPredictionTable(fit, type = ciType()), digits = 3),
    p(tags$small(bmlPredictionNote, intervalNote(ciType()))),
    h4(paste0("Per-group posterior means (", intervalWords(ciType()), "):")),
    plotOutput("bmlRegForestOut", height = "300px"),
    h4("Posterior densities:"),
    plotOutput("bmlRegDensOut", height = "350px"),
    bmlExtrasUI("bmlReg"),
    br()
  )
})


## ---- download / upload / clear for the Saved Plots and Models tab ----
## (one cache holds both tabs' models, so a single set of controls covers both)

output$rds_file.bml <- downloadHandler(
  filename = function() {
    "bayesian_multilevel_models.RDS"
  },
  content = function(file) {
    saveRDS(cacheForDownload("bml"), file)
  }
)

observeEvent(input$SavedBmlModelsUp, {
  newrows_bml <- normalizeTauPriorLabels(readRDS(input$SavedBmlModelsUp$datapath))
  myrvs$previousBmlModels <- c(useCache("bml"), newrows_bml)
})

observeEvent(input$ClearBmlModels, {
  clearCache("bml")
})


## ---- reporting extras shared by both tabs (v1.6) ----

bmlPredictionNote <- paste(
  "The range in which the true effect of a new effect size from a new paper is expected",
  "to fall with 95% probability: the posterior of the mean plus both sources of",
  "heterogeneity (between papers and within papers).  Computed exactly from the posterior",
  "draws, without further simulation.")

## the extras section: shrinkage forest, triplot, and posterior predictive checks
## on request.  prefix "bml" = overall tab (model bmlModel()), "bmlReg" =
## regression tab (bmlRegModel()).
bmlExtrasUI <- function(prefix) {
  tagList(
    h4("Observed and shrunken estimates:"),
    p(tags$small("Each effect size's observed value with its 95% sampling interval (open",
                 "circle, grey), and the model's posterior estimate of its true effect with",
                 "its 95% credible interval (filled circle, black).")),
    plotOutput(paste0(prefix, "ShrinkOut"), height = "auto"),
    h4("Prior, likelihood and posterior:"),
    if (prefix == "bmlReg") uiOutput("bmlRegTriplotChooser"),
    plotOutput(paste0(prefix, "TriplotOut"), height = "350px"),
    uiOutput(paste0(prefix, "TriplotNote")),
    h4("Posterior predictive checks:"),
    p(tags$small("Replicates the data from the fitted model with NEW random effects: new",
                 "effect-size-level effects, and (second table) new papers as well, so that the",
                 "replicates carry the heterogeneity the model claims.  The posterior predictive",
                 "p (ppp) is the share of replicates whose statistic is at least the observed",
                 "one; values near 0 or 1 flag misfit.  Seed 4242.")),
    checkboxInput(paste0(prefix, "PpcAllDraws"),
                  paste("One replicate per posterior draw (all 12,000 with the app's sampler settings),",
                        "instead of 1,000; about 12 times slower"), value = FALSE),
    actionButton(paste0(prefix, "PpcButton"), "Run posterior predictive checks"),
    uiOutput(paste0(prefix, "PpcOut"))
  )
}

bmlExtrasServer <- function(prefix, model) {
  output[[paste0(prefix, "ShrinkOut")]] <- renderPlot({
    bmlShrinkageForest(bmlShrinkage(model(), ciType()))
  }, height = function() max(300, nrow(MAml()) * 13 + 120))

  triplotTarget <- reactive({
    fit <- model(); snap <- myrvs$bayesSnapshot
    md <- bmlMeanDraws(fit)
    target <- if (prefix == "bml") colnames(md)[1] else {
      t0 <- input$bmlRegTriplotTarget
      if (!isTruthy(t0)) colnames(md)[1] else t0 }
    if (identical(target, "contrast")) {
      lv <- contrastLevels()
      ct <- if (!is.null(lv)) bmlContrast(fit, lv[1], lv[2])
      validate(need(!is.null(ct), "Choose two levels of this model's moderator for the contrast."))
      list(draws = ct$draws, prior = bmlTriplotPrior("contrast", snap$mupriormean, snap$mupriorsd),
           label = paste0("Hedges' g: ", lv[1], " − ", lv[2]))
    } else {
      validate(need(target %in% colnames(md), "Choose a parameter."))
      list(draws = md[, target], prior = bmlTriplotPrior("mean", snap$mupriormean, snap$mupriorsd),
           label = paste0("Hedges' g: ", target))
    }
  })
  output[[paste0(prefix, "TriplotOut")]] <- renderPlot({
    tt <- triplotTarget()
    bmlTriplot(tt$draws, tt$prior[["mean"]], tt$prior[["sd"]], tt$label)
  }, height = 350)
  output[[paste0(prefix, "TriplotNote")]] <- renderUI({
    tt <- triplotTarget()
    v <- attr(bmlTriplot(tt$draws, tt$prior[["mean"]], tt$prior[["sd"]], tt$label), "values")
    p(tags$small(sprintf(paste("Prior Normal(%s, %.3g).  The prior supplies %.1f%% of the",
                               "posterior precision (normal approximation)."),
                         format(v[["priorMean"]]), v[["priorSd"]],
                         100 * v[["priorShareOfPrecision"]])))
  })

  ## posterior predictive checks: only on request (they redraw the data twice per replicate)
  ## 1,000 replicates by default; with the checkbox ticked, one per posterior draw
  ## (ndraws = NULL), as in analyses that report this check from every draw
  ppc <- eventReactive(input[[paste0(prefix, "PpcButton")]], {
    fit <- model()
    nd <- if (isTRUE(input[[paste0(prefix, "PpcAllDraws")]])) NULL else 1000
    withProgress(message = "Replicating the data...", value = 0.1, {
      yr <- bmlMixedReplicates(fit, "es", seed = 4242, ndraws = nd)
      es <- bmlPpcTable(fit, yr)
      incProgress(0.45)
      pa <- bmlPpcTable(fit, bmlMixedReplicates(fit, "paper", seed = 4242, ndraws = nd))
      list(es = es, paper = pa, n = nrow(yr))
    })
  })
  output[[paste0(prefix, "PpcOut")]] <- renderUI({
    r <- ppc()
    tagList(p(tags$small(sprintf("%s replicates per table, seed 4242.", format(r$n, big.mark = ",")))),
            p(tags$b("New effect-size-level effects:")), renderTable(r$es, digits = 3),
            p(tags$b("New papers and new effect-size-level effects:")), renderTable(r$paper, digits = 3))
  })
}
bmlExtrasServer("bml", bmlModel)
bmlExtrasServer("bmlReg", bmlRegModel)

output$bmlRegTriplotChooser <- renderUI({
  fit <- bmlRegModel()
  lv <- colnames(bmlMeanDraws(fit))
  ch <- c(setNames(lv, paste("mean:", lv)),
          if (!is.null(contrastLevels())) c("contrast (A − B)" = "contrast"))
  selectInput("bmlRegTriplotTarget", "Parameter", choices = ch,
              selected = if (!is.null(contrastLevels())) "contrast" else lv[1])
})

output$bmlRegContrastOut <- renderUI({
  fit <- bmlRegModel()
  lv <- contrastLevels()
  if (is.null(lv)) return(p(tags$small('To compare two levels, choose them under "Contrast between',
                                       'two levels" in the "Moderator Selection" tab.')))
  ct <- bmlContrast(fit, lv[1], lv[2], ciType())
  if (is.null(ct)) return(p(tags$small(paste0(
    "The chosen levels are not both in this model (fitted for the moderator \"",
    isolate(myrvs$bayesSnapshot$moderatorName), "\" at the last recalculation).  Choose two of its",
    " levels, or recalculate after changing the moderator."))))
  tagList(renderTable(ct$table, digits = 3),
          p(tags$small("Computed from the posterior draws of the two level means.  P(> 0): the",
                       "posterior probability that level A's mean exceeds level B's.",
                       intervalNote(ciType()))))
})
