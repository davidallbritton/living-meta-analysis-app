#######################################################################################
################### A General Tool for Living Meta-Analysis #################
#######################################################################################
# v.1.3 2026.07.19
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
# cached in myrvs$previousBmlModels (seeded from the shared process-wide
# defaultBmlSeed; downloadable in "Saved Plots and Models").
#
# This file is sourced (local = T) from server.R.
######################################################################################


## session cache, seeded from the process-wide seed loaded in global.R
## (shared across sessions via copy-on-write, like the other model caches);
## entries are keyed by data + priors + moderatorName ("" = overall model),
## so both tabs' models live side by side in the one cache
myrvs$previousBmlModels <- defaultBmlSeed

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
  checkOldBmlModels(myrvs$previousBmlModels, MA = MA, tauprior = snap$tauprior,
                    mupriorsd = snap$mupriorsd, scaletau = snap$scaletau,
                    mupriormean = snap$mupriormean, moderatorName = moderatorName,
                    rho = snap$rhoCHE, che = !isTRUE(snap$skipCHE))
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
                          rho = snap$rhoCHE, che = !isTRUE(snap$skipCHE)),
    error = function(e) e)
  if (inherits(newbml, "error")) {
    validate(need(FALSE, paste0(
      "The Bayesian multilevel model could not be fit on this server (",
      conditionMessage(newbml), ").  Stan model compilation often fails on hosted ",
      "servers with limited memory; running the app locally supports these tabs.  ",
      "The frequentist multilevel tabs are unaffected.")))
  }
  newrow <- list(MA = MA, tauprior = snap$tauprior, mupriorsd = snap$mupriorsd,
                 scaletau = snap$scaletau, mupriormean = snap$mupriormean,
                 moderatorName = moderatorName, rho = snap$rhoCHE,
                 che = !isTRUE(snap$skipCHE), bml = newbml)
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
                  "Sampling takes a little while, and the FIRST multilevel fit in a",
                  "session also compiles the model (about 1-2 minutes extra).",
                  "Later fits skip the compilation."),
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
  bmlForestPlot(bmlModel())
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
    p(tags$b("Overall model (no moderator), using the priors as of the last recalculation.")),
    p(tags$small(bmlDiagnosticsText(fit))),
    h4("Posterior summary:"),
    renderTable(bmlSummaryTable(fit), digits = 3),
    h4("Per-paper posterior estimates (95% CrI):"),
    plotOutput("bmlForestOut", height = "auto"),
    h4("Posterior densities:"),
    plotOutput("bmlDensOut", height = "350px"),
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

output$bmlRegForestOut <- renderPlot({ bmlForestPlot(bmlRegModel()) }, height = 300)
output$bmlRegDensOut <- renderPlot({ bmlDensityPlot(bmlRegModel()) }, height = 350)

output$bmlRegContent <- renderUI({
  fit <- bmlRegModel()
  moderatorName <- isolate(myrvs$bayesSnapshot$moderatorName)
  tagList(
    bmlSkipWarning(myrvs$bayesSnapshot),
    p(tags$b(paste0("Moderator: ", moderatorName,
                    " (per-group posterior means), using the settings as of the last recalculation."))),
    p(tags$small(bmlDiagnosticsText(fit))),
    h4("Posterior summary:"),
    renderTable(bmlSummaryTable(fit), digits = 3),
    h4("Per-group posterior means (95% CrI):"),
    plotOutput("bmlRegForestOut", height = "300px"),
    h4("Posterior densities:"),
    plotOutput("bmlRegDensOut", height = "350px"),
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
    saveRDS(myrvs$previousBmlModels, file)
  }
)

observeEvent(input$SavedBmlModelsUp, {
  newrows_bml <- normalizeTauPriorLabels(readRDS(input$SavedBmlModelsUp$datapath))
  myrvs$previousBmlModels <- c(myrvs$previousBmlModels, newrows_bml)
})

observeEvent(input$ClearBmlModels, {
  myrvs$previousBmlModels <- list()
})
