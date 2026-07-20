#######################################################################################
################### A General Tool for Living Meta-Analysis #################
#######################################################################################
# v.1.3 2026.07.19
#
################### Bayesian Meta-Regression panel (server) ###########################
#
# Bayesian version of the frequentist "Meta-Regression" tab.  Fits a Bayesian
# meta-regression with one categorical moderator using bmr() from the bayesmeta
# package (see buildBmrModel() in metaRegressionFunctions.R).
#
# Reuses the sidebar "Moderator Selection" (input$includeModerator,
# input$moderator_variable) and "Prior specifications" inputs, so the τ and μ
# priors match the other Bayesian analyses.
#
# Computational-cost handling mirrors the non-moderator bma() model:
#   * the model is only built when the Bayesian Meta-Regression tab is active,
#   * the user must confirm via a modal before a NEW (uncached) model is built,
#   * every model is cached in myrvs$previousBmrModels and never evicted, so an
#     identical (data + priors + moderator) request is retrieved, not recomputed.
#
# This file is sourced (local = T) from server.R.
######################################################################################


## Store previously calculated bmr models so they are never recomputed unnecessarily.
## Seeded from the process-wide cache loaded once in global.R (mirrors the bma() /
## robustness-plot seeds in server.R), so the default dataset's Bayesian
## meta-regressions load instantly instead of being rebuilt.  The seed entries are
## shared across sessions via copy-on-write; this session's additions never modify
## the shared seed.
myrvs$previousBmrModels <- defaultBmrSeed

## Trigger controlling when a bmr model may be built (set TRUE only after the
## user confirms the modal, or automatically when the request is already cached).
myrvs$triggerBmr <- FALSE


## Modal to warn when a NEW Bayesian meta-regression model build is requested.
## (Parallels the bma() modal in server.R.)
observe({
  # Reactively depend on MA()
  MA()
  # Only relevant on the Bayesian Meta-Regression tab, with a moderator chosen
  req(input$mainTabset == "bayesian_meta_regression")
  # settings come from the RECALCULATION-TIME snapshot (set in MA()): changing
  # the moderator or priors has no effect here until "(Re)Calculate" is pressed
  snap <- myrvs$bayesSnapshot
  req(!is.null(snap), nzchar(snap$moderatorName))
  # blocked combination (Papers aggregation blends this moderator within papers):
  # no confirm dialog; bmrModel() shows the explanation instead
  req(!moderatorBlockedByPapersAgg(snap$moderatorName))
  #
  ## check the cache first (same key the bmr reactive uses)
  isolate({
    MA            <- MA()
    moderatorName <- snap$moderatorName
    tauprior      <- snap$tauprior
    mupriorsd     <- snap$mupriorsd
    scaletau      <- snap$scaletau
    mupriormean   <- snap$mupriormean
    old_bmr <- checkOldBmrModels(myrvs$previousBmrModels, MA = MA, tauprior = tauprior,
                                 mupriorsd = mupriorsd, scaletau = scaletau,
                                 mupriormean = mupriormean, moderatorName = moderatorName)
  })
  #
  if (!isTruthy(old_bmr)) {   # only warn if this model is not already cached
    # block stale results until the user confirms: a trigger that is already TRUE
    # would stay TRUE on confirmation (same-value reactiveValues writes do not
    # invalidate), leaving the previous model on display after a moderator or
    # prior change without a recalculation
    myrvs$triggerBmr <- FALSE
    shinyalert(
      title = "Are you sure you want to do this new Bayesian meta-regression?  It could take a long time.",
      type = "warning",
      showCancelButton = TRUE,
      confirmButtonText = "Yes, continue!",
      cancelButtonText = "No, not right now.",
      callbackR = function(value) {
        myrvs$triggerBmr <- value
      }
    )
  } else {
    myrvs$triggerBmr <- TRUE   # cached: retrieve it without a warning
  }
})


## The bmr model reactive, used by all Bayesian Meta-Regression outputs.
bmrModel <- reactive({
  # Papers aggregation + a moderator that varies within papers would analyze
  # blended, arbitrarily labeled composites; refuse with an explanation
  validate(need(!moderatorBlockedByPapersAgg(myrvs$bayesSnapshot$moderatorName),
                paste('Not computed: the analysis aggregates effect sizes over Papers, but at least',
                      'one selected paper has effect sizes in different groups of this moderator.',
                      'Aggregation would blend those groups together within each paper.',
                      'Switch "Aggregate over" to ID in the "Study criteria" panel and press',
                      '"(Re)Calculate Meta-Analysis", or use the "Bayesian Multilevel Regression"',
                      'tab (which never aggregates), to analyze this moderator.')))
  req(myrvs$triggerBmr)   # only run after user confirmation (or a cache hit)
  isolate({               # settings come from the RECALCULATION-TIME snapshot (set in MA())
    snap          <- myrvs$bayesSnapshot
    MA            <- MA()
    moderatorName <- snap$moderatorName
    tauprior      <- snap$tauprior
    mupriorsd     <- snap$mupriorsd
    scaletau      <- snap$scaletau
    mupriormean   <- snap$mupriormean
    req(nzchar(moderatorName))

    # figure out how many non-empty groups we actually have
    moderator <- MA[[moderatorName]]
    if (!is.factor(moderator)) moderator <- as.factor(as.character(moderator))
    moderator <- droplevels(moderator)
    validate(
      need(nlevels(moderator) >= 2,
           paste("A meta-regression needs at least two non-empty moderator groups.",
                 "Adjust your study selection criteria or choose a different moderator.")),
      need(nrow(MA) > nlevels(moderator),
           "There are too few studies relative to the number of moderator groups to fit the meta-regression.")
    )

    # retrieve a cached model if we have one, otherwise build and cache a new one
    old_bmr <- checkOldBmrModels(myrvs$previousBmrModels, MA = MA, tauprior = tauprior,
                                 mupriorsd = mupriorsd, scaletau = scaletau,
                                 mupriormean = mupriormean, moderatorName = moderatorName)
    if (isTruthy(old_bmr)) {
      old_bmr
    } else {
      newbmr <- buildBmrModel(MA = MA, moderatorName = moderatorName, tauprior = tauprior,
                              scaletau = scaletau, mupriormean = mupriormean, mupriorsd = mupriorsd)
      updateBmrModels(MA = MA, moderatorName = moderatorName, tauprior = tauprior,
                      mupriorsd = mupriorsd, scaletau = scaletau, mupriormean = mupriormean,
                      bmr = newbmr)
      newbmr
    }
  })  # end isolate()
})   # end bmrModel() definition


## Append a newly built bmr model to the cache (side effect on myrvs only).
## Mirrors updateModels() for the bma() cache.
updateBmrModels <- function(MA, moderatorName, tauprior, mupriorsd, scaletau, mupriormean, bmr) {
  newrow <- list(MA = MA, moderatorName = moderatorName, tauprior = tauprior,
                 mupriorsd = mupriorsd, scaletau = scaletau, mupriormean = mupriormean, bmr = bmr)
  myrvs$previousBmrModels[length(myrvs$previousBmrModels) + 1] <- list(newrow)
  length(myrvs$previousBmrModels)   # unused return value
}


## ---- Outputs for the Bayesian Meta-Regression tab ----

output$bmrModeratorLabel <- renderText({
  # the snapshotted moderator: the one the displayed model actually uses
  req(myrvs$bayesSnapshot, nzchar(myrvs$bayesSnapshot$moderatorName))
  paste("Moderator variable is:  ", myrvs$bayesSnapshot$moderatorName)
})

# Plot height defaults to grow with the number of studies, but the user can
# override it with the "Plot height" box.  Whenever the meta-analysis data (MA)
# changes, reset the box to the default so it tracks the new study count.
# (Changing the height only re-renders the plot; it never rebuilds the model.)
bmrDefaultForestHeight <- reactive(nrow(MA()) * 25 + 250)
observe({
  updateNumericInput(session, "bmrForestHeightInput", value = bmrDefaultForestHeight())
})
bmrForestHeight <- reactive({
  h <- input$bmrForestHeightInput
  if (is.null(h) || is.na(h)) bmrDefaultForestHeight() else h
})

# Forest plot: studies grouped within each moderator level (like the frequentist
# forestByGroup plot), with a Bayesian posterior summary polygon per group.
# Symbol size (efac) comes from the "Symbol size" slider; height from the box above.
#
# Rendered inside a renderUI (mirroring the frequentist Meta-Regression tab) so the
# plot element gets an EXPLICIT pixel height instead of "auto".  The height is read
# here, in the renderUI body, so changing it re-lays-out the element cleanly; with
# "auto" the container collapsed and regrew on every re-render, scrolling the page.
output$bmrForestUI <- renderUI({
  MA <- MA()
  req(input$moderator_variable)
  moderator <- MA[[input$moderator_variable]]
  renderPlot({
    efac <- if (is.null(input$bmrEfac)) 0.3 else input$bmrEfac
    forestBmrByGroup(bmrModel(), MA = MA, moderator = moderator, xlab = "Hedges' g",
                     efac = efac)
  }, height = bmrForestHeight())
})

# Marginal posterior summary (tau + one column per moderator group)
output$bmrSummary <- renderPrint({
  bmrModel()$summary
})


## ---- Managing "cached" Bayesian meta-regression models ----
## Mirrors the "Saved Plots and Models" tab handlers for bma() models and
## robustness plots in server.R, letting the user download the current session's
## bmr models and re-upload them in a future session so they are never recomputed.

#### observer to clear the saved regression models when the button is pressed
observeEvent(input$ClearBmrModels, {
  myrvs$previousBmrModels <- list()
})

#### section for uploading saved regression models from a file
#### for reading an .RDS file containing previously calculated bmr models
observeEvent(input$SavedBmrModelsUp, {
  # Read the data from the RDS file the user uploaded:
  fileExtension <- tools::file_ext(input$SavedBmrModelsUp$datapath)
  output$inputFileErrorBmrUp <- renderUI({  # create error message in case file not uploaded successfully
    if (!is.null(input$SavedBmrModelsUp)) p(style = "color:red", "***File was not read***")
  })
  validate(need(fileExtension == "RDS" | fileExtension == "rds" | fileExtension == "Rds", "Please upload an RDS file"))
  newrows_bmr <- normalizeTauPriorLabels(readRDS(input$SavedBmrModelsUp$datapath))
  oldrows_bmr <- myrvs$previousBmrModels
  allrows_bmr <- c(newrows_bmr, oldrows_bmr)
  myrvs$previousBmrModels <- allrows_bmr
  output$inputFileErrorBmrUp <- renderUI(NULL) # remove error message if file uploaded successfully
})

#### For downloading saved regression models (can be uploaded in this format)
output$rds_file.bmr <- downloadHandler(
  filename = function() {
    "bayesian_meta_regression_models.RDS"
  },
  content = function(file) {
    saveRDS(myrvs$previousBmrModels, file)
  }
)
