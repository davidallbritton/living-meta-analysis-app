#######################################################################################
################### A General Tool for Living Meta-Analysis #################
#######################################################################################
# v.1.0 2026.07.14
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
myrvs$previousBmrModels <- list()

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
  req(input$includeModerator == "Yes")
  req(input$moderator_variable)
  #
  ## check the cache first (same key the bmr reactive uses)
  isolate({
    MA            <- MA()
    moderatorName <- input$moderator_variable
    tauprior      <- input$tauprior
    mupriorsd     <- input$mupriorsd
    scaletau      <- input$scaletau
    mupriormean   <- input$mupriormean
    old_bmr <- checkOldBmrModels(myrvs$previousBmrModels, MA = MA, tauprior = tauprior,
                                 mupriorsd = mupriorsd, scaletau = scaletau,
                                 mupriormean = mupriormean, moderatorName = moderatorName)
  })
  #
  if (!isTruthy(old_bmr)) {   # only warn if this model is not already cached
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
  req(myrvs$triggerBmr)   # only run after user confirmation (or a cache hit)
  isolate({               # so changing priors does not rebuild before "Re-Calculate"
    MA            <- MA()
    moderatorName <- input$moderator_variable
    tauprior      <- input$tauprior
    mupriorsd     <- input$mupriorsd
    scaletau      <- input$scaletau
    mupriormean   <- input$mupriormean
    req(moderatorName)

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
  paste("Moderator variable is:  ", input$moderator_variable)
})

# Plot height grows with the number of studies (does not trigger a model build)
bmrForestHeight <- reactive(nrow(MA()) * 25 + 250)

# Forest plot: studies grouped within each moderator level (like the frequentist
# forestByGroup plot), with a Bayesian posterior summary polygon per group.
output$bmrForest <- renderPlot({
  MA <- MA()
  moderator <- MA[[input$moderator_variable]]
  forestBmrByGroup(bmrModel(), MA = MA, moderator = moderator, xlab = "Hedges' g")
}, height = bmrForestHeight)

# Marginal posterior summary (tau + one column per moderator group)
output$bmrSummary <- renderPrint({
  bmrModel()$summary
})
