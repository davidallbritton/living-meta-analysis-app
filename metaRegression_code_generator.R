## generating static R code for the meta-regression panel
##

metaRegCode <- reactiveVal("##### No moderator chosen; no meta-regression done ########")
  
observe({   #update the meta-regression plot code when a moderator is selected
  req(input$includeModerator)
  req(input$includeModerator == "Yes")
  moderatorChosen <- input$moderator_variable
  # blocked in the app (Papers aggregation blends this moderator within papers):
  # skip only the AGGREGATED variant; the multilevel variant remains valid
  blockedAgg <- moderatorBlockedByPapersAgg(moderatorChosen)
  # efac follows the "Symbol size" slider on the frequentist meta-regression tab
  efacFreq <- if (is.null(input$freq_efac)) 0.3 else input$freq_efac
  # BOTH variants are emitted, each under its family switch: the standard
  # two-level version on the aggregated data and the multilevel (CHE) version
  # on the unaggregated data
  sharedArgs <- paste0('col="red", border="red", efac=', efacFreq, ', caterpillar=FALSE')
  aggregatedSection <- if (blockedAgg) '
  ## standard two-level version SKIPPED: the app aggregated over Papers and the
  ## chosen moderator varies within at least one selected paper, so aggregation
  ## would blend its groups together.  Use ID aggregation to enable this variant.
' else paste0('
  ## standard two-level version (aggregated data):
  if (run_aggregated == "Yes") {
    forestByGroup(MA=MA_aggregated, moderator=MA_aggregated[["', moderatorChosen, '"]],
                  ', sharedArgs, ', multilevel=FALSE)
  }')
  metaRegText <- paste0('
  ########## Meta-regression plots with one categorical moderator
  #   Uses forestByGroup() from the downloaded HelperFunctions.R file.
  ## additional arguments that might be useful: xlim, psize, xlab, cex
  ## For a caterpillar plot, use caterpillar=TRUE (not slab=NA)
  ## You can also add any other arguments that metafor::forest() allows
  #', aggregatedSection, '
  ## multilevel (CHE) version (unaggregated data):
  if (run_multilevel == "Yes") {
    forestByGroup(MA=MA_multilevel, moderator=MA_multilevel[["', moderatorChosen, '"]],
                  ', sharedArgs, ', multilevel=TRUE, rho=rhoCHE)
  }')
  
  isolate({   # this updates the reactive value so it can be used outside this "observe" block
    metaRegCode(metaRegText)
  })
})


## generating static R code for the Bayesian meta-regression panel
##
bmrCode <- reactiveVal("##### No moderator chosen; no Bayesian meta-regression done ########")

observe({   # update the Bayesian meta-regression code when a moderator is selected
  # the bmr model uses the RECALCULATION-TIME moderator (see bayesSnapshot in
  # MA()), so the generated code records that one, matching the displayed model
  moderatorChosen <- myrvs$bayesSnapshot$moderatorName
  req(isTruthy(moderatorChosen), nzchar(moderatorChosen))
  # blocked in the app (Papers aggregation blends this moderator within papers)
  if (moderatorBlockedByPapersAgg(moderatorChosen)) {
    isolate(bmrCode(paste(
      "##### Bayesian meta-regression skipped: the analysis aggregates effect sizes over",
      "Papers, but the chosen moderator varies within at least one selected paper, so",
      "aggregation would blend its groups together.  Use ID aggregation (or Multilevel,",
      "with the frequentist tabs) to analyze this moderator. #####")))
    return()
  }
  # efac follows the "Symbol size" slider on the Bayesian meta-regression tab
  efacBmr <- if (is.null(input$bmrEfac)) 0.3 else input$bmrEfac

  bmrText <- sprintf('
########## Bayesian meta-regression with one categorical moderator #############
#   Fits bmr() from the bayesmeta package using cell-means coding, so each
#   coefficient is one moderator group pooled posterior effect.  Uses the same
#   tau/mu priors recorded above.  buildBmrModel() and forestBmrByGroup() are
#   defined in the downloaded HelperFunctions.R file.
#   NOTE: like bma(), this Bayesian model can be slow to fit.
#
if (calculate_bma == "Yes") {   # reuse the same on/off switch used for the bma() model
                                # (uses the AGGREGATED data, like the app tab)
  bmrModel <- buildBmrModel(MA_aggregated, moderatorName = "%s",
                            tauprior = tauprior, scaletau = scaletau,
                            mupriormean = mupriormean, mupriorsd = mupriorsd)

  ## marginal posterior summary (tau + one column per moderator group)
  print(bmrModel$summary)

  ## grouped forest plot with a Bayesian posterior polygon per moderator group
  forestBmrByGroup(bmrModel, MA = MA_aggregated, moderator = MA_aggregated[["%s"]],
                   xlab = "Hedges g", efac = %s)
}
', moderatorChosen, moderatorChosen, efacBmr)

  isolate({   # update the reactive value so it can be used outside this "observe" block
    bmrCode(bmrText)
  })
})

