## generating static R code for the meta-regression panel
##

metaRegCode <- reactiveVal("##### No moderator chosen; no meta-regression done ########")
  
observe({   #update the meta-regression plot code when a moderator is selected
  req(input$includeModerator)
  req(input$includeModerator == "Yes")
  moderatorChosen <- input$moderator_variable
  moderatorArgText <- paste0('moderator=MA[["', moderatorChosen, '"]]')
  # efac follows the "Symbol size" slider on the frequentist meta-regression tab
  efacFreq <- if (is.null(input$freq_efac)) 0.3 else input$freq_efac
  # three-level rma.mv models when the "Multilevel" aggregation option is chosen
  multilevelArg <- if (identical(input$aggregation, "Multilevel")) "TRUE" else "FALSE"
  plotArgs <- paste0('MA=MA, ', moderatorArgText, ', col="red", border="red", efac=', efacFreq,
                     ', caterpillar=FALSE, multilevel=', multilevelArg)

  metaRegText <- '
  ########## Code for making the meta-regression plot with one categorical moderator
  #   Uses a function from a separate .R file to make the plot.  
  ## additional arguments that might be useful: xlim, psize, xlab, cex
  ## For a caterpillar plot, use caterpillar=TRUE (not slab=NA)
  ## You can also add any other arguments that metafor::forest() allows
  #
  forestByGroup('
  
  metaRegText <- paste0(metaRegText, plotArgs, ")")
  
  isolate({   # this updates the reactive value so it can be used outside this "observe" block
    metaRegCode(metaRegText)
  })
})


## generating static R code for the Bayesian meta-regression panel
##
bmrCode <- reactiveVal("##### No moderator chosen; no Bayesian meta-regression done ########")

observe({   # update the Bayesian meta-regression code when a moderator is selected
  req(input$includeModerator)
  req(input$includeModerator == "Yes")
  moderatorChosen <- input$moderator_variable
  req(moderatorChosen)
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
  bmrModel <- buildBmrModel(MA, moderatorName = "%s",
                            tauprior = tauprior, scaletau = scaletau,
                            mupriormean = mupriormean, mupriorsd = mupriorsd)

  ## marginal posterior summary (tau + one column per moderator group)
  print(bmrModel$summary)

  ## grouped forest plot with a Bayesian posterior polygon per moderator group
  forestBmrByGroup(bmrModel, MA = MA, moderator = MA[["%s"]],
                   xlab = "Hedges g", efac = %s)
}
', moderatorChosen, moderatorChosen, efacBmr)

  isolate({   # update the reactive value so it can be used outside this "observe" block
    bmrCode(bmrText)
  })
})

