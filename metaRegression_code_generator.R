## generating static R code for the meta-regression panel
##

metaRegCode <- reactiveVal("##### No moderator chosen; no meta-regression done ########")
  
observe({   #update the meta-regression plot code when a moderator is selected
  req(input$includeModerator)
  req(input$includeModerator == "Yes")
  moderatorChosen <- input$moderator_variable
  moderatorArgText <- paste0('moderator=MA[["', moderatorChosen, '"]]')
  plotArgs <- paste0('MA=MA, ', moderatorArgText, ', col="red", border="red", efac=0.3, caterpillar=FALSE')

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

