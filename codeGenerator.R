########## codeGenerator.R #############
#
# Reactives and functions for generating
# non-reactive R code that reproduces
# the analyses of the shiny script.
#
########################################
# David Allbritton
# December 2023
########################################

################### Generate MA ########
## Generate non-reactive R code to create MA object
#  This code can be displayed or downloaded for reproducibility
#
observeEvent(MA(), {
  # Generate the code for the required selection fields based on current inputs
  code_for_MA.inputs <- sprintf("
## R code to create MA object that contains selected data for all analyses
#
Variable.Factor.Names <- %s
Variable.Numeric.Names <- %s
Design <- %s
Publication.Year <- c(%s, %s)
N_Intervention <- c(%s, %s)
included <- %s
aggregation <- '%s'
#",
                                paste("c(", paste(dQuote(myrvs$Variable.Factor.Names), collapse = ", "), ")", sep=""),
                                paste("c(", paste(dQuote(myrvs$Variable.Numeric.Names), collapse = ", "), ")", sep=""),
                                paste("c(", paste(dQuote(input$Design), collapse = ", "), ")", sep=""),
                                input$Publication.Year[1], input$Publication.Year[2],
                                input$N_Intervention[1],  input$N_Intervention[2],
                                paste("c(", paste(dQuote(input$included), collapse = ", "), ")", sep=""),
                                input$aggregation

  )
  # Initialize the string that contains the non-reactive R code that will be output
  code_for_MA <- paste0(code_for_MA.inputs)
  #
  # Write the code for filtering based on user-defined variable selection factors
  someCode <- sprintf("
Variable.Factors.selected <- list() ")
  code_for_MA <- paste0(code_for_MA, someCode)
  #
  # loop over the variable factor names to write code
  for (varName in Variable.Factor.Names)  {
    keepValues <- input[[varName]]
    someCode <- sprintf("
Variable.Factors.selected[[%s]] <- c(%s) ",
                        dQuote(varName),
                        paste("c(", paste(dQuote(keepValues), collapse = ", "), ")", sep="")
    )
    code_for_MA <- paste0(code_for_MA, someCode)
  }
  # Assign the generated code to output
  output$MAcodeOutput <- renderText({ code_for_MA })
})














# 
# 
# # need to redo this without shinymeta:
# output$freq_funnel <- metaRender(renderPlot, {
#   # Use ..() to call the reactive expression fma()
#   funnel((fma()), xlab = "Observed outcome")
# })
# 
# # Render the captured R code for the funnel plot
# output$funnel_code <- renderPrint({
#   expandChain(output$freq_funnel())
# })
# 




