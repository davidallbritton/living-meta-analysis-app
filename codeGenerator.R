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

## Print out initial static code that does not depend on app user input selections:
####### beginning of static code block                       
initialCode <- '

### nonreactive R code generated within the app for reproducibility

##############  Edit this part by hand as needed ######################
## data file name; change as needed.  It can be .xlsx, .xls, or .csv ##
input_file <- "originalData.xlsx"       # Change this to your file path
#######################################################################

## source the "HelperFunctions.R" file that is used to reformat the input data, etc.
source("HelperFunctions.R")  # Functions from this file that are used:
                             # reformat.df

## load libraries
# get  needed libraries from ui.R 
# commenting out the ones that are shiny-specific
library(purrr)
library(metafor)
library(readxl)
library(writexl)
library(tools)
# library(shiny)
library(bayesmeta)
library(cowplot)
library(dplyr)
library(DT)
library(data.table)
library(esc)
library(ggplot2)
library(MAd)
library(readr)
library(R.rsp)
# library(shinyBS)
# library(shinycssloaders)
# library(shinythemes)
library(stringr)
library(tidyr)
library(xtable)
# library(shinyalert)
# library(shinyjs)
# library(shinymeta)

## Function to read data based on file extension
read_data <- function(file_path) {
  file_extension <- tools::file_ext(file_path)
  #
  if (file_extension %in% c("xlsx", "xls")) {
    df <- read_excel(file_path) %>% as.data.frame()
  } else if (file_extension == "csv") {
    df <- read_csv(file_path, show_col_types = FALSE) %>% as.data.frame()
  } else {
    stop("File format not supported. Must be .xlsx, .xls, or .csv.")
  }
  #
  return(df)
}

## Read and process the data from the input file
df_as_uploaded <- read_data(input_file)
newrvs <- reformat.df(df_as_uploaded)
df <- newrvs$df  # reformatted for use in the analyses

#'
####### end of static code block                       
                       

################### Generate MA ########
## Generate non-reactive R code to create MA object
#  This code can be displayed or downloaded for reproducibility
#
observeEvent(MA(), {
  # Initialize the code with the static content from above
  code_for_MA <- paste0(initialCode)
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
  code_for_MA <- paste0(code_for_MA, code_for_MA.inputs)
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
  
  
  ## need to do the same thing for the numerics now...
  
  ## copy in some more static code...
  
  ## code_for_MA should be done at this point.
  
  
})  # end of observeEvent(MA())
################### end of Generate MA ########
































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




