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
  #
  ##########
  ## Generate the code for the required selection fields based on current inputs
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
  # Add that to the string that contains the non-reactive R code that will be output
  code_for_MA <- paste0(code_for_MA, code_for_MA.inputs)
  #
  ##############
  ## Write the code for filtering based on user-defined variable selection factors
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
                        paste(dQuote(keepValues), collapse = ", ")
    )
    code_for_MA <- paste0(code_for_MA, someCode)
  }
  # Assign the generated code to output
  output$MAcodeOutput <- renderText({ code_for_MA })
  #
  #
  ##############
  ## Write the code for filtering based on user-defined variable selection numerics
  someCode <- sprintf("
Variable.Numerics.selected <- list() ")
  code_for_MA <- paste0(code_for_MA, someCode)
  #
  # loop over the variable numeric names to write code
  for (varName in Variable.Numeric.Names)  {
    keepValues <- input[[varName]]
    someCode <- sprintf("
Variable.Numerics.selected[[%s]] <- c(%s) ",
                        dQuote(varName),
                        paste(dQuote(keepValues), collapse = ", ")
    )
    code_for_MA <- paste0(code_for_MA, someCode)
  }
  #
  ################## More code that does not depend on UI selections in the app
  ## Create the MA object; More static code to copy without changing:
  code_for_MA_function <- '

  ## function to create MA data object for all analyses
createMA.nonReactive <- function(df, Variable.Factor.Names, Variable.Numeric.Names,
                                 Variable.Factors.selected, Variable.Numerics.selected,
                                 Design.include, Publication.Year.include, N_Intervention.include,
                                 included, aggregation, method = "BHHR", cor = 0.5) {
  # Create subset based on chosen inclusion criteria
  df_sub <- df %>% filter(Design %in% Design.include,
                          Publication.Year >= Publication.Year.include[1],
                          Publication.Year <= Publication.Year.include[2],
                          N_Intervention >= N_Intervention.include[1],
                          N_Intervention <= N_Intervention.include[2],
                          Paper.and.Exp %in% included)

  # Subset based on selection factors
  for (varName in Variable.Factor.Names) {
    keepValues <- Variable.Factors.selected[[varName]]
    df_sub <- df_sub[df_sub[,varName] %in% keepValues, ]
  }

  # Subset based on selection numerics
  for (varName in Variable.Numeric.Names) {
    df_sub <- df_sub[df_sub[,varName] >= Variable.Numerics.selected[[varName]][1], ]
    df_sub <- df_sub[df_sub[,varName] <= Variable.Numerics.selected[[varName]][2], ]
  }

  # Replace ID with Paper.Number if aggregating over papers
  if (aggregation == "Papers") {
    df_sub$ID <- df_sub$Paper.Number
    df_sub$study <- df_sub$Paper
  }

  # Aggregate effect sizes
  aggES <- agg(id = ID,
               es = yi,
               var = vi,
               data = df_sub,
               cor = cor,
               method = method)

  # Merging aggregated ES with original dataframe
  MAnr <- merge(x = aggES, y = df_sub, by.x = "id", by.y = "ID")
  MAnr <- unique(setDT(MAnr)[sort.list(id)], by = "id")
  MAnr <- with(MAnr, MAnr[order(MAnr$es)])

  return(MAnr)
}

## Create MA, using the inputs recorded from the shiny app UI
MA <- createMA.nonReactive(df, Variable.Factor.Names, Variable.Numeric.Names,
                           Variable.Factors.selected, Variable.Numerics.selected,
                           Design, Publication.Year, N_Intervention,
                           included, aggregation)

  '
  ########### End of this static code section
  #
  code_for_MA <- paste0(code_for_MA, code_for_MA_function)  # adding that static code

################### end of Generate MA ########


####### Get the code to record the priors ######

someCode <- sprintf('
## Record the priors for Bayesian analyses ##
tauprior    <-  %s
mupriorsd   <-  %s
scaletau    <-  %s
mupriormean <-  %s
#
                ',
        input$tauprior,
        input$mupriorsd,
        input$scaletau,
        input$mupriormean
        )

  code_for_bma <- paste0(someCode)   # start a new block of code

  #####




  someCode <- '

#### Create bma object for Bayesian functions ####

# Function to Create bma bayesmeta object needed for all outputs
createMA.nonReactive <- function(MA, tauprior, mupriorsd, scaletau, mupriormean) {
    ## Generate bayesmeta-object "bma" depending on tau prior chosen
        if (tauprior == "Half cauchy") {
        bma <- bayesmeta(y = MA$es,sigma = sqrt(MA$var), labels = MA$study,
                         tau.prior = function(t) dhalfcauchy(t, scale = scaletau),
                         mu.prior = c("mean" = mupriormean, "sd" = mupriorsd))
      } else if (tauprior == "Half student t") {
        bma <- bayesmeta(y = MA$es,sigma = sqrt(MA$var), labels = MA$study,
                         tau.prior = function(t) dhalfnormal(t, scale = scaletau),
                         mu.prior = c("mean" = mupriormean, "sd" = mupriorsd))
      } else {
        bma <- bayesmeta(y = MA$es,sigma = sqrt(MA$var), labels = MA$study,
                         tau.prior = tauprior,
                         mu.prior = c("mean" = mupriormean, "sd" = mupriorsd))
      }
  bma
    }  #### end of createMA.nonReactive function

# calculate bma bayesmeta object
bma <- createMA.nonReactive(MA, tauprior, mupriorsd, scaletau, mupriormean)

'

  code_for_bma <- paste0(code_for_bma, someCode)   # update code block





  # Put together all the code
  code_for_R_script <- paste0(code_for_MA, code_for_bma)

  # Assign the generated code to output
  output$R_code_Output <- renderText({ code_for_R_script })

  ## code_for_MA should be done at this point.               #debugging
  write(code_for_R_script, file = "nonReactiveVErsion_part1.R")    #debugging


})  # end of observeEvent(MA())































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




