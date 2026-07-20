########## codeGenerator.R #############
#
# Reactives and functions for generating
# non-reactive R code that reproduces
# the analyses of the shiny script.
#
########################################
# David Allbritton
# December 2023
# v.1.3 2026.07.19
########################################

############### functions for generating the code ############
escapeAndDQuote <- function(x) {  # replacement for dQuote for strings that contain double quotes
  # First, escape internal double quotes
  escaped <- gsub("\"", "\\\"", x, fixed = TRUE)
  # Now wrap the string with double quotes
  dquotedEscaped <- paste0("\"", escaped, "\"")
  return(dquotedEscaped)
}

#  check_for_bad_chars -- in HelperFunctions.R

##############################################################


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
                             # reformat.df()
                             # is_outlier()
                             # priorposteriorlikelihood.ggplot()
                             # tauprior.ggplot
                             # robustness
                             # check_for_bad_chars

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

if(check_for_bad_chars(df)) {  
  # Give a warning if the data contains newlines and other problematic characters
  warning("Your datafile contains newlines, carriage returns, or other problematic characters. 
          This probably will cause problems and the analysis produced here will not be 
          identical to what you produced interactively in the Shiny app.")
}

#'
####### end of static code block


################### Generate MA ########
## Generate non-reactive R code to create MA object and export the code to a file
#
observeEvent(reactiveTriggers(), {  # was just MA(), but needs additional reactive triggers, therefore updated to this
  ## Data provenance comment for the top of the generated script.  Deliberately a
  ## note about the STARTING file only: the session's data may since have been
  ## extended via "Add a Study", so we never claim the analyzed data equals that
  ## file -- the "current data" download is the authoritative copy.
  dataSourceName <- if (is.null(myrvs$currentInputFile)) {
    "the app default data set (Vasilev et al. 2018 plus 2023 updates)"
  } else {
    myrvs$currentInputFile
  }
  addedRows <- if (!is.null(myrvs$df.updated) && !is.null(myrvs$df.original)) {
    nrow(myrvs$df.updated) - nrow(myrvs$df.original)
  } else 0
  provenanceNote <- sprintf('
## Data provenance: the app session that generated this code STARTED from:
##     %s
%s## The exact data that was analyzed (including any additions) can be saved from
## the app ("Downloads" tab -> current data file); set input_file below to that
## file to reproduce the session exactly.
', dataSourceName,
    if (addedRows > 0) {
      sprintf('## NOTE: %d effect size(s) were then added via the "Add a Study" tab, so the\n## analyzed data is NOT identical to that starting file.\n', addedRows)
    } else "")

  # Initialize the code with the provenance note plus the static content from above
  code_for_MA <- paste0(provenanceNote, initialCode)
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
## Assumed within-paper correlation between the SAMPLING ERRORS of different
## effect sizes from one paper (the app's Dependence slider).  Used by the
## aggregation, the frequentist CHE model and the Bayesian multilevel models.
## Primary studies essentially never report it, so it is a working assumption;
## 0.5 is conventional.  Vary it to check how sensitive the results are.
rhoCHE <- %s
## Escape hatch used in the app for the BAYESIAN multilevel models only: TRUE
## drops the correlated-errors correction (fast but statistically wrong -- the
## credible intervals come out too narrow).  Set to FALSE to do it properly.
skipCHE <- %s
#",
                                paste("c(", paste(escapeAndDQuote(myrvs$Variable.Factor.Names), collapse = ",\n "), ")", sep=""),
                                paste("c(", paste(escapeAndDQuote(myrvs$Variable.Numeric.Names), collapse = ",\n "), ")", sep=""),
                                paste("c(", paste(escapeAndDQuote(input$Design), collapse = ", "), ")", sep=""),
                                input$Publication.Year[1], input$Publication.Year[2],
                                input$N_Intervention[1],  input$N_Intervention[2],
                                paste("c(", paste(escapeAndDQuote(enc2utf8(input$included)), collapse = ",\n "), ")", sep=""),
                                input$aggregation,
                                format(if (isTruthy(myrvs$bayesSnapshot$rhoCHE))
                                         myrvs$bayesSnapshot$rhoCHE else 0.5),
                                if (isTRUE(myrvs$bayesSnapshot$skipCHE)) "TRUE" else "FALSE"

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
  for (varName in myrvs$Variable.Factor.Names)  {
    keepValues <- input[[varName]]   
    someCode <- sprintf("
Variable.Factors.selected[[%s]] <- c(%s) ",
                        escapeAndDQuote(varName),
                        paste(escapeAndDQuote(keepValues), collapse = ",\n ") 
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
  for (varName in myrvs$Variable.Numeric.Names)  {
    keepValues <- input[[varName]]
    someCode <- sprintf("
Variable.Numerics.selected[[%s]] <- c(%s) ",
                        escapeAndDQuote(varName),
                        paste(keepValues, collapse = ", ")
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

  # "Multilevel": no aggregation -- keep every effect-size row; the frequentist
  # models below then fit a three-level rma.mv (effect sizes nested in papers)
  if (aggregation == "Multilevel") {
    MAnr <- as.data.frame(df_sub)
    MAnr$es  <- MAnr$yi
    MAnr$var <- MAnr$vi
    MAnr <- MAnr[order(MAnr$es), ]
    # metafor requires unique study labels; multi-ES experiments repeat Paper.and.Exp
    MAnr$study <- make.unique(as.character(MAnr$Paper.and.Exp))
    return(MAnr)
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

## Create the TWO analysis datasets, using the inputs recorded from the app UI.
## MA_aggregated feeds the standard two-level analyses (rma, bayesmeta, bmr);
## MA_multilevel keeps every effect size as its own row for the multilevel
## analyses (frequentist CHE rma.mv and the brms Bayesian multilevel model).
## Both families can therefore run from ONE downloaded script, whichever
## aggregation mode the app was in.
aggregationAggregated <- if (aggregation == "Multilevel") "ID" else aggregation
MA_aggregated <- createMA.nonReactive(df, Variable.Factor.Names, Variable.Numeric.Names,
                           Variable.Factors.selected, Variable.Numerics.selected,
                           Design, Publication.Year, N_Intervention,
                           included, aggregationAggregated, cor = rhoCHE)
MA_multilevel <- createMA.nonReactive(df, Variable.Factor.Names, Variable.Numeric.Names,
                           Variable.Factors.selected, Variable.Numerics.selected,
                           Design, Publication.Year, N_Intervention,
                           included, "Multilevel", cor = rhoCHE)
## MA = the dataset matching the mode chosen in the app (used by the boxplot)
MA <- if (aggregation == "Multilevel") MA_multilevel else MA_aggregated

## Which analysis families to run (both default to "Yes"; the frequentist parts
## are fast -- the slow Bayesian parts have their own switches further down)
run_aggregated <- "Yes"   ## standard two-level analyses on MA_aggregated
run_multilevel <- "Yes"   ## multilevel (CHE / brms) analyses on MA_multilevel

  '
  ########### End of this static code section
  #
  code_for_MA <- paste0(code_for_MA, code_for_MA_function)  # adding that static code

################### end of Generate MA ########


####### Get the code to record the priors ######
#
someCode <- sprintf('
## Record the priors for Bayesian analyses ##
tauprior    <-  %s
mupriorsd   <-  %s
scaletau    <-  %s
mupriormean <-  %s
#
                ',
                    # the RECALCULATION-TIME snapshot: the priors the displayed
                    # Bayesian models actually used (not the live input values)
                    paste(dQuote(myrvs$bayesSnapshot$tauprior), collapse = ", "),
                    paste(dQuote(myrvs$bayesSnapshot$mupriorsd), collapse = ", "),
                    paste(dQuote(myrvs$bayesSnapshot$scaletau), collapse = ", "),
                    paste(dQuote(myrvs$bayesSnapshot$mupriormean), collapse = ", ")
        )

  code_for_bma <- paste0(someCode)   # start a new block of code
  
  ## Reformat the priors
  someCode <- '
## reformat the priors to work for bayesmeta
if (mupriorsd == "") mupriorsd <- NULL else mupriorsd <- as.numeric(mupriorsd)
if (scaletau == "") scaletau <- NULL else scaletau <- as.numeric(scaletau)
if (mupriormean == "") mupriormean <- NULL else mupriormean <- as.numeric(mupriormean)
#
'
  code_for_bma <- paste0(code_for_bma, someCode)   # update the block of code
  ####### End of code to record the priors ######
  


  ################### Generate bma ###############################
  ## Generate non-reactive R code to create bma bayesmeta object
  #

  someCode <- '

#### Create bma object for Bayesian functions ####

# Function to Create bma bayesmeta object needed for all outputs
createMA.nonReactive <- function(MA, tauprior, mupriorsd, scaletau, mupriormean) {
    ## Generate bayesmeta-object "bma" depending on tau prior chosen
        if (tauprior == "Half cauchy") {
        bma <- bayesmeta(y = MA$es,sigma = sqrt(MA$var), labels = MA$study,
                         tau.prior = function(t) dhalfcauchy(t, scale = scaletau),
                         mu.prior = c("mean" = mupriormean, "sd" = mupriorsd))
      } else if (tauprior == "Half normal") {
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

# calculate bma bayesmeta object (always from the AGGREGATED data)
calculate_bma <- "Yes"  ## Change to "No" if you want to skip the slow Bayesian calculations
if (run_aggregated == "No") calculate_bma <- "No"   # bayesmeta uses the aggregated data
if(calculate_bma == "Yes") {
  bma <- createMA.nonReactive(MA_aggregated, tauprior, mupriorsd, scaletau, mupriormean)
}

'

  code_for_bma <- paste0(code_for_bma, someCode)   # update code block
  #
  ############### End of  Generate bma ###############################
  
  

  ################### Create code_for_plots ###############################
  ## Generate non-reactive R code for the main panel outputs
  #
  code_for_plots <- '
  
## Create a list called "output" to make it easier to adapt the shiny code
output <- list()

######### Create the plots etc. from the results panels ####################

# Outliers panel
create_boxplot <- function(MA) {
  MAo <- MA %>% tibble::rownames_to_column(var = "outlier") %>% mutate(is_outlier=ifelse(is_outlier(es), es, as.numeric(NA)))
  MAo$study[which(is.na(MAo$is_outlier))] <- as.numeric(NA)
  ggplot(MAo, aes(x = factor(0), es)) +
    geom_boxplot(outlier.size = 3.5, outlier.colour = "#D55E00", outlier.shape = 18, fill = "lightgrey") +
    geom_text(aes(label=study),na.rm = T, nudge_y = 0.02, nudge_x = 0.05) +
    stat_boxplot(geom="errorbar", width = 0.05) +
    scale_x_discrete(breaks = NULL) +
    xlab(NULL) + ylab("Hedges g") +
    theme_minimal_hgrid(12)
}
#
output$boxplot <- create_boxplot(MA)
output$boxplot  


########## frequentist analyses

### Frequentist Forest Plot (used by both analysis families below)
create_freq_forest <- function(fma) {
  model <- fma
  # Increase bottom margin to make space for the text
  par(mar = c(5, 4, 4, 2) + 0.1)  # Adjust the bottom margin (the first value)
  # Generate the forest plot
  plot <- metafor::forest.rma(x = model, showweights = TRUE, addfit = TRUE,
                              order = "obs", xlab = "Hedges g",
                              addpred = TRUE,
                              efac = 0,
                              col = "red",
                              border = "red")
  # Add Cochrans Q and heterogeneity statistics as text below the plot
  # (multilevel rma.mv models have no I2; show the variance components instead)
  hetText <- if (inherits(model, "rma.mv")) {
    paste0("Sigma2 between papers = ", round(model$sigma2[1], 3),
           ",  sigma2 within papers = ", round(model$sigma2[2], 3),
           "   (CHE model; summary CI is cluster-robust)")
  } else {
    paste0("I² = ", round(model$I2, 2), "%")
  }
  mtext(side = 1, line = 4,
        text = paste0("Cochrans Q = ", round(model$QE, 2),
                      " (p = ",  format(round(model$QEp, 4), nsmall = 4), ")\n",
                      hetText),
        adj = 0, cex = 0.8)
  # Return the plot
  plot
}
#
######### frequentist analyses: standard two-level model (aggregated data) #########
if (run_aggregated == "Yes") {
  fma <- rma(MA_aggregated$es, MA_aggregated$var, slab=MA_aggregated$study)
  output$freq_forest <- create_freq_forest(fma)
  # Funnel plot (frequentist)
  output$freq_funnel <- funnel(fma, xlab = "Observed outcome")
}

######### frequentist analyses: multilevel CHE model (unaggregated data) #########
# fitMultilevelCHE() (in the downloaded HelperFunctions.R): three-level rma.mv
# with effect sizes nested in papers, within-paper sampling correlation imputed
# at rhoCHE (set above), and cluster-robust (CR2) tests and confidence intervals
# (requires the clubSandwich package)
if (run_multilevel == "Yes") {
  fma_multilevel <- fitMultilevelCHE(MA_multilevel, rho = rhoCHE)
  output$freq_forest_multilevel <- create_freq_forest(fma_multilevel)
  output$freq_funnel_multilevel <- funnel(fma_multilevel, xlab = "Observed outcome")
}


######### bayesian analyses (bayesmeta; aggregated data):
# (skipped entirely when calculate_bma was set to "No" above, since they all
#  need the bma object)
if (calculate_bma == "Yes") {

# Forest Plot panel
output$forest <-  forestplot.bayesmeta(bma, xlab = "Hedges g")
# output$forest

# Funnel Plot panel
output$funnel <-  funnel.bayesmeta(bma, main = "")
# output$funnel

# Statistics panel
output$statistics_panel <-  capture.output({
  cat("Statistics for Bayesian analysis")
  cat("\n\n")
  cat("Bayes Factors:")
  cat("\n")
  print(bma$bayesfactor[1,])
  cat("\n\n")
  cat("Marginal posterior summary:")
  cat("\n")
  print( bma$summary)
  cat("\n")
  cat("Maximum-likelihood:")
  cat("\n")
  print(bma$ML)
  cat("\n\n")
  cat("Joint maximum a-posteriori:")
  cat("\n")
  print( bma$MAP[1,])
})
cat(output$statistics_panel, sep = "\n")


# Additional plots panel
output$evupdate <- priorposteriorlikelihood.ggplot(bma, lowerbound = 0 - (mupriormean + 1) * 1.5, upperbound = 0 + (mupriormean + 1) * 1.5)
output$taupriorplot <-   tauprior.ggplot(bma)
#
print("Prior, posterior, & likelihood")
output$evupdate 
print("Joint posterior density")
output$joint <- plot.bayesmeta(bma, which=2, main = "Joint posterior density of heterogeneity Tau and effect mu")
print("Tau prior distribution")
output$taupriorplot

}  # end of bayesian analyses (calculate_bma guard)

# Bayes factor robustness plot panel (bayesmeta; uses the AGGREGATED data)
#
create_robustplot <- function(tauprior, mupriorsd, scaletau){
  robust = "Yes"  ##Change to "no" if you want to skip this very time consuming section
  robustggplot <- NULL
  if (robust == "Yes" & tauprior == "Half cauchy") {
    robustggplot <- robustness(MA_aggregated,SD = mupriorsd, tauprior = function(t) dhalfcauchy(t, scale = scaletau))
  } else if (robust == "Yes" & tauprior == "Half normal") {
    robustggplot <- robustness(MA_aggregated,SD = mupriorsd, tauprior = function(t) dhalfnormal(t, scale = scaletau))
  }
  robustggplot
}
#
if (calculate_bma == "Yes") {   # needs proper bayesmeta priors and aggregated data
  output$robustplot <- create_robustplot(tauprior, mupriorsd, scaletau)
  print("Bayes Factors over a variety of prior standard deviations:")
  output$robustplot
}
'
  ########## End of  Create code_for_plots ###############################

  codeForDescriptives   <- descriptivesCode()
  codeForMetaRegression <- metaRegCode()
  codeForBmr            <- bmrCode()

  ####### Bayesian multilevel (brms) section ###########
  # Reproduces the "Bayesian Multilevel" / "Bayesian Multilevel Regression" tabs.
  # The moderator is the RECALCULATION-TIME one (bayesSnapshot), matching the app.
  bmlModeratorRecorded <- if (!is.null(myrvs$bayesSnapshot)) myrvs$bayesSnapshot$moderatorName else ""
  code_for_bml <- sprintf('

######### Bayesian multilevel analyses (brms / Stan; UNaggregated data) #########
#   The Bayesian analogue of the multilevel CHE model: a three-level model with
#   effect sizes nested in papers AND their within-paper sampling errors
#   correlated at rhoCHE (brms fcor()), fit with the brms package.  The functions
#   (fitBayesianMultilevel, bmlData, bmlSummaryTable, bmlDiagnosticsText,
#   bmlForestPlot, bmlDensityPlot) are in the downloaded HelperFunctions.R file.
#   NOTE: slow -- the FIRST fit in an R session also compiles the Stan model
#   (about 1-2 minutes); later fits reuse the compiled model.
#   Requires the brms package, a Half cauchy or Half normal tau prior, and a
#   proper (filled-in) mu prior.
calculate_brms <- "No"   ## set to "Yes" to run the (slow) Bayesian multilevel models
bmlModeratorName <- %s    ## moderator for the multilevel regression ("" = none chosen)
if (run_multilevel == "Yes" && calculate_brms == "Yes") {
  properMuPrior <- length(mupriormean) == 1 && !is.na(mupriormean) &&
                   length(mupriorsd) == 1 && !is.na(mupriorsd)
  if (tauprior %%in%% bmlSupportedTauPriors && properMuPrior) {
    ## overall model
    bml <- fitBayesianMultilevel(bmlData(MA_multilevel), tauprior = tauprior,
                                 scaletau = scaletau, mupriormean = mupriormean,
                                 mupriorsd = mupriorsd, rho = rhoCHE, che = !skipCHE)
    cat(bmlDiagnosticsText(bml), "\\n")
    print(bmlSummaryTable(bml))
    print(bmlForestPlot(bml))
    print(bmlDensityPlot(bml))
    ## per-group model (multilevel regression), if a moderator was chosen
    if (nzchar(bmlModeratorName)) {
      bmlReg <- fitBayesianMultilevel(bmlData(MA_multilevel, moderatorName = bmlModeratorName),
                                      tauprior = tauprior, scaletau = scaletau,
                                      mupriormean = mupriormean, mupriorsd = mupriorsd,
                                      rho = rhoCHE, che = !skipCHE)
      cat(bmlDiagnosticsText(bmlReg), "\\n")
      print(bmlSummaryTable(bmlReg))
      print(bmlForestPlot(bmlReg))
    }
  } else {
    cat("Bayesian multilevel skipped: needs a Half cauchy or Half normal tau prior",
        "and a proper (filled-in) mu prior.\\n")
  }
}
', escapeAndDQuote(enc2utf8(bmlModeratorRecorded)))

  ####### Wrapping up and saving the code for display and downloading ###########
  # Put together all the R code
  code_for_R_script <- paste0(code_for_MA, code_for_bma, code_for_plots, codeForDescriptives, codeForMetaRegression, codeForBmr, code_for_bml)
  
  # Create headers and footers for R markdown file
  
  markdown_header <- paste(
    '---',
    'title: "Untitled"',
    'output: html_document',
    '---',
    '',
    '```{r setup, include=FALSE}',
    'knitr::opts_chunk$set(echo = TRUE)',
    '```',
    '',
    '## R Markdown',
    '',
    'This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.',
    '',
    'When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document.',
    '',
    '```{r rcode}',
    '',
    sep = "\n"
  )
  
  markdown_footer <- "```"
  
  # Put the R code into markdown form
  code_for_R_markdown  <- paste(markdown_header, code_for_R_script, markdown_footer, sep = "\n")
  

  # Assign the generated code and markdown to output
  output$R_code_Output <- renderText({ code_for_R_script })
  output$R_markdown_Output <- renderText({ code_for_R_markdown })
  
   ## Download handler for the R code file
   output$downloadCode <- downloadHandler(
     filename = function() {
       "R_code_script.R"
     },
     content = function(file) {
       writeLines(enc2utf8(code_for_R_script), file)
     }
   )
   
   ## Download handler for the R markdown file
   output$downloadMarkdown <- downloadHandler(
     filename = function() {
       "R_code_markdown.Rmd"
     },
     content = function(file) {
       writeLines(enc2utf8(code_for_R_markdown), file)
     }
   )
   
   ## Download file for R code -- done in server.R
   ## Download file for R markdown -- done in  server.R
   
})  # end of observeEvent(MA())

## adding code for the meta-regression plot
source("metaRegression_code_generator.R", local = T)

## adding code for the descriptives tab
source("descriptives_code_generator.R", local = T)

## adding additional reactives to trigger updating the R code before downloading it
## this will replace MA() as the trigger for the observeEvent above
reactiveTriggers <- reactive(list(MA(), metaRegCode(), bmrCode(), descriptivesCode()))


