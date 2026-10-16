################### A General Tool for Living Meta-Analysis #################
# v.1.5 2026.10.16
#----
# default data file for initial display, before user uploads their own data file:
load(file = "data/2023updatedData.Rda")             # loads a dataframe called "df"

####################################################################################

# Load required packages and source helper functions #----
library(purrr)
library(metafor)
library(clubSandwich)  # cluster-robust (CR2) inference for the multilevel CHE model
library(readxl)
library(writexl)
library(tools)
library(shiny)
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
library(shinyBS)
library(shinycssloaders)
library(shinythemes)
library(stringr)
library(tidyr)
library(xtable)
library(shinyalert)
library(shinyjs)
# library(shinymeta)

options(useFancyQuotes = F)

################### load external R source files ################################
source("HelperFunctions.R")
source("effect_sizes.R")
source("metaRegressionFunctions.R")
source("bayesianMultilevelFunctions.R")   # brms three-level model + compiled-template store


################## Precalculated model/plot caches (loaded ONCE per R process) ###
# Each session seeds its myrvs$previous* caches from these shared lists (see
# server.R and bayesianMetaRegression_server.R).  Because R lists copy on write,
# every session shares the same underlying model objects in memory: appending a
# session's newly computed models copies only the list of pointers, never the
# multi-MB cached entries themselves.  This keeps the memory cost of the
# preloaded models constant per R process, no matter how many sessions connect
# (previously each session called readRDS() itself, holding its own full copy).
loadSeedRDS <- function(path) {
  if (file.exists(path)) normalizeTauPriorLabels(readRDS(path)) else list()
}
defaultModelsSeed <- loadSeedRDS("data/defaultPrecalculatedModels.RDS")
defaultPlotsSeed  <- loadSeedRDS("data/defaultPrecalculatedPlots.RDS")
defaultBmrSeed    <- loadSeedRDS("data/defaultPrecalculatedBmrModels.RDS")
defaultBmlSeed    <- loadSeedRDS("data/defaultPrecalculatedBmlModels.RDS")



################## Output tabs that can be marked as "included" ####################
# The analysis-output tabs of the main panel, in display order.  Each one carries an
# "Include this tab in the analysis" checkbox (input id "includeTab_<value>"); the
# ticked set is highlighted in the tab strip, labelled in the generated R code, and
# saved in the selections .json file.  Purely declarative -- nothing is gated by it.
#
# NAMES are the tabPanel value= (the ids written to the .json; keep stable), VALUES
# are the display titles used in the generated code's labels.  ui.R, the code
# generators and selections_server.R all read this one list.
#
# The Explanation / Included Studies / Current data / Downloads / R Code tabs are
# deliberately absent: no section of the generated script corresponds to them.
includableTabs <- c(
  descriptives                       = "Descriptives",
  outlier_check                      = "Outlier check",
  forest_plot                        = "Forest plot",
  funnel_plot                        = "Funnel plot",
  meta_regression                    = "Meta-Regression",
  frequentist_multilevel             = "Frequentist Multilevel",
  frequentist_multilevel_regression  = "Frequentist Multilevel Regression",
  bayesian_forest_plot               = "Bayesian Forest plot",
  bayesian_funnel_plot               = "Bayesian Funnel plot",
  bayesian_statistics                = "Bayesian Statistics",
  bayesian_additional_plots          = "Bayesian Additional plots",
  bayesian_robustness                = "Bayes factor robustness check",
  bayesian_meta_regression           = "Bayesian Meta-Regression",
  bayesian_multilevel                = "Bayesian Multilevel",
  bayesian_multilevel_regression     = "Bayesian Multilevel Regression"
)
includableTabValues <- names(includableTabs)

################## Constants #######################################################
printButton <- HTML('<p  style="text-align:right; font-size: 8px;"><button  onClick="window.print()">PRINT</button></p>')
r_estimate <- 0.74326344959  # Vasilev et al.'s estimate of the correlation between outcomes in a single study (for "within" designs)
thisYear <- 2024    # default for entering new data points
first_shiny_meta_paper_full <- "Wolf, V., Kühnel, A., Teckentrup, V., Koenig, J., & Kroemer, N. B. (2021). Does transcutaneous auricular vagus nerve stimulation affect vagally mediated heart rate variability? A living and interactive Bayesian meta‐analysis. Psychophysiology, 58(11), e13933."
first_shiny_meta_paper <- "Wolf, et al. (2021)"
first_shiny_meta_paper_doi <- "https://doi.org/10.1111/psyp.13933"
first_shiny_meta_paper_app <- "https://vinzentwolf.shinyapps.io/taVNSHRVmeta"
noise_meta_paper_full <- "Vasilev, M. R., Kirkby, J. A., & Angele, B. (2018). Auditory distraction during reading: A Bayesian meta-analysis of a continuing controversy. Perspectives on Psychological Science, 13(5), 567-597."
noise_meta_paper <- "Vasilev, et al. (2018)"

################## Stan compilation flags on shinyapps.io ##########################
# g++ compiling a Stan model with the default "-g -O2" flags is OOM-killed even on
# 4 GB instances (server logs 2026-07-20: oom events during make).  Compile with
# -O1 and no debug symbols there instead: far lower compiler memory, slightly
# slower sampling.  Local machines keep the defaults.
if (Sys.getenv("R_CONFIG_ACTIVE") == "shinyapps") {
  dir.create(path.expand("~/.R"), showWarnings = FALSE)
  writeLines("CXX17FLAGS = -O1 -g0", path.expand("~/.R/Makevars"))
}
