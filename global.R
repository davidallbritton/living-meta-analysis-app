################### A General Tool for Living Meta-Analysis #################
# v.1.2 2026.07.18
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
