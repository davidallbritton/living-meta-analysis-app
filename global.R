################### A General Tool for Living Meta-Analysis #################
# v.1.6 2026.11.22
####################################################################################
#
# Version history.  The Explanation tab lists only what is new compared with the app
# described in the 2024 paper; the changes between releases are recorded here.
#
# v1.3 (earliest in this repository) and v1.4
#   Meta-regression with a categorical moderator (frequentist and Bayesian); the
#   Descriptives tab; live study counts in the selection criteria; the curated
#   collection of data files; saving and loading of selection settings; multilevel
#   analyses (frequentist CHE and Bayesian brms three-level models, each with a
#   moderator-regression variant) in their own tabs beside the standard aggregated
#   ones, with downloadable R code for both families.  v1.4: the "Include this tab in
#   the analysis" checkbox on each results tab, which marks the tab in the tab strip,
#   labels its section of the downloaded R code, and is saved in the selections file.
#
# v1.5
#   The standard (aggregated) frequentist random-effects models -- the overall model,
#   the subgroup models and the test of subgroup differences -- use Knapp-Hartung
#   tests and intervals with the scaling factor truncated at 1 (metafor test =
#   "adhoc") instead of Wald z tests, so their intervals are never narrower, and
#   usually wider, than in v1.4.  The test of subgroup differences is reported as an
#   F test with both degrees of freedom in both regression tabs (the multilevel test
#   was already an F test but had been labelled Q).  Multilevel and Bayesian models
#   unchanged; the stated fitting times of the Bayesian multilevel models corrected.
#
# v1.5.1
#   Precomputed Bayesian results for the default data also load instantly with
#   "Aggregate over" set to Papers (rounding differences between computers had
#   stopped them from matching, see sameMAData()); on the hosted app the Bayesian
#   multilevel models for the default data are precomputed too.  No results change.
#
# v1.5.2
#   A within-subject study entered in "Add a Study" from means with both SDs gets the
#   mean difference divided by sqrt((SD1^2 + SD2^2)/2) (metafor SMCRPH), with the
#   variance for unequal SDs; earlier versions divided by the SD of the difference
#   scores (SMCC), a different metric that inflated these effect sizes whenever
#   r > .5.  Standard errors entered for a within-subject study are converted to SDs
#   with the total N.  Studies added earlier keep their stored values.  The
#   Explanation tab states the metric of each effect-size measure and lists
#   references.  Uploaded and curated data files analyse as before.
#
# v1.6
#   * Sharing table (sharing_server.R): an optional structured sampling covariance
#     for the multilevel models -- only effect sizes computed from overlapping data
#     are correlated; the largest valid rho for the selection is shown, and fits
#     above it are refused.  Without a sharing table these models are as in v1.5.2.
#   * Regression tabs: a contrast between two chosen moderator levels (Bayesian tabs
#     with P(> 0)).  In both frequentist regression tabs each subgroup's estimate now
#     comes from one shared model with a mean per level (fitCheCellMeans() /
#     fitRmaCellMeans(), the model the test of subgroup differences already used)
#     instead of a separate model per subgroup, so these estimates differ from
#     v1.5.2; the standard tab still shows each subgroup's own heterogeneity, for
#     description, and a table of the estimates is added under each plot.
#   * Bayesian multilevel tabs: HDIs, prediction intervals, a shrinkage forest plot,
#     a prior-likelihood-posterior plot and posterior predictive checks.
#   * Selections: restored selections that leave out rows only because the data
#     changed since saving get a dialog offering to include them; saved files record
#     the data at save time.  Loading a data file resets the output-tab ticks and the
#     Descriptives crosstabs.
#   * Data files: one reader for uploads, curated files and a configured copy's
#     default (readDataFile()); curated entries can bring a sharing table, saved
#     selections and precomputed results (index columns sharing, selections,
#     precomputed; a local index can override the released one); a configured copy
#     names its defaults in data/default_data.txt, default_sharing.txt,
#     default_selections.txt and default_precomputed.txt.  After a data file loads,
#     one dialog offers its sharing table and selections.
#   * Loaded-files record (loadedFiles_server.R): a "Loaded now" box, a header naming
#     the files the displayed results come from, and the same record in the
#     downloads and the R script's header.
#   * Precomputed results (precomputed_server.R): read at first use, attached per
#     data source and released when the data changes; "Saved Plots and Models"
#     downloads keep only the current data's results; saved models no longer carry
#     the session or the brms template store (fitBayesmeta(), tauPriorFunction()).
#   * Inputs carry password-manager "ignore" markers (ui.R).
#
####################################################################################

# Load required packages and source helper functions #----
library(purrr)
library(metafor)
if (packageVersion("metafor") < "4.6.0")   # escalc(measure = "SMCRPH") for added within-subject studies
  stop("This app needs metafor version 4.6-0 or later.")
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

################## Curated data files (data/curated/) ##############################
# The index of curated files: data/curated/curated_index.csv, then the optional
# data/curated/curated_index_local.csv (same columns) for a local copy's extra
# files.  A file listed in both keeps only its local row, in the released row's
# place, so a local copy can relabel or annotate a released file.  Only files that
# exist are kept; NULL if there are none.  See data/curated/README.md.
readCuratedIndex <- function() {
  files <- file.path("data/curated", c("curated_index.csv", "curated_index_local.csv"))
  files <- files[file.exists(files)]
  if (!length(files)) return(NULL)
  idx <- as.data.frame(dplyr::bind_rows(lapply(files, function(f)
    readr::read_csv(f, show_col_types = FALSE, col_types = readr::cols(.default = "c")))))
  firstPos <- match(idx$filename, idx$filename)
  keep <- !duplicated(idx$filename, fromLast = TRUE)
  idx <- idx[keep, , drop = FALSE][order(firstPos[keep]), , drop = FALSE]
  idx <- idx[file.exists(file.path("data/curated", idx$filename)), , drop = FALSE]
  if (nrow(idx)) idx else NULL
}

################## Default data file for initial display ###########################
# Before a user loads a data file, the app shows a default data set: the built-in
# data/2023updatedData.Rda, unless this app copy names its own.  A copy can open on
# another data set (with default selections and a default sharing table, this makes
# a copy that opens on a published analysis) in either of two ways:
#   * data/default_data.txt: its first line names a data file in the app's format,
#     relative to data/ (for example "curated/mydata.csv", so a curated entry and the
#     startup default can use the same file).  It is read like an upload.
#   * data/default_data.Rda: a data frame called "df" in the app's data-file format.
# Its display name is the first line of data/default_data_label.txt if there is one,
# else the curated index's label for that file, else the file name.
#   defaultDataFile    the default data's file name, as shown in the app
#   defaultDataSource  "built-in" or "default" (this copy's own default)
#   defaultDataBuiltIn TRUE for the built-in data
#   defaultDataLabel   the display name
defaultDataFile <- NULL
if (file.exists("data/default_data.txt")) {
  f <- trimws(readLines("data/default_data.txt", n = 1, warn = FALSE, encoding = "UTF-8"))
  df <- if (length(f) && nzchar(f) && file.exists(file.path("data", f)))
    tryCatch(readDataFile(file.path("data", f)), error = function(e) {
      warning("data/default_data.txt: ", f, " could not be used: ", conditionMessage(e), call. = FALSE)
      NULL })
  else { warning("data/default_data.txt names a file that is not in data/", call. = FALSE); NULL }
  if (!is.null(df)) {
    names(df) <- gsub(":", "_", names(df))          # as removeColons() does for uploads
    defaultDataFile <- basename(f)
    curated <- if (startsWith(f, "curated/")) readCuratedIndex()
    curatedLabel <- if (!is.null(curated)) curated$label[curated$filename == basename(f)]
    defaultDataLabel <- if (length(curatedLabel) == 1) curatedLabel else basename(f)
  }
}
if (is.null(defaultDataFile) && file.exists("data/default_data.Rda")) {
  load(file = "data/default_data.Rda")              # loads a dataframe called "df"
  defaultDataFile <- "default_data.Rda"
  defaultDataLabel <- "default_data.Rda"
}
if (is.null(defaultDataFile)) {
  load(file = "data/2023updatedData.Rda")           # loads a dataframe called "df"
  defaultDataFile <- "2023updatedData.Rda"
  defaultDataLabel <- "Vasilev et al. 2018 plus 2023 updates"
  defaultDataSource <- "built-in"
  defaultDataBuiltIn <- TRUE
} else {
  defaultDataSource <- "default"
  defaultDataBuiltIn <- FALSE
  if (file.exists("data/default_data_label.txt"))
    defaultDataLabel <- trimws(readLines("data/default_data_label.txt", n = 1, warn = FALSE,
                                         encoding = "UTF-8"))
}
suppressWarnings(rm(f, curated, curatedLabel))


################## Precomputed models and plots (shared by the R process) ##########
# A data source can come with precomputed results, in the same four forms as the
# caches of "Saved Plots and Models": bayesmeta models, Bayes-factor robustness
# plots, Bayesian meta-regression models and Bayesian multilevel models.  A set of
# them is identified by a setId:
#   "built-in"            the built-in data's set: data/defaultPrecalculated*.RDS
#   "<folder>"            a folder relative to data/ holding any of models.RDS,
#                         plots.RDS, bmr.RDS and bml.RDS (each a list as saved by
#                         the matching download) -- named by a curated entry's
#                         "precomputed" column, or by data/default_precomputed.txt
# A session attaches the set of the data it has loaded (precomputed_server.R).
# Each file is read only when a session first looks in that cache, then kept here
# and shared by every session (R lists copy on write, so a session adding its own
# fits copies pointers, not the cached objects); it is dropped once no session has
# the set attached.  So the built-in multilevel fits (about 290 MB in memory) are
# never read unless someone fits a multilevel model on the built-in data.
precomputedTypes <- c("models", "plots", "bmr", "bml")
precomputedFiles <- function(setId) {
  if (identical(setId, "built-in"))
    c(models = "data/defaultPrecalculatedModels.RDS", plots = "data/defaultPrecalculatedPlots.RDS",
      bmr = "data/defaultPrecalculatedBmrModels.RDS", bml = "data/defaultPrecalculatedBmlModels.RDS")
  else setNames(file.path("data", setId, paste0(precomputedTypes, ".RDS")), precomputedTypes)
}
precomputedExists <- function(setId) !is.null(setId) && any(file.exists(precomputedFiles(setId)))
precomputedStore <- new.env()     # "<setId>::<type>" -> list of cache entries
precomputedUsers <- new.env()     # setId -> number of sessions with it attached
precomputedEntries <- function(setId, type) {
  key <- paste0(setId, "::", type)
  if (is.null(precomputedStore[[key]])) {
    f <- precomputedFiles(setId)[[type]]
    precomputedStore[[key]] <- if (file.exists(f)) normalizeTauPriorLabels(readRDS(f)) else list()
  }
  precomputedStore[[key]]
}
acquirePrecomputed <- function(setId) {
  n <- precomputedUsers[[setId]]
  precomputedUsers[[setId]] <- if (is.null(n)) 1L else n + 1L
}
releasePrecomputed <- function(setId) {
  n <- precomputedUsers[[setId]]
  if (is.null(n)) return(invisible())
  if (n <= 1L) {
    rm(list = setId, envir = precomputedUsers)
    keys <- intersect(paste0(setId, "::", precomputedTypes), ls(precomputedStore))
    if (length(keys)) rm(list = keys, envir = precomputedStore)
  } else precomputedUsers[[setId]] <- n - 1L
}
## the default data's set: the built-in one for the built-in data; for a configured
## copy, the folder named on the first line of data/default_precomputed.txt
## (relative to data/), if any
defaultPrecomputedSet <- if (defaultDataBuiltIn) "built-in" else {
  if (file.exists("data/default_precomputed.txt")) {
    f <- trimws(readLines("data/default_precomputed.txt", n = 1, warn = FALSE, encoding = "UTF-8"))
    if (length(f) && nzchar(f) && precomputedExists(f)) f
    else { warning("data/default_precomputed.txt names no folder of precomputed results in data/",
                   call. = FALSE); NULL }
  }
}



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
