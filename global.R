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
#     precomputed; a local index can override the released one).  The default data
#     set is the curated entry marked in the index's "default" column (a local
#     index's mark wins), loaded with everything it brings; the built-in .Rda is
#     only a fallback.  After a data file loads,
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
# files.  An entry is a data file together with its selections file (column
# "selections", blank for none), so several entries can share one data file, each
# a different analysis of it with its own label, selections and precomputed
# results.  An entry listed in both indexes (same data file and selections file)
# keeps only its local row, in the released row's place, so a local copy can
# relabel or annotate a released entry.  Only entries whose data file exists are
# kept; NULL if there are none.  See data/curated/README.md.
#   .local  TRUE for a row from the local index
#   .value  the entry's value in the "curatedDataFile" dropdown: its file name, or,
#           for entries that share a data file, the file name and selections file
readCuratedIndex <- function() {
  files <- file.path("data/curated", c("curated_index.csv", "curated_index_local.csv"))
  files <- files[file.exists(files)]
  if (!length(files)) return(NULL)
  idx <- as.data.frame(dplyr::bind_rows(lapply(files, function(f)
    dplyr::mutate(readr::read_csv(f, show_col_types = FALSE, col_types = readr::cols(.default = "c")),
                  .local = basename(f) == "curated_index_local.csv"))))
  sel <- if ("selections" %in% names(idx)) trimws(ifelse(is.na(idx$selections), "", idx$selections))
         else rep("", nrow(idx))
  key <- paste(idx$filename, sel, sep = "\r")
  firstPos <- match(key, key)
  keep <- !duplicated(key, fromLast = TRUE)
  idx <- idx[keep, , drop = FALSE][order(firstPos[keep]), , drop = FALSE]
  sel <- sel[keep][order(firstPos[keep])]
  ok <- file.exists(file.path("data/curated", idx$filename))
  idx <- idx[ok, , drop = FALSE]; sel <- sel[ok]
  if (!nrow(idx)) return(NULL)
  shared <- duplicated(idx$filename) | duplicated(idx$filename, fromLast = TRUE)
  idx$.value <- ifelse(shared, paste0(idx$filename, " | ", ifelse(nzchar(sel), sel, "no selections")),
                       idx$filename)
  idx
}

################## Default data set for initial display ###########################
# Before a user loads a data file, the app shows the curated entry marked "yes" in
# the curated index's "default" column, with everything that entry brings: its
# sharing table, saved selections and precomputed results (as if it had been
# chosen from the dropdown).  An entry marked in a local copy's
# curated_index_local.csv takes precedence over the released index, so a copy can
# open on its own data set (for example a published analysis).  Without a marked
# entry, or if its file cannot be read, the app falls back on the built-in
# data/2023updatedData.Rda (with a v1.5.2-style data/default_selections.json, if
# present).
#   defaultDataFile        the default data's file name, as shown in the app
#   defaultDataSource      "default" (a curated entry) or "built-in" (the fallback)
#   defaultDataBuiltIn     TRUE for the fallback
#   defaultDataLabel       the display name (the entry's label)
#   defaultSharingFile     the default's sharing table, relative to data/ (or NULL)
#   defaultSelectionsFile  the default's selections file, relative to data/ (or NULL)
#   defaultEntry           the entry's row of the curated index (or NULL); its
#                          precomputed results are set up below (defaultPrecomputedSet)
curatedField <- function(row, col) {
  v <- if (!is.null(row) && col %in% names(row)) as.character(row[[col]]) else NA_character_
  if (length(v) == 1 && !is.na(v) && nzchar(trimws(v))) trimws(v) else NULL
}
defaultEntry <- local({
  idx <- readCuratedIndex()
  if (is.null(idx) || !"default" %in% names(idx)) return(NULL)
  marked <- idx[tolower(trimws(idx$default)) %in% c("yes", "true", "1"), , drop = FALSE]
  if (!nrow(marked)) return(NULL)
  marked <- marked[order(!marked$.local), , drop = FALSE]     # a local entry first
  marked[1, , drop = FALSE]
})
df <- if (!is.null(defaultEntry))
  tryCatch(readDataFile(file.path("data/curated", defaultEntry$filename)), error = function(e) {
    warning("The default curated entry ", defaultEntry$filename, " could not be read: ",
            conditionMessage(e), "; using the built-in data.", call. = FALSE)
    NULL })
if (!is.null(df)) {
  names(df) <- gsub(":", "_", names(df))          # as removeColons() does for uploads
  defaultDataFile <- defaultEntry$filename
  defaultDataLabel <- defaultEntry$label
  defaultDataSource <- "default"
  defaultDataBuiltIn <- FALSE
  existing <- function(f) if (!is.null(f) && file.exists(file.path("data/curated", f))) file.path("curated", f)
  defaultSharingFile <- existing(curatedField(defaultEntry, "sharing"))
  defaultSelectionsFile <- existing(curatedField(defaultEntry, "selections"))
} else {
  defaultEntry <- NULL
  load(file = "data/2023updatedData.Rda")           # loads a dataframe called "df"
  defaultDataFile <- "2023updatedData.Rda"
  defaultDataLabel <- "Vasilev et al. 2018 plus 2023 updates"
  defaultDataSource <- "built-in"
  defaultDataBuiltIn <- TRUE
  defaultSharingFile <- NULL
  defaultSelectionsFile <- if (file.exists("data/default_selections.json")) "default_selections.json"
}


################## Precomputed models and plots (shared by the R process) ##########
# A data source can come with precomputed results, in the same four forms as the
# caches of "Saved Plots and Models": bayesmeta models, Bayes-factor robustness
# plots, Bayesian meta-regression models and Bayesian multilevel models.  A set of
# them is identified by a setId, a folder relative to data/ holding any of
# models.RDS, plots.RDS, bmr.RDS and bml.RDS (each a list as saved by the matching
# download), named by a curated entry's "precomputed" column (for example
# "curated/vasilev_reading_noise_2023_update_precomputed", the default data's set).
# A session attaches the set of the data it has loaded (precomputed_server.R).
# Each file is read only when a session first looks in that cache, then kept here
# and shared by every session (R lists copy on write, so a session adding its own
# fits copies pointers, not the cached objects); it is dropped once no session has
# the set attached.  So the default data's multilevel fits (about 290 MB in memory)
# are never read unless someone fits a multilevel model on that data.
precomputedTypes <- c("models", "plots", "bmr", "bml")
precomputedFiles <- function(setId)
  setNames(file.path("data", setId, paste0(precomputedTypes, ".RDS")), precomputedTypes)
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
## the default data's set: the "precomputed" folder of the default curated entry
defaultPrecomputedSet <- local({
  f <- curatedField(defaultEntry, "precomputed")
  if (!is.null(f) && precomputedExists(file.path("curated", f))) file.path("curated", f)
})



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
