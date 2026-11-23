#######################################################################################
################### A General Tool for Living Meta-Analysis #################
#######################################################################################
# v.1.6 2026.11.22
#
################### Sharing table (structured sampling covariance) ####################
#
# An optional .csv (columns ID and unit) saying which effect sizes were computed
# from overlapping data.  When one is loaded, the multilevel analyses (frequentist
# CHE and the Bayesian multilevel tabs) correlate the sampling errors of only the
# effect sizes that share a unit, instead of every pair from the same paper.  The
# reader, the covariance builder and the rule itself are in HelperFunctions.R
# (readSharingTable, buildSamplingV).
#
# Sources of a sharing table, each replacing the previous one:
#   * the "Load sharing table..." upload in the "Load Data File" tab;
#   * data/default_sharing.csv, loaded at startup with the default data set
#     (with data/default_selections.json and the default data file, this lets a
#     copy of the app open on a published analysis);
#   * a curated data file whose row in data/curated/curated_index.csv names a
#     sharing table in the optional column "sharing".
# Loading a different data file removes the sharing table (loadDataFile() in
# server.R), because its IDs belong to the previous data.
#
# Like rho and the priors, the table is captured in myrvs$bayesSnapshot by MA() at
# "(Re)Calculate Meta-Analysis", and the analyses read it only from there.
#
# This file is sourced (local = T) from server.R.
######################################################################################

myrvs$sharingTable <- NULL
myrvs$sharingTableName <- NULL
## how the snapshotted table applies to the rows of the last recalculation
## (set by MA(); NULL when no table was in use)
myrvs$sharingStatus <- NULL

## Read a sharing table from a path and make it the current one.  Returns TRUE
## on success; on failure keeps the previous table and shows the reason.
loadSharingFile <- function(path, displayName) {
  st <- tryCatch(readSharingTable(path), error = function(e) e)
  if (inherits(st, "error")) {
    showNotification(paste0('The sharing table "', displayName, '" could not be used: ',
                            conditionMessage(st), "."), type = "error", duration = 20)
    return(FALSE)
  }
  myrvs$sharingTable <- st
  myrvs$sharingTableName <- displayName
  dataIds <- as.character(isolate(myrvs$df.reactive)$ID)
  notInData <- setdiff(unique(st$ID), dataIds)
  showNotification(paste0('Sharing table "', displayName, '" loaded: ', nrow(st),
                          " membership line(s) for ", length(unique(st$ID)), " effect size ID(s)",
                          if (length(notInData))
                            paste0("; ", length(notInData), " of those IDs are not in the current data")
                          else "",
                          '.  Press "(Re)Calculate Meta-Analysis" to use it.'),
                   type = if (length(notInData)) "warning" else "message", duration = 12)
  TRUE
}

clearSharingTable <- function() {
  myrvs$sharingTable <- NULL
  myrvs$sharingTableName <- NULL
  shinyjs::reset("SharingUp")
}

## startup: this app copy's default sharing table, with the default data set
observeEvent(myrvs$df.reactive, once = TRUE, {
  if (file.exists("data/default_sharing.csv"))
    loadSharingFile("data/default_sharing.csv", "data/default_sharing.csv")
})

observeEvent(input$SharingUp, {
  loadSharingFile(input$SharingUp$datapath, input$SharingUp$name)
})

observeEvent(input$clearSharing, {
  clearSharingTable()
  showNotification('Sharing table removed.  Press "(Re)Calculate Meta-Analysis" to return to the flat structure.',
                   type = "message", duration = 8)
})

## One-paragraph description of the sampling covariance a set of results used,
## from the snapshot of the last recalculation.  Shown in every multilevel tab.
covarianceNote <- function(snap = myrvs$bayesSnapshot) {
  if (is.null(snap)) return(NULL)
  rhoText <- paste0("ρ = ", format(snap$rhoCHE))
  if (is.null(snap$sharing)) {
    return(p(tags$small(tags$b("Sampling covariance: flat."),
                        "Every pair of effect sizes from the same paper is correlated at", rhoText,
                        "(no sharing table loaded).")))
  }
  stt <- myrvs$sharingStatus
  p(tags$small(tags$b("Sampling covariance: structured by the sharing table",
                      paste0('"', snap$sharingName, '".')),
               "Effect sizes that share a unit are correlated at", paste0(rhoText, ";"),
               "all other pairs are independent.",
               if (!is.null(stt))
                 paste0(stt$nShared, " pair(s) correlated; ",
                        stt$nUnmatched, " of ", stt$nRows,
                        " effect size(s) are not in the table and are treated as independent.")))
}

## status shown in the "Load Data File" and "Dependence" tabs
sharingStatusUI <- function(withButton = FALSE) {
  st <- myrvs$sharingTable
  snap <- myrvs$bayesSnapshot
  stt <- myrvs$sharingStatus
  current <- if (is.null(st)) {
    p(tags$small("No sharing table loaded: the multilevel analyses use the flat structure,",
                 "in which every pair of effect sizes from the same paper is correlated at ρ."))
  } else {
    dataIds <- as.character(myrvs$df.reactive$ID)
    notInData <- setdiff(unique(st$ID), dataIds)
    tagList(
      p(tags$small(tags$b(paste0('Sharing table loaded: "', myrvs$sharingTableName, '"')),
                   paste0("(", nrow(st), " membership line(s), ", length(unique(st$ID)),
                          " effect size ID(s)",
                          if (length(notInData)) paste0(", ", length(notInData),
                                                        " of them not in the current data") else "",
                          ")."))),
      if (withButton) actionButton("clearSharing", "Remove sharing table", class = "btn-sm")
    )
  }
  lastRun <- if (!is.null(snap) && !is.null(snap$sharing) && !is.null(stt)) {
    above <- isTRUE(snap$rhoCHE > 0 && snap$rhoCHE >= stt$rhoCeiling - 1e-8)
    p(tags$small(
      "At the last recalculation: ", stt$nRows, " effect sizes, ",
      stt$nUnmatched, " of them not in the table (treated as independent), ",
      stt$nShared, " pair(s) correlated.  Largest ρ for which the sampling covariance",
      " is positive definite with this selection: ",
      tags$b(paste0(if (is.finite(stt$rhoCeiling)) sprintf("below %.3f", stt$rhoCeiling)
                    else "any value", ".")),
      if (above) tags$span(style = "color:#c0392b;",
                           paste0(" The ρ used (", format(snap$rhoCHE), ") is not below it, so the",
                                  " multilevel analyses cannot be fitted; lower ρ and recalculate."))))
  }
  tagList(current, lastRun)
}
output$sharingStatusLoad <- renderUI(sharingStatusUI(withButton = TRUE))
output$sharingStatusDependence <- renderUI(sharingStatusUI())
output$covarianceNoteFml <- renderUI({ MAml(); covarianceNote() })
