#######################################################################################
################### A General Tool for Living Meta-Analysis #################
#######################################################################################
# v.1.6 2026.11.22
#
################### What is loaded, and what the results are from ####################
#
# Keeps a record of the three files an analysis depends on, each with its own file
# name and where it came from:
#   * the data file      (myrvs$dataFile, myrvs$dataSource: "built-in", "default",
#                         "curated" or "upload"; set by loadDataFile() in server.R)
#   * the sharing table  (myrvs$sharingTableName, myrvs$sharingSource; sharing_server.R)
#   * the selections file whose settings were applied (myrvs$selectionsFile,
#                         myrvs$selectionsSource; set when selections_server.R applies
#                         a file, cleared when another data file is loaded)
# and shows it twice:
#   * "Loaded now" (output$loadedFilesNow, top of the "Load Data File" tab): the
#     current files, with a note when the settings have been changed since the
#     selections file was applied;
#   * the header above "(Re)Calculate Meta-Analysis" (output$currentDataFile): the
#     files and settings the displayed results were computed from (captured at
#     "(Re)Calculate"), with a note when anything has changed since; before the
#     first calculation it shows what is loaded.
# loadedFilesTable() gives the same record as a two-column table for the downloads
# (selections file, current-data and bayesmeta workbooks, descriptives workbook) and
# the generated R script's header.
#
# This file is sourced (local = T) from server.R, after selections_server.R and
# sharing_server.R.
######################################################################################

myrvs$dataFile <- defaultDataFile           # global.R
myrvs$dataSource <- defaultDataSource
myrvs$selectionsFile <- NULL
myrvs$selectionsSource <- NULL
myrvs$selectionsBaseline <- NULL            # the settings just after the file was applied
myrvs$resultsInfo <- NULL                   # the record at the last "(Re)Calculate"

clearSelectionsRecord <- function() {
  myrvs$selectionsFile <- NULL
  myrvs$selectionsSource <- NULL
  myrvs$selectionsBaseline <- NULL
}

## Called once a selections file's settings have been sent to the inputs.  The
## browser reports the new values over the next moments, so the baseline for
## "changed since loading" is taken once two readings half a second apart agree.
recordSelectionsFile <- function(fileName, source) {
  if (is.null(fileName)) return(invisible())
  myrvs$selectionsFile <- fileName
  myrvs$selectionsSource <- if (is.null(source)) "upload" else source
  myrvs$selectionsBaseline <- NULL
  settleSelectionsBaseline()
}
settleSelectionsBaseline <- function(prev = NULL, tries = 0) {
  domain <- getDefaultReactiveDomain()
  later::later(function() {
    if (is.null(domain) || domain$isClosed()) return(invisible())   # the session has ended
    withReactiveDomain(domain, isolate({
    if (is.null(myrvs$selectionsFile)) return(invisible())   # another data file came in
    cur <- collectSelections()
    if (tries >= 10 || (!is.null(prev) && sameSettings(cur, prev)))
      myrvs$selectionsBaseline <- cur
    else settleSelectionsBaseline(cur, tries + 1)
  }))}, 0.5)
}

## Two sets of selection settings (collectSelections()) are the same if every input
## present in both has the same value; an input that has only just rendered (absent
## from one of them) is not a change.
sameSettings <- function(a, b) {
  k <- intersect(names(a), names(b))
  norm <- function(v) if (is.numeric(v)) as.numeric(v) else as.character(unlist(v))
  identical(lapply(a[k], norm), lapply(b[k], norm))
}

selectionsChanged <- reactive({
  b <- myrvs$selectionsBaseline
  !is.null(myrvs$selectionsFile) && !is.null(b) && !sameSettings(collectSelections(), b)
})

## the current record
loadedState <- reactive({
  dfU <- myrvs$df.updated; dfO <- myrvs$df.original
  list(dataLabel  = if (is.null(myrvs$currentInputFile)) defaultDataLabel else myrvs$currentInputFile,
       dataFile   = myrvs$dataFile,
       dataSource = myrvs$dataSource,
       nfiles     = myrvs$nfiles,
       nES        = if (is.null(dfU)) NA else nrow(dfU),
       nAdded     = if (is.null(dfU) || is.null(dfO)) 0 else nrow(dfU) - nrow(dfO),
       sharingName   = myrvs$sharingTableName,
       sharingSource = myrvs$sharingSource,
       sharingLines  = if (is.null(myrvs$sharingTable)) NA else nrow(myrvs$sharingTable),
       selectionsFile    = myrvs$selectionsFile,
       selectionsSource  = myrvs$selectionsSource,
       selectionsChanged = selectionsChanged(),
       precomputedSet    = myrvs$precomputedSet)
})

dataSourceText <- c("built-in" = "the app's built-in data", default = "the default data set, from the curated collection",
                    curated = "from the curated collection", upload = "uploaded")
sharingSourceText <- c(default = "comes with the default data set",
                       curated = "comes with the curated data file", upload = "uploaded")
selectionsSourceText <- c(default = "applied at startup", curated = "came with the curated data file",
                          upload = "loaded from a file")
srcText <- function(table, key) if (is.null(key) || is.na(table[key])) "" else unname(table[key])

## The record as plain-text values (for downloads and the generated script)
loadedFilesValues <- function(st) {
  c("Data set"      = st$dataLabel,
    "Data file"     = paste0(st$dataFile, " (", srcText(dataSourceText, st$dataSource), ")"),
    "Effect sizes"  = paste0(st$nES, if (st$nAdded > 0)
                               paste0(", including ", st$nAdded, ' added via "Add a Study"') else ""),
    "Sharing table" = if (is.null(st$sharingName))
                        "none (every pair of effect sizes from the same paper correlated at rho)"
                      else paste0(st$sharingName, " (", st$sharingLines, " membership lines; ",
                                  srcText(sharingSourceText, st$sharingSource), ")"),
    "Selections file" = if (is.null(st$selectionsFile))
                          "none (standard starting settings, or settings made by hand)"
                        else paste0(st$selectionsFile, " (", srcText(selectionsSourceText, st$selectionsSource),
                                    if (isTRUE(st$selectionsChanged)) "; settings changed since" else "", ")"),
    "Precomputed results" = precomputedText(st$precomputedSet))
}
## which precomputed results come with the data, in words (precomputed_server.R)
precomputedText <- function(setId) {
  if (is.null(setId)) return("none")
  kinds <- c(models = "bayesmeta models", plots = "robustness plots",
             bmr = "meta-regression models", bml = "multilevel models")
  f <- precomputedFiles(setId)
  have <- kinds[setNames(file.exists(f), names(f))[names(kinds)]]
  paste0(if (identical(setId, "built-in")) "the built-in set" else basename(setId),
         " (", paste(have, collapse = ", "), ")")
}
loadedFilesTable <- function(st = isolate(loadedState())) {
  v <- loadedFilesValues(st)
  data.frame(Item = names(v), Value = unname(v), check.names = FALSE)
}

## The record as a list for the app.  compact = TRUE for the narrow header.
loadedFilesList <- function(st, compact = FALSE) {
  muted <- function(...) span(style = "color:#666", ...)
  dataLine <- tagList(
    tags$b("Data: "),
    if (!identical(st$dataLabel, st$dataFile)) tagList(st$dataLabel, " "),
    tags$code(st$dataFile),
    if (!compact) muted(paste0(" (", srcText(dataSourceText, st$dataSource), "; ", st$nES, " effect sizes",
                               if (st$nAdded > 0) paste0(", including ", st$nAdded, ' added via "Add a Study"')
                               else "", ")"))
    else if (st$nAdded > 0) muted(paste0(" + ", st$nAdded, " added")))
  sharingLine <- tagList(
    tags$b("Sharing table: "),
    if (is.null(st$sharingName))
      muted(if (compact) "none" else "none: every pair of effect sizes from the same paper is correlated at ρ")
    else tagList(tags$code(st$sharingName),
                 if (!compact) muted(paste0(" (", st$sharingLines, " membership lines; ",
                                            srcText(sharingSourceText, st$sharingSource), ")"))))
  selLine <- tagList(
    tags$b("Selections: "),
    if (is.null(st$selectionsFile))
      muted(if (compact) "none loaded" else "none loaded: the standard starting settings, or settings made by hand")
    else tagList(tags$code(st$selectionsFile),
                 if (!compact) muted(paste0(" (", srcText(selectionsSourceText, st$selectionsSource), ")")),
                 if (isTRUE(st$selectionsChanged))
                   span(style = "color:#a94442", if (compact) " (changed)" else
                        " — settings have been changed since it was applied")))
  tags$ul(style = paste0("padding-left:18px; margin-bottom:4px;", if (compact) " font-size:90%;" else ""),
          tags$li(dataLine), tags$li(sharingLine), tags$li(selLine),
          if (!compact) tags$li(tags$b("Precomputed results: "),
                                if (is.null(st$precomputedSet)) muted("none")
                                else muted(precomputedText(st$precomputedSet))))
}

output$loadedFilesNow <- renderUI({
  div(class = "well well-sm", style = "margin-top:6px; margin-bottom:10px; font-size:90%",
      tags$b("Loaded now"),
      loadedFilesList(loadedState()))
})

## capture the record (and the settings) at each "(Re)Calculate"
observeEvent(input$recalculateButton, {
  myrvs$recalculatedSinceUpload <- 1
  myrvs$resultsInfo <- c(loadedState(), list(settings = collectSelections()))
}, priority = 10)

output$currentDataFile <- renderUI({
  st <- loadedState()
  ri <- myrvs$resultsInfo
  if (is.null(ri))
    return(div(style = "margin-bottom:6px",
               p(style = "margin-bottom:2px", "Loaded (no results yet):"),
               loadedFilesList(st, compact = TRUE)))
  changes <- c(
    "the data" = !identical(ri[c("dataLabel", "dataFile", "nfiles", "nES")],
                            st[c("dataLabel", "dataFile", "nfiles", "nES")]),
    "the sharing table" = !identical(ri$sharingName, st$sharingName) ||
                          !identical(ri$sharingLines, st$sharingLines),
    "the selections file" = !identical(ri$selectionsFile, st$selectionsFile),
    "the settings" = !sameSettings(collectSelections(), ri$settings))
  div(style = "margin-bottom:6px",
      p(style = "margin-bottom:2px", "The currently displayed results are from:"),
      loadedFilesList(ri, compact = TRUE),
      if (any(changes))
        p(style = "color:#a94442; font-size:90%; margin-bottom:2px",
          paste0("Changed since then: ", paste(names(changes)[changes], collapse = ", "),
                 '.  Press "(Re)Calculate Meta-Analysis" to update the results.')))
})

## ---- Companion files offered after a data file loads ----
## After a data file is loaded (uploaded, or a curated file that does not supply
## both), one dialog offers the two optional files that go with it: a sharing table
## and a selections file, each only if the data did not come with it.  Each upload
## works like its counterpart in the "Load Data File" tab.  The dialog closes by
## itself once every offered file is loaded, or with "Continue".
companionOffer <- reactiveValues(sharing = FALSE, selections = FALSE,
                                 sharingDone = NULL, selectionsDone = NULL, savedSharing = NULL)

offerCompanionFiles <- function(dataName, sharing = TRUE, selections = TRUE) {
  if (!sharing && !selections) return(invisible())
  companionOffer$sharing <- sharing; companionOffer$selections <- selections
  companionOffer$sharingDone <- NULL; companionOffer$selectionsDone <- NULL
  companionOffer$savedSharing <- NULL
  both <- sharing && selections
  showModal(modalDialog(
    title = if (both) "Load the files that go with this data? (optional)"
            else if (sharing) "Load a sharing table for this data file? (optional)"
            else "Load saved selections for this data file? (optional)",
    p(paste0("If you have ",
             if (both) "a sharing table or saved selections" else if (sharing) "a sharing table"
             else "a saved selections file",
             ' for "', dataName, '", load ', if (both) "them" else "it", " here.  You can also load ",
             if (both) "them" else "it", ' later in the "Load Data File" tab.')),
    if (sharing) tagList(
      tags$b("Sharing table"),
      p(style = "font-size:90%",
        "Which effect sizes were computed from overlapping data (the same participants, or",
        "a shared control group), so that the multilevel analyses correlate only those",
        "effect sizes.  Without one, every pair of effect sizes from the same paper is",
        "treated as correlated.  A .csv with columns", tags$code("ID"), "and", tags$code("unit"),
        ", one line per membership."),
      fileInput("SharingUpModal", label = NULL, buttonLabel = "Load sharing table...",
                accept = ".csv")),
    if (selections) tagList(
      tags$b("Selections"),
      p(style = "font-size:90%",
        'A selections file saved with "Save current selections" (.json): study criteria,',
        "moderator, priors and the other settings of a saved analysis, for example a",
        "published one.  Without one, the analysis starts with every study included."),
      fileInput("SelectionsUpModal", label = NULL, buttonLabel = "Load selections...",
                accept = ".json")),
    uiOutput("companionStatus"),
    footer = modalButton("Continue"),
    easyClose = TRUE))
}

output$companionStatus <- renderUI({
  done <- c(if (!is.null(companionOffer$sharingDone))
              paste0("Sharing table loaded: ", companionOffer$sharingDone, "."),
            if (!is.null(companionOffer$selectionsDone))
              paste0("Selections loaded: ", companionOffer$selectionsDone, "."))
  note <- if (!is.null(companionOffer$savedSharing) && is.null(companionOffer$sharingDone))
    paste0('These selections were saved with the sharing table "', companionOffer$savedSharing,
           '"; load it above to reproduce the saved analysis.')
  if (!length(done) && is.null(note)) return(NULL)
  div(style = "font-size:90%",
      if (length(done)) p(style = "color:#3c763d; margin-bottom:4px", paste(done, collapse = "  ")),
      if (!is.null(note)) p(style = "color:#8a6d3b", note))
})

## close the dialog once every offered file has been loaded
closeCompanionIfDone <- function() {
  if ((!companionOffer$sharing || !is.null(companionOffer$sharingDone)) &&
      (!companionOffer$selections || !is.null(companionOffer$selectionsDone)))
    removeModal()
}

observeEvent(input$SharingUpModal, {
  if (loadSharingFile(input$SharingUpModal$datapath, input$SharingUpModal$name, "upload")) {
    companionOffer$sharingDone <- input$SharingUpModal$name
    closeCompanionIfDone()
  }
})

observeEvent(input$SelectionsUpModal, {
  ## the sharing-table check waits if the dialog still offers a sharing table
  sharingStillOffered <- companionOffer$sharing && is.null(companionOffer$sharingDone)
  parsed <- loadSelectionsUpload(input$SelectionsUpModal$datapath, input$SelectionsUpModal$name,
                                 checkSharing = !sharingStillOffered)
  if (is.null(parsed)) return()
  companionOffer$selectionsDone <- input$SelectionsUpModal$name
  if (sharingStillOffered && length(parsed$sharingTable) && !is.na(parsed$sharingTable[1]))
    companionOffer$savedSharing <- as.character(parsed$sharingTable[1])
  closeCompanionIfDone()
})
