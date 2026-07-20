#######################################################################################
################### A General Tool for Living Meta-Analysis #################
#######################################################################################
#
################### Save / load selection settings (server) ##########################
#
# Lets the user save every left-panel selection to a .json file and restore it later:
#   - Study criteria: aggregation, live counts, Design, Publication.Year,
#     N_Intervention, the data file's own numeric sliders and factor checkboxes,
#     and the include/exclude-specific-studies list
#   - Moderator Selection: includeModerator, moderator_variable
#   - Prior specifications: mupriormean, mupriorsd, robust, tauprior, scaletau
#   - Dependence (rho): rhoCHE, skipCHE
#   - Descriptives tab: the ticked factor/numeric variables and the variables of
#     every crosstab (a "descriptives" block, applied by descriptives_server.R)
#
# The saved file records which data file it was made for.  Loading a file saved for
# a different data set still applies whatever matches (with a warning) -- that is
# deliberate: the point of a "living" meta-analysis is re-running published
# selections against an updated data file.  Restoring is EXACT: factor levels or
# studies that were not ticked when saved stay unticked, and levels/studies/variables
# that no longer exist are skipped (reported in a notification).  New studies or
# levels added to the data since the save are therefore NOT ticked automatically.
#
# A selections file placed at data/default_selections.json in the app directory is
# applied automatically at startup (only while the default data set is loaded).
# Together with replacing the default data file, that is all it takes to make a
# copy of this app that reproduces the selections of, e.g., a published article.
#
# Applying selections uses the same update*Input mechanism as the "Reset filters"
# button.  The apply step waits until input$Design exists, i.e. until the
# dynamically rendered study-criteria panel is live in the browser, so it works
# both at startup and for a mid-session upload.
#
# This file is sourced (local = T) from server.R.
######################################################################################


## display name of the current data set (same string the descriptives download uses)
currentDataName <- function() {
  if (is.null(myrvs$currentInputFile)) "default (Vasilev et al. 2018 plus 2023 updates)"
  else myrvs$currentInputFile
}

## every input id a selections file may carry, for the current data set
selectionInputIds <- function() {
  c("aggregation", "liveCounts",
    "Design", "Publication.Year", "N_Intervention",
    myrvs$Variable.Numeric.Names, myrvs$Variable.Factor.Names,
    "included",
    "includeModerator", "moderator_variable",
    "mupriormean", "mupriorsd", "robust", "tauprior", "scaletau",
    "rhoCHE", "skipCHE")
}

## current values of all selection inputs (inputs that have not rendered yet are
## simply absent from the result, and absent entries are skipped when applying)
collectSelections <- function() {
  ids <- selectionInputIds()
  sel <- lapply(ids, function(id) input[[id]])
  names(sel) <- ids
  Filter(Negate(is.null), sel)
}

## current Descriptives-tab choices: the ticked variables and the variables of
## every complete crosstab.  This block is ALWAYS written (empty when nothing is
## selected, or when the tab was never opened), so loading a file also clears a
## Descriptives tab that had selections -- the restore stays exact.
collectDescriptives <- function() {
  cts <- lapply(myCrosstabIds(), function(id) input[[paste0("crosstabVars_", id)]])
  # complete crosstabs only; length >= 2 also keeps every saved crosstab a json
  # ARRAY (auto_unbox would turn a 1-variable crosstab into a bare string,
  # making one 2-variable crosstab and two 1-variable ones indistinguishable)
  cts <- Filter(function(v) length(v) >= 2, cts)
  list(factors   = as.character(input$descFactorsChosen),
       numerics  = as.character(input$descNumericsChosen),
       crosstabs = cts)
}


## ---- Save: download the current selections as a .json file ----

## file-name and body of the selection-settings file, shared by the two download
## buttons that serve it: "Save current selections" (Load Data File tab) and the
## list-of-selections download on the Downloads tab (output$listInputs, server.R)
selectionsFileName <- function() paste0("selection_settings_", Sys.Date(), ".json")
writeSelectionsJson <- function(file) {
  jsonlite::write_json(
    list(app      = "A General Tool for Living Meta-Analysis",
         type     = "selection-settings",
         format   = 1,
         saved    = as.character(Sys.Date()),
         dataFile = currentDataName(),
         selections   = collectSelections(),
         descriptives = collectDescriptives()),
    file, auto_unbox = TRUE, pretty = TRUE, digits = NA)
}

output$selectionsDown <- downloadHandler(
  filename = function() selectionsFileName(),
  content = function(file) writeSelectionsJson(file)
)


## ---- Load: apply a selections list to the inputs ----

## selections waiting to be applied (set by the upload observer and the startup
## default loader; consumed by the apply observer below)
pendingSelections <- reactiveVal(NULL)

applySelections <- function(sel) {
  df <- myrvs$df.reactive
  fN <- myrvs$Variable.Factor.Names
  nN <- myrvs$Variable.Numeric.Names
  notes <- character(0)

  lvlsOf <- function(v) {
    lv <- levels(df[[v]])
    if (is.null(lv)) sort(unique(as.character(df[[v]]))) else lv
  }
  applyCheckboxGroup <- function(id, lvls) {
    if (is.null(sel[[id]])) return()
    saved <- as.character(unlist(sel[[id]]))
    keep <- intersect(lvls, saved)
    if (length(keep) < length(saved))
      notes <<- c(notes, sprintf("%s: %d saved selection(s) not in the current data",
                                 id, length(saved) - length(keep)))
    updateCheckboxGroupInput(session, id, selected = keep)
  }
  applySlider <- function(id) {
    saved <- suppressWarnings(as.numeric(unlist(sel[[id]])))
    if (length(saved) == 2 && !anyNA(saved))
      updateSliderInput(session, id, value = saved)  # the slider clamps to its range
  }
  applyRadio <- function(id, valid) {
    saved <- as.character(unlist(sel[[id]]))
    if (!length(saved)) return()
    if (!saved[1] %in% valid) {
      notes <<- c(notes, sprintf('%s: saved value "%s" is not available', id, saved[1]))
      return()
    }
    updateRadioButtons(session, id, selected = saved[1])
  }
  applyNumeric <- function(id) {
    saved <- suppressWarnings(as.numeric(unlist(sel[[id]])))
    if (length(saved) == 1 && !is.na(saved))
      updateNumericInput(session, id, value = saved)
  }
  ## single-value slider (applySlider above is for two-ended range sliders)
  applyOneSlider <- function(id) {
    saved <- suppressWarnings(as.numeric(unlist(sel[[id]])))
    if (length(saved) == 1 && !is.na(saved))
      updateSliderInput(session, id, value = saved)  # the slider clamps to its range
  }

  ## study criteria
  ## ("Multilevel" was a radio option in older saved files; the multilevel
  ## analyses now always run in their own tabs, so map it to ID)
  if (identical(as.character(unlist(sel$aggregation)), "Multilevel")) {
    sel$aggregation <- "ID"
    notes <- c(notes, paste('aggregation "Multilevel" (older file) mapped to ID;',
                            'the multilevel tabs are now always available'))
  }
  applyRadio("aggregation", c("ID", "Papers"))
  if (!is.null(sel$liveCounts))
    updateCheckboxInput(session, "liveCounts", value = isTRUE(unlist(sel$liveCounts)))
  applyCheckboxGroup("Design", lvlsOf("Design"))
  applySlider("Publication.Year")
  applySlider("N_Intervention")
  for (v in intersect(nN, names(sel))) applySlider(v)
  for (v in intersect(fN, names(sel))) applyCheckboxGroup(v, lvlsOf(v))
  applyCheckboxGroup("included", lvlsOf("Paper.and.Exp"))
  # saved selection variables that do not exist in the current data at all
  gone <- setdiff(names(sel), selectionInputIds())
  if (length(gone))
    notes <- c(notes, paste("variables not in the current data:", paste(gone, collapse = ", ")))

  ## moderator
  applyRadio("includeModerator", c("No", "Yes"))
  applyRadio("moderator_variable", c("Design", fN))

  ## priors ("Half student t" was the pre-v1.1 label for the half-normal tau prior)
  if (identical(as.character(unlist(sel$tauprior)), "Half student t")) sel$tauprior <- "Half normal"
  applyNumeric("mupriormean")
  applyNumeric("mupriorsd")
  applyRadio("robust", c("No", "Yes"))
  applyRadio("tauprior", c("Half cauchy", "Half normal", "uniform", "sqrt", "Jeffreys",
                           "BergerDeely", "conventional", "DuMouchel", "shrinkage", "I2"))
  applyNumeric("scaletau")

  ## dependence: within-paper sampling correlation (absent from files saved before
  ## this input existed -- applyOneSlider then simply leaves the 0.5 default)
  applyOneSlider("rhoCHE")
  applyRadio("skipCHE", c("No", "Yes"))

  if (length(notes))
    showNotification(paste("Some saved selections could not be applied --",
                           paste(notes, collapse = ";  ")),
                     type = "warning", duration = 15)
  showNotification('Selections applied.  Press "(Re)Calculate Meta-Analysis" to update the results.',
                   type = "message", duration = 8)
}

## apply pending selections once the study-criteria panel is live in the browser
## (input$Design reporting a value implies the whole rendered UI is bound, so the
## update messages will not be lost)
observe({
  sel <- pendingSelections()
  req(sel, myrvs$df.reactive, input$Design)
  pendingSelections(NULL)
  applySelections(sel)
})

## read + sanity-check a selections file; returns the parsed file or NULL.
## Valid = our type marker plus at least one non-empty block (selections or
## descriptives), so a stray unrelated .json is still rejected.
readSelectionsFile <- function(path) {
  parsed <- tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE),
                     error = function(e) NULL)
  if (is.null(parsed) || !identical(parsed$type, "selection-settings")) return(NULL)
  hasSel  <- is.list(parsed$selections)   && length(parsed$selections)   > 0
  hasDesc <- is.list(parsed$descriptives) && length(Filter(length, parsed$descriptives)) > 0
  if (!hasSel && !hasDesc) return(NULL)
  parsed
}

## the user uploads a selections file
observeEvent(input$SelectionsUp, {
  parsed <- readSelectionsFile(input$SelectionsUp$datapath)
  if (is.null(parsed)) {
    showNotification("This is not a selection-settings file saved by this app.",
                     type = "error", duration = 10)
    return()
  }
  if (!identical(parsed$dataFile, currentDataName()))
    showNotification(sprintf(paste('These selections were saved for the data file "%s", but the',
                                   'current data is "%s".  Whatever matches will be applied;',
                                   'please review the Study criteria panel.'),
                             parsed$dataFile, currentDataName()),
                     type = "warning", duration = 15)
  if (length(parsed$selections)) pendingSelections(parsed$selections)
  # files saved before the descriptives block existed simply lack it: leave the tab alone
  if (!is.null(parsed$descriptives)) pendingDescriptives(parsed$descriptives)
})

## at startup, auto-apply data/default_selections.json (if this app copy has one)
## while the default data set is still loaded; fires once, when the data first
## becomes available (ignoreNULL skips the pre-load NULL state)
observeEvent(myrvs$df.reactive, once = TRUE, {
  if (!is.null(myrvs$currentInputFile)) return()     # another data set beat us to it
  if (!file.exists("data/default_selections.json")) return()
  parsed <- readSelectionsFile("data/default_selections.json")
  if (is.null(parsed)) {
    showNotification("data/default_selections.json could not be read; using standard defaults.",
                     type = "warning", duration = 10)
    return()
  }
  showNotification("Applying this app copy's default selections (data/default_selections.json).",
                   type = "message", duration = 8)
  if (length(parsed$selections)) pendingSelections(parsed$selections)
  if (!is.null(parsed$descriptives)) pendingDescriptives(parsed$descriptives)
})
