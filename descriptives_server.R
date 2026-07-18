#######################################################################################
################### A General Tool for Living Meta-Analysis #################
#######################################################################################
#
################### Descriptives panel (server) #######################################
#
# Descriptive statistics for the CURRENTLY SELECTED data: the effect-size rows that
# pass all of the current sidebar study-selection filters (via applyAllFilters() in
# HelperFunctions.R).  Like the "Live counts" badges -- and unlike the results tabs --
# everything here updates immediately as the selection changes, with no Recalculate.
#
# The user ticks variables for individual descriptives (factors -> frequency tables
# with ES and study counts; numerics -> summary statistics), and can build any
# number of crosstabs ("Add another crosstab"), each over 2 to 4 variables.  A
# crosstab of factors shows effect-size counts; including ONE numeric variable
# switches its cells to that variable's means, shown as "mean (n)".  Variable
# choices use the same labels (plain column names) as the sidebar selection panel.
#
# The tables are computed in reactives (descNumericSummary / descFreqTables /
# descCrosstab) shared by the on-screen display and the .xlsx download handler,
# so what is downloaded is exactly what is displayed.
#
# This file is sourced (local = T) from server.R.
######################################################################################


## The currently selected rows (live; updates as the selection panel changes)
descSelectedData <- reactive({
  df <- myrvs$df.reactive
  req(df)
  applyAllFilters(df, input, myrvs$Variable.Factor.Names, myrvs$Variable.Numeric.Names)
})

## Variables offered for descriptives: the same selection variables shown in the
## sidebar "Study criteria" panel, using the same labels (plain column names)
descFactorVars  <- reactive(c("Design", myrvs$Variable.Factor.Names))
descNumericVars <- reactive(c("Publication.Year", "N_Intervention", myrvs$Variable.Numeric.Names))


## ---- Table computations (shared by the display and the download) ----

## Summary statistics for the ticked numeric variables: one row per variable.
## NULL when none are ticked.  (Table built by descNumericSummaryTable() in
## HelperFunctions.R, shared with the downloaded reproducibility code.)
descNumericSummary <- reactive({
  chosenN <- input$descNumericsChosen
  if (!length(chosenN)) return(NULL)
  d <- descSelectedData()
  req(nrow(d) > 0)
  descNumericSummaryTable(d, chosenN)
})

## Levels of a selection factor that are currently ticked in the sidebar, in the
## data's level order.  Ticked levels are shown EVERYWHERE (frequency tables and
## crosstabs) even when other criteria leave them with zero rows -- an informative
## zero ("a category you meant to include contributes nothing").  Unticked levels
## are omitted everywhere: their zero is trivially true.  Falls back to all levels
## while the sidebar checkboxes have not rendered yet.
descTickedLevels <- function(v) {
  fullData <- myrvs$df.reactive
  lvls <- levels(fullData[[v]])
  if (is.null(lvls)) lvls <- sort(unique(as.character(fullData[[v]])))
  ticked <- input[[v]]
  if (is.null(ticked)) lvls else intersect(lvls, ticked)
}

## Frequency tables for the ticked factor variables: a named list of data frames
## (rows are the factor's ticked levels, via descTickedLevels()).
## Empty list when none are ticked.  (Tables built by descFreqTable() in
## HelperFunctions.R, shared with the downloaded reproducibility code.)
descFreqTables <- reactive({
  chosenF <- input$descFactorsChosen
  if (!length(chosenF)) return(list())
  d <- descSelectedData()
  req(nrow(d) > 0)
  out <- lapply(chosenF, function(v) descFreqTable(d, v, descTickedLevels(v)))
  names(out) <- chosenF
  out
})

## Build one crosstab from its chosen variables (2-4 of them).  Factors form the
## dimensions; if ONE numeric variable is included, cells show its mean as
## "mean (n)" instead of effect-size counts.  Returns NULL when fewer than two
## variables are chosen, a character validation message when the choice is
## invalid, or list(title, table = <wide data frame>, digits).
descCrosstabBuild <- function(vars) {
  if (is.null(vars) || length(vars) < 2) return(NULL)
  numVars <- intersect(vars, descNumericVars())
  facVars <- setdiff(vars, numVars)          # keeps the chosen order
  if (length(numVars) > 1)
    return(paste("Choose at most one numeric variable per crosstab (its cell",
                 "means are shown); the other variables must be factors."))
  d <- descSelectedData()
  if (nrow(d) == 0) return("The current selection contains no data.")
  # ticked levels are kept even when empty (zero rows/columns); unticked dropped
  lvlsList <- lapply(facVars, descTickedLevels); names(lvlsList) <- facVars
  if (length(numVars) == 1) {
    title <- if (length(facVars) == 1)
      sprintf("Mean %s (n effect sizes) by %s:", numVars, facVars)
    else
      sprintf("Mean %s (n effect sizes):  %s (rows)  ×  %s (columns), with marginal means:",
              numVars, paste(facVars[-length(facVars)], collapse = " × "),
              facVars[length(facVars)])
    list(title = title,
         table = crosstabMeansWideTable(d, facVars, numVars, lvlsList),
         digits = 2)
  } else {
    dims <- lapply(facVars, function(v) factor(as.character(d[[v]]), levels = lvlsList[[v]]))
    names(dims) <- facVars
    crosstab <- do.call(table, c(dims, list(useNA = "ifany")))
    title <- sprintf("Effect-size counts:  %s (rows)  ×  %s (columns), with totals:",
                     paste(facVars[-length(facVars)], collapse = " × "),
                     facVars[length(facVars)])
    list(title = title, table = crosstabWideTable(crosstab), digits = 0)
  }
}


## ---- Outputs for the Descriptives tab ----

## Headline: how much data is currently selected
output$descSummaryLine <- renderUI({
  d <- descSelectedData()
  k <- length(unique(as.character(d$Paper)))
  p(tags$b(sprintf("Currently selected:  %d effect sizes from %d studies.", nrow(d), k)))
})

## Variable choosers (rebuilt when the data set changes; ticks then reset, which is
## correct because the available variables may have changed)
output$descriptivesChooser <- renderUI({
  fluidRow(
    column(6, checkboxGroupInput("descFactorsChosen",
                                 "Factors (frequency tables):",
                                 choices = descFactorVars())),
    column(6, checkboxGroupInput("descNumericsChosen",
                                 "Numeric variables (summary statistics):",
                                 choices = descNumericVars()))
  )
})

## Individual descriptives for the ticked variables
output$descriptivesTables <- renderUI({
  if (!length(input$descFactorsChosen) && !length(input$descNumericsChosen)) {
    return(p(em("Tick one or more variables above to see their descriptives.")))
  }
  validate(need(nrow(descSelectedData()) > 0, "The current selection contains no data."))
  items <- list()
  numTable <- descNumericSummary()
  if (!is.null(numTable)) {
    items <- c(items, list(h5(tags$b("Numeric variables (per effect size)")),
                           renderTable(numTable, digits = 2)))
  }
  freqTables <- descFreqTables()
  # one closure per table via lapply: renderTable() evaluates its data lazily, so a
  # `for` loop (one shared environment) would show every factor the LAST table only
  freqItems <- lapply(names(freqTables), function(v) {
    ft <- freqTables[[v]]
    list(h5(tags$b(paste0("Frequencies:  ", v))),
         renderTable(ft, digits = 1))
  })
  items <- c(items, unlist(freqItems, recursive = FALSE))
  tagList(items)
})

## ---- Crosstabs: as many as the user likes ("Add another crosstab") ----
##
## Each crosstab is identified by an id that lives in myCrosstabIds; its variable
## picker is input[[crosstabVars_<id>]] and its table is output[[crosstabOut_<id>]].
## Ids are never reused; removing a crosstab just drops its id (its stale input
## value is harmless because everything iterates over the current ids only).
myCrosstabIds  <- reactiveVal(1L)
nextCrosstabId <- reactiveVal(2L)

## per-id pieces that must be created exactly once: the table output and the
## Remove-button observer
makeCrosstabUnit <- function(id) {
  output[[paste0("crosstabOut_", id)]] <- renderUI({
    ct <- descCrosstabBuild(input[[paste0("crosstabVars_", id)]])
    if (is.null(ct)) {
      return(p(em("Choose at least two variables above to build this crosstab.")))
    }
    if (is.character(ct)) return(p(em(ct)))
    tagList(p(ct$title), renderTable(ct$table, digits = ct$digits))
  })
  observeEvent(input[[paste0("removeCrosstab_", id)]], {
    myCrosstabIds(setdiff(myCrosstabIds(), id))
  })
}
makeCrosstabUnit(1L)

observeEvent(input$addCrosstab, {
  id <- nextCrosstabId()
  makeCrosstabUnit(id)
  myCrosstabIds(c(myCrosstabIds(), id))
  nextCrosstabId(id + 1L)
})

## The whole crosstab section: one picker + table per current id, then the Add
## button.  Re-renders when a crosstab is added/removed or the data set changes;
## isolate() keeps each picker's current selection across those re-renders
## (selectize itself drops any selected variable that no longer exists).
output$crosstabChooser <- renderUI({
  ids <- myCrosstabIds()
  choices <- list("Factors (table dimensions)" = descFactorVars(),
                  "Numeric (cell means)"       = descNumericVars())
  units <- lapply(seq_along(ids), function(i) {
    id <- ids[i]
    inputId <- paste0("crosstabVars_", id)
    # a freshly restored crosstab (from a selection-settings file) takes its
    # variables from crosstabInitialSel until its own input has reported a value
    sel0 <- isolate(input[[inputId]])
    if (is.null(sel0)) sel0 <- isolate(crosstabInitialSel())[[as.character(id)]]
    tagList(
      fluidRow(
        column(9, selectizeInput(inputId,
                                 sprintf("Crosstab %d:", i),
                                 choices = choices,
                                 selected = sel0,
                                 multiple = TRUE,
                                 options = list(maxItems = 4,
                                                placeholder = "Choose 2 to 4 variables..."))),
        column(3, if (length(ids) > 1)
          actionButton(paste0("removeCrosstab_", id), "Remove",
                       style = "margin-top: 25px;"))
      ),
      uiOutput(paste0("crosstabOut_", id)),
      br()
    )
  })
  tagList(
    p(tags$b("Crosstabs."),
      'Cross-tabulate 2 to 4 variables; the last factor chosen supplies the',
      'columns.  With factors only, cells are effect-size counts (with Sum',
      'row/column).  Include one numeric variable to show its cell means',
      'instead, as "mean (n)", with marginal means in an "All" row/column.'),
    units,
    actionButton("addCrosstab", "Add another crosstab")
  )
})


## ---- Restoring saved Descriptives choices (from a selection-settings file) ----
##
## pendingDescriptives holds the "descriptives" block of a loaded selection-
## settings file (set by selections_server.R) until this tab's choosers are live
## in the browser -- they only render when the Descriptives tab is first opened,
## so the wait can span tab switches.  input$addCrosstab existing (the action
## button reports its initial 0 once bound) implies the choosers are rendered
## and bound, so the update messages below will not be lost.
pendingDescriptives <- reactiveVal(NULL)

## initial variable choices for restored crosstab pickers, keyed by crosstab id
## (as character); consulted by the chooser renderUI while a picker's own input
## has not reported a value yet.  Replaced wholesale on each restore.
crosstabInitialSel <- reactiveVal(list())

## the saved crosstabs block comes back from JSON in several shapes: a list of
## character vectors (mixed widths), a matrix (all crosstabs the same width),
## a bare vector (a single crosstab), or NULL/empty
normalizeCrosstabList <- function(cts) {
  if (is.null(cts) || !length(cts)) return(list())
  if (is.matrix(cts))
    return(lapply(seq_len(nrow(cts)), function(i) as.character(cts[i, ])))
  if (!is.list(cts)) return(list(as.character(cts)))
  lapply(cts, function(v) as.character(unlist(v)))
}

observe({
  pd <- pendingDescriptives()
  req(pd, !is.null(input$addCrosstab))     # wait until the choosers are live
  pendingDescriptives(NULL)
  # ticked variables (saved names not offered for the current data are dropped)
  updateCheckboxGroupInput(session, "descFactorsChosen",
    selected = intersect(descFactorVars(), as.character(unlist(pd$factors))))
  updateCheckboxGroupInput(session, "descNumericsChosen",
    selected = intersect(descNumericVars(), as.character(unlist(pd$numerics))))
  # replace the whole crosstab set with FRESH ids: a fresh picker has no
  # lingering input value, so the chooser renderUI takes its selection from
  # crosstabInitialSel and nothing from the pre-restore state leaks through
  ctList <- normalizeCrosstabList(pd$crosstabs)
  initSel <- list()
  newIds  <- integer(0)
  for (vars in ctList) {
    id <- nextCrosstabId(); nextCrosstabId(id + 1L)
    makeCrosstabUnit(id)
    initSel[[as.character(id)]] <- vars
    newIds <- c(newIds, id)
  }
  if (!length(newIds)) {                   # nothing saved: back to one empty picker
    id <- nextCrosstabId(); nextCrosstabId(id + 1L)
    makeCrosstabUnit(id)
    newIds <- id
  }
  crosstabInitialSel(initSel)
  myCrosstabIds(newIds)
})


## ---- Download the displayed tables as a multi-sheet .xlsx workbook ----

## Excel sheet names: max 31 chars, no []:*?/\ characters
## (in the bracket expression, the literal [ must come last so no "[:" sequence
## forms, which POSIX regex would read as a character-class opener)
descSheetName <- function(name, prefix = "") {
  name <- gsub("[]:*?/\\\\[]", "_", paste0(prefix, name))
  substr(name, 1, 31)
}

output$descriptivesDown <- downloadHandler(
  filename = function() {
    paste0("descriptives_", Sys.Date(), ".xlsx")
  },
  content = function(file) {
    d <- descSelectedData()
    sheetList <- list(
      "Selection summary" = data.frame(
        Item  = c("Data file", "Effect sizes selected", "Studies selected"),
        Value = c(if (is.null(myrvs$currentInputFile)) "default (Vasilev et al. 2018 plus 2023 updates)"
                  else myrvs$currentInputFile,
                  nrow(d), length(unique(as.character(d$Paper))))
      )
    )
    numTable <- descNumericSummary()
    if (!is.null(numTable)) sheetList[["Numeric summaries"]] <- numTable
    freqTables <- descFreqTables()
    for (v in names(freqTables)) {
      sheetList[[descSheetName(v, prefix = "freq ")]] <- freqTables[[v]]
    }
    # one sheet per complete crosstab, same wide layout as displayed on screen
    ctNum <- 0
    for (id in myCrosstabIds()) {
      vars <- input[[paste0("crosstabVars_", id)]]
      ct <- descCrosstabBuild(vars)
      if (is.null(ct) || is.character(ct)) next   # incomplete or invalid: skip
      ctNum <- ctNum + 1
      sheetList[[descSheetName(paste(c("Crosstab", ctNum, vars), collapse = " "))]] <- ct$table
    }
    writexl::write_xlsx(sheetList, file)
  }
)
