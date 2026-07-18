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
# with ES and study counts; numerics -> summary statistics), and can cross-tabulate
# 2 to 4 of the factor variables.  Variable choices use the same labels (plain column
# names) as the sidebar selection panel.
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

## Crosstab of the 2-4 chosen factors (cell counts are effect sizes).
## NULL until at least two factors are chosen.  Returns a list:
##   vars  : the chosen variable names
##   table : the table object (2-way: with margins added for display)
descCrosstab <- reactive({
  vars <- input$crosstabVars
  if (is.null(vars) || length(vars) < 2) return(NULL)
  d <- descSelectedData()
  req(nrow(d) > 0)
  # ticked levels are kept even when empty (zero rows/columns); unticked dropped
  dims <- lapply(vars, function(v) factor(as.character(d[[v]]), levels = descTickedLevels(v)))
  names(dims) <- vars
  crosstab <- do.call(table, c(dims, list(useNA = "ifany")))
  list(vars = vars, table = crosstab)
})


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

## Crosstab chooser (factors only; counts of effect sizes need categories, so
## numeric selection variables are not offered here)
output$crosstabChooser <- renderUI({
  selectizeInput("crosstabVars",
                 "Cross-tabulate 2 to 4 factors (cell counts are effect sizes):",
                 choices = descFactorVars(), multiple = TRUE,
                 options = list(maxItems = 4,
                                placeholder = "Choose 2 to 4 factors..."))
})

## The crosstab itself: the leading factor(s) form (grouped) rows, the last factor
## supplies the columns, with Sum row and column.  All widths (2-4 factors) go
## through crosstabWideTable(); renderTable() cannot be handed a `table` object
## directly because it as.data.frame()s it into long format.
output$crosstabOut <- renderUI({
  ct <- descCrosstab()
  if (is.null(ct)) {
    return(p(em("Choose at least two factors above to build a crosstab.")))
  }
  nRowVars <- length(ct$vars) - 1
  tagList(
    p(paste0(paste(ct$vars[seq_len(nRowVars)], collapse = " × "), " (rows)  ×  ",
             ct$vars[length(ct$vars)], " (columns), with totals:")),
    renderTable(crosstabWideTable(ct$table), digits = 0)
  )
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
    ct <- descCrosstab()
    if (!is.null(ct)) {
      # same wide layout as displayed on screen, whatever the number of factors
      sheetList[["Crosstab"]] <- crosstabWideTable(ct$table)
    }
    writexl::write_xlsx(sheetList, file)
  }
)
