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
## NULL when none are ticked.
descNumericSummary <- reactive({
  chosenN <- input$descNumericsChosen
  if (!length(chosenN)) return(NULL)
  d <- descSelectedData()
  req(nrow(d) > 0)
  do.call(rbind, lapply(chosenN, function(v) {
    x <- suppressWarnings(as.numeric(d[[v]]))
    n <- sum(!is.na(x))
    if (n == 0) {
      data.frame(Variable = v, n = 0L, Mean = NA_real_, SD = NA_real_,
                 Median = NA_real_, Min = NA_real_, Max = NA_real_,
                 Missing = sum(is.na(x)), check.names = FALSE)
    } else {
      data.frame(Variable = v, n = n,
                 Mean = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE),
                 Median = median(x, na.rm = TRUE),
                 Min = min(x, na.rm = TRUE), Max = max(x, na.rm = TRUE),
                 Missing = sum(is.na(x)), check.names = FALSE)
    }
  }))
})

## Frequency tables for the ticked factor variables: a named list of data frames
## (levels come from the full current data set, so filtered-out levels show 0).
## Empty list when none are ticked.
descFreqTables <- reactive({
  chosenF <- input$descFactorsChosen
  if (!length(chosenF)) return(list())
  d <- descSelectedData()
  req(nrow(d) > 0)
  st <- as.character(d$Paper)
  fullData <- myrvs$df.reactive
  out <- lapply(chosenF, function(v) {
    lvls <- levels(fullData[[v]])
    if (is.null(lvls)) lvls <- sort(unique(as.character(fullData[[v]])))
    x <- as.character(d[[v]])
    nES   <- vapply(lvls, function(l) sum(!is.na(x) & x == l), integer(1))
    nStud <- vapply(lvls, function(l) length(unique(st[!is.na(x) & x == l])), integer(1))
    freqTable <- data.frame(Level = lvls, `n ES` = nES, `k studies` = nStud,
                            `% of ES` = 100 * nES / nrow(d), check.names = FALSE)
    if (any(is.na(x))) {
      freqTable <- rbind(freqTable,
                         data.frame(Level = "(missing)", `n ES` = sum(is.na(x)),
                                    `k studies` = length(unique(st[is.na(x)])),
                                    `% of ES` = 100 * sum(is.na(x)) / nrow(d),
                                    check.names = FALSE))
    }
    freqTable
  })
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
  dims <- lapply(vars, function(v) factor(as.character(d[[v]])))
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

## Flatten a 3- or 4-way crosstab into a wide display table: one column per level
## of the LAST factor, one row per combination of the other factors (first factor
## outermost, repeated outer labels blanked, ftable-style), plus Sum row/column.
crosstabWideTable <- function(crosstab) {
  vars    <- names(dimnames(crosstab))
  rowVars <- vars[-length(vars)]
  colVar  <- vars[length(vars)]
  long <- as.data.frame(crosstab)
  # as.data.frame() applies make.names() to the dim names, silently renaming
  # non-syntactic variables (e.g. "Meta-analysis.Source" -> "Meta.analysis.Source");
  # restore the true names (columns are in dim order, then the count column)
  names(long) <- c(vars, "Freq")
  # show NA levels as "(missing)"; appearance order preserves each dim's level order
  for (v in vars) {
    x <- as.character(long[[v]]); x[is.na(x)] <- "(missing)"
    long[[v]] <- factor(x, levels = unique(x))
  }
  long <- long[do.call(order, long[rowVars]), ]   # first factor outermost
  key  <- function(d) do.call(paste, c(d, list(sep = "\r")))
  wide <- unique(long[rowVars])
  colLevels <- levels(long[[colVar]])
  for (cl in colLevels) {
    sub <- long[long[[colVar]] == cl, c(rowVars, "Freq")]
    wide[[cl]] <- sub$Freq[match(key(wide[rowVars]), key(sub[rowVars]))]
  }
  wide$Sum <- rowSums(wide[colLevels])
  wide[rowVars] <- lapply(wide[rowVars], as.character)
  # blank repeated outer labels (a label repeats only if everything left of it does
  # too); compute all comparison keys BEFORE any blanking, or the first column's
  # blanks corrupt the later columns' keys and their label leaks through once
  if (length(rowVars) > 1) {
    keys <- lapply(seq_len(length(rowVars) - 1),
                   function(i) key(wide[rowVars[seq_len(i)]]))
    for (i in seq_len(length(rowVars) - 1)) {
      k <- keys[[i]]
      wide[[rowVars[i]]][c(FALSE, k[-1] == k[-length(k)])] <- ""
    }
  }
  totalRow <- wide[1, , drop = FALSE]
  totalRow[rowVars] <- ""; totalRow[[rowVars[1]]] <- "Sum"
  for (cl in c(colLevels, "Sum")) totalRow[[cl]] <- sum(wide[[cl]])
  rbind(wide, totalRow)
}

## The crosstab itself: rows × columns with totals; for 3- and 4-way tables the
## leading factors form grouped rows and the last factor supplies the columns
output$crosstabOut <- renderUI({
  ct <- descCrosstab()
  if (is.null(ct)) {
    return(p(em("Choose at least two factors above to build a crosstab.")))
  }
  if (length(ct$vars) == 2) {
    tagList(
      p(paste0(ct$vars[1], " (rows)  ×  ", ct$vars[2], " (columns), with totals:")),
      renderTable(addmargins(ct$table), rownames = TRUE, digits = 0)
    )
  } else {
    tagList(
      p(paste0("Crosstab of ", paste(ct$vars, collapse = " × "),
               " (last factor as columns, with totals):")),
      renderTable(crosstabWideTable(ct$table), digits = 0)
    )
  }
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
      if (length(ct$vars) == 2) {
        am <- addmargins(ct$table)
        m  <- as.data.frame.matrix(am)
        names(m) <- colnames(am)   # undo make.names() mangling of level labels
        m <- cbind(stats::setNames(data.frame(rownames(m)), ct$vars[1]), m)
        sheetList[["Crosstab"]] <- m
      } else {
        # 3- and 4-way: same wide layout as displayed on screen
        sheetList[["Crosstab"]] <- crosstabWideTable(ct$table)
      }
    }
    writexl::write_xlsx(sheetList, file)
  }
)
