## generating static R code for the Descriptives panel
## (sourced local = T from codeGenerator.R, like metaRegression_code_generator.R)
##

descriptivesCode <- reactiveVal("##### Nothing selected in the Descriptives tab; no descriptive statistics code generated ########")

observe({   # update the descriptives code whenever the Descriptives tab choices change
  chosenF  <- input$descFactorsChosen
  chosenN  <- input$descNumericsChosen
  # the variable choices of every current crosstab (complete ones only)
  ctList   <- lapply(myCrosstabIds(), function(id) input[[paste0("crosstabVars_", id)]])
  ctList   <- Filter(function(v) length(v) >= 2, ctList)
  if (!length(chosenF) && !length(chosenN) && !length(ctList)) {
    isolate(descriptivesCode(
      "##### Nothing selected in the Descriptives tab; no descriptive statistics code generated ########"))
    return()
  }

  asVector <- function(x) {
    if (!length(x)) "character(0)"
    else paste0("c(", paste(escapeAndDQuote(enc2utf8(x)), collapse = ",\n                       "), ")")
  }
  asListOfVectors <- function(xs) {
    if (!length(xs)) "list()"
    else paste0("list(\n  ", paste(vapply(xs, asVector, character(1)),
                                   collapse = ",\n  "), ")")
  }

  ## the recorded choices (dynamic part)
  choicesText <- sprintf('
########## Descriptive statistics for the currently selected data ##########
#   Reproduces the "Descriptives" tab: summaries of the selected effect-size
#   rows (all selection criteria applied, BEFORE effect-size aggregation).
#   descFreqTable(), descNumericSummaryTable(), crosstabWideTable(), and
#   crosstabMeansWideTable() are defined in the downloaded HelperFunctions.R
#   file.  Factor levels ticked in the selection criteria always appear (zero
#   counts included); unticked levels are omitted.

## variables chosen in the Descriptives tab (one vector per crosstab):
descFactorsChosen  <- %s
descNumericsChosen <- %s
crosstabList       <- %s
', asVector(chosenF), asVector(chosenN), asListOfVectors(ctList))

  ## the computation (static part; reuses the selection variables recorded above
  ## for the MA object: Variable.Factors.selected, Design, Publication.Year, etc.)
  staticText <- '
## the selected rows: same filters as the MA object, without aggregation
createSelectedData.nonReactive <- function(df, Variable.Factor.Names, Variable.Numeric.Names,
                                           Variable.Factors.selected, Variable.Numerics.selected,
                                           Design.include, Publication.Year.include,
                                           N_Intervention.include, included) {
  df_sub <- df %>% filter(Design %in% Design.include,
                          Publication.Year >= Publication.Year.include[1],
                          Publication.Year <= Publication.Year.include[2],
                          N_Intervention >= N_Intervention.include[1],
                          N_Intervention <= N_Intervention.include[2],
                          Paper.and.Exp %in% included)
  for (varName in Variable.Factor.Names) {
    df_sub <- df_sub[df_sub[, varName] %in% Variable.Factors.selected[[varName]], ]
  }
  for (varName in Variable.Numeric.Names) {
    df_sub <- df_sub[df_sub[, varName] >= Variable.Numerics.selected[[varName]][1], ]
    df_sub <- df_sub[df_sub[, varName] <= Variable.Numerics.selected[[varName]][2], ]
  }
  df_sub
}
selectedData <- createSelectedData.nonReactive(df, Variable.Factor.Names, Variable.Numeric.Names,
                                               Variable.Factors.selected, Variable.Numerics.selected,
                                               Design, Publication.Year, N_Intervention, included)

## ticked levels of a selection factor, in the data level order
tickedLevelsFor <- function(v) {
  lv <- levels(df[[v]]); if (is.null(lv)) lv <- sort(unique(as.character(df[[v]])))
  ticked <- if (v == "Design") Design else Variable.Factors.selected[[v]]
  if (is.null(ticked)) lv else intersect(lv, ticked)
}

cat(sprintf("Currently selected: %d effect sizes from %d studies\\n",
            nrow(selectedData), length(unique(as.character(selectedData$Paper)))))

if (length(descNumericsChosen)) {
  cat("\\nNumeric variables (per effect size):\\n")
  print(descNumericSummaryTable(selectedData, descNumericsChosen), row.names = FALSE)
}
for (v in descFactorsChosen) {
  cat("\\nFrequencies: ", v, "\\n")
  print(descFreqTable(selectedData, v, lvls = tickedLevelsFor(v)), row.names = FALSE)
}
## crosstabs: factors form the dimensions (cells are effect-size counts); if a
## numeric variable is included, its cell means are shown as "mean (n)" instead
numericCandidates <- c("Publication.Year", "N_Intervention", Variable.Numeric.Names)
for (ctVars in crosstabList) {
  numVar  <- intersect(ctVars, numericCandidates)
  facVars <- setdiff(ctVars, numVar)
  if (length(numVar) > 1 || length(facVars) < 1) next   # invalid in the app too; skip
  lvlsList <- lapply(facVars, tickedLevelsFor); names(lvlsList) <- facVars
  if (length(numVar) == 1) {
    cat("\\nCell means of ", numVar, " by ", paste(facVars, collapse = " x "),
        ", as mean (n ES):\\n", sep = "")
    print(crosstabMeansWideTable(selectedData, facVars, numVar, lvlsList), row.names = FALSE)
  } else {
    cat("\\nCrosstab of ", paste(facVars, collapse = " x "),
        " (n ES, last factor as columns):\\n", sep = "")
    dims <- lapply(facVars, function(v) factor(as.character(selectedData[[v]]),
                                               levels = lvlsList[[v]]))
    names(dims) <- facVars
    print(crosstabWideTable(do.call(table, c(dims, list(useNA = "ifany")))), row.names = FALSE)
  }
}
'

  isolate({   # update the reactive value so it can be used outside this "observe" block
    descriptivesCode(paste0(choicesText, staticText))
  })
})
