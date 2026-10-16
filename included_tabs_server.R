#######################################################################################
################### A General Tool for Living Meta-Analysis #################
#######################################################################################
# v.1.5 2026.10.16
#
################### "Included in the analysis" output tabs (server) ##################
#
# Each analysis-output tab (the list is includableTabs, in global.R) carries an
# "Include this tab in the analysis" checkbox.  The ticked set is PURELY DECLARATIVE:
# it records which results are actually part of the analysis being reported.  Nothing
# is gated by it -- every tab still computes, and the downloaded R script still runs
# every section.  Ticking a tab does three things:
#
#   1. highlights it in the main tab strip (bold + a leading bullet), so the tabs you
#      are using stand out from the ones you are not;
#   2. labels the matching section of the generated R code -- a banner comment plus a
#      cat() marker that prints when the script runs (see codeGenerator.R);
#   3. is saved in, and restored from, the selections .json file, so a published copy
#      of this app reopens with the same tabs marked (see selections_server.R).
#
# The checkboxes are STATIC inputs inside tabsetPanel (see includeTabCheckbox() in
# ui.R), so they exist from page load whether or not their tab is ever opened.  That
# is deliberate: the Descriptives choosers are renderUI outputs and therefore stay
# unrendered (and their inputs NULL) until that tab is first shown, which would make
# the generated code depend on which tabs the user happened to visit.
#
# This file is sourced (local = T) from server.R.
######################################################################################


## the tab values currently ticked, in includableTabs order (never NULL: character(0)
## when nothing is ticked)
includedTabValues <- reactive({
  ticked <- vapply(includableTabValues,
                   function(v) isTRUE(input[[paste0("includeTab_", v)]]),
                   logical(1))
  includableTabValues[ticked]
})

## Bold + leading bullet for the ticked tabs.  Pure CSS: bootstrap renders each tab as
## <li><a data-value="..."> inside #mainTabset, so the ticked values can be targeted
## directly -- no javascript, and no hideTab()/showTab() (tabs are marked, never
## hidden).  The uiOutput sits outside the tabset in ui.R, so it is always visible and
## never suspended.
output$includedTabStyle <- renderUI({
  vals <- includedTabValues()
  if (!length(vals)) return(NULL)
  one <- function(v, suffix = "") paste0('#mainTabset > li > a[data-value="', v, '"]', suffix)
  ## NOTE: the ::before must be appended to EACH selector, not to the comma-joined
  ## list -- "a, b::before" attaches the pseudo-element to b only, so every tab but
  ## the last would come out bold with no bullet.
  selBold <- paste0(one(vals), collapse = ", ")
  selDot  <- paste0(one(vals, "::before"), collapse = ", ")
  tags$style(HTML(paste0(
    selBold, " { font-weight: 700; }\n",
    ## \25CF = filled circle, \00a0 = non-breaking space (keeps the bullet off the title)
    selDot, ' { content: "\\25CF\\00a0"; color: #3498DB; }'
  )))
})
