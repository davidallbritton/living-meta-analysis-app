######### tab panel for selecting a moderator and saying "yes" do meta-analysis 
#
# This section is referenced by this part of the UI.R file:
# tabPanel("Moderator Selection",
#          uiOutput("moderatorSelection_ui")
# ),
#
output$moderatorSelection_ui <- renderUI({
  Variable.Factor.Names <- myrvs$Variable.Factor.Names
  tagList(
    p("Choose a categorical moderator here to divide the studies into subgroups.",
      "The selected moderator is used by all four meta-regression results tabs:",
      '"Meta-Regression" and "Bayesian Meta-Regression" (aggregated data), and',
      '"Frequentist Multilevel Regression" and "Bayesian Multilevel Regression"',
      "(unaggregated, effect sizes nested in papers).",
      "The choices are the factor variables of the current data file.",
      'After changing the moderator, press "(Re)Calculate Meta-Analysis" to update the results.'),
    radioButtons("includeModerator", "Do you want to include a moderator for meta-regression?",
                 choices = list("No" = "No", "Yes" = "Yes"), selected = "No"),
    conditionalPanel(
      condition = "input.includeModerator == 'Yes'",
      radioButtons("moderator_variable", "Select one moderator variable",
                   choices = c("Design", Variable.Factor.Names)),
      uiOutput("moderatorAggWarning"),
      uiOutput("contrastChooser_ui")
    ))
})

## Optional contrast between two levels of the moderator (level A minus level B),
## reported by the two MULTILEVEL regression tabs.  It changes no model (the
## Bayesian contrast comes from the fitted draws; the frequentist one is a linear
## combination of the cell means), so it applies live, without recalculation.
## A level wanted by a restored selections file (myrvs$contrastRestore) is
## selected once the moderator's levels are on offer.
myrvs$contrastRestore <- NULL
output$contrastChooser_ui <- renderUI({
  req(input$moderator_variable)
  df <- myrvs$df.reactive
  req(df, input$moderator_variable %in% names(df))
  lv <- levels(droplevels(factor(as.character(df[[input$moderator_variable]]))))
  wanted <- myrvs$contrastRestore
  pick <- function(id, k) {
    cur <- if (!is.null(wanted)) wanted[[k]] else isolate(input[[id]])
    if (length(cur) && cur %in% lv) cur else ""
  }
  choices <- c("(none)" = "", lv)
  tagList(
    p(tags$b("Contrast between two levels (optional)"), br(),
      tags$small("Reported in the two multilevel regression tabs: the difference between the",
                 "two levels' means (A \u2212 B), with its interval, and in the Bayesian tab the",
                 "posterior probability that it is above zero.  Applies without recalculation.")),
    selectInput("contrastLevelA", "Level A", choices = choices, selected = pick("contrastLevelA", 1)),
    selectInput("contrastLevelB", "Level B", choices = choices, selected = pick("contrastLevelB", 2))
  )
})
## the inputs must exist even if the Moderator Selection tab is never opened, so
## that saved selections always carry them (as for moderatorSelection_ui)
outputOptions(output, "contrastChooser_ui", suspendWhenHidden = FALSE)
## once the restored levels have been offered (or cannot be, because they are not
## levels of the current moderator), stop forcing them
observeEvent(list(input$contrastLevelA, input$contrastLevelB, input$moderator_variable), {
  w <- myrvs$contrastRestore
  if (is.null(w)) return()
  df <- myrvs$df.reactive
  v <- input$moderator_variable
  lv <- if (!is.null(v) && v %in% names(df)) unique(as.character(df[[v]])) else character(0)
  offered <- function(k, id) identical(input[[id]], if (w[[k]] %in% lv) w[[k]] else "")
  if (offered(1, "contrastLevelA") && offered(2, "contrastLevelB")) myrvs$contrastRestore <- NULL
}, ignoreInit = TRUE)

## the chosen pair, or NULL if incomplete
contrastLevels <- function() {
  a <- input$contrastLevelA; b <- input$contrastLevelB
  if (isTruthy(a) && isTruthy(b) && !identical(a, b)) c(a, b) else NULL
}

## Live warning under the moderator choices: with "Aggregate over: Papers", a
## moderator that varies within one of the currently selected papers would be
## blended within papers by the aggregation, so the meta-regression tabs will
## refuse it (moderatorBlockedByPapersAgg, checked at recalculation time).
## This warning uses the LIVE selection so the user sees it immediately, before
## pressing (Re)Calculate.
output$moderatorAggWarning <- renderUI({
  req(isTRUE(input$includeModerator == "Yes"), input$moderator_variable)
  req(isTRUE(input$aggregation == "Papers"))
  df <- myrvs$df.reactive
  req(df)
  d <- applyAllFilters(df, input, myrvs$Variable.Factor.Names, myrvs$Variable.Numeric.Names)
  req(nrow(d) > 0)
  v <- input$moderator_variable
  varies <- any(tapply(as.character(d[[v]]), as.character(d$Paper.Number),
                       function(x) length(unique(x)) > 1))
  req(varies)
  p(style = "color: #b30000;",
    tags$b("Warning:"), 'with "Aggregate over: Papers", at least one selected paper has',
    'effect sizes in different', tags$b(v), 'groups.  Aggregating would blend those groups',
    'together within each paper, so the AGGREGATED meta-regression tabs will not analyze',
    'this combination.  Switch "Aggregate over" to ID (in the "Study criteria" tab), or',
    'use the multilevel regression tabs, which never aggregate.')
})


######### tab panel for forest plot results
#
##  goes with this section of the UI.R file:
# tabPanel("Meta-Regression",
#          printButton,
#          h4("Frequentist Meta-Regression"),
#          uiOutput("metaRegressionOutputUI")
# ),


## set plot height and x axis limits:
reactiveValuesListMetaRegression <- reactiveValues(
  plot_height = 1000,  # initial default value, you can adjust this
  xlim_min = NULL,     # not used for now
  xlim_max = NULL      # not used for now
)

# Update plot height based on the number of rows in MA when MA is available
observe({
  MA_data <- MA()
  reactiveValuesListMetaRegression$plot_height <- nrow(MA_data) * 12 + 400
})

# New values when "update" button is pressed
observeEvent(input$update_x_axis, {
  # update height
  if (!is.null(input$freq_forest_height_input) && !is.na(input$freq_forest_height_input)) {
    reactiveValuesListMetaRegression$plot_height <- as.numeric(input$freq_forest_height_input)
  }
})

# A reactive that depends on those values
freq_forest_height_mod <- reactive({
  reactiveValuesListMetaRegression$plot_height
})   # height value for panel with a moderator


# draw the plot:
output$metaRegressionOutputUI <- renderUI({
  MA <- MA()
  moderatorName <- input$moderator_variable
  
  # check to make sure input$moderator_variable exists before assigning its value to something:
  if (is.null(input$moderator_variable)) {
    warning("First you must select a valid moderator in the Moderator Selection tab...")
    moderator <- NULL
  } else {
    moderator <- MA[[moderatorName]]
  }

  # Papers aggregation + a moderator that varies within papers: the subgroup
  # analysis would assign blended composites to arbitrary groups, so refuse it
  # with an explanation instead of a plot
  if (isTRUE(input$includeModerator == "Yes") && moderatorBlockedByPapersAgg(moderatorName)) {
    return(tagList(
      printButton,
      h4("Frequentist Meta-Regression: Forest Plot with Subgroups"),
      p(tags$b("Not available for this combination of settings.")),
      p('The current analysis aggregates effect sizes over Papers, but at least one selected',
        'paper contributes effect sizes in different', tags$b(moderatorName), 'groups.',
        "Aggregation combines each paper's effect sizes into a single value FIRST, so a",
        'subgroup analysis by', tags$b(moderatorName), 'would blend groups within papers and',
        "assign each paper's combined effect size to an arbitrary group."),
      p('To analyze this moderator, switch "Aggregate over" to ID in the "Study criteria"',
        'panel and press "(Re)Calculate Meta-Analysis", or use the "Frequentist Multilevel',
        'Regression" tab (which never aggregates).  (Aggregating over Papers remains fine',
        'for moderators that are constant within each paper.)')
    ))
  }

  tagList(
    printButton,
    h4("Frequentist Meta-Regression: Forest Plot with Subgroups"),
    p("Will only be computed if 'Yes' is selected for 'Include Moderator' and a",
      "categorical moderator is selected in the 'Moderator Selection' tab (left panel).") ,
    conditionalPanel(
      condition = "input.includeModerator == 'Yes'",
      tagList(
        renderText(paste("Moderator variable is:  ", moderatorName)),
        renderPlot({
          plot_args <- list(
            MA = MA,
            moderator = moderator,
            col = "red",       # color of the summary polygon
            border = "red",    # color of the summary polygon
            # symbol size (CI ends + diamonds) from the "Symbol size" slider
            efac = if (is.null(input$freq_efac)) 0.3 else input$freq_efac,
            multilevel = FALSE   # this tab is the standard two-level version
          )
          
          if (input$update_x_axis > 0) {
            x_min <- as.numeric(input$x_min)
            x_max <- as.numeric(input$x_max)
            
            if (!is.na(x_min) && !is.na(x_max)) {
              plot_args$xlim <- c(x_min, x_max)
            }
            
            if (input$caterpillar) {
              plot_args$caterpillar <- TRUE
            }
          }
          
          ### This creates the plot and passes the chosen arguments:
          do.call(forestByGroup, plot_args)
          ## additional arguments that might be useful: xlim, psize, xlab
          ## For caterpillar plot, caterpillar=TRUE (not slab=NA)
          ## You can also add any other arguments that forest() allows
        }, 
        height = freq_forest_height_mod()),    
        uiOutput("rmaContrastOut"),
        
        # input boxes for adjusting x axis:
        hr(),
        actionButton("update_x_axis", "Update plot", style = "margin-bottom: 3px;"),
        tags$style(type='text/css', "
          #update_x_axis {
          height: 5px;
          line-height: 3px;
          background-color: tan; /* Change this to your desired color */
          color: black; /* Change text color if needed */
          }
        "),
        p("Update x axis values:"),
        fluidRow(
          column(2, textInput("x_min", label = NULL, placeholder = "Min", width = "100px")),
          tags$style(type='text/css', "#x_min { height: 3px; }"),
          column(2, textInput("x_max", label = NULL, placeholder = "Max", width = "100px")),
          tags$style(type='text/css', "#x_max { height: 3px; }")
        ),
        checkboxInput("caterpillar", label="Caterpillar plot",value = F),
        textInput("freq_forest_height_input", "Change plot height: (also resets x axis)", value = freq_forest_height_mod()),
        tags$style(type='text/css', "#freq_forest_height_input { height: 3px; }"),
        sliderInput("freq_efac", "Symbol size (efac):",
                    min = 0.1, max = 2.0, value = 0.3, step = 0.05)
      )
    )
  )   # end of tagList
})



######### "Frequentist Multilevel Regression" tab: the same grouped forest plot,
######### fit as three-level CHE models on the UNaggregated data (MAml()).
######### Always available (the aggregation setting does not affect it); the
######### moderator choice applies live, like the standard tab above.
freq_forest_height_mlreg <- reactive(nrow(MAml()) * 12 + 400)
output$metaRegressionMlOutputUI <- renderUI({
  MAmlData <- MAml()
  moderatorName <- input$moderator_variable
  tagList(
    printButton,
    h4("Frequentist Multilevel Meta-Regression: Forest Plot with Subgroups"),
    p("Three-level CHE models (effect sizes nested in papers, cluster-robust",
      "inference) on the UNaggregated data.  Will only be computed if 'Yes' is",
      "selected for 'Include Moderator' and a categorical moderator is selected",
      "in the 'Moderator Selection' tab (left panel)."),
    conditionalPanel(
      condition = "input.includeModerator == 'Yes'",
      tagList(
        renderText(paste("Moderator variable is:  ", moderatorName)),
        covarianceNote(),
        renderPlot({
          req(moderatorName)
          forestByGroup(MA = MAmlData, moderator = MAmlData[[moderatorName]],
                        col = "red", border = "red",
                        efac = if (is.null(input$freq_efac)) 0.3 else input$freq_efac,
                        multilevel = TRUE,
                        # assumed within-paper sampling correlation, as of the
                        # last (Re)Calculate
                        rho = req(myrvs$bayesSnapshot)$rhoCHE,
                        sharing = myrvs$bayesSnapshot$sharing)
        },
        height = freq_forest_height_mlreg()),
        uiOutput("cheContrastOut")
      )
    )
  )
})


## contrast between two moderator levels in the frequentist multilevel model
output$cheContrastOut <- renderUI({
  lv <- contrastLevels()
  if (is.null(lv)) return(p(tags$small('To compare two levels, choose them under "Contrast between',
                                       'two levels" in the "Moderator Selection" tab.')))
  MAmlData <- MAml()
  moderatorName <- input$moderator_variable
  req(moderatorName %in% names(MAmlData))
  snap <- req(myrvs$bayesSnapshot)
  ct <- tryCatch(cheContrast(MAmlData, MAmlData[[moderatorName]], lv[1], lv[2],
                             rho = snap$rhoCHE, sharing = snap$sharing),
                 error = function(e) e)
  if (inherits(ct, "error")) return(p(style = "color:#c0392b;", conditionMessage(ct)))
  if (is.null(ct)) return(p("One of the chosen levels has no effect sizes in the current selection."))
  tagList(h4("Contrast between two moderator levels"),
          p(tags$small("Three-level CHE model with one mean per level (~ 0 + moderator);",
                       "cluster-robust (CR2) standard error, Satterthwaite degrees of freedom.")),
          renderTable(ct, digits = 3))
})


## contrast between two moderator levels in the standard (aggregated) model
output$rmaContrastOut <- renderUI({
  lv <- contrastLevels()
  if (is.null(lv)) return(p(tags$small('To compare two levels, choose them under "Contrast between',
                                       'two levels" in the "Moderator Selection" tab.')))
  MA <- MA()
  moderatorName <- input$moderator_variable
  req(moderatorName %in% names(MA))
  if (moderatorBlockedByPapersAgg(moderatorName)) return(NULL)
  ct <- tryCatch(rmaContrast(MA, MA[[moderatorName]], lv[1], lv[2]), error = function(e) e)
  if (inherits(ct, "error")) return(p(style = "color:#c0392b;", conditionMessage(ct)))
  if (is.null(ct)) return(p("One of the chosen levels has no effect sizes in the current selection."))
  tagList(h4("Contrast between two moderator levels"),
          p(tags$small("Random-effects model with one mean per level (~ 0 + moderator);",
                       "Knapp-Hartung test and interval, scaling factor truncated at 1",
                       '(the same adjustment as the forest plot above).')),
          renderTable(ct, digits = 3))
})
