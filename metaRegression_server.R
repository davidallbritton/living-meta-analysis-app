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
      'The selected moderator is used by both the "Meta-Regression" (frequentist) and',
      '"Bayesian Meta-Regression" results tabs.',
      "The choices are the factor variables of the current data file.",
      'After changing the moderator, press "(Re)Calculate Meta-Analysis" to update the results.'),
    radioButtons("includeModerator", "Do you want to include a moderator for meta-regression?",
                 choices = list("No" = "No", "Yes" = "Yes"), selected = "No"),
    conditionalPanel(
      condition = "input.includeModerator == 'Yes'",
      radioButtons("moderator_variable", "Select one moderator variable",
                   choices = c("Design", Variable.Factor.Names)),
      uiOutput("moderatorAggWarning")
    ))
})

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
        renderPlot({
          req(moderatorName)
          forestByGroup(MA = MAmlData, moderator = MAmlData[[moderatorName]],
                        col = "red", border = "red",
                        efac = if (is.null(input$freq_efac)) 0.3 else input$freq_efac,
                        multilevel = TRUE)
        },
        height = freq_forest_height_mlreg())
      )
    )
  )
})
