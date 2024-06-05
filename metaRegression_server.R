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
    radioButtons("includeModerator", "Do you want to include a moderator for meta-regression?",
                 choices = list("No" = "No", "Yes" = "Yes"), selected = "No"),
    conditionalPanel(
      condition = "input.includeModerator == 'Yes'",
      radioButtons("moderator_variable", "Select one moderator variable",
                   choices = Variable.Factor.Names)
    ))
})


######### tab panel for forest plot results
#
##  goes with this section of the UI.R file:
# tabPanel("Meta-Regression",
#          printButton,
#          h4("Frequentist Meta-Regression"),
#          uiOutput("metaRegressionOutputUI")
# ),


## set plot height:
# Reactive values to store the data
reactiveValuesListMetaRegression <- reactiveValues(
  plot_height = 1000  # initial default value, you can adjust this
)

# Update plot height based on the number of rows in MA when MA is available
observe({
  MA_data <- MA()
  reactiveValuesListMetaRegression$plot_height <- nrow(MA_data) * 12 + 400
})

# New values when "update" button is pressed
observeEvent(input$update_x_axis, {
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
  
  tagList(
    printButton,
    h4("Frequentist Meta-Regression: Forest Plot with Subgroups"),
    p("Will only be computed if 'Yes' is selected for 'Include Moderator'",
      "and one categorical moderator is selected.") ,
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
            efac = .3
          )
          
          if (input$update_x_axis > 0) {
            x_min <- as.numeric(input$x_min)
            x_max <- as.numeric(input$x_max)
            
            if (!is.na(x_min) && !is.na(x_max)) {
              plot_args$xlim <- c(x_min, x_max)
            }
          }
          
          do.call(forestByGroup, plot_args)
        }, 
        height = freq_forest_height_mod()),    
        
        # input boxes for adjusting x axis:
        hr(),
        fluidRow(
          column(2, 
                 actionButton("update_x_axis", "Update plot"),
                 tags$style(type='text/css', "
    #update_x_axis {
      height: 5px;
      line-height: 3px;
      background-color: tan; /* Change this to your desired color */
      color: black; /* Change text color if needed */
    }
  ")
          ),
          column(2, textInput("x_min", label = NULL, placeholder = "x Min", width = "100px")),
          tags$style(type='text/css', "#x_min { height: 3px; }"),
          column(2, textInput("x_max", label = NULL, placeholder = "x Max", width = "100px")),
          tags$style(type='text/css', "#x_max { height: 3px; }")
        ),
        textInput("freq_forest_height_input", "Plot height:", value = freq_forest_height_mod()),
        tags$style(type='text/css', "#freq_forest_height_input { height: 3px; }")
      )
    )
  )   # end of tagList
})


observeEvent(reactiveValuesListMetaRegression$plot_height, {
  print("The value just changed!!!  ")
})