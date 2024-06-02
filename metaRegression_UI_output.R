

######### tab panel for selecting a moderator and saying "yes" do moderator 

# tabPanel("Moderator Selection",
#          uiOutput("moderatorSelection_ui")
# ),


######### tab panel for forest plot results

# tabPanel("Meta-Regression",
#          printButton,
#          h4("Frequentist Meta-Regression"),
#          plotOutput("metaRegressionOutput")
# ),

# Render the forest plot
output$metaRegressionOutput <- renderPlot({
  MA <- MA()
  moderatorName <- input$moderator_variable
  forestByGroup(MA=MA, moderator=MA$moderator_column)
})


tabPanel("Meta-Regression", value = "bayesian_robustness",
         printButton,
         h4("Forest Plot with Subgroups"),
         p("Will only be computed if 'Yes' is selected for 'Include Moderator'",),br(),
         p("and one categorical moderator is selected."),
         
         conditionalPanel(condition = "input.includeModerator == 'Yes'", 
                          plotOutput("robustplot") %>% withSpinner(type = 6, color = "#3498DB"),
 
),
