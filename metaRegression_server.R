# server code for meta-regression
# generates parts of the UI and generates the results tab for meta-regression

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

print("working ok")  # debugging




######### tab panel for forest plot results
#
##  goes with this section of the UI.R file:
# tabPanel("Meta-Regression",
#          printButton,
#          h4("Frequentist Meta-Regression"),
#          uiOutput("metaRegressionOutputUI")
# ),

output$metaRegressionOutputUI <- renderUI({
  MA <- MA()
  moderatorName <- input$moderator_variable
  conditionalPanel(
    condition = "input.includeModerator == 'Yes'",
    tagList(
      renderText(paste("Moderator variable is:  ", moderatorName)),
  #    renderPlot(forestByGroup(MA=MA, moderator=MA$moderatorName))
  p("debugging   d")
    )
  )

  
})
print("still working ok")  # debugging

# 
# tabPanel("Meta-Regression", value = "bayesian_robustness",
#          printButton,
#          h4("Forest Plot with Subgroups"),
#          p("Will only be computed if 'Yes' is selected for 'Include Moderator'",),br(),
#          p("and one categorical moderator is selected."),
#          
#          conditionalPanel(condition = "input.includeModerator == 'Yes'", 
#                           plotOutput("robustplot") %>% withSpinner(type = 6, color = "#3498DB"),
#  
# ),


