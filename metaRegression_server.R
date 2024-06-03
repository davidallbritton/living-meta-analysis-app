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

##  set the plot height....
# freq_forest_height_mod <- reactive(nrow(MA()) * 12 + 200)
freq_forest_height_mod <- reactive(nrow(MA()) * 12 + 400)

output$metaRegressionOutputUI <- renderUI({
  MA <- MA()
  moderatorName <- input$moderator_variable
  moderator <- MA[[moderatorName]]
  
  tagList(
    printButton,
    h4("Forest Plot with Subgroups"),
    p("Will only be computed if 'Yes' is selected for 'Include Moderator'",),br(),
    p("and one categorical moderator is selected.") ,
    conditionalPanel(
      condition = "input.includeModerator == 'Yes'",
      tagList(
        renderText(paste("Moderator variable is:  ", moderatorName)),
        renderPlot(forestByGroup(MA=MA, moderator=moderator, 
                                 # col = "red",
                                 # border = "red",
                                 efac = .3
                                 ), 
                   height=freq_forest_height_mod),
        # efac = 0     to remove the vertical tics on the ends of the error bars
        p("height = ", freq_forest_height_mod()),  # debugging
        p("debugging")
      )
    )
  )   # end of tagList
})  

print("still working ok")  # debugging





