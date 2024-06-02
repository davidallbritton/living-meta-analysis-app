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


# output$dynamic_ui <- renderUI({
#   tagList(
#     radioButtons("includeModerator", "Do you want to include a moderator for meta-regression?",
#                  choices = list("No" = "No", "Yes" = "Yes"), selected = "No"),
#     conditionalPanel(
#       condition = "input.includeModerator == 'Yes'",
#       radioButtons("variable_factor", "Select one option:",
#                    choices = Variable.Factor.Names)
#     )
#   )
# })




print("working ok")

# ## loop over the variable factor columns
# lapply(Variable.Factor.Names, function(varName) {                             
#   checkboxGroupInput(inputId = varName, label = p(varName,style="color:#333333"), 
#                      choices = levels(df[,varName]), selected = levels(df[,varName]))
# })
# 
# 
# for (varName in Variable.Factor.Names)  {
#   keepValues <- input[[varName]]
#   df_sub <- df_sub[df_sub[,varName] %in% keepValues, ]
# }

















######### tab panel for forest plot results

# tabPanel("Meta-Regression",
#          printButton,
#          h4("Frequentist Meta-Regression"),
#          plotOutput("metaRegressionOutput")
# ),

# # Render the forest plot
# output$metaRegressionOutput <- renderPlot({
#   MA <- MA()
#   moderatorName <- input$moderator_variable
#   forestByGroup(MA=MA, moderator=MA$moderator_column)
# })

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
