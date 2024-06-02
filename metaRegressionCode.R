# code for meta-regression tab; include in server

### plan: 1. make a new tab in the left panel to choose mediators, and
#         have a "yes/no" selector for mediation like the one for 
#         Bayesian robustness analysis
#         2. make a new results tab for meta-regression, display only if "yes"
#         3. to make the output for the meta-regression tab, calculate a new
#         frequentist model (fmaReg, a reactive like fma) that has 
#         moderators in the equation.  Should depend on MA.
#         4. put the server code for the meta-regression tab in a separate file
#         5. "source" the code in the server function in server.R

# and see this for forest plot code with subgroups:  
# https://www.metafor-project.org/doku.php/plots:forest_plot_with_subgroups


#### Code to insert into the server function for meta-regression

# create UI stuff for "moderatorSelection_ui"
#   yes/no button for doing meta-regression
#   list all the factors and numerics to choose as moderators, non selected

# calculate fmaReg using the selected moderators

# create UI stuff for "metaRegressionUI"
#   model test statistics and plots???

# switch between full forest plot (height = nrows + x) and 
# caterpillar plot (height = 60; scaled proportionlly)


# fma()  is the reactive containing the frequentist meta-analysis model without moderators
# MA()  is the reactive containing the currently subsetted data
#
# load a nonreactive copy of each of them, subsetting only to remove the duplicate
# effect sizes in the Sedlmeier paper
#


################ Functions ###############################################
source("metaRegressionFunctions.R")
################ Functions ###############################################


forestByGroup(MA=MA, MA$Instruction_category, xlab="Hedges g")
 ## additional arguments that might be useful: xlim, psize, xlab
 ## For caterpillar plot, caterpillar=TRUE (not slab=NA)
 ## You can also add any other arguments that forest() allows

forestByGroup(MA=MA, moderator=MA$Instruction_category, xlab="Hedges g", caterpillar = T, xlim=c(-25,15))


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
