## generating static R code for the meta-regression panel
##

# = copied from server
## = possible static R code to paste, but properly quoted and dereferenced

#  freq_forest_height_mod()  ## not used in static R code

#  reactiveValuesListMetaRegression$plot_args_metareg

#  input$includeModerator (Yes/No)
#  input$moderator_variable
#  moderatorName <- input$moderator_variable
#  moderator <- MA[[moderatorName]]
#  renderText(paste("Moderator variable is:  ", moderatorName))

# do.call(forestByGroup, plot_args)
## additional arguments that might be useful: xlim, psize, xlab
## For caterpillar plot, caterpillar=TRUE (not slab=NA)
## You can also add any other arguments that forest() allows
# }, 
# height = freq_forest_height_mod()),  ## not used in non-reactive R code

# ***** give up on retrieving the plotting parameters; just list the possible ones in comments....
# ****************************

metaRegCode <- reactiveVal("\n# No moderator chosen; no meta-regression done\n")
  
observe({   #update the meta-regression plot code when a moderator is selected
                      print("observe started...")   # debugging
  req(input$includeModerator)
                     print("first req passed...")   # debugging
  req(input$includeModerator == "Yes")
                     print("2nd req passed...")   # debugging
  moderatorChosen <- input$moderator_variable
  moderatorArgText <- paste0('moderator=MA[["', moderatorChosen, '"]]')
                         print("moderator is ...")   # debugging
                        print(moderatorArgText)   # debugging
  plotArgs <- paste0('MA=MA, ', moderatorArgText, ', col="red", border="red", efac=0.3, caterpillar=FALSE')
                         print("plotArgs is ...")   # debugging
                         print(plotArgs)   # debugging
                         cat(plotArgs)   # debugging
                         
  isolate({
    metaRegCode("#   moderator chosen; will be updated")  
  })
 

})

## debugging:  
observeEvent(metaRegCode(),{
  print("the current metaregression code is:  ")
  print(metaRegCode())
})



# 
# 
# ######### tab panel for selecting a moderator and saying "yes" do meta-analysis 
# #
# # This section is referenced by this part of the UI.R file:
# # tabPanel("Moderator Selection",
# #          uiOutput("moderatorSelection_ui")
# # ),
# #
# output$moderatorSelection_ui <- renderUI({
#   Variable.Factor.Names <- myrvs$Variable.Factor.Names
#   tagList(
#     radioButtons("includeModerator", "Do you want to include a moderator for meta-regression?",
#                  choices = list("No" = "No", "Yes" = "Yes"), selected = "No"),
#     conditionalPanel(
#       condition = "input.includeModerator == 'Yes'",
#       radioButtons("moderator_variable", "Select one moderator variable",
#                    choices = Variable.Factor.Names)
#     ))
# })
# 
# 
# ######### tab panel for forest plot results
# #
# ##  goes with this section of the UI.R file:
# # tabPanel("Meta-Regression",
# #          printButton,
# #          h4("Frequentist Meta-Regression"),
# #          uiOutput("metaRegressionOutputUI")
# # ),
# 
# 
# ## set plot height and x axis limits:
# reactiveValuesListMetaRegression <- reactiveValues(
#   plot_height = 1000,  # initial default value, you can adjust this
#   xlim_min = NULL,     # not used for now
#   xlim_max = NULL      # not used for now
# )
# 
# # Update plot height based on the number of rows in MA when MA is available
# observe({
#   MA_data <- MA()
#   reactiveValuesListMetaRegression$plot_height <- nrow(MA_data) * 12 + 400
# })
# 
# # New values when "update" button is pressed
# observeEvent(input$update_x_axis, {
#   # update height
#   if (!is.null(input$freq_forest_height_input) && !is.na(input$freq_forest_height_input)) {
#     reactiveValuesListMetaRegression$plot_height <- as.numeric(input$freq_forest_height_input)
#   }
# })
# 
# # A reactive that depends on those values
# freq_forest_height_mod <- reactive({
#   reactiveValuesListMetaRegression$plot_height
# })   # height value for panel with a moderator
# 
# 
# # draw the plot:
# output$metaRegressionOutputUI <- renderUI({
#   MA <- MA()
#   moderatorName <- input$moderator_variable
#   
#   # check to make sure input$moderator_variable exists before assigning its value to something:
#   if (is.null(input$moderator_variable)) {
#     warning("First you must select a valid moderator in the Moderator Selection tab...")
#     moderator <- NULL
#   } else {
#     moderator <- MA[[moderatorName]]
#   }
#   
#   tagList(
#     printButton,
#     h4("Frequentist Meta-Regression: Forest Plot with Subgroups"),
#     p("Will only be computed if 'Yes' is selected for 'Include Moderator'",
#       "and one categorical moderator is selected.") ,
#     conditionalPanel(
#       condition = "input.includeModerator == 'Yes'",
#       tagList(
#         renderText(paste("Moderator variable is:  ", moderatorName)),
#         renderPlot({
#           plot_args <- list(
#             MA = MA, 
#             moderator = moderator,
#             col = "red",       # color of the summary polygon
#             border = "red",    # color of the summary polygon
#             efac = .3
#           )
#           
#           if (input$update_x_axis > 0) {
#             x_min <- as.numeric(input$x_min)
#             x_max <- as.numeric(input$x_max)
#             
#             if (!is.na(x_min) && !is.na(x_max)) {
#               plot_args$xlim <- c(x_min, x_max)
#             }
#             
#             if (input$caterpillar) {
#               plot_args$caterpillar <- TRUE
#             }
#           }
#           
#           ### This creates the plot and passes the chosen arguments:
#           do.call(forestByGroup, plot_args)
#           ## additional arguments that might be useful: xlim, psize, xlab
#           ## For caterpillar plot, caterpillar=TRUE (not slab=NA)
#           ## You can also add any other arguments that forest() allows
#         }, 
#         height = freq_forest_height_mod()),    
#         
#         # input boxes for adjusting x axis:
#         hr(),
#         actionButton("update_x_axis", "Update plot", style = "margin-bottom: 3px;"),
#         tags$style(type='text/css', "
#           #update_x_axis {
#           height: 5px;
#           line-height: 3px;
#           background-color: tan; /* Change this to your desired color */
#           color: black; /* Change text color if needed */
#           }
#         "),
#         p("Update x axis values:"),
#         fluidRow(
#           column(2, textInput("x_min", label = NULL, placeholder = "Min", width = "100px")),
#           tags$style(type='text/css', "#x_min { height: 3px; }"),
#           column(2, textInput("x_max", label = NULL, placeholder = "Max", width = "100px")),
#           tags$style(type='text/css', "#x_max { height: 3px; }")
#         ),
#         checkboxInput("caterpillar", label="Caterpillar plot",value = F),
#         textInput("freq_forest_height_input", "Change plot height: (also resets x axis)", value = freq_forest_height_mod()),
#         tags$style(type='text/css', "#freq_forest_height_input { height: 3px; }")
#       )
#     )
#   )   # end of tagList
# })
# 
