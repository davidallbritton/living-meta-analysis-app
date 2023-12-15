# temp scratchpad
file_path <- 'C:\\Users\\dallbrit\\Downloads\\temporiginal8.csv'
file_path3 <- 'C:\\Users\\dallbrit\\Downloads\\temporiginal7.csv'
file_path2 <- 'C:\\Users\\dallbrit\\Downloads\\temporiginal8.xlsx'
file_path4 <- 'C:\\Users\\dallbrit\\Downloads\\temporiginal7.xlsx'

df <- readr::read_csv(file_path, show_col_types = FALSE, encoding = "UTF-8") %>% as.data.frame()
#df3 <- readr::read_csv(file_path3, show_col_types = FALSE)  %>% as.data.frame()
df2 <- readxl::read_excel(file_path2)  %>% as.data.frame()
#df4 <- readxl::read_excel(file_path4)  %>% as.data.frame()

#identical(df3[,4], df4[,4])
#identical(df3, df4)
identical(df, df2)
identical(df[1,], df2[1,])
identical(df[2,], df2[2,])



bayesian_forest_plot
bayesian_funnel_plot
bayesian_statistics
bayesian_additional_plots
bayesian_robustness


bayesian_robustness
checkOldPlots



















## modal to warn when Bayesian robust plot pane update is requested
observe({
  # Reactively depend on MA()
  MA()
  # Trigger only when the robustness tab is selected
  req(input$mainTabset %in% c("bayesian_robustness"))   # for that panel only
  #
  ##  repeating code  that checks for cached robustness plots:
  isolate({    old_plot <- FALSE
  # copy reactive values for use in this block.  Use as arguments to functions.
  MA <- MA() 
  MA_nofactors <- as.data.frame(MA) 
  MA_nofactors <-  MA_nofactors %>% mutate_if(is.factor, as.character)
  robust <- input$robust
  tauprior <- input$tauprior
  mupriorsd <- input$mupriorsd
  scaletau <- input$scaletau
  old_plot <- checkOldPlots(myrvs$previousPlots, MA=MA_nofactors, tauprior=tauprior, mupriorsd=mupriorsd, scaletau=scaletau, robust=robust)
  print("oldbma is truthy?  !isTruthy(old_plot)")  # debugging
  print(!isTruthy(old_plot))  # debugging
  if(!isTruthy(old_plot))  { # skip the shinyalert if that plot is already cached
  #
  # Display the shinyalert  
  shinyalert(
    title = "Are you sure you want to do this new Bayesian robustness analysis?  It could take a VERY long time.",
    type = "warning",
    showCancelButton = TRUE,
    confirmButtonText = "Yes, continue!",
    cancelButtonText = "No, not right now.",
    callbackR = function(value) {
      myrvs$triggerBmaRobust <- value
    }
  )}
  else  myrvs$triggerBmaRobust <- TRUE
  })
})


myrvs$triggerBma <- FALSE
myrvs$triggerBmaRobust <- FALSE


MA <- MA() 
MA_nofactors <- as.data.frame(MA) 
MA_nofactors <-  MA_nofactors %>% mutate_if(is.factor, as.character)
robust <- input$robust
tauprior <- input$tauprior
mupriorsd <- input$mupriorsd
scaletau <- input$scaletau
old_plot <- checkOldPlots(myrvs$previousPlots, MA=MA_nofactors, tauprior=tauprior, mupriorsd=mupriorsd, scaletau=scaletau, robust=robust)
if(isTruthy(old_plot)) robustggplot <- old_plot  # retrieve previously calculated







observe({
  # Reactively depend on MA()
  MA()
  
  # Trigger the shinyalert and the rest of the logic
  shinyalert(
    title = "Are you sure you want to do that?", 
    text = "This action cannot be undone.", 
    type = "warning",
    showCancelButton = TRUE, 
    confirmButtonText = "Yes, continue!",
    cancelButtonText = "No, cancel",
    callbackR = function(value) {
      # This callback function is executed after the modal is closed.
      # 'value' is TRUE if the user clicked 'Yes, continue!', FALSE otherwise.
      if (value) {
        # User confirmed the action, continue with execution
        output$currentDataFile <- renderUI({
          isolate({
            myrvs$recalculatedSinceUpload <- 1
            if (myrvs$nfiles > 0) {
              p(
                "The currently displayed results are from ", 
                span(style = "white-space: nowrap", myrvs$currentInputFile)
              )
            } else {
              p("The currently displayed results are from Vasilev et al., 2018 plus updates as of 2023")
            }
          })
        })
      } else {
        # User cancelled the action, do nothing or perform some other action
        print("Action cancelled by the user")
      }
    }
  )
})







