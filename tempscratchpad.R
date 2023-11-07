# NOT THE REAL FILE!!!!!!!!!!!!!!!!!


bayesmeta_models (8).RDS

tempstuff <- readRDS("bayesmeta_models (0.8).RDS")
tempstuff <- readRDS("tempprevmods.RDS")
tempstuff <- readRDS("defaultPrecalculatedModels.RDS")

tempbma2reactivex <- readRDS("bma2reactivex.RDS")
tempbma2x <- readRDS("bma2x.RDS")

identical(tempbma2reactivex, tempbma2x)
identical(as.list(tempbma2reactivex), as.list(tempbma2x))

attributes(tempbma2reactivex)
attributes(tempbma2x)



SavedModelsUp


updateModels <- function(MA, printed_bma, bma) {
  newrow <- list(MA = MA, printed_bma = printed_bma, bma = bma) 
  myrvs$previousModels[length(myrvs$previousModels) + 1] <- list(newrow)
  # debugging prints; delete later:
  print("length of myrvs$previousModels")  # debugging
  print(length(myrvs$previousModels))  # debugging
  tempprevmods <- myrvs$previousModels   # debugging???
  saveRDS(tempprevmods, file = "tempprevmods.RDS")   # debugging???
  length(myrvs$previousModels)  # unused return value
}


#### for reading .RDS file containing previously calculated bayesmeta models
observeEvent(input$SavedModelsUp, {
  # Read the data from the RDS file the user uploaded:
  fileExtension <- tools::file_ext(input$SavedModelsUp$datapath)
  output$inputFileErrorBup <- renderUI({  # create error message in case file not uploaded successfully
    if (!is.null(input$SavedModelsUp)) p(style = "color:red", "***File was not read***")
  })
  validate(need(fileExtension == "RDS" | fileExtension == "rds" | fileExtension == "Rds" , "Please upload an RDS file"))
  newrows_bma <- readRDS(input$SavedModelsUp$datapath)
  oldrows_bma <- myrvs$previousModels
  allrows_bma <- c(newrows_bma, oldrows_bma)
  myrvs$previousModels <- allrows_bma
  output$inputFileErrorBup <- renderUI(NULL) # remove error message if file uploaded successfully
})
















tabPanel("Saved Bayesian Models",   
         p("The models created by bayesmeta can take a long time to compute.",
           "To save time you can choose to save bayesmeta models and avoid recomputing them.",
           'Each bayesmeta model computed during the current session is automatically',
           "saved for reuse during the session.  You can also choose to download",
           "the saved models, upload a previously downloaded set of saved models,",
           "or clear the current list of saved models."
         ),
         
         
         