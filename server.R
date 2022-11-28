#######################################################################################
################### A Universal Tool for BAYSEIAN META-ANALYSIS #################
#######################################################################################

################### Shiny App v.0.2 2022.11.21 SERVER ###################################
#
# Derived and adapted from https://vinzentwolf.shinyapps.io/taVNSHRVmeta/
# as described in https://doi.org/10.1111/psyp.13933
#
###################################################################################

# Define server logic
server <- function(input, output) {
  
  ## Initialize with stored data, which will be replaced when a data file is uploaded
  ## by the user
  myrvs <- reactiveValues(currentInputFile = NULL)   
  myrvs$nfiles <- 0
  observe({
    if(is.null(input$DataFileUp)){   #this trigger works because input$DataFileUp gets initialized to null when the app first loads, which triggers the observer
      isolate({                   #isolate so that changes in myrvs do not trigger the observer
        newrvs <- reformat.df(df)
        myrvs$df.reactive <- newrvs$df
        myrvs$df.original <- newrvs$df.original
        myrvs$Variable.Factor.Names <- newrvs$Variable.Factor.Names
        myrvs$Variable.Numeric.Names <- newrvs$Variable.Numeric.Names
        myrvs$na.warning <- newrvs$na.warning
        message("Initialized with stored data, before any file is uploaded")   ## ** for debugging  
      })
    }
  })
  
  ## When the user uploads a data file, replace the existing data and update the UI
  observeEvent(input$DataFileUp, {
    # Read the data from the excel or csv file the user uploaded:
     ### may want to insert here some format checking before importing data file ** ###
    fileExtension <- tools::file_ext(input$DataFileUp$datapath)
    message(fileExtension)   ########## for debugging ***
    message("fileextension above")     ########## for debugging ***
    message("DataFileUp$name")     ########## for debugging ***
    message(input$DataFileUp$name)     ########## for debugging ***
    output$inputFileError <- renderUI({  # create error message in case file not uploaded successfully
      if (!is.null(input$DataFileUp)) p(style = "color:red", "***File was not read.  Must be .xls or .xlsx***")
    })
    message("before validate")
    validate(need(fileExtension == "xlsx" | fileExtension == "xls" , "Please upload an Excel file"))
    message("after validate")
    df <- readxl::read_excel(input$DataFileUp$datapath) %>% as.data.frame()
    newrvs <- reformat.df(df)
    myrvs$df.reactive <- newrvs$df
    myrvs$df.original <- newrvs$df.original
    myrvs$Variable.Factor.Names <- newrvs$Variable.Factor.Names
    myrvs$Variable.Numeric.Names <- newrvs$Variable.Numeric.Names
    myrvs$na.warning <- newrvs$na.warning
    myrvs$nfiles <- myrvs$nfiles + 1
    myrvs$currentInputFile <- input$DataFileUp$name
    message("after validate")
    output$inputFileError <- renderUI(NULL) # remove error message if file uploaded successfully
    
    message("----------------------- input file uploaded")   ### ** for debugging
    nrow(myrvs$df.reactive) %>% message()   ### ** for debugging
  })
  
  
      

  observeEvent(input$replacementSubmitButton, {
    output$currentDataFile <- renderUI({
      isolate({
        if (myrvs$nfiles > 0) {
          p(
            "The currently displayed results are from ", 
            span(style = "white-space: nowrap", myrvs$currentInputFile)
          )
        } else {
          p("The currently displayed results are from Vasilev et al., 2018 (the default data file)")
        }
      })
    })
  })
  
  
  

  ##   tabPanel("Study criteria",    ## creating UI content for this tabPanel ##
  output$studyCriteria <- renderUI({
    message(" === now in rendereUI block")     ### ** for debugging
    ## read in the reactive values to use in creating the UI tabPanel entries:
    df <- myrvs$df.reactive
    Variable.Factor.Names <- myrvs$Variable.Factor.Names 
    Variable.Numeric.Names <- myrvs$Variable.Numeric.Names 
    na.warning <- myrvs$na.warning
    
    # putting a <div> around the whole thing:
    times <- myrvs$nfiles
    message("times ****")
    message(times)
    message(letters[(times %% length(letters)) + 1])
    div(id=letters[(times %% length(letters)) + 1],     
    tagList(    
             br(), 
             radioButtons(inputId = "aggregation", label = p("Aggregate over", style="color:#333333",
                                                             tags$style(type = "text/css", "#q18 {vertical-align: top;}"),
                                                             bsButton("q118", label = "", icon = icon("info"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),    
                          choices = c(ID = "ID", Papers = "Papers")),
             bsPopover(id="q118", title = "Aggregation.",
                       content = paste0("<p>Aggregate effect sizes over ID (default) or over papers.",
                                        "<p>Selecting Papers will compute a single aggregated effect size for each paper. Selecting ID will aggregate based on the numbers in the ID column in the data file.  If you want no aggregation, make sure the data file has a unique ID number for each line.",
                                        "<p>Default: ID."),
                       placement = "right", 
                       trigger = "click",
                       options = list(container = "body")),
             checkboxGroupInput(inputId = "Design", label = p("Study design",style="color:#333333",
                                                              tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                                              bsButton("q1", label = "", icon = icon("info"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")), 
                                choices = levels(df$Design), selected = levels(df$Design)),
             bsPopover(id="q1", title = "Study design.",
                       content = paste0("<p>Choose to include effect sizes calculated within subjects, between subjects, or both.",
                                        "<p>Default: both."),
                       placement = "right", 
                       trigger = "click",
                       options = list(container = "body")),
             
             sliderInput(inputId = "pubyear", label = p("Publication year",style="color:#333333",
                                                        tags$style(type = "text/css", "#q8 {vertical-align: top;}"),
                                                        bsButton("q8", label = "", icon = icon("info"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")), 
                         min = min(df$Publication.Year), max = max(df$Publication.Year), value = c(min(df$Publication.Year), max(df$Publication.Year)), step = 1, sep = "", ticks = F),
             bsPopover(id="q8", title = "Publication year.",
                       content = paste0("<p>Choose to include effect sizes from studies with a certain range of publication years.",
                                        "<p>Default: all."),
                       placement = "right", 
                       trigger = "click",
                       options = list(container = "body")),
             
             
             ## loop over the variable factor columns
                                            lapply(Variable.Factor.Names, function(varName) {                             
                                              checkboxGroupInput(inputId = varName, label = p(varName,style="color:#333333"), 
                                                                 choices = levels(df[,varName]), selected = levels(df[,varName]))
                                            }),
             
             ### loop over the variable numeric selection columns
                                           lapply(Variable.Numeric.Names, function(varName) {
                                             pp <- if(!is.na(na.warning[varName])) p(na.warning[varName]) else ""
                                             ss <- sliderInput(inputId = varName, label = p(varName ,style="color:#333333"), 
                                                         min = min(df[,varName], na.rm = T), max = max(df[,varName], na.rm = T), value = c(min(df[,varName], na.rm = T), max(df[,varName], na.rm = T)), ticks = F)
                                             list(ss, pp)
                                           }), 
          

        checkboxGroupInput(
          inputId = "included", label = p("Include/exclude specific studies",style="color:#333333",
                                           tags$style(type = "text/css", "#q9 {vertical-align: top;}"),
                                           bsButton("q9", label = "", icon = icon("info"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")), 
           choices = levels(df$Paper.and.Exp), selected = levels(df$Paper.and.Exp)
        ),
        bsPopover(id="q9", title = "Include/exclude specific studies.",
                  content = paste0("<p>Exclude specific studies by removing the tick mark.",
                                   "<p>This selection is hierarchically below the other inclusion/exclusion criteria.",
                                   "<p>If a study is excluded by one of the selected criteria above, this study will not be included in the analysis, even though it is still ticked here.",
                                   "<p>However, if a study is selected for inclusion by the criteria above, but is unticked here, the study will not be included."),
                  placement = "right", 
                  trigger = "click",
                  options = list(container = "body"))
      )
    )
  })
  
  
  message("$$$$$$$$$$$$ nfiles and submit button:")
  isolate(message(myrvs$nfiles))
  isolate(message(input$replacementSubmitButton))
  
  # Create a trigger to redo the outputs whenever the "recalculate" button is 
  # clicked.  Could also add other triggers by accessing other reactive variables
  triggerRecalc <- reactive({
    message("^^^ triggerRecalc is getting updated")   ### for debugging ***
    isolate(message(myrvs$nfiles))   ### for debugging ***
    isolate(message(input$replacementSubmitButton)) #  input$replacementSubmitButton   ### for debugging ***
    input$replacementSubmitButton  # + myrvs$nfiles   ## turns out it is better to only recalculated on press of action button
  })

  isolate(message("myrvs$nfiles"))   ### for debugging ***
  isolate(message(myrvs$nfiles))   ### for debugging ***
  
  # Create MA reactive for all outputs
  MA <- eventReactive(triggerRecalc(), {
    message(" **MA reactive section ....")  ### for debugging ***
    # import the reactive version of the data and the relevant column names
    df <- myrvs$df.reactive
    Variable.Factor.Names <- myrvs$Variable.Factor.Names 
    Variable.Numeric.Names <- myrvs$Variable.Numeric.Names 
    ## Create subset based on chosen inclusion criteria
    df_sub <- df %>% filter(Design %in% input$Design,
                            Publication.Year >= input$pubyear[1], Publication.Year <= input$pubyear[2],
                            Paper.and.Exp %in% input$included)
 
    ## Create subset based on the above plus input-file defined selection factors
    for (varName in Variable.Factor.Names)  {
      keepValues <- input[[varName]]
      df_sub <- df_sub[df_sub[,varName] %in% keepValues, ]
    }

    
    ## Create subset based on the above plus input-file defined selection numerics
    for (varName in Variable.Numeric.Names)  {
      df_sub <- df_sub[df_sub[,varName] >= input[[varName]][1], ]
      df_sub <- df_sub[df_sub[,varName] <= input[[varName]][2], ]
    }
    
    # replace ID with Paper.Number if aggregating over papers:
    if (input$aggregation == "Papers") {
      df_sub$ID <- df_sub$Paper.Number
      df_sub$study <- df_sub$Paper
    }
    
    ## Aggregate effect sizes
    aggES <- agg(id     = ID,
                 es     = yi,
                 var    = vi,
                 data   = df_sub,
                 cor = .5,
                 method = "BHHR")
    ## Merging aggregated ES with original dataframe 
    MA <- merge(x = aggES, y = df_sub, by.x = "id", by.y = "ID") 
    MA <- unique(setDT(MA) [sort.list(id)], by = "id")
    MA <- with(MA, MA[order(MA$es)])
#    })
  })

  # Create bma reactive needed for all outputs
  bma <- reactive({
    message(" ****bma reactive section ....")  ### for debugging ***
    MA()    #trigger to update bma
    ## Generate bayesmeta-object "bma" depending on tau prior chosen
   isolate({
    if (input$tauprior == "Half cauchy") {
      bma <- bayesmeta(y = MA()$es,sigma = sqrt(MA()$var), labels = MA()$study, 
                       tau.prior = function(t) dhalfcauchy(t, scale = input$scaletau), 
                       mu.prior = c("mean" = input$mupriormean, "sd" = input$mupriorsd))
    } else if (input$tauprior == "Half student t") {
      bma <- bayesmeta(y = MA()$es,sigma = sqrt(MA()$var), labels = MA()$study, 
                       tau.prior = function(t) dhalfnormal(t, scale = input$scaletau), 
                       mu.prior = c("mean" = input$mupriormean, "sd" = input$mupriorsd))
    } else {
      bma <- bayesmeta(y = MA()$es,sigma = sqrt(MA()$var), labels = MA()$study, 
                       tau.prior = input$tauprior, 
                       mu.prior = c("mean" = input$mupriormean, "sd" = input$mupriorsd))
    }
   })
  })
  
  # Study overview panel  
  output$studies <- DT::renderDataTable({
#    message(" &&& output$studies panel creation.")  ### for debugging ***
    MAs <- as.data.frame(MA())
    MAclean <-  mutate(MAs, "Included Studies" = study) %>% 
      select("Included Studies", Publication.Year)
    DT::datatable(MAclean,
                  options = list(pageLength = nrow(MAclean)))
  })
 
    ## Warning message if 3 or less studies are included
    output$warning <- renderPrint({
      MAs <- as.data.frame(MA())
      if (nrow(MAs) < 4) {print('WARNING: With the chosen inclusion criteria, 3 or fewer studies will be included in the analysis.')}
  })
  # Outliers panel
  output$boxplot <- renderPlot({
    MAo <- MA() %>% tibble::rownames_to_column(var = "outlier") %>% mutate(is_outlier=ifelse(is_outlier(es), es, as.numeric(NA)))
    MAo$study[which(is.na(MAo$is_outlier))] <- as.numeric(NA)
    ggplot(MAo, aes(x = factor(0), es)) +
      geom_boxplot(outlier.size = 3.5, outlier.colour = "#D55E00", outlier.shape = 18, fill = "lightgrey") +
      geom_text(aes(label=study),na.rm = T, nudge_y = 0.02, nudge_x = 0.05) +
      stat_boxplot(geom="errorbar", width = 0.05) +
      scale_x_discrete(breaks = NULL) +
      xlab(NULL) + ylab("Hedges' g") +
      theme_minimal_hgrid(12)
  }, width = 600, height = 600)
  
  # Forest plot panel height
  forest_height <- reactive(length(bma()$y) * 25 + 200)
  
  # Forest Plot panel
  output$forest <- renderPlot({
    forestplot.bayesmeta(bma(), xlab = "Hedges' g")
  }, height = forest_height)
  
  # Funnel Plot panel
  output$funnel <- renderPlot({
    funnel.bayesmeta(bma(), main = "")
  })
  # Statistics panel
  output$bf <- renderPrint ({
    bma()$bayesfactor[1,]
  })
  output$summary <- renderPrint({
    bma()$summary
  })
  output$ML <- renderPrint({
    bma()$ML
  })
  output$MAP <- renderPrint({
    bma()$MAP[1,]
  })
  
  # Full texts screened panel  #deleted this###
 
  # Additional plots panel
  output$evupdate <- renderPlot({
    bma()  #trigger recalculation
    isolate({
      priorposteriorlikelihood.ggplot(bma(), lowerbound = 0 - (input$mupriormean + 1) * 1.5, upperbound = 0 + (input$mupriormean + 1) * 1.5)
    })
  }, width = 800)
  output$joint <- renderPlot({
    plot.bayesmeta(bma(), which=2, main = "")
  }, width = 800)
  output$taupriorplot <- renderPlot({
    tauprior.ggplot(bma())
  }, width = 800)
  
  # Bayes factor robustness plot panel
  output$warning2 <- renderPrint({
    print("WARNING: Plot will not be computed, because an improper τ prior was chosen. Proper τ priors are 'Half student t' and 'Half cauchy'.")})
  output$robustplot <- renderPlot({
    MA()  #trigger recalculation
    isolate({
      if (input$robust == "Yes" &
          input$tauprior == "Half cauchy") {
        robustness(MA(),SD = input$mupriorsd, tauprior = function(t) dhalfcauchy(t, scale = input$scaletau))
      } else if (input$robust == "Yes" &
                 input$tauprior == "Half student t") {
        robustness(MA(),SD = input$mupriorsd, tauprior = function(t) dhalfnormal(t, scale = input$scaletau))
      } 
    })
  }, width = 800)
  
  # Downloads panel
  #
  ##### create download buttons to display in UI
  #
  output$downloadButtons <- renderUI({
    if (input$replacementSubmitButton){
      tagList(
        p(),
        downloadButton("originalData", "Data as originally uploaded"),
        p(),
        downloadButton("currentData", "Data as currently in use (primarily for debugging)"),
        p(),
        downloadButton("listInputs", "List of all UI input selections")
      ) 
    }
    else(p("Must (Re)Calculate first...."))
  })
  #
  ##### create things for the UI to download here....
  output$originalData <- downloadHandler(   
    filename = function() {
      "currentData.xlsx" 
    },
    content = function (file) {
      writexl::write_xlsx(myrvs$df.original, file)
    }
  )
  #  
  output$currentData <- downloadHandler(
    filename = function() {
      "currentData.xlsx" 
    },
    content = function (file) {
      if (is.null(myrvs$currentInputFile)) {Source <- "Vasilev et al., 2018"} else Source <- myrvs$currentInputFile
      Source <- req(as.data.frame(Source))
      sheetList <- list(original_Data = myrvs$df.original, current_Data = myrvs$df.reactive, selected_data = req(as.data.frame(MA())), Source = Source )
      writexl::write_xlsx(sheetList, file)
    }
  )
  #
  output$listInputs <- downloadHandler(
    filename = function() {
      "currentInputSelections.xlsx" 
    },
    content = function (file) {
      inputslist <<- reactiveValuesToList(input) # sends it to the global environment; for debugging ***
      inputslist <- reactiveValuesToList(input)
      ns <- names(inputslist)
      skipnames1 <- c("website","q8","q18","q118", "replacementSubmitButton","q16","q19", "q1","q17", "q20" ,"email1", "q9"  )
      skipnames2 <- c( "studies_cells_selected" ,  "studies_rows_all"    ,     "studies_rows_selected"  , 
                       "studies_state"     ,       "studies_search"       ,    "studies_cell_clicked"  ,  
                       "studies_columns_selected", "studies_rows_current")
      skipnames <- c(skipnames1, skipnames2)
      namestolist <- ns[! ns %in% skipnames]
      orderednames <- c("mupriormean", "mupriorsd", "tauprior", "scaletau", "robust", "DataFileUp", "aggregation", "Design", "pubyear","included" )
      extranames <- namestolist[! namestolist %in% orderednames]
      allnames <- c(orderednames, extranames)
      ilist <- inputslist[allnames]
      ilist2 <- lapply(ilist, function(x) as.data.frame(x))
      writexl::write_xlsx(ilist2, col_names = F, file)
    }
  )

  
}
