#######################################################################################
################### A General Tool for BAYSEIAN META-ANALYSIS #################
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
        myrvs$df.reactive <- newrvs$df          # for manipulating and calculating; gets altered
        myrvs$df.original <- newrvs$df.original # as originally loaded or uploaded; does not change
        myrvs$df.updated <- myrvs$df.original   # original data plus any data points input by the user
        myrvs$Variable.Factor.Names <- newrvs$Variable.Factor.Names
        myrvs$Variable.Numeric.Names <- newrvs$Variable.Numeric.Names
        myrvs$na.warning <- newrvs$na.warning
        myrvs$recalculatedSinceUpload <- 0
      })
    }
    tempnames <<- myrvs$Variable.Factor.Names
    
  })
  
  ## When the user uploads a data file, replace the existing data and update the UI
  observeEvent(input$DataFileUp, {
    # Read the data from the excel file the user uploaded:
    fileExtension <- tools::file_ext(input$DataFileUp$datapath)
    output$inputFileError <- renderUI({  # create error message in case file not uploaded successfully
      if (!is.null(input$DataFileUp)) p(style = "color:red", "***File was not read.  Must be .xls or .xlsx***")
    })
    validate(need(fileExtension == "xlsx" | fileExtension == "xls" , "Please upload an Excel file"))
    df <- readxl::read_excel(input$DataFileUp$datapath) %>% as.data.frame()
    newrvs <- reformat.df(df)
    myrvs$df.reactive <- newrvs$df
    myrvs$df.original <- newrvs$df.original
    myrvs$df.updated <- myrvs$df.original 
    myrvs$Variable.Factor.Names <- newrvs$Variable.Factor.Names
    myrvs$Variable.Numeric.Names <- newrvs$Variable.Numeric.Names
    myrvs$na.warning <- newrvs$na.warning
    myrvs$nfiles <- myrvs$nfiles + 1
    myrvs$recalculatedSinceUpload <- 0
    myrvs$currentInputFile <- input$DataFileUp$name
    output$inputFileError <- renderUI(NULL) # remove error message if file uploaded successfully
  })
  
  observeEvent(input$recalculateButton, {
    output$currentDataFile <- renderUI({
      isolate({
        myrvs$recalculatedSinceUpload <- 1
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
  
  #################### 
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
  #################### End of study criteria panel 
  
  

  ##  addStudies
  #################### 
  ##   tabPanel   addStudies   ## creating UI content for this tabPanel ##
  output$addStudies <- renderUI({
    
    ## read in the reactive values to use in creating the UI tabPanel entries:
    df <- myrvs$df.reactive
    Variable.Factor.Names <- myrvs$Variable.Factor.Names 
    Variable.Numeric.Names <- myrvs$Variable.Numeric.Names 
    na.warning <- myrvs$na.warning
    
    # preparing to put a <div> around the whole thing:
    times <- myrvs$nfiles
    message("times ****")
    message(times)
    message(letters[(times %% length(letters)) + 11])
    
    if (myrvs$recalculatedSinceUpload > 0){      # force recalculation before adding a study
      div(id=letters[(times %% length(letters)) + 11],     
          tagList(    
            br(),
            p("Add new studies here.  Each effect size from a multi-experiment or multi-measurement",
              "paper should be a separate entry with a unique ID number.  You will need to provide ", 
              "either group means and variabilities OR an effect size measure (g or d) and its variance.",
              "After adding one or more",
              "studies and recalculating, you can download the updated data file.", 
              "An alternative method for adding studies is to download the original data as a .xlsx file,",
              "add new rows of data, then upload the new .xlsx file for analysis."),
            hr(),
            numericInput(inputId = "ID_add",  label = "ID number for new effect size", max(df$ID) +1, min = max(df$ID) +1),
            textInput(inputId = "Paper_add", label = "Paper (citation)", as.character(max(df$ID) +1)),
            # might want to allow to select an existing study or "add new" and then enter one,
            # so that I can use the existing paper # if they are adding a new 
            # experiment or effect size for an already existing or previously entered paper
            #   Might also want to collect exp# and es# (default=1) in case they are 
            #   adding multiple effect sizes from a single paper. 
            numericInput(inputId = "pubyear_add",  label = "Publication Year", value =2023),
            numericInput(inputId = "Experiment.Number_add",  label = "Experiment.Number (within paper)", value =1),
            numericInput(inputId = "Effect.Size.Number_add",  label = "Effect.Size.Number (within experiment)", value =1),
            radioButtons(inputId = "Design_add", label = p("Study design"), 
                         choices = levels(df$Design)),
            numericInput(inputId = "r_add",  
                         label = 'Within-study outcome correlation (for "within" designs)', 
                         value = r_estimate,  min = 0, max = 1, step = 0.01),
            
            hr(),
            ## loop over the variable factor columns
            lapply(Variable.Factor.Names, function(varName) {  
              varName_add <- paste0(varName, "_add")
              radioButtons(inputId = varName_add, label = p(varName), 
                           choices = c(levels(df[,varName]), "Other (not listed)"), selected = "")
            }),
            
            ### loop over the variable numeric selection columns
            lapply(Variable.Numeric.Names, function(varName) {
              varName_add <- paste0(varName, "_add")
              numericInput(inputId = varName_add, label = varName, value = "")
            }), 
            hr(),
            
            p(strong("Required:")," Number of subjects in each group"),
            numericInput(inputId = "Total.N_add",  label = "Total N", value =0),
            numericInput(inputId = "N_Intervention_add",  label = "Intervention N", value =0),
            numericInput(inputId = "N_Control_add",  label = "Control N", value =0),
            
            hr(),
            p(strong("Required:"),"Either group means and variabilities, OR effect size information"),
            p("Group means and variabilities:"),
            numericInput(inputId = "mean_E_add",  label = "Intervention mean", value =NULL), ### ****
            numericInput(inputId = "mean_C_add",  label = "Control mean", value =""),
            radioButtons(inputId = "reverseCode_add", choices = c("More is better (e.g., % correct)", "More is worse (e.g., % errors, RT)"), 
                         selected = "More is better (e.g., % correct)", label = "Means require regular coding (more is better) or reverse coding (more is worse)?"),
            numericInput(inputId = "var_E_add",  label = "Intervention variability (SD, SE, or variance)", value =""),
            numericInput(inputId = "var_C_add",  label = "Control variability (SD, SE, or variance)", value =""),
            radioButtons(inputId = "var_type_add", choices = c("Standard deviation", "Variance", "Standard error"), 
                         selected = "Standard deviation", label = "Variance type (SD is preferred; variance is acceptable; SE is discouraged"),
            hr(),
            p("Effect size (either g or d) and variance:", 
            ),
            numericInput(inputId = "g_add",  label = "effect size (g)", value =""),
            numericInput(inputId = "g_var_add",  label = "variance of g", value =""),
            numericInput(inputId = "d_add",  label = "effect size (d)", value =""),
            numericInput(inputId = "d_var_add",  label = "variance of d", value =""),
            
            actionButton(inputId = "add1study","Add this study", icon("sync"), style = "color: green; background-color: white"),
            
            p()   ###  *** p() is for debugging only. need a submit button here, perhaps
            # with error checking for between/within and total N, and for 
            # whether all required info is provided
          )
      ) 
    }
    else(p("Must (Re)Calculate first...."))
  })
  #################### End of add studies panel 
  
  
  
  ########### Adding a study that was input by the user
  observeEvent(input$add1study, {     #when a study is input to add, do this:
    ### check the user input for errors
    
    ### add the study 
    other.Names_add <- c("ID_add",  "Paper_add", "pubyear_add",  "Experiment.Number_add", 
                         "Effect.Size.Number_add", "Design_add", 
                         "r_add",
                         "Total.N_add", "N_Intervention_add", "N_Control_add", 
                         "mean_E_add",  "mean_C_add", 
                         "reverseCode_add",
                         "var_E_add",   "var_C_add",   "var_type_add",
                         "g_add",  "g_var_add",  "d_add",  "d_var_add"
    )
    other.Names <- str_replace(other.Names_add, "_add", "")
    # The inputfields and calculatedfields will need to be added to the dataset
    calculatedfields <- c("Paper.and.Exp", "yi", "vi")
    inputfields <- c(myrvs$Variable.Factor.Names, myrvs$Variable.Numeric.Names, other.Names)
    
    # Create the study label: Paper.and.Exp
    Paper.and.Exp <- paste0(input$Paper_add, " Exp. ", input$Experiment.Number_add)
    
    # Get the values for yi and vi:
    ### call the function and pass it arguments from the user input; assign yi and vi values
    gstats <- getEffectSize (g = as.numeric(input$g_add), 
                             g_var = as.numeric(input$g_var_add), 
                             d = as.numeric(input$d_add), 
                             d_var = as.numeric(input$d_var_add), 
                             mean_E = as.numeric(input$mean_E_add), 
                             mean_C = as.numeric(input$mean_C_add),
                             var_E = as.numeric(input$var_E_add), 
                             var_C = as.numeric(input$var_C_add), 
                             var_type = input$var_type_add, 
                             Total.N = as.numeric(input$Total.N_add), 
                             N_Intervention = as.numeric(input$N_Intervention_add), 
                             N_Control = as.numeric(input$N_Control_add), 
                             Design = input$Design_add, 
                             r = as.numeric(input$r_add),
                             reverseCode = input$reverseCode_add
                             )

    tempgstats <<- gstats  #### *** debugging

    # create a 1-row dataframe for the values that were input by the user and the calculated effect size:
    newstudy <- data.frame(yi = gstats$yi, vi = gstats$vi, Paper.and.Exp = Paper.and.Exp)
    
    tempnewstudy <<- newstudy  #### *** debugging
    
    
    
    # add the new row of data to the current data:
    
    
    # update the relevant data frames:
    
    
      
      
          
          
      ### what if the original data file did not contain columns for all the input variables?
      ### need to add them if they don't exist.  The "other.Names" might not exist.
      # display the updated data file.  Maybe add an output tab for displaying the data file.
      ### data file dependency should be:  df.original -> df.updated -> df.reactive 
      # 1. add the new data to myrvs$df.updated
      # 2. newdatalist <- reformat.df(myrvs$df.updated) 
      # 3. copy newdatalist$df.reactive to myrvs$df.reactive
      # 4. trigger the "must recalculate" message like uploading a new data file does
      ### ******* stopped here
      
    ### temp stuff:
    message(inputfields)                 ### for debugging ***
    message(length(inputfields))       ### for debugging ***
    message(other.Names_add)       ### for debugging ***
    message(length(other.Names_add))       ### for debugging ***
    message("Here is the g and g_var to be added")       ### for debugging ***
    #   message(g)       ### for debugging ***
    #   message(g_var)       ### for debugging ***
    
  })  # end of adding a study
  
  
  
  
  
  # Create MA reactive for all outputs
  MA <- eventReactive(input$recalculateButton, {
    # import the reactive version of the data and the relevant column names
    df <- myrvs$df.reactive
    Variable.Factor.Names <- myrvs$Variable.Factor.Names 
    Variable.Numeric.Names <- myrvs$Variable.Numeric.Names 
    ## Create subset based on chosen inclusion criteria
    df_sub <- df %>% filter(Design %in% input$Design,
                            Publication.Year >= input$pubyear[1], Publication.Year <= input$pubyear[2],
                            Paper.and.Exp %in% input$included)
    #
    ## Create subset based on the above plus input-file defined selection factors
    for (varName in Variable.Factor.Names)  {
      keepValues <- input[[varName]]
      df_sub <- df_sub[df_sub[,varName] %in% keepValues, ]
    }
    #
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
    #
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
#   ma2 <<- MA() # for debugging ***
#   bma2 <<- bma # for debugging ***
#   bma # for debugging ***
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
    if (myrvs$recalculatedSinceUpload > 0){
      tagList(
        p(),
        downloadButton("originalData", "Data as originally uploaded"),
        p(),
        downloadButton("currentData", "Data as currently in use (primarily for debugging)"),
        p(),
        downloadButton("listInputs", "List of all selected Study Criteria and Prior Specifications"),
        p(),
        downloadButton("bayesmetaCall", "Function call, parameters, and selected data for the current analysis")
      ) 
    }
    else(p("Must (Re)Calculate first...."))
  })
  #
  ##### create things for the UI to download here....
  output$originalData <- downloadHandler(   
    filename = function() {
      "originalData.xlsx" 
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
  output$bayesmetaCall <- downloadHandler(
    filename = function() {
      "bayesmeta_call.xlsx" 
    },
    content = function (file) {
      if (is.null(myrvs$currentInputFile)) {Source <- "Vasilev et al., 2018"} else Source <- myrvs$currentInputFile
      Source <- req(as.data.frame(Source))
      Bayesmeta.Summary <- req(as.data.frame(bma()$summary))
      Bayesmeta.Summary$statistic <-   row.names(Bayesmeta.Summary) 
      Bayesmeta.Summary <- select(Bayesmeta.Summary, statistic, tau, mu, theta)
      Bayesmeta.Call <- capture.output(bma()$call) %>% paste(collapse = "") %>% as.data.frame() 
        names(Bayesmeta.Call) <- "bayesmeta analysis command"
      mupriormean <- req(as.data.frame(input$mupriormean))
      mupriorsd <- req(as.data.frame(input$mupriorsd))
      tauprior <- req(as.data.frame(input$tauprior))
      scaletau <- req(as.data.frame(input$scaletau))
      robust <- req(as.data.frame(input$robust))
      sheetList <- list(Bayesmeta.Summary = Bayesmeta.Summary, 
                        Bayesmeta.Call = Bayesmeta.Call, 
                        mupriormean = mupriormean,
                        mupriorsd = mupriorsd,
                        tauprior = tauprior,
                        scaletau = scaletau,
                        MA.selected.data = req(as.data.frame(MA())), 
                        Source = Source 
                        )
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
      skipnames1 <- c("website","q8","q18","q118", "recalculateButton","q16","q19", "q1","q17", "q20" ,"email1", "q9"  )
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
