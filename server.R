#######################################################################################
################### A Universal Tool for BAYSEIAN META-ANALYSIS #################
#######################################################################################

################### Shiny App v.0.1 2022.11.10 SERVER ###################################
#
# Derived and adapted from https://vinzentwolf.shinyapps.io/taVNSHRVmeta/
# as described in https://doi.org/10.1111/psyp.13933
#
###################################################################################

# Define server logic
server <- function(input, output) {
  
  ##### need to move all the data reading and formatting into a reactive context
  

  ## Initialize with stored data, which will be replaced when a data file is uploaded
  ## by the user
  myrvs <- reactiveValues()
  observe({
    if(is.null(input$infile1)){   #this trigger works because input$infile1 gets initialized to null when the app first loads, which triggers the observer
      isolate({                   #isolate so that changes in myrvs do not trigger the observer
        newrvs <- reformat.df(df)
        myrvs$df.reactive <- newrvs$df
        myrvs$df.original <- newrvs$df.original
        myrvs$Variable.Factor.Names <- newrvs$Variable.Factor.Names
        myrvs$Variable.Numeric.Names <- newrvs$Variable.Numeric.Names
        myrvs$na.warning <- newrvs$na.warning
        message("names(myrvs)")   ## ** for debugging  
        message(names(myrvs))   ## ** for debugging  
      })
    }
  })
  
  ## When the user uploads a data file, replace the existing data and update the UI
  observeEvent(input$infile1, {
    # Read the data from the excel or csv file the user uploaded:
     ### may want to insert here some format checking before importing data file ** ###
    df <- readxl::read_excel(input$infile1$datapath) %>% as.data.frame()
    newrvs <- reformat.df(df)
    myrvs$df.reactive <- newrvs$df
    myrvs$df.original <- newrvs$df.original
    myrvs$Variable.Factor.Names <- newrvs$Variable.Factor.Names
    myrvs$Variable.Numeric.Names <- newrvs$Variable.Numeric.Names
    myrvs$na.warning <- newrvs$na.warning
    
     message("input file uploaded")   ### ** for debugging
     message("myrvs$df.reactive:")   ### ** for debugging
     nrow(myrvs$df.reactive) %>% message()   ### ** for debugging
     message("what's in input$infile1")     ### ** for debugging
     message(input$infile1)     ### ** for debugging
    
  })


  
  output$tempstudylist <- renderUI({
    input$datafile1   # the trigger to make the UI get rewritten
    isolate({
      tagList(
        checkboxGroupInput(
          inputId = "included", label = p("Include/exclude specific studies",style="color:#333333",
                                           tags$style(type = "text/css", "#q9 {vertical-align: top;}"),
                                           bsButton("q9", label = "", icon = icon("info"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")), 
           choices = levels(df$Paper.and.Exp), selected = levels(df$Paper.and.Exp)
        )
      )
    })
  })
  
  
  
  
  
  

  # Create MA reactive for all outputs
  MA <- reactive({
    # import the reactive version of the data
    df <- myrvs$df.reactive
    ## Create subset based on chosen inclusion criteria
    df_sub <- df %>% filter(Design %in% input$Design,
                            Publication.Year >= input$pubyear[1], Publication.Year <= input$pubyear[2],
                            Paper.and.Exp %in% input$included)
 
    ## Create subset based on the above plus input-file defined selection factors
#    for (varName in Variable.Factor.Names)  {
#      keepValues <- input[[varName]]
#      df_sub <- df_sub[df_sub[,varName] %in% keepValues, ]
#    }
    
    ## Create subset based on the above plus input-file defined selection numerics
#    for (varName in Variable.Numeric.Names)  {
#      df_sub <- df_sub[df_sub[,varName] >= input[[varName]][1], ]
#      df_sub <- df_sub[df_sub[,varName] <= input[[varName]][2], ]
#    }
    
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
  })

  # Create bma reactive needed for all outputs
  bma <- reactive({
    ## Generate bayesmeta-object "bma" depending on tau prior chosen
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
  
  # Study overview panel  
  output$studies <- DT::renderDataTable({
    MA <- as.data.frame(MA())
    MAclean <-  mutate(MA, "Included Studies" = study) %>% 
      select("Included Studies", Publication.Year)
    DT::datatable(MAclean,
                  options = list(pageLength = nrow(MAclean)))
  })
 
    ## Warning message if 3 or less studies are included
    output$warning <- renderPrint({
      MA <- as.data.frame(MA())
      if (nrow(MA) < 4) {print('WARNING: With the chosen inclusion criteria, 3 or fewer studies will be included in the analysis.')}
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
    priorposteriorlikelihood.ggplot(bma(), lowerbound = 0 - (input$mupriormean + 1) * 1.5, upperbound = 0 + (input$mupriormean + 1) * 1.5)
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
    if (input$robust == "Yes" &
        input$tauprior == "Half cauchy") {
      robustness(MA(),SD = input$mupriorsd, tauprior = function(t) dhalfcauchy(t, scale = input$scaletau))
    } else if (input$robust == "Yes" &
               input$tauprior == "Half student t") {
      robustness(MA(),SD = input$mupriorsd, tauprior = function(t) dhalfnormal(t, scale = input$scaletau))
    } 
  }, width = 800)
}
