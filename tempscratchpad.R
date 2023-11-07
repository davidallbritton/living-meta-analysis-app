# NOT THE REAL FILE!!!!!!!!!!!!!!!!!


tempstuff <- readRDS("tempprevmods33.RDS")
tempstuff <- readRDS("tempprevmods.RDS")
tempstuff <- readRDS("defaultPrecalculatedModels.RDS")

tempbma2reactivex <- readRDS("bma2reactivex.RDS")
tempbma2x <- readRDS("bma2x.RDS")

identical(tempbma2reactivex, tempbma2x)
identical(as.list(tempbma2reactivex), as.list(tempbma2x))

attributes(tempbma2reactivex)
attributes(tempbma2x)

############ "Saved Bayesian Models" panel
tabPanel("Saved Bayesian Models",   
         p("The models created by bayesmeta can take a long time to compute.",
           "To save time you can choose to save bayesmeta models and avoid recomputing them.",
           'Each bayesmeta model computed during the current session is automatically',
           "saved for reuse during the session.  You can also choose to download",
           "the saved models, upload a previously downloaded set of saved models,",
           "or clear the current list of saved models."
         ),
         hr(),
         #
         p(tags$strong("Clear Saved Models."),
           "Some precalculated bayesmeta models for the default dataset may be loaded automatically when the app starts.",
           "You can delete them here if they are not relevant for your meta-analysis."
         ),
         actionButton("ClearModels", "Delete Models"),
         hr(),
         #
         p(tags$strong("Download Saved Models."),  
           "R object (RDS file) containing bayesmeta models along with the data and code used to generate them.",
           tags$i("(Also available on the Downloads tab)")),
         downloadButton("rds_file.bma", "Download"),
         hr(),
         p(tags$strong("Upload Precalculated Models."),  ####### change this
           "If you downloaded saved bayesmeta models during a previous",
           "session, you can upload them again to save calculation time.",
           "They will be appended to the current list of saved models, ",
           "so if you want to replace the current list you can first",
           "Clear Saved Models before uploading."
         ),
         downloadButton("rds_file.bma", "Download"),
         hr()
         
),
















tabPanel("Saved Bayesian Models",   
         p("The models created by bayesmeta can take a long time to compute.",
           "To save time you can choose to save bayesmeta models and avoid recomputing them.",
           'Each bayesmeta model computed during the current session is automatically',
           "saved for reuse during the session.  You can also choose to download",
           "the saved models, upload a previously downloaded set of saved models,",
           "or clear the current list of saved models."
         ),
         
         
         