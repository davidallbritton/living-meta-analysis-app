


bma <- bayesmeta(y = MA()$es,sigma = sqrt(MA()$var), labels = MA()$study, 
                 tau.prior = function(t) dhalfcauchy(t, scale = input$scaletau), 
                 mu.prior = c("mean" = input$mupriormean, "sd" = input$mupriorsd))

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

writexl::write_xlsx( ilist2, path = "templist2.xlsx", col_names = F)


ns[! ns %in% wrongnames]
wrongnames[! wrongnames %in% ns] -> skipnames2

skipnames2 <- c( "studies_cells_selected" ,  "studies_rows_all"    ,     "studies_rows_selected"  , 
                  "studies_state"     ,       "studies_search"       ,    "studies_cell_clicked"  ,  
                  "studies_columns_selected", "studies_rows_current")

bma2$call -> tempcall
capture.output(bma2$call) -> tempccc
tempddd <- paste(tempccc, collapse = "")
tempdf <- as.data.frame(tempddd)

Bayesmeta.Call <- capture.output(bma2$call) %>% paste(collapse = "") %>% as.data.frame() 
  
