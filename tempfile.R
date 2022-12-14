
# comparing Martin's functions to escalc

r <- 0.74
#within:
d <- Cohens_d(M_C = data$mean_C[NCD[i]], M_E = data$mean_E[NCD[i]], S_C = data$var_C[NCD[i]],
         S_E = data$var_E[NCD[i]], N = data$N_C[NCD[i]], r = r,
         design = as.character(data$design[NCD[i]]), type = type)

d_var <- Cohens_d_var(d = data$d[NCD[i]], N = data$N_C[NCD[i]], r = r,
                                  design = as.character(data$design[NCD[i]]))

g <- Hedges_g(d = data$d[NCD[i]], N = data$N_C[NCD[i]], 
                          design =  as.character(data$design[NCD[i]]))

g_var <- Hedges_g_var(d_var = data$d_var[NCD[i]], N = data$N_C[NCD[i]], 
                                  design = as.character(data$design[NCD[i]]))
  

#between:
d <- Cohens_d(M_C = data$mean_C[NCD[i]], M_E = data$mean_E[NCD[i]], S_C = data$var_C[NCD[i]],
         S_E = data$var_E[NCD[i]], N_C = data$N_C[NCD[i]], N_E = data$N_E[NCD[i]],
         design = as.character(data$design[NCD[i]]), type = type)
d_var <- Cohens_d_var(d = data$d[NCD[i]], N_C = data$N_C[NCD[i]], N_E = data$N_E[NCD[i]],
                                  design = as.character(data$design[NCD[i]]))

g <- Hedges_g(d = data$d[NCD[i]], N_C = data$N_C[NCD[i]], N_E = data$N_E[NCD[i]],
                          design =  as.character(data$design[NCD[i]]))

g_var <- Hedges_g_var(d_var = data$d_var[NCD[i]], N_C = data$N_C[NCD[i]], N_E = data$N_E[NCD[i]],
                                  design = as.character(data$design[NCD[i]]))


# values from Martin's data file:
mean_E = 7.6
  mean_C = 9.14
  sd1i = 2.16
  sd2i = 3.55
  N_Intervention = 24
  N_Control = 24
  d = -.5241
  d_var = .086195
  g = -.51551
  gvar = .083392
  

metafor::escalc (measure = "SMD", m1i = mean_E, m2i = mean_C, 
                 sd1i = sd1i, sd2i = sd2i, n1i = N_Intervention, n2i = N_Control)

metafor::escalc (measure = "SMD", di = d,  n1i = N_Intervention, n2i = N_Control)

compute.es::des(d= d, n.1 = N_Intervention, n.2 = N_Control, dig = 6)

# metafor::escalc seems to be reporting d_var as g_var; the g_var values
# from metafor do not match those from compute.es or from Martin's data file








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
  
writexl::write_xlsx( tempdf, path = "tempcall.xlsx", col_names = T)

bma2$summary
as.data.frame(bma2$summary)
row.names(bma2$summary)
Bayesmeta.Summary <- req(as.data.frame(bma2$summary))
Bayesmeta.Summary$statistic <-   row.names(Bayesmeta.Summary) 
Bayesmeta.Summary <- select(Bayesmeta.Summary, statistic, tau, mu, theta)
Bayesmeta.Summary



temp1 <- data.frame(a=c(1:5), b=c(6:10 ))
temp2 <- data.frame(a=c(11:15), x=c(16:20 ))
plyr::rbind.fill(temp1,temp2)
bind_rows(temp1,temp2)





getEffectSize (g = 22, 
               g_var = 11, 
#               d = as.numeric(input$d_add), 
#               d_var = as.numeric(input$d_var_add), 
#               mean_E = as.numeric(input$mean_E_add), 
#               mean_C = as.numeric(input$mean_C_add),
#               var_E = as.numeric(input$var_E_add), 
#               var_C = as.numeric(input$var_C_add), 
#               var_type = input$var_type_add, 
#               Total.N = as.numeric(input$Total.N_add), 
#               N_Intervention = as.numeric(input$N_Intervention_add), 
#               N_Control = as.numeric(input$N_Control_add), 
#               Design = input$Design_add, 
#               r = as.numeric(input$r_add),
#               reverseCode = input$reverseCode_add
)

