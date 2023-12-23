
source("nonReactiveVersion_part_1.R")  ## just for this temp file

#########  all this can probably be a literal string

## Create a list called "output" to make it easier to adapt the shiny code
output <- list()

######### Create the plots etc. from the results panels ####################

# Outliers panel
create_boxplot <- function(MA) {
  MAo <- MA %>% tibble::rownames_to_column(var = "outlier") %>% mutate(is_outlier=ifelse(is_outlier(es), es, as.numeric(NA)))
  MAo$study[which(is.na(MAo$is_outlier))] <- as.numeric(NA)
  ggplot(MAo, aes(x = factor(0), es)) +
    geom_boxplot(outlier.size = 3.5, outlier.colour = "#D55E00", outlier.shape = 18, fill = "lightgrey") +
    geom_text(aes(label=study),na.rm = T, nudge_y = 0.02, nudge_x = 0.05) +
    stat_boxplot(geom="errorbar", width = 0.05) +
    scale_x_discrete(breaks = NULL) +
    xlab(NULL) + ylab("Hedges' g") +
    theme_minimal_hgrid(12)
}
#
output$boxplot <- create_boxplot(MA)
  

########## frequentist analyses

# Create model for frequentist meta-analysis
fma <- rma(MA$es, MA$var, slab=MA$study)
  
### Frequentist Forest Plot
create_freq_forest <- function(fma) {
  model <- fma 
  # Increase bottom margin to make space for the text
  par(mar = c(5, 4, 4, 2) + 0.1)  # Adjust the bottom margin (the first value)
  # Generate the forest plot
  plot <- metafor::forest.rma(x = model, showweights = TRUE, addfit = TRUE,
                              order = "obs", xlab = "Hedges' g", 
                              addpred = TRUE, 
                              efac = 0,
                              col = "red",
                              border = "red")
  # Add Cochran's Q, its p-value, and I² statistic as text
  # Position the text below the plot
  mtext(side = 1, line = 4, 
        text = paste0("Cochran's Q = ", round(model$QE, 2), 
                      " (p = ",  format(round(model$QEp, 4), nsmall = 4), ")\n",
                      "I² = ", round(model$I2, 2), "%"),
        adj = 0, cex = 0.8)
  # Return the plot
  plot
}
#
output$freq_forest <- create_freq_forest(fma)
output$freq_forest

# Funnel plot (frequentist)
output$freq_funnel <- funnel(fma, xlab = "Observed outcome")
output$freq_funnel


######### bayesian analyses:

# Forest Plot panel
output$forest <-  forestplot.bayesmeta(bma, xlab = "Hedges' g")
output$forest

# Funnel Plot panel
output$funnel <-  funnel.bayesmeta(bma, main = "")
output$funnel

# Statistics panel
output$statistics_panel <-  capture.output({
  cat("Statistics for Bayesian analysis")
  cat("\n\n")
  cat("Bayes Factors:")
  cat("\n")
  print(bma$bayesfactor[1,])
  cat("\n\n")
  cat("Marginal posterior summary:")
  cat("\n")
  print( bma$summary)
  cat("\n")
  cat("Maximum-likelihood:")
  cat("\n")
  print(bma$ML)
  cat("\n\n")
  cat("Joint maximum a-posteriori:")
  cat("\n")
  print( bma$MAP[1,])
})
cat(output$statistics_panel, sep = "\n")


# Additional plots panel
output$evupdate <- priorposteriorlikelihood.ggplot(bma, lowerbound = 0 - (mupriormean + 1) * 1.5, upperbound = 0 + (mupriormean + 1) * 1.5)
output$taupriorplot <-   tauprior.ggplot(bma)
#
print("Prior, posterior, & likelihood")
output$evupdate 
print("Joint posterior density")
output$joint <- plot.bayesmeta(bma, which=2, main = "Joint posterior density of heterogeneity Tau and effect mu")
print("Tau prior distribution")
output$taupriorplot




## *** do this last
# Bayes factor robustness plot panel
output$warning2 <- renderPrint({
  print("WARNING: Plot will not be computed, because an improper τ prior was chosen. Proper τ priors are 'Half student t' and 'Half cauchy'.")
})
#
output$robustplot <- renderPlot({
  MA <- MA()  #trigger recalculation
  MA_nofactors <- as.data.frame(MA) 
  MA_nofactors <-  MA_nofactors %>% mutate_if(is.factor, as.character)
  robust <- input$robust
  tauprior <- input$tauprior
  mupriorsd <- input$mupriorsd
  scaletau <- input$scaletau
  old_plot <- checkOldPlots(myrvs$previousPlots, MA=MA_nofactors, tauprior=tauprior, mupriorsd=mupriorsd, scaletau=scaletau, robust=robust)
  if(isTruthy(old_plot)) robustggplot <- old_plot  # retrieve previously calculated plot
  else {
    robustggplot <- NULL
    if (robust == "Yes" & tauprior == "Half cauchy" & isTruthy(myrvs$triggerBmaRobust)) {
      robustggplot <- robustness(MA,SD = mupriorsd, tauprior = function(t) dhalfcauchy(t, scale = scaletau))
    } else if (robust == "Yes" & tauprior == "Half student t" & isTruthy(myrvs$triggerBmaRobust)) {
      robustggplot <- robustness(MA,SD = mupriorsd, tauprior = function(t) dhalfnormal(t, scale = scaletau))
    }
    ## store the new plot in the list of old plots; this function only has a side effect, no return value
    if (isTruthy(robustggplot)) {
      updated <- updatePlots(MA=MA_nofactors, tauprior=tauprior, mupriorsd=mupriorsd, scaletau=scaletau, robust=robust, robustggplot=robustggplot)
    }
  }
  robustggplot
}, width = 800)

