#######################################################################################
################### EFFECT OF taVNS ON HRV - A BAYSEIAN META-ANALYSIS #################
#######################################################################################

################### Helper functions #################################################
#----
# Load files

#  Data file format
#
#     The data file should have one effect size per row
#     (So if a paper has 3 effect sizes there should be 3 rows for that paper;
#      If there are 2 studies/experiments in the paper and 3 effect sizes 
#      from each experiment, then there should be 6 rows for that paper)
#
#     The data file must have columns called "yi" and "vi" for the effect size
#     and effect size variance, assumed to be for Hedge's g (or call them g and g_variance?###)
# 
#     The data file must contain the following columns:
#       Study_Design
#       Publication_Year
#       study   [paper in brief citation format:  Author, year]   ###study is ref'ed in a function###
#       Paper_Number
#       Experiment_Number  [in case there are multiple experiments/studies in a paper]
#       Effect_Size_Number [in case there are multiple measurements in an experiment/study]
#       .... more??? ...
#       .... required data columns too?  [in addition to yi and vi]
#       

###### delete these 3 lines some day:
## don't load the original app's data file:   
## load(file = "df.RDa")
## load(file = "screened.RDa")

# Load the new data file
load(file = "df2.RDa")  # data from Vasilev et al., altered for this app

# Preset factor list:
# Change some columns to factors in case they were not already.  Should here
# list all the "required" columns for data files that are factors
# ### need to change this list once we standardize what an input data file contains###
df <- dplyr::mutate_at(df, vars(design, sample, cit, task, measure), as.factor)

# Variable factor list:  Follows the column labeled "Begin.Factors"
# Add a column to the end of the data file to mark the end of the variable factor list
# if it does not already exist
if(!("End.Factors" %in% colnames(df))) {df$"End.Factors" <- ""}    


#  The input data file should contain columns for Experiment_Number and Effect_Size_Number
#  for use in aggregating effects within each paper.
#  The following lines are just in case an input file does not have those columns,
#  because the paper had only one experiment and one measurement.
#
if(!("Experiment_Number" %in% colnames(df))) {df$"Experiment_Number" <- 1}    # experiment number within a paper
if(!("Effect_Size_Number" %in% colnames(df))) {df$"Effect_Size_Number" <- 1}  # effect size number within an experiment

######### delete the following two lines once the ui.R and HelperFunctions.R are changed to
######### use the new variable names above; The following 2 are needed to make things work until then
if(!("Study.No" %in% colnames(df))) {df$"Study.No" <- 1}  # experiment number within a paper
if(!("ES.No" %in% colnames(df))) {df$"ES.No" <- 1}        # effect size number within an experiment
#########

#
# copy our "cit" column to a "study" column.  cit includes "Exp 2" etc for multi-
# experiment studies, but their "study" column does not. If we later add a "study" 
# column to the data file that only lists one label (author and year) for each
# paper, then we could aggregate over each study. 
if(!("study" %in% colnames(df))) {df$study <- df$cit}
#
###

###  The original app expects yi and vi instead of g and g_var, so add those columns:
if(!("yi" %in% colnames(df))) {df$yi <- df$g}
if(!("vi" %in% colnames(df))) {df$vi <- df$g_var}


## temporarily(?) copy to the original app's name for the data file / data frame:
## Or perhaps it would be better to continue in the server
## and ui to use "df" as they did?
####### df <- gen

################## Get list of variable factors from input data file ##############
#
Variable.Factor.Names <-  colnames(select(df, Begin.Factors:End.Factors & !c(Begin.Factors, End.Factors)))


##################  Functions   ####################################################

# Generate function "priorposteriorlikelihood.ggplot"
## Plots prior, posterior and likelihood distribution
priorposteriorlikelihood.ggplot <- function(bma, lowerbound, upperbound) {
  effect <- seq(lowerbound, upperbound, length = 200)
  colors <- c("posterior" = "#D55E00", "prior" = "#009E73", "likelihood" = "#0072B2")
  devAskNewPage(ask = FALSE)
  ggplot(data = NULL, aes(x=effect,y=bma$dposterior(mu = effect))) + 
    geom_line(aes(x = effect, y = bma$likelihood(mu = effect), col = "likelihood")) +
    geom_line(aes(x = effect, y = bma$dposterior(mu = effect), col = "posterior")) + 
    geom_line(aes(x = effect, y = bma$dprior(mu = effect), col = "prior")) + 
    geom_vline(xintercept = 0, col = "gray") + 
    theme_minimal_hgrid(12) + 
    labs(x = "effect μ", y = "probability density", color = "legend") + 
    scale_color_manual(values = colors)
}

# Generate function "tauprior.ggplot"
## Plots tau prior distribution
tauprior.ggplot <- function(bma) {
  effect <- seq(0, 2, length = 200)
  devAskNewPage(ask = FALSE)
  ggplot(data = NULL, aes(x=effect,y=bma$dprior(tau = effect))) + 
    geom_line(aes(x = effect, y = bma$dprior(tau = effect))) + 
    geom_vline(xintercept = 0, col = "gray") + 
    theme_minimal_hgrid(12) + 
    labs(x = "heterogeneity τ", y = "probability density") 
}

# Generate function "robustness"
## Plots Bayes factor robustness plot for shiny app
robustness <- function(MA,SD, tauprior) {
  narrow <- bayesmeta(y = MA$es,sigma = sqrt(MA$var), labels = MA$study, 
                    tau.prior = tauprior, 
                    mu.prior = c("mean" = 0, "sd" = (SD/2)))
  default <- bayesmeta(y = MA$es,sigma = sqrt(MA$var), labels = MA$study, 
                    tau.prior = tauprior, 
                    mu.prior = c("mean" = 0, "sd" = 1.5))
  user <- bayesmeta(y = MA$es,sigma = sqrt(MA$var), labels = MA$study, 
                    tau.prior = tauprior, 
                    mu.prior = c("mean" = 0, "sd" = SD))
  wide <- bayesmeta(y = MA$es,sigma = sqrt(MA$var), labels = MA$study, 
                    tau.prior = tauprior, 
                    mu.prior = c("mean" = 0, "sd" = SD+1))
  ultrawide <- bayesmeta(y = MA$es,sigma = sqrt(MA$var), labels = MA$study, 
                         tau.prior = tauprior, 
                         mu.prior = c("mean" = 0, "sd" = SD+2))
  BFS <- c(narrow$bayesfactor[1,2], user$bayesfactor[1,2], wide$bayesfactor[1,2], ultrawide$bayesfactor[1,2])
  SDS <- c(SD/2,SD,SD+1,SD+2)
  names <- c("Narrow", "User","Wide","Ultrawide")
  deflabel <- data.frame(Ref = "Default", val = 1.5, stringsAsFactors = F)
  ggplot(data = NULL, aes(SDS,BFS)) +
    geom_hline(yintercept = 1, color = "grey", size = 0.3, alpha = .6) + 
    geom_hline(yintercept = 3, color = "grey", size = 0.3, alpha = .6) + 
    geom_hline(yintercept = 10, color = "grey", size = 0.3, alpha = .6) + 
    geom_hline(yintercept = 30, color = "grey", size = 0.3, alpha = .6) + 
    geom_vline(xintercept = 1.5, color = "#D55E00", size = 0.3, alpha = .4) +
    geom_point(aes(SDS,BFS), alpha = .75) + 
    geom_text(aes(label = round(BFS, digits = 2)), hjust = -0.75) +
    geom_point(aes(1.5, default$bayesfactor[1,2]), alpha = .75) +
    scale_x_continuous(breaks = c(SDS, 1.5), limits = c(SDS[1]-.5,SDS[4]+3)) +
    scale_y_continuous(limits = c(0,BFS[4]+10)) +
    geom_line(aes(SDS,BFS), alpha = .4) +
    geom_text(aes(label = names),vjust = -1) +
    geom_text(mapping = aes(x = val, y = 0, label = Ref, hjust = -.2, vjust = -1), data = deflabel, color = "#D55E00") +
    labs(x = "standard deviations", y = "BF01") +
    theme_cowplot(12) +
    annotate("text", label = "Moderate evidence for H0", x = SDS[4] + 2, y = 6.5) + #3-10
    annotate("text", label = "Strong evidence for H0", x = SDS[4] + 2, y = 20) + #10-30
    annotate("text", label = "Very strong evidence for H0", x = SDS[4] + 2, y = (BFS[4]+10+30)/2) + #30-100
    annotate("text", label = "Anecdotal evidence for H0", x = SDS[4] + 2, y = 2) + #1-3
    annotate("text", label = "Anecdotal evidence for H1", x = SDS[4] + 2, y = 0) #0-1
}

# Generate function "robustness2"
## Plots Bayes factor robustness plot for paper
robustness2 <- function(MA,SD, tauprior) {
  narrow <- bayesmeta(y = MA$es,sigma = sqrt(MA$var), labels = MA$study, 
                      tau.prior = tauprior, 
                      mu.prior = c("mean" = 0, "sd" = (SD/2)))
  default <- bayesmeta(y = MA$es,sigma = sqrt(MA$var), labels = MA$study, 
                    tau.prior = tauprior, 
                    mu.prior = c("mean" = 0, "sd" = SD))
  wide <- bayesmeta(y = MA$es,sigma = sqrt(MA$var), labels = MA$study, 
                    tau.prior = tauprior, 
                    mu.prior = c("mean" = 0, "sd" = SD+1))
  ultrawide <- bayesmeta(y = MA$es,sigma = sqrt(MA$var), labels = MA$study, 
                         tau.prior = tauprior, 
                         mu.prior = c("mean" = 0, "sd" = SD+2))
  BFS <- c(narrow$bayesfactor[1,2], default$bayesfactor[1,2], wide$bayesfactor[1,2], ultrawide$bayesfactor[1,2])
  SDS <- c(SD/2,SD,SD+1,SD+2)
  names <- c("Narrow", "Default","Wide","Ultrawide")
  ggplot(data = NULL, aes(SDS,BFS)) +
    geom_hline(yintercept = 1, color = "grey", size = 0.3, alpha = .6) + 
    geom_hline(yintercept = 3, color = "grey", size = 0.3, alpha = .6) + 
    geom_hline(yintercept = 10, color = "grey", size = 0.3, alpha = .6) + 
    geom_hline(yintercept = 30, color = "grey", size = 0.3, alpha = .6) + 
    geom_point(aes(SDS,BFS), alpha = .75) + 
    geom_point(aes(1.5, default$bayesfactor[1,2]), alpha = .75) +
    geom_text(aes(label = round(BFS, digits = 2)), hjust = -0.75) +
    scale_x_continuous(breaks = c(SDS, 1.5), limits = c(SDS[1]-.5,SDS[4]+3)) +
    scale_y_continuous(limits = c(-1,BFS[4]+10)) +
    geom_line(aes(SDS,BFS), alpha = .4) +
    geom_text(aes(label = names),vjust = -1, size = 6) +
    labs(x = "Standard deviations", y = "BF01") +
    theme_classic(18) +
#    ggtitle("A.") +
    annotate("text", label = "Moderate evidence for H0", x = SDS[4] + 2, y = 6.5, size = 3) + #3-10
    annotate("text", label = "Strong evidence for H0", x = SDS[4] + 2, y = 20, size = 3) + #10-30
    annotate("text", label = "Very strong evidence for H0", x = SDS[4] + 2, y = (BFS[4]+10+30)/2, size = 3) + #30-100
    annotate("text", label = "Anecdotal evidence for H0", x = SDS[4] + 2, y = 2, size = 3) + #1-3
    annotate("text", label = "Anecdotal evidence for H1", x = SDS[4] + 2, y = -0.3, size = 3) + #0-1
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
}

# Generate function "is_outlier"
## Check for outliers
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


################### Helper functions for frequentist analysis ########################

# Generate function "myMareg
## Meta analysis regression with summary, forest and funnel plot
myMareg <- function(formula = es~1,
                    var = var,
                    data,
                    addfit_forest = T) {
  model <- mareg(formula = formula,
                 var = var,
                 data = data,
                 slab = data$study)
  
  result <- list(summary(model),
                 confint(model),
                 metafor::forest.rma(x = model, showweights = T, addfit = addfit_forest,
                                     order = "obs",
                                     xlim=c(-10,8)),
                 funnel(model, xlab = "Observed outcome"))
  listnames <- c("Modelsummary", "Model-Fit", "Forestplot", "Funnelplot")
  names(result) <- listnames
  return(result)
  
}

# Generate function "myCoef"
## Coefficients table
myCoef <- function(coef){
  coef<-round(coef,digits = 3)
  DT::datatable(coef,
                rownames=c("Intercept"),
                colnames=c("b","S.E.","z","lower CI","upper CI","p"))
}

# Generate function "myFit"
## Model fit
myFit <- function(fit){
  fit<-round(fit,digits = 3)
  DT::datatable(fit)
}

# Generate function "myUni"
## Model uniqueness
myUni<-function(rand){
  uni<-as.data.frame(rand)
  uni$estimate<-round(uni$estimate,digits = 2)
  uni$ci.lb<-round(uni$ci.lb,digits = 2)
  uni$ci.ub<-round(uni$ci.ub,digits = 2)
  DT::datatable(uni,
                colnames = c("estimate","lower CI","upper CI"))
}


