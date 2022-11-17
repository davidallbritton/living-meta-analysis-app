#######################################################################################
################### A Universal Tool for BAYSEIAN META-ANALYSIS #######################
#######################################################################################

###################  Shiny App v.0.1 2022.11.10 UI ####################################
# revised for Vasilev et al. data 2022  

# Load required packages and source helper functions #----
library(shiny)
library(bayesmeta)
library(cowplot)
library(dplyr)
library(DT)
library(data.table)
library(esc)
library(ggplot2)
library(MAd)
library(readr)
library(R.rsp)
library(shinyBS)
library(shinycssloaders)
library(shinythemes)
library(stringr)
library(tidyr)
library(xtable)
source("HelperFunctions.R")
#----

# Define UI
ui <- fluidPage(theme = shinytheme("cosmo"),
                titlePanel(title = div("A Universal Tool for BAYSEIAN META-ANALYSIS"),
                           windowTitle = "A Universal Tool for BAYSEIAN META-ANALYSIS"),
                sidebarLayout(
                  sidebarPanel(fluidRow(
                  
                    
                    tabPanel("Upload Data", 
                             br(),
                             fileInput("infile1", "Upload your data file")
                    ),
                    
                    submitButton("Re-Calculate Meta-Analysis", icon("sync")),
                    
                    
                    
                    
                    tabsetPanel(
                      tabPanel("Study criteria",    ##### ** this tabPanel needs to be created in server.R ###
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
                               
                              
                               checkboxGroupInput(inputId = "included", label = p("Include/exclude specific studies",style="color:#333333",
                                                                          tags$style(type = "text/css", "#q9 {vertical-align: top;}"),
                                                                          bsButton("q9", label = "", icon = icon("info"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")), 
                                                  choices = levels(df$Paper.and.Exp), selected = levels(df$Paper.and.Exp)),
                                bsPopover(id="q9", title = "Include/exclude specific studies.",
                                    content = paste0("<p>Exclude specific studies by removing the tick mark.",
                                                     "<p>This selection is hierarchically below the other inclusion/exclusion criteria.",
                                                     "<p>If a study is excluded by one of the selected criteria above, this study will not be included in the analysis, even though it is still ticked here.",
                                                     "<p>However, if a study is selected for inclusion by the criteria above, but is unticked here, the study will not be included."),
                                   placement = "right", 
                                   trigger = "click",
                                   options = list(container = "body"))
                               ),
  
                      tabPanel("Prior specifications",
                               br(), numericInput(inputId = "mupriormean", label = p("µ prior mean",style="color:#333333",
                                                                               tags$style(type = "text/css", "#q16 {vertical-align: top;}"),
                                                                               bsButton("q16", label = "", icon = icon("info"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),   
                                            value = 0, step = 0.1),
                                        bsPopover(id="q16", title = "µ prior mean.",
                                            content = paste0("<p>Set the mean of your µ prior (effect).",
                                                             "<p>Note that the results and their interpretability are drastically influenced by prior choices.",
                                                             "<p>Default: 0"),
                                            placement = "right", 
                                            trigger = "click",
                                            options = list(container = "body")),
                               numericInput(inputId = "mupriorsd", label = p("µ prior standard deviation", style="color:#333333",
                                                                             tags$style(type = "text/css", "#q17 {vertical-align: top;}"),
                                                                             bsButton("q17", label = "", icon = icon("info"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),    
                                            value = 1.5, step = 0.1, min = 0),
                                        bsPopover(id="q17", title = "µ prior standard deviation.",
                                            content = paste0("<p>Set the standard deviation of your µ prior (effect).",
                                                             "<p>Note that the results and their interpretability are drastically influenced by prior choices.",
                                                             "<p>Default: 1.5"),
                                            placement = "right", 
                                            trigger = "click",
                                            options = list(container = "body")),
                               radioButtons(inputId = "robust", label = p("µ Bayes Factor robustness check", style="color:#333333",
                                                                          tags$style(type = "text/css", "#q18 {vertical-align: top;}"),
                                                                          bsButton("q18", label = "", icon = icon("info"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),    
                                            choices = c(No = "No", Yes = "Yes")),
                                        bsPopover(id="q18", title = "µ Bayes Factor robustness check.",
                                            content = paste0("<p>Bayes Factors over a variety of prior standard deviations will be plotted.",
                                                             "<p>Note that selecting Yes will lead to an increase in computation time and that the plot will only be computed if priors for τ and μ are proper.",
                                                             "<p>Default: No."),
                                            placement = "right", 
                                            trigger = "click",
                                            options = list(container = "body")),
                               radioButtons(inputId = "tauprior", label = p("τ prior", style="color:#333333",
                                                                            tags$style(type = "text/css", "#q19 {vertical-align: top;}"),
                                                                            bsButton("q19", label = "", icon = icon("info"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),    
                                            choices = c("Half cauchy", "Half student t","uniform", "sqrt", "Jeffreys", "BergerDeely", "conventional", "DuMouchel", "shrinkage", "I2")),
                                        bsPopover(id="q19", title = "τ prior.",
                                            content = paste0("<p>Choose your τ prior.",
                                                             "<p>Note that the results and their interpretability are drastically influenced by prior choices.",
                                                             "<p>Default: Half cauchy."),
                                            placement = "right", 
                                            trigger = "click",
                                            options = list(container = "body")),
                               numericInput(inputId="scaletau", label = p("τ prior scale (for half cauchy or half student t)",style="color:#333333",
                                                                          tags$style(type = "text/css", "#q20 {vertical-align: top;}"),
                                                                          bsButton("q20", label = "", icon = icon("info"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),    
                                            value=0.5, step=0.05),
                               bsPopover(id="q20", title = "τ prior scale.",
                                         content = paste0("<p>Set the scale of your τ prior (if a half cauchy or a half student t prior is selected).",
                                                          "<p>Note that the results and their interpretability are drastically influenced by prior choices.",
                                                          "<p>Default: 0.5"),
                                         placement = "right", 
                                         trigger = "click",
                                         options = list(container = "body")),
                               a("Further information on choosing an appropriate τ prior.", href="https://cran.r-project.org/web/packages/bayesmeta/bayesmeta.pdf", target = "_blank")),
                      hr()
                    ))),
                  mainPanel(
                    fluidRow(
                      tabsetPanel(
                        tabPanel("Explanation", br(),
                                 h3("Welcome to the interactive Bayesian meta-analysis tool [fill in some info here!]"), br(),
                                 h4("Purpose:"),
                                 p("[something can go here]."), br(),
                                 h4("Explanation:"),
                                 p("[something can go here]"), br(),
                                 h4("Paper:"),
                                 ("This app accompanies the following "),
                                 a("paper", href="https://doi.org/", target = "_blank"), 
                                 h4("Code and data:"),
                                 ("This app's R Code and sample dataset can be found "),
                                 a("here", href="put github location here some day!!", target = "_blank"),
                                 ("on GitHub."), br(), br(),
                                 h4("Adding new results:"),
                                 p("Someday you will be able to upload your own data! ****"), br(), br(),
                                 h4("Contact:"),
                                 p("The app is maintained by [someone goes here!]"),
                                 p("Contact/Visit us:"),
                                   a(shiny::actionButton(inputId = "email1", 
                                                         label = "Mail", 
                                                         icon = icon("envelope", lib = "font-awesome")),
                                     href="mailto:dallbrit@depaul.edu"),
                                   a(shiny::actionButton(inputId = "website", 
                                                         label = "Web", 
                                                         icon = icon("globe", lib = "font-awesome")),
                                     href="http://www.depaul.edu")
              
                                 ),
                        tabPanel("Included Studies", br(),
                          h4('This table lists all the studies included by the current selected criteria (updated only when "Re-Calculate Meta-Analysis" button is pressed)'),
                          textOutput("warning"), br(),
                          DT::dataTableOutput("studies") %>% withSpinner(type = 6, color = "#3498DB"), br()
                        ),
                        tabPanel("Outlier check", br(),
                                 h4("Boxplot graph:"), plotOutput("boxplot") %>% withSpinner(type = 6, color = "#3498DB")),
                        tabPanel("Forest plot", br(),
                                 h4("Forest plot with 95% credible intervals:"),
                                 plotOutput("forest") %>% withSpinner(type = 6, color = "#3498DB")),
                        tabPanel("Funnel plot", br(),
                                 h4("Funnel plot to assess publication bias:"),
                                 plotOutput("funnel")  %>% withSpinner(type = 6, color = "#3498DB")),
                        tabPanel("Statistics", br(),
                                 h4("Parameters:"),
                                 p("τ (tau): posterior distribution of heterogeneity."),
                                 p("μ (mu): posterior distribution of effect."),
                                 p("θ (theta): 'predictive distribution, that expresses the posterior knowledge about a future observation, i.e., an additional draw θk+1 from the underlying population of studies.' (Röver, 2020, p. 16)."),
                                 ("Statistics are calculated/estimated using the bayesmeta package "), 
                                 a("(Röver, 2020).", href="http://dx.doi.org/10.18637/jss.v093.i06", target = "_blank"),
                                 ("The corresponding github page can be found "),
                                 a("here.", href = "https://github.com/cran/bayesmeta", target = "_blank"), br(),
                                 h4("Bayes factors:"), verbatimTextOutput("bf") %>% withSpinner(type = 6, color = "#3498DB"),
                                 p("Bayes factors are only computed if the priors for τ and μ are proper."), br(),
                                 h4("Marginal posterior summary:"), verbatimTextOutput("summary") %>% withSpinner(type = 6, color = "#3498DB"), br(),
                                 h4("Maximum-likelihood:"), verbatimTextOutput("ML") %>% withSpinner(type = 6, color = "#3498DB"), br(),
                                 h4("Joint maximum-a-posteriori:"), verbatimTextOutput("MAP") %>% withSpinner(type = 6, color = "#3498DB")),
                        tabPanel("Additional plots", br(),
                                 h4("Joint posterior density of heterogeneity τ and effect μ:"), plotOutput("joint") %>%  withSpinner(type = 6, color = "#3498DB"),
                                 p("Darker shading corresponds to higher probability density."),
                                 p("Red lines indicate (approximate) 2-dimensional credible regions,"),
                                 p("green lines show marginal posterior medians and 95% credible intervals,"),
                                 p("blue lines show conditional posterior mean effect as a function of the heterogeneity along with a 95% interval."),
                                 p("Red cross (+): posterior mode"),
                                 p("Pink cross (x): ML estimate"), br(),
                                 h4("Prior, posterior, & likelihood:"), plotOutput("evupdate") %>% withSpinner(type = 6, color = "#3498DB"), br(),
                                 h4("τ prior distribution:"), plotOutput("taupriorplot") %>% withSpinner(type = 6, color = "#3498DB")),
                        tabPanel("Bayes factor robustness check", br(),
                                 h4("Bayes Factors over a variety of prior standard deviations:"),
                                 p("Will only be computed if 'Yes' is selected for 'µ Bayes Factor robustness check' and the priors for τ and μ are proper."),br(),
                                 conditionalPanel(condition = "input.tauprior == 'uniform' | 
                                                               input.tauprior == 'sqrt' |
                                                               input.tauprior == 'Jeffreys' |
                                                               input.tauprior == 'BergerDeely' | 
                                                               input.tauprior == 'conventional' | 
                                                               input.tauprior == 'DuMouchel' | 
                                                               input.tauprior == 'shrinkage' | 
                                                               input.tauprior == 'I2'",
                                                  textOutput("warning2")),
                                 conditionalPanel(condition = "input.robust == 'Yes'", 
                                                  plotOutput("robustplot") %>% withSpinner(type = 6, color = "#3498DB"),
                                 p("Default: 1.5 (orange horizontal line)"),
                                 p("Narrow: user selected standard deviation / 2"),
                                 p("User: user selected standard deviation"),
                                 p("Wide: user selected standard deviation + 1"),
                                 p("Ultrawide: user selected standard deviation + 2"),
                                 p("Interpretations of Bayes factors are based on Jeffreys (1961) with slight modifications by Lee and Wagenmakers (2013) and should be considered with caution.")))
                          )
                        )
                      )
                    )
                )                               
                
