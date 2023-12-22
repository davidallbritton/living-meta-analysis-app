'
### nonreactive R code generated within the app for reproducibility

##############  Edit this part by hand as needed ######################
## data file name; change as needed.  It can be .xlsx, .xls, or .csv ##
input_file <- "originalData.xlsx"       # Change this to your file path
#######################################################################

## source the "HelperFunctions.R" file that is used to reformat the input data, etc.
source("HelperFunctions.R")  # Functions from this file that are used:
  # reformat.df

## load libraries
# get  needed libraries from ui.R 
# commenting out the ones that are shiny-specific
library(purrr)
library(metafor)
library(readxl)
library(writexl)
library(tools)
# library(shiny)
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
# library(shinyBS)
# library(shinycssloaders)
# library(shinythemes)
library(stringr)
library(tidyr)
library(xtable)
# library(shinyalert)
# library(shinyjs)
# library(shinymeta)

## Function to read data based on file extension
read_data <- function(file_path) {
  file_extension <- tools::file_ext(file_path)
  #
  if (file_extension %in% c("xlsx", "xls")) {
    df <- read_excel(file_path) %>% as.data.frame()
  } else if (file_extension == "csv") {
    df <- read_csv(file_path, show_col_types = FALSE) %>% as.data.frame()
  } else {
    stop("File format not supported. Must be .xlsx, .xls, or .csv.")
  }
  #
  return(df)
}

## Read and process the data from the input file
df_as_uploaded <- read_data(input_file)
newrvs <- reformat.df(df_as_uploaded)
df <- newrvs$df  # reformatted for use in the analyses

#'

### ** The above is static predefined code that does not need to be generated from the app ###


### ** This section is generated in the shiny app based on UI input values ** ###
#
# R code to create MA object that contains selected data for all analyses
Variable.Factor.Names <- c("Meta-analysis.Source", "sample", "Measure", "task", "sound")
Variable.Numeric.Names <- c("IF", "db")
Design <- c("between", "within")
Publication.Year <- c(1937, 2023)
N_Intervention <- c(9, 334)
included <- c("Adam (2019)", "Ahuja (2016)", "Anderson & Fuller (2010)", "Armstrong & Chung (2000)", "Armstrong et al. (1991)", "Avila et al. (2011)", "Baker & Madell (1965)", "Cauchard et al. (2012)", "Christensen and Hansson (2018)", "Cool et al. (1994), Exp.2", "Corradine (2020)", "Dackombe (2020)", "Daoussis & McKelvie (1986)", "Dockrell & Shield (2006)", "Dove (2009)", "Doyle & Furnham (2012)", "Du et al. (2020)", "Etaugh & Michals (1975)", "Etaugh & Ptasnik (1982)", "Falcon (2017), Sample 1", "Falcon (2017), Sample 2", "Fendrick (1937)", "Fogelson (1973)", "Freeburne & Fleischer (1952)", "Furnham & Allass (1999)", "Furnham & Bradley (1997)", "Furnham & Strbac (2002)", "Furnham at al. (1999)", "Furnham et al. (1994)", "Gillis (2016)", "Goldenberg (2021)", "Halin (2016)", "Halin et al. (2014)", "Hao and Conway (2021)", "Henderson et al. (1945)", "Herring and Scott (2018)", "Hyönä & Ekholm (2016), Exp.1", "Johansson (1983)", "Johansson et al. (2012)", "Kaul et al. (2020)", "Kelly (1994)", "Kiger (1989)", "Kou et al. (2017)", "Ljung et al. (2009)", "Madsen (1987), Exp.1", "Martin et al. (1988), Exp.1", "Martin et al. (1988), Exp.2", "Martin et al. (1988), Exp.4", "Martin et al. (1988), Exp.5", "Miller & Schyb (1989)", "Miller (2014)", "Mitchell (1949)", "Mohan and Thomas (2020)", "Moreno (2020)", "Moreno and Woodruff (2021)", "Mullikin & Henk (1985)", "Murphy et al. (2018)", "Perham & Currie (2014)", "Pool et al. (2000), Exp.1", "Pool et al. (2000), Exp.2", "Quan and Kuo (2023)", "Que et al. (2020)", "Ren et al. (2019)", "Ross et al. (2021)", "Sörqvist (2010b), Exp.1a", "Sörqvist (2010b), Exp.1b", "Sörqvist et al. (2010a)", "Sörqvist et al. (2010c), Exp.1", "Sörqvist et al. (2010c), Exp.2", "Su et al. (2017), Sample 1", "Tucker & Bushman (1991)", "Vasilev et al. (2019)", "Vasilev et al. (2023), Exp.1", "Vasilev et al. (2023), Exp.2", "Vasilev et al. (2023), Exp.3", "Vasilev et al. (2023), Exp.4", "Vasilev et al. (n.d.)", "Zdorova et al. (2023), Exp.1", "Zhang et al. (2018), Exp.1")
aggregation <- 'ID'


## This part should stay the same always:
## *** but need to figure out how to handle the variable.factor and numeric.names...


## function to create MA data object for all analyses
createMA.nonReactive <- function(df, Variable.Factor.Names, Variable.Numeric.Names, 
                                 Variable.Factors.selected, Variable.Numerics.selected,
                                 Design.include, Publication.Year.include, N_Intervention.include, 
                                 included, aggregation, method = "BHHR", cor = 0.5) {
  # Create subset based on chosen inclusion criteria
  df_sub <- df %>% filter(Design %in% Design.include,
                          Publication.Year >= Publication.Year.include[1], 
                          Publication.Year <= Publication.Year.include[2],
                          N_Intervention >= N_Intervention.include[1],
                          N_Intervention <= N_Intervention.include[2],
                          Paper.and.Exp %in% included)
  
  # # Subset based on selection factors
  # for (varName in Variable.Factor.Names) {
  #   keepValues <- df[[varName]]  # Adjust as needed
  #   df_sub <- df_sub[df_sub[,varName] %in% keepValues, ]
  # }
  #
  # # Subset based on selection numerics
  # for (varName in Variable.Numeric.Names) {
  #   df_sub <- df_sub[df_sub[,varName] >= df[[varName]][1], ]
  #   df_sub <- df_sub[df_sub[,varName] <= df[[varName]][2], ]
  # }

  # Replace ID with Paper.Number if aggregating over papers
  if (aggregation == "Papers") {
    df_sub$ID <- df_sub$Paper.Number
    df_sub$study <- df_sub$Paper
  }

  # Aggregate effect sizes
  aggES <- agg(id = ID,
               es = yi,
               var = vi,
               data = df_sub,
               cor = cor,
               method = method)

  # Merging aggregated ES with original dataframe
  MAnr <- merge(x = aggES, y = df_sub, by.x = "id", by.y = "ID")
  MAnr <- unique(setDT(MAnr)[sort.list(id)], by = "id")
  MAnr <- with(MAnr, MAnr[order(MAnr$es)])

  return(MAnr)
}

## Create MA, using the inputs recorded from the shiny app UI
MA <- createMA.nonReactive(df, Variable.Factor.Names, Variable.Numeric.Names, Design, Publication.Year, N_Intervention, included, aggregation)


