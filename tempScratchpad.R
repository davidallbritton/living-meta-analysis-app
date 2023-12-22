## R code to create MA object that contains selected data for all analyses
Variable.Factor.Names <- c("Meta-analysis.Source", "sample", "Measure", "task", "sound")
Variable.Numeric.Names <- c("IF", "db")
Design <- c("between", "within")
Publication.Year <- c(1937, 2023)
N_Intervention <- c(9, 334)
included <- c("Adam (2019)", "Ahuja (2016)", "Anderson & Fuller (2010)", "Armstrong & Chung (2000)", "Armstrong et al. (1991)", "Avila et al. (2011)", "Baker & Madell (1965)", "Cauchard et al. (2012)", "Christensen and Hansson (2018)", "Cool et al. (1994), Exp.2", "Corradine (2020)", "Dackombe (2020)", "Daoussis & McKelvie (1986)", "Dockrell & Shield (2006)", "Dove (2009)", "Doyle & Furnham (2012)", "Du et al. (2020)", "Etaugh & Michals (1975)", "Etaugh & Ptasnik (1982)", "Falcon (2017), Sample 1", "Falcon (2017), Sample 2", "Fendrick (1937)", "Fogelson (1973)", "Freeburne & Fleischer (1952)", "Furnham & Allass (1999)", "Furnham & Bradley (1997)", "Furnham & Strbac (2002)", "Furnham at al. (1999)", "Furnham et al. (1994)", "Gillis (2016)", "Goldenberg (2021)", "Halin (2016)", "Halin et al. (2014)", "Hao and Conway (2021)", "Henderson et al. (1945)", "Herring and Scott (2018)", "Hyönä & Ekholm (2016), Exp.1", "Johansson (1983)", "Johansson et al. (2012)", "Kaul et al. (2020)", "Kelly (1994)", "Kiger (1989)", "Kou et al. (2017)", "Ljung et al. (2009)", "Madsen (1987), Exp.1", "Martin et al. (1988), Exp.1", "Martin et al. (1988), Exp.2", "Martin et al. (1988), Exp.4", "Martin et al. (1988), Exp.5", "Miller & Schyb (1989)", "Miller (2014)", "Mitchell (1949)", "Mohan and Thomas (2020)", "Moreno (2020)", "Moreno and Woodruff (2021)", "Mullikin & Henk (1985)", "Murphy et al. (2018)", "Perham & Currie (2014)", "Pool et al. (2000), Exp.1", "Pool et al. (2000), Exp.2", "Quan and Kuo (2023)", "Que et al. (2020)", "Ren et al. (2019)", "Ross et al. (2021)", "Sörqvist (2010b), Exp.1a", "Sörqvist (2010b), Exp.1b", "Sörqvist et al. (2010a)", "Sörqvist et al. (2010c), Exp.1", "Sörqvist et al. (2010c), Exp.2", "Su et al. (2017), Sample 1", "Tucker & Bushman (1991)", "Vasilev et al. (2019)", "Vasilev et al. (2023), Exp.1", "Vasilev et al. (2023), Exp.2", "Vasilev et al. (2023), Exp.3", "Vasilev et al. (2023), Exp.4", "Vasilev et al. (n.d.)", "Zdorova et al. (2023), Exp.1", "Zhang et al. (2018), Exp.1")
aggregation <- 'ID'
#
Variable.Factors.selected <- list() 
Variable.Factors.selected[["Meta-analysis.Source"]] <- c(c("2023 additional studies", "Vasilev, et al. (2018)")) 
Variable.Factors.selected[["sample"]] <- c(c("adults", "children")) 
Variable.Factors.selected[["Measure"]] <- c(c("num_correct", "perc_correct", "prop_correct", "reading_score")) 
Variable.Factors.selected[["task"]] <- c(c("reading comprehension", "reading comprehension (easy font)", "reading comprehension/ select word", "reading test (composite)", "select word", "text word learning")) 
Variable.Factors.selected[["sound"]] <- c(c("music", "music+noise", "music+speech", "music+speech+noise", "noise", "noise+music", "noise+speech", "speech", "speech+music", "speech+noise")) 






# Assuming the necessary libraries are loaded
# library(dplyr)
# library(data.table)
# etc...

##  This part may need to be edited by the user unless a standard filename is used
#  Define the input file


## This part depends on shiny input values:
# record the selection values

#   Variable.Factor.Names, Variable.Numeric.Names, 
#   Design, Publication.Year, N_Intervention, included, aggregation

observeEvent(MA(), {
  # Generate the code based on current inputs
  code_for_MA <- sprintf("
# Non-reactive R code
Variable.Factor.Names <- %s
Variable.Numeric.Names <- %s
Design <- %s
Publication.Year <- c(%s, %s)
N_Intervention <- c(%s, %s)
included <- %s
aggregation <- '%s'",
                            toString(input$Variable_Factor_Names),
                            toString(input$Variable_Numeric_Names),
                            toString(input$Design),
                            input$Publication_Year[1], input$Publication_Year[2],
                            input$N_Intervention[1], input$N_Intervention[2],
                            toString(input$included),
                            input$aggregation
  )
  
  # Assign the generated code to output
  output$MAcodeOutput <- renderText({ code_for_MA })
})




# record the priors


## This part should stay the same always:
## *** but need to figure out how to handle the variable.factor and numeric.names...

createMA.nonReactive <- function(df, Variable.Factor.Names, Variable.Numeric.Names, Design, Publication.Year, N_Intervention, included, aggregation, method = "BHHR", cor = 0.5) {
  # Create subset based on chosen inclusion criteria
  df_sub <- df %>% filter(Design %in% Design,
                          Publication.Year >= Publication.Year[1], 
                          Publication.Year <= Publication.Year[2],
                          N_Intervention >= N_Intervention[1],
                          N_Intervention <= N_Intervention[2],
                          Paper.and.Exp %in% included)
  
  # Subset based on selection factors
  for (varName in Variable.Factor.Names) {
    keepValues <- df[[varName]]  # Adjust as needed
    df_sub <- df_sub[df_sub[,varName] %in% keepValues, ]
  }
  
  # Subset based on selection numerics
  for (varName in Variable.Numeric.Names) {
    df_sub <- df_sub[df_sub[,varName] >= df[[varName]][1], ]
    df_sub <- df_sub[df_sub[,varName] <= df[[varName]][2], ]
  }
  
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

# Example usage:
# MAnr <- createMAnr(df, Variable.Factor.Names, Variable.Numeric.Names, Design, Publication.Year, N_Intervention, included, aggregation)
