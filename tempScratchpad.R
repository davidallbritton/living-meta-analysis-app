

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

createMAnr <- function(df, Variable.Factor.Names, Variable.Numeric.Names, Design, Publication.Year, N_Intervention, included, aggregation, method = "BHHR", cor = 0.5) {
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
