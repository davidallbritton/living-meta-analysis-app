

sliderInput(inputId = varName, label = p(varName ,style="color:#333333"), 
            min = min(df[,varName]), max = max(df$Publication.Year), value = c(min(df$Publication.Year), max(df$Publication.Year)), step = 1, sep = "", ticks = F),


varName <- "IF"  #for debugging only
sliderInput(inputId = varName, label = p(varName ,style="color:#333333"), 
            min = min(df[,varName], na.rm = T), max = max(df[,varName], na.rm = T), value = c(min(df[,varName], na.rm = T), max(df[,varName], na.rm = T)), ticks = F),
## loop over the variable numeric columns
lapply(Variable.Numeric.Names, function(varName) {                             
  checkboxGroupInput(inputId = varName, label = p(varName,style="color:#333333"), 
                     choices = levels(df[,varName]), selected = levels(df[,varName]))
}),

if(!is.na(na.warning[varName])) p(na.warning[varName])

################## Get list of numeric selection variables from input data file ##############
Variable.Numeric.Names <-  colnames(select(df, Begin.Selection.Numerics:End.Selection.Numerics & !c(Begin.Selection.Numerics, End.Selection.Numerics)))


tabPanel("Upload Data", 
         br(),
         fileInput("infile1", "Upload your data file")
),


