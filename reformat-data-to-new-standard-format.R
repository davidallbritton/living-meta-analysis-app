# temporary file for reformatting the data file...
#    saves the reformatted data file as the data frame "df" in the file
#    "df3.Rda"
#
#  Nov. 8, 2022

# load the Vasilev data as dataframe "df"
load(file = "df2.RDa")  # data from Vasilev et al., altered for this app

dfnew <- df

# manually put in the correct experiment numbers for multi-experiment papers
dfnew[dfnew$ID == 34,]$Study.No <- 2
dfnew[dfnew$ID == 35,]$Study.No <- 4
dfnew[dfnew$ID == 36,]$Study.No <- 5
dfnew[dfnew$ID == 37,]$Study.No <- 2
dfnew[dfnew$ID == 41,]$Study.No <- 2
dfnew[dfnew$ID == 50,]$Study.No <- 2
dfnew[dfnew$ID == 52,]$Study.No <- 2
dfnew[dfnew$ID == 60,]$ES.No <- 2
dfnew$study <- sub(").*$", ")", df$study) %>% as.factor()

# give each paper a unique paper number:
dfnew$Paper.Number <- dfnew$ID
pnum <- dfnew$Paper.Number[1]
for (ir in 1:nrow(dfnew)) {
  if (dfnew$Study.No[ir] + dfnew$ES.No[ir] <= 2) {pnum <- dfnew$Paper.Number[ir]}
  dfnew$Paper.Number[ir] <- pnum
}

# Add some columns that will be required in the new data input file format:
dfnew$DOI <- "not recorded"
dfnew$Article.Title <- "not recorded"
dfnew$Begin.Selection.Numerics <- ""
dfnew$End.Selection.Numerics <- ""
dfnew$Begin.Selection.Factors <- ""
dfnew$End.Selection.Factors <- ""

# add a column for total N
dfnew <- mutate(dfnew, Total.N = ifelse(design == "between", N_E + N_C, N_E))

# reorder the columns
dfnew2 <- select(dfnew, yi, vi, ID, Paper.Number, Study.No, ES.No, cit, study,
  year,              DOI, Article.Title,  journal, design, Total.N, N_E, N_C, 
  Begin.Selection.Factors, sample, Measure, task, sound, journal, End.Selection.Factors,
  Begin.Selection.Numerics, IF, db, End.Selection.Numerics,  sound_type,
  mean_C:var_type, d:CI95_R
                 )

#   make new names for the columns:
 dfnew3 <- dfnew2
 newnames <- c("yi", "vi", "ID", "Paper.Number", "Experiment.Number", "Effect.Size.Number", "Paper.and.Exp", "Paper", 
   "Publication.Year", "DOI", "Article.Title", "Journal", "Design",  "Total.N",  "N_Intervention", "N_Control", 
   "Begin.Selection.Factors", "sample", "Measure", "task", "sound",  "End.Selection.Factors", 
   "Begin.Selection.Numerics", "IF", "db", "End.Selection.Numerics", "sound_type",
   "mean_C" ,                  "var_C"   ,                 "mean_E"      ,            
    "var_E"         ,           "var_type"          ,       "d"        ,               
    "d_var"         ,           "g"         ,               "g_var"    ,               
    "CI95_L"     ,              "CI95_R"    
                )
 names(dfnew3) <- newnames
 
 # All columns before "Begin.Selection.Factors" are required, and 
 # all after are (I think at this point) optional.
 
# save the reformatted data frame to a file:
df <- dfnew3
save(df, file = "df3.Rda")
