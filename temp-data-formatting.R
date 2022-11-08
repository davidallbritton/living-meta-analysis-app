# temporary file for reformatting the data file...

# load the Vasilev data as dataframe "df"
load(file = "df2.RDa")  # data from Vasilev et al., altered for this app


dfnew <- df

dfnew[dfnew$ID == 34,]$Study.No <- 2
dfnew[dfnew$ID == 35,]$Study.No <- 4
dfnew[dfnew$ID == 36,]$Study.No <- 5
dfnew[dfnew$ID == 37,]$Study.No <- 2
dfnew[dfnew$ID == 41,]$Study.No <- 2
dfnew[dfnew$ID == 50,]$Study.No <- 2
dfnew[dfnew$ID == 52,]$Study.No <- 2
dfnew[dfnew$ID == 60,]$ES.No <- 2
dfnew$study <- sub(").*$", ")", df$study) %>% as.factor()

df <- dfnew
save(df, file = "df3.Rda")
