#----
# Load files

# default data file for initial display, before user uploads their own data file:
load(file = "2023updatedData.Rda")             # loads a dataframe called "df"

################## Constants #######################################################
printButton <- HTML('<p  style="text-align:right; font-size: 8px;"><button  onClick="window.print()">PRINT</button></p>')
r_estimate <- 0.74326344959  # Vasilev et al.'s estimate of the correlation between outcomes in a single study (for "within" designs)
thisYear <- 2023    # default for entering new data points
first_shiny_meta_paper_full <- "Wolf, V., Kühnel, A., Teckentrup, V., Koenig, J., & Kroemer, N. B. (2021). Does transcutaneous auricular vagus nerve stimulation affect vagally mediated heart rate variability? A living and interactive Bayesian meta‐analysis. Psychophysiology, 58(11), e13933."
first_shiny_meta_paper <- "Wolf, et al. (2021)"
first_shiny_meta_paper_doi <- "https://doi.org/10.1111/psyp.13933"
first_shiny_meta_paper_app <- "https://vinzentwolf.shinyapps.io/taVNSHRVmeta"
noise_meta_paper_full <- "Vasilev, M. R., Kirkby, J. A., & Angele, B. (2018). Auditory distraction during reading: A Bayesian meta-analysis of a continuing controversy. Perspectives on Psychological Science, 13(5), 567-597."
noise_meta_paper <- "Vasilev, et al. (2018)"
