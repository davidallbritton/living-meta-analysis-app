# code for meta-regression tab; include in server

### plan: 1. make a new tab in the left panel to choose mediators, and
#         have a "yes/no" selector for mediation like the one for 
#         Bayesian robustness analysis
#         2. make a new results tab for meta-regression, display only if "yes"
#         3. to make the output for the meta-regression tab, calculate a new
#         frequentist model (fmaReg, a reactive like fma) that has 
#         moderators in the equation.  Should depend on MA.
#         4. put the server code for the meta-regression tab in a separate file
#         5. "source" the code in the server function in server.R

# and see this for forest plot code with subgroups:  
# https://www.metafor-project.org/doku.php/plots:forest_plot_with_subgroups


#### Code to insert into the server function for meta-regression

# create UI stuff for "moderatorSelection_ui"
#   yes/no button for doing meta-regression
#   list all the factors and numerics to choose as moderators, non selected

# calculate fmaReg using the selected moderators

# create UI stuff for "metaRegressionUI"
#   model test statistics and plots???

# switch between full forest plot (height = nrows + x) and 
# caterpillar plot (height = 60; scaled proportionlly)


# fma()  is the reactive containing the frequentist meta-analysis model without moderators
# MA()  is the reactive containing the currently subsetted data
#
# load a nonreactive copy of each of them, subsetting only to remove the duplicate
# effect sizes in the Sedlmeier paper
#

######## temp debugging stuff:
# fma <- readRDS("/Users/David/Downloads/fma.RDS") 
MA <- readRDS("/Users/David/Downloads/MA.RDS") 
# fma <- readRDS("/Users/dallbrit//Downloads/fma.RDS") 
# MA <- readRDS("/Users/dallbrit//Downloads/MA.RDS") 
#
# fmabak <- fma
# df <- MA
# cexSize <-  0.6 
#
## try with a smaller dataset
### MA <- MA[MA$Instruction_category != "Instruction_NatFreq"]
 MA <- MA[MA$Instruction_category != "Instruction_Bayes_prob"]
#
# moderator <- MA$Instruction_category
#
######## end of temp debugging stuff:

################ Functions ###############################################
source("metaRegressionFunctions.R")
################ Functions ###############################################


forestByGroup(MA=MA, MA$Instruction_category, xlab="Hedges g")
 ## additional arguments that might be useful: xlim, psize, xlab
 ## For caterpillar plot, caterpillar=TRUE (not slab=NA)
 ## You can also add any other arguments that forest() allows

forestByGroup(MA=MA, MA$Instruction_category, xlab="Hedges g", caterpillar = T)

