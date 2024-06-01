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
fma <- readRDS("/Users/David/Downloads/fma.RDS") 
MA <- readRDS("/Users/David/Downloads/MA.RDS") 
fma <- readRDS("/Users/dallbrit//Downloads/fma.RDS") 
MA <- readRDS("/Users/dallbrit//Downloads/MA.RDS") 

fmabak <- fma
df <- MA
moderator <- MA$Instruction_category

forestByGroup <- function(MA, moderator) {  
  # MA is the dataframe with yi=es, vi=var
  # moderator is a vector of the same length as MA, such as MA$moderatorFactor
  fma <- rma(MA$es, MA$var, slab=MA$study)
  forest(fma)  # temp; debugging; actual forest call will be later *****
  #
  # if the moderator is not a factor, make it one
  if (!is.factor(moderator)) {
    moderator <- as.factor(as.character(moderator))
  }
  #
  # calculate some things for the height of the plot
  groupSpace <-  4     # space around each group for group summary; 2 above and 2 below
  spaceBelow <- groupSpace / 2
  spaceAbove <- groupSpace / 2
  belowGroup <- -1.5   # how far below the group data to put the group summary
  belowPlot <-  -1.8   # how far below plot to put the meta-regression summary
  topSpace  <-  2      # how much room to leave at the top for labels
  nDataPoints <- length(fma$yi)
  GroupNames <- levels(moderator)  
  nGroups <- length(levels(moderator)) 
  totalHeight <- nDataPoints + (nGroups * groupSpace) + (topSpace)
  # need the number in each group; top and bottom row for each group
  groupSizes <- table(moderator)          # number in each level of GroupNames
    
  lineNum <- 0  # initialize the line of the forest plot to zero
  groupRowsPlot <- integer(nGroups)
  for (groupname in levels(moderator)) {
    print (groupname)
    lineNum <- lineNum + spaceBelow
    gSize <- groupSizes[groupname]
  }
  
}

forestByGroup(MA, MA$Instruction_category)
