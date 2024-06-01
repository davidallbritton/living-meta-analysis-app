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
fma <- readRDS("/Users/David/Downloads/fma.RDS") 
MA <- readRDS("/Users/David/Downloads/MA.RDS") 
fma <- readRDS("/Users/dallbrit//Downloads/fma.RDS") 
MA <- readRDS("/Users/dallbrit//Downloads/MA.RDS") 

fmabak <- fma
df <- MA
moderator <- MA$Instruction_category
######## end of temp debugging stuff:

################ Functions ###############################################

### a little helper function to add Q-test, I^2, and tau^2 estimate info
# (from https://www.metafor-project.org/doku.php/plots:forest_plot_with_subgroups)
mlabfun <- function(text, x) {
  list(bquote(paste(.(text),
                    " (Q = ", .(fmtx(x$QE, digits=2)),
                    ", df = ", .(x$k - x$p), ", ",
                    .(fmtp(x$QEp, digits=3, pname="p", add0=TRUE, sep=TRUE, equal=TRUE)), "; ",
                    I^2, " = ", .(fmtx(x$I2, digits=1)), "%, ",
                    tau^2, " = ", .(fmtx(x$tau2, digits=2)), ")")))}



### function to create the forest plot with a categorical moderator
forestByGroup <- function(MA, moderator) {  
  # MA is the dataframe with yi=es, vi=var
  # moderator is a vector of the same length as MA, such as MA$moderatorFactor
  fma <- rma(MA$es, MA$var, slab=MA$study)
  #
  # if the moderator is not a factor, make it one
  if (!is.factor(moderator)) {
    moderator <- as.factor(as.character(moderator))
  }
  #
  # calculate some things for the height of the plot
  spaceBelow <- 2
  spaceAbove <- 2
  groupSpace <-  spaceBelow + spaceAbove     # space around each group for group summary; 2 above and 2 below
  belowGroup <- -1.5   # how far below the group data to put the group summary
  belowPlot <-  -1.8   # how far below plot to put the meta-regression summary
  topSpace  <-  2      # how much room to leave at the top for labels
  nDataPoints <- length(fma$yi)
  GroupNames <- levels(moderator)  
  nGroups <- length(levels(moderator)) 
  totalHeight <- nDataPoints + (nGroups * groupSpace) + (topSpace)
  # need the number in each group; top and bottom row for each group
  groupSizes <- table(moderator)          # number in each level of GroupNames

  # initialize values for the loop
  submodels <- list()
  groupLabelRow <- integer(nGroups)
  groupModelRow <- integer(nGroups)
  lineNum <-  spaceBelow + 1 # initialize the line of the forest plot to the row for the first ES
  rowsString <- "rows=c("    # initialize the string
  #
  for(i in 1:nGroups) {
    groupname <- GroupNames[i]
    groupsize <- groupSizes[i]
    y <- (lineNum + groupsize - 1)
    rowsString <- paste0(rowsString, lineNum, ":", y)
    if(i < nGroups) rowsString <- paste0(rowsString, ", ")
    else rowsString <- paste0(rowsString, ")")
    groupLabelRow[i] <- y + 1
    groupModelRow[i] <- lineNum + belowGroup
    # create a model for each group
    subMA <- MA[moderator == groupname,]
    subfma <- rma(subMA$es, subMA$var, slab=subMA$study)
    submodels[[i]] <- subfma
    # update for the next entry
    lineNum <- (y + groupSpace + 1)
    #
    print (i)  # debugging 
    print (groupname)  # debugging 
    print(rowsString)  # debugging 
  }
 
 # forest(fma)  # temp; debugging; actual forest call will be later *****
  forest(fma, 
         xlim=c(-16, 4.6),  
         cex=0.75, 
         ylim=c(-1, 27), 
         order=alloc, 
         rows=c(3:4,9:15,20:23),
         mlab=mlabfun("RE Model for All Studies", fma),
         psize=1, header="Study")
  
  
  
}

forestByGroup(MA, MA$Instruction_category)
