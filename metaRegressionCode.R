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
MA <- MA[MA$Instruction_category != "Instruction_NatFreq"]
MA <- MA[MA$Instruction_category != "Instruction_Bayes_prob"]
#
# moderator <- MA$Instruction_category
#
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


###### start of forestByGroup() function
### function to create a forest plot with a categorical moderator
forestByGroup <- function(x=MA, moderator, slab=MA$study, cex=0.6, header="Study",addpred=TRUE, caterpillar=F, ...) {  
  ## additional arguments that might be useful: xlim, psize, xlab
  ## For caterpillar plot, caterpillar=TRUE (not slab=NA)
  ## You can also add any other arguments that forest() allows
  #
  # MA is the dataframe with yi=es, vi=var
  # moderator is a vector of the same length as MA, such as MA$moderatorFactor
  #
  MA <- x  # used x as argument name for consistency with forest()
  #
  fma <- rma(MA$es, MA$var, slab=slab)
  #
  # if the moderator is not a factor, make it one
  if (!is.factor(moderator)) {
    moderator <- as.factor(as.character(moderator))
  }
  
  # calculate some things for the height of the plot
  #
  ### Things you might want to change:
  spaceBelow <- 2
  spaceAbove <- 2
  groupSpace <-  spaceBelow + spaceAbove     # space around each group for group summary; 2 above and 2 below
  belowGroup <- -1.5   # how far below the group data to put the group summary
  belowPlot <-  -2.5     # how far below plot to put the meta-regression summary
  topSpace  <-  2      # how much room to leave at the top for labels
  #
  ### Things you probably do not want to change:
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
  rowsString <- "c("    # initialize the string
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
    subslab <- slab[moderator == groupname]
    subfma <- rma(subMA$es, subMA$var, slab=subslab)
    submodels[[i]] <- subfma
    # update for the next entry
    lineNum <- (y + groupSpace + 1)
  }
  
  rowsVector <- eval(parse(text = rowsString))
 
  # Define the list of arguments for forest()
  args_list <- list(...)
  args_list$x <- fma
  args_list$cex <- cex
  args_list$ylim <- c(-2, totalHeight)
  args_list$order <- moderator
  args_list$rows <- rowsVector
  args_list$mlab <- mlabfun("RE Model for All Studies", fma)
  args_list$header <- header
  args_list$addpred <- addpred
  if(caterpillar) args_list$slab <- NA

  # Call the forest function with the dynamically constructed argument list
  do.call(forest, args_list)
  
  ### set font expansion factor (as in forest() above) and use a bold font
  op <- par(cex=cex, font=2)
  
  ### switch to bold italic font
  par(font=4)
  ### add text for the subgroups
  usr <- par("usr")
  text(usr[1], groupLabelRow, pos=4, GroupNames, xpd=TRUE)
  
  # restore original plot parameters
  par(op)
  #  dev.off()  resets plot parameters  
  
  # add models for subgroups
  for(i in 1:nGroups) {
    addpoly(submodels[[i]], 
            row=groupModelRow[i], 
            mlab=mlabfun("RE Model for Subgroup", submodels[[i]]),
            xpd = TRUE,
            addpred=addpred
            )
  }
  
  # add model testing moderator
  fmaMod <- rma(MA$es, MA$var, mods = ~ moderator)
  #
  ### add text for the test of subgroup differences
  text(usr[1], belowPlot, pos=4, cex=cex, xpd=TRUE,
       bquote(paste("Test for Subgroup Differences: ", 
                    Q[M], " = ", .(fmtx(fmaMod$QM, digits=2)),
                    ", df = ", .(fmaMod$p - 1), ", ",
                    .(fmtp(fmaMod$QMp, digits=2, pname="p", add0=TRUE, sep=TRUE, equal=TRUE)))))
}
###### end of forestByGroup() function





forestByGroup(x=MA, MA$Instruction_category, xlab="Hedges g")
 ## additional arguments that might be useful: xlim, psize, xlab
 ## For caterpillar plot, caterpillar=TRUE (not slab=NA)
 ## You can also add any other arguments that forest() allows

