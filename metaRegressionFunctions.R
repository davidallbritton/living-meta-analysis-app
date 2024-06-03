# include this along with the other helper functions 

##############################################################################
################ Functions for meta-regression ###############################

############## a little helper function to add Q-test, I^2, and tau^2 estimate info
# (from https://www.metafor-project.org/doku.php/plots:forest_plot_with_subgroups)
mlabfun <- function(text, x) {
  list(bquote(paste(.(text),
                    " (Q = ", .(fmtx(x$QE, digits=2)),
                    ", df = ", .(x$k - x$p), ", ",
                    .(fmtp(x$QEp, digits=3, pname="p", add0=TRUE, sep=TRUE, equal=TRUE)), "; ",
                    I^2, " = ", .(fmtx(x$I2, digits=1)), "%, ",
                    tau^2, " = ", .(fmtx(x$tau2, digits=2)), ")")))}


##################### function to create a forest plot with a categorical moderator
###### start of forestByGroup() function
forestByGroup <- function(MA=MA, moderator, slab=MA$study, cex=1, 
                          header="Study",addpred=TRUE, caterpillar=F,
         ##                 col="black", border="black",
                          ...) {  
  ## additional arguments that might be useful: xlim, psize, xlab
  ## For caterpillar plot, use caterpillar=TRUE (not slab=NA as you would in forest() )
  ## You can also add any other arguments that forest() allows
  # MA is the dataframe with yi=es, vi=var. Note it is not a model object like x is for forest()
  # moderator is a vector of the same length as MA, such as MA$moderatorFactor
  
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

  # calculate row numbers and models for subgroups by looping over subgroups:
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
  
  # get color arguments for subgroup polygons
  argsForSubs <- list()
  # Check if col and border are in args_list
  if ("col" %in% names(args_list)) {
    argsForSubs$col <- args_list$col
  }
  if ("border" %in% names(args_list)) {
    argsForSubs$border <- args_list$border
  }

  # add models for subgroups
  for (i in 1:nGroups) {
    # Define the base arguments for addpoly
    baseArgs <- list(x=submodels[[i]],
                     row=groupModelRow[i],
                     mlab=mlabfun("RE Model for Subgroup", submodels[[i]]),
                     xpd=TRUE,
                     addpred=addpred)
    # Combine the base arguments with the additional arguments
    combinedArgs <- c(baseArgs, argsForSubs)
    # Call addpoly with the combined arguments
    do.call(addpoly, combinedArgs)
  }
  
  #### The simpler version that does not pass color arguments:
  # # add models for subgroups
  # for(i in 1:nGroups) {
  #   addpoly(submodels[[i]],
  #           row=groupModelRow[i],
  #           mlab=mlabfun("RE Model for Subgroup", submodels[[i]]),
  #           xpd = TRUE,
  #           addpred=addpred
  #           )
  # }
  ####

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


##############################################################################
################ End of Functions for meta-regression ########################

