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
  # drop any empty subgroups so they are left out of the plot.
  # (e.g. a moderator group whose studies were all deselected in the
  #  selection tab still lingers as an empty factor level, which would
  #  otherwise make rma() fail on a subgroup with zero data points)
  moderator <- droplevels(moderator)

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
  do.call(metafor::forest, args_list)
  
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


##################### Bayesian meta-regression with one categorical moderator
###### start of buildBmrModel() function
# Fits a Bayesian meta-regression using bmr() from the bayesmeta package.
# Uses cell-means coding (~ moderator - 1) so that each regression coefficient
# is the pooled posterior effect for one level of the moderator, giving a
# per-group estimate + 95% credible interval that parallels the frequentist
# subgroup forest plot in forestByGroup().
#
# The tau prior is mapped exactly as in the non-moderator bayesmeta() model
# (see the bma() reactive in server.R) so priors are consistent across tabs.
# The chosen mu prior (mupriormean / mupriorsd) is applied to every group's
# coefficient via bmr()'s beta prior.
buildBmrModel <- function(MA, moderatorName, tauprior, scaletau, mupriormean, mupriorsd) {
  moderator <- MA[[moderatorName]]
  if (!is.factor(moderator)) moderator <- as.factor(as.character(moderator))
  moderator <- droplevels(moderator)   # drop empty subgroups (same fix as forestByGroup())

  # cell-means design matrix: one coefficient per moderator level
  X <- model.matrix(~ moderator - 1)
  colnames(X) <- levels(moderator)     # clean names to just the level labels
  betaMean <- rep(mupriormean, ncol(X))
  betaSD   <- rep(mupriorsd,   ncol(X))

  if (tauprior == "Half cauchy") {
    bmr(y = MA$es, sigma = sqrt(MA$var), labels = MA$study, X = X,
        tau.prior = function(t) dhalfcauchy(t, scale = scaletau),
        beta.prior.mean = betaMean, beta.prior.sd = betaSD)
  } else if (tauprior == "Half student t") {
    bmr(y = MA$es, sigma = sqrt(MA$var), labels = MA$study, X = X,
        tau.prior = function(t) dhalfnormal(t, scale = scaletau),
        beta.prior.mean = betaMean, beta.prior.sd = betaSD)
  } else {
    bmr(y = MA$es, sigma = sqrt(MA$var), labels = MA$study, X = X,
        tau.prior = tauprior,
        beta.prior.mean = betaMean, beta.prior.sd = betaSD)
  }
}
###### end of buildBmrModel() function


##################### Forest plot for a Bayesian meta-regression (bmr) model
###### start of forestBmrByGroup() function
# Draws a forest plot for a bmr() model that is laid out like the frequentist
# forestByGroup() plot: the individual studies are grouped WITHIN each level of
# the moderator, and each group gets a Bayesian posterior summary polygon
# (posterior mean + 95% credible interval, taken from the bmr model that was fit
# with cell-means coding, so each coefficient is that group's pooled effect).
#
#   bmr       : a model returned by buildBmrModel() (cell-means coded)
#   MA        : the data frame with es, var, study (same MA used to fit bmr)
#   moderator : the moderator vector (same length as MA), e.g. MA[[moderatorName]]
#
# Only observed study effect sizes are plotted (as in forestByGroup); no overall
# pooled polygon is drawn, because a cell-means meta-regression has a per-group
# estimate rather than a single overall effect.
#   efac      : vertical expansion factor for the CI arrow ends and the summary
#               diamonds (metafor default is 1, which is too tall for the close
#               row spacing here; 0.3 matches the frequentist forestByGroup call).
forestBmrByGroup <- function(bmr, MA, moderator, slab = MA$study, cex = 1,
                             header = "Study", efac = 0.3, ...) {
  # match the group structure used when the model was fit
  if (!is.factor(moderator)) moderator <- as.factor(as.character(moderator))
  moderator  <- droplevels(moderator)
  GroupNames <- levels(moderator)
  nGroups    <- length(GroupNames)
  groupSizes <- table(moderator)

  # order studies by group so each group's rows are contiguous
  ord      <- order(moderator)
  es       <- MA$es[ord]
  vv       <- MA$var[ord]
  slab     <- slab[ord]

  # layout constants (same as forestByGroup)
  spaceBelow <- 2
  spaceAbove <- 2
  groupSpace <- spaceBelow + spaceAbove
  belowGroup <- -1.5
  topSpace   <- 2
  nDataPoints <- length(es)
  totalHeight <- nDataPoints + (nGroups * groupSpace) + topSpace

  # assign plot rows for each group's studies, its label, and its summary polygon
  rowsVector    <- integer(0)
  groupLabelRow <- integer(nGroups)
  groupModelRow <- integer(nGroups)
  lineNum <- spaceBelow + 1
  for (i in 1:nGroups) {
    y <- lineNum + groupSizes[i] - 1
    rowsVector       <- c(rowsVector, lineNum:y)
    groupLabelRow[i] <- y + 1
    groupModelRow[i] <- lineNum + belowGroup
    lineNum <- y + groupSpace + 1
  }

  # plot the individual observed studies, grouped by moderator level
  # (addfit is not relevant here: forest.default draws no model polygon)
  metafor::forest(x = es, vi = vv, slab = slab, rows = rowsVector,
                  ylim = c(-2, totalHeight), cex = cex, header = header,
                  efac = efac, ...)

  # add the moderator-level labels (bold italic), as in forestByGroup
  op <- par(cex = cex, font = 4)
  usr <- par("usr")
  text(usr[1], groupLabelRow, pos = 4, GroupNames, xpd = TRUE)
  par(op)

  # add a Bayesian posterior summary polygon for each group, using the bmr model's
  # per-group posterior mean and 95% credible interval.
  # bmr() sanitises the coefficient names (e.g. "a+b" -> "a.b") but keeps them in
  # the same order as the moderator levels, so reference the summary columns via
  # bmr$variables[i] while labelling the polygon with the original level GroupNames[i].
  s    <- bmr$summary
  vars <- bmr$variables
  for (i in 1:nGroups) {
    metafor::addpoly(x = s["mean", vars[i]],
                     ci.lb = s["95% lower", vars[i]],
                     ci.ub = s["95% upper", vars[i]],
                     rows = groupModelRow[i],
                     mlab = paste0("Bayesian estimate: ", GroupNames[i]),
                     efac = efac, xpd = TRUE, cex = cex, ...)
  }
}
###### end of forestBmrByGroup() function


##############################################################################
################ End of Functions for meta-regression ########################

