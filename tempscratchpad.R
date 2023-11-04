# NOT THE REAL FILE!!!!!!!!!!!!!!!!!

myrvs$previousModels <- data.frame()

tempprevmods <- myrvs$previousModels   # debugging
saveRDS(tempprevmods, file = "tempprevmods.RDS")   # debugging
tempMA <- MA()        # debugging
saveRDS(tempMA, file = "tempMA.RDS")   # debugging
tempprinted_bma <- printed_bma()        # debugging
saveRDS(tempprinted_bma, file = "tempprinted_bma.RDS")   # debugging

prev <- readRDS("tempprevmods.RDS")
ma1 <- readRDS("tempMA.RDS")
bp1 <- readRDS("tempprinted_bma.RDS")
ma1 <- readRDS("tempMA.RDS")
oldbma <- readRDS("old_bma.RDS")


identical(prev[[1]]$MA, prev[[2]]$MA)
identical(prev[[1]]$printed_bma, prev[[2]]$printed_bma)

identical(prev[[1]]$MA, ma1)
identical(prev[[1]]$printed_bma, bp1)



# for assigning new models, within the bma calculation block:
#  myrvs$previousModels[1] <- list(MA = MA(), printed_bma = printed_bma(), bma = bma)



    # get the data and bayesmeta parameters
    MA <- MA()
    printed_bma <- printed_bma()
    # check to see if they are identical to any of the previously recorded ones
    
    # if they are, copy bma() from the stored one that goes with them and don't
    # calculate bma() again.

    # if they are not identical, then calculate bma() as usual
    # and add the three things to the "previousbmas" list

# function for checking old models
    checkOldModels <- function(dataframewitholdmodels)
    if (nrow(dataframewitholdmodels)) {
      #check against all rows of myrvs$previousModels
      # if match, return that bma
      # else return false
    }


### for debugging
##  maybe this should be a function, and it should be called in the bma() section
##  instead of being a separate observe() of the recalculate button.
# function: get_previous_bma (MA, printed_bma, previous_models)
#   check whether any old ones are identical to both. if so, return old bma()
#   if not, return FALSE
# in the bma() section, do this:
#   isNewOne <- get_previous_bma(MA(), printed_bma())
#   if (isNewOne) bma <- isNewOne
#   else {the rest of the usual stuff to assign a value to bma}
#   bma
#

# create a shinymeta expansion of the reactive bma()
printed_bma <- reactive(
  expandChain(bma())
)



a <- list(x=c(1:5), y="asdfas")










tempplot <- readRDS("robustplot1.RDS")
tempplot <- readRDS("robustplot1_allstudies_default_priors.RDS")

print(tempplot)

tempthing <- readRDS("MA1.RDS")
tempthing2 <- readRDS("MA_2023_updated_allStudies_defaultPriors.RDS")

identical(tempthing2, tempthing)

identical (1, 1.0)


bma_2023_updated_allStudies_defaultPriors.RDS
bma_2023_allstudies_uniformPrior.RDS
MA_2023_updated_allStudies_defaultPriors.RDS
MA_2023_allstudies_uniformPrior.RDS


b1 <- readRDS("bma_2023_updated_allStudies_defaultPriors.RDS")
b2 <- readRDS("bma_2023_allstudies_uniformPrior.RDS")
m1 <- readRDS("bma_2023_updated_allStudies_defaultPriors.RDS")
m2 <- readRDS("bma_2023_allstudies_uniformPrior.RDS")



b1 <- readRDS("bma_2023_updated_allStudies_defaultPriors.RDS")
b2 <- readRDS("bma_2023_allstudies_uniformPrior.RDS")
m1 <- readRDS("MA_2023_updated_allStudies_defaultPriors.RDS")
m2 <- readRDS("MA_2023_allstudies_uniformPrior.RDS")

#MA1a.RDS

identical(m1,m2)
identical(b1,b2)


b1 <- readRDS("bma1.3.RDS")
b2 <- readRDS("bma1.3u.RDS")
m1 <- readRDS("MA1.3.RDS")
m2 <- readRDS("MA1.3u.RDS")
c1 <- readRDS("bc1.3.RDS")
c2 <- readRDS("bc1.3u.RDS")

identical(b1,b2)
identical(m1,m2)
identical(c1,c2)
## would need a shinymeta expansion of BayesmetaCall instead







