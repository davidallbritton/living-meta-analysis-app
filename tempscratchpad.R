# NOT THE REAL FILE!!!!!!!!!!!!!!!!!


tempstuff <- readRDS("tempprevmods33.RDS")
tempstuff <- readRDS("tempprevmods.RDS")
tempstuff <- readRDS("defaultPrecalculatedModels.RDS")

tempbma2reactivex <- readRDS("bma2reactivex.RDS")
tempbma2x <- readRDS("bma2x.RDS")

identical(tempbma2reactivex, tempbma2x)
identical(as.list(tempbma2reactivex), as.list(tempbma2x))

attributes(tempbma2reactivex)
attributes(tempbma2x)

Large bayesmeta


# defaultPrecalculatedModels.RDS

identical (tempstuff[[1]][["MA"]], tempstuff[[2]][["MA"]])
identical (tempstuff[[1]]["MA"], tempstuff[[2]]["MA"])

myrvs$previousModels <- readRDS("defaultPrecalculatedModels.RDS")

tempprevmods <- myrvs$previousModels   # debugging???
saveRDS(tempprevmods, file = "tempprevmods.RDS")   # debugging???

old_bma <- checkOldModels(myrvs$previousModels, MA(), printed_bma)

checkOldModels <- function(listPrevious, MAcurrent, printed_bma_current) {
  return_bma <- FALSE
  if(length(listPrevious)) {
    for (i in 1:length(listPrevious)) {
      print("XXX"); print(identical(listPrevious[[i]]$MA, MAcurrent)); print(identical(listPrevious[[i]]$printed_bma, printed_bma_current)) # debugging
      print("XXX"); print(identical(as.data.frame(listPrevious[[i]]$MA), as.data.frame(MAcurrent))); print(identical(listPrevious[[i]]$printed_bma, printed_bma_current)) # debugging
      if(identical(as.data.frame(listPrevious[[i]]$MA), as.data.frame(MAcurrent))) if(identical(listPrevious[[i]]$printed_bma, printed_bma_current)) {
        return_bma <- listPrevious[[i]]$bma
      }
    }
  }
  if(isTruthy(return_bma)) return_bma else FALSE
}


In an R shiny app, this is how the bma() reactive is defined:
  
  bma <- metaReactive({
    req(MA())           # trigger to update bma; MA() gets updated only when "recalculate" button is pressed
    isolate({           # so that changes in priors do not trigger bma() update before "recalculate" button is pressed
      req(printed_bma())  # to make sure the meta-expansion text is up to date
      printed_bma <- as.character(printed_bma())
      ## Generate bayesmeta-object "bma" depending on tau prior chosen
      old_bma <- FALSE
      old_bma <- checkOldModels(myrvs$previousModels, MA(), printed_bma)
      if(isTruthy(old_bma)) bma <- old_bma  # retrieve previously calculated bma model
      else { #### if there is no prevously calculated bma model to retrieve, calculate a new one
        MA <- MA()
        if (..(input$tauprior) == "Half cauchy") {
          bma <- bayesmeta(y = MA$es,sigma = sqrt(MA$var), labels = MA$study, 
                           tau.prior = function(t) dhalfcauchy(t, scale = ..(input$scaletau)), 
                           mu.prior = c("mean" = ..(input$mupriormean), "sd" = ..(input$mupriorsd)))
        } else if (..(input$tauprior) == "Half student t") {
          bma <- bayesmeta(y = MA$es,sigma = sqrt(MA$var), labels = MA$study, 
                           tau.prior = function(t) dhalfnormal(t, scale = ..(input$scaletau)), 
                           mu.prior = c("mean" = ..(input$mupriormean), "sd" = ..(input$mupriorsd)))
        } else {
          bma <- bayesmeta(y = MA$es,sigma = sqrt(MA$var), labels = MA$study, 
                           tau.prior = ..(input$tauprior), 
                           mu.prior = c("mean" = ..(input$mupriormean), "sd" = ..(input$mupriorsd)))
        }
        ## store the new model 
        updateModels(MA = MA, printed_bma = printed_bma, bma = bma)
        bma
      }  ####
    })  # end of isolate()
  })   # end of bma() definition

Once bma() has been calculated, it is stored in a file using this code:
  
  tempprevmods <- myrvs$previousModels   
saveRDS(tempprevmods, file = "tempprevmods.RDS")   

The next time the app is loaded, it uploads the old values of bma() thus:
  
  myrvs$previousModels <- readRDS("tempprevmods.RDS")


Here is the problem.  The following bit of code works fine when bma() is calculated within the app, but it produces an error
message when bma() has been loaded from the file "tempprevmods.RDS"

output$evupdate <- renderPlot({
  priorposteriorlikelihood.ggplot(bma(), lowerbound = 0 - (input$mupriormean + 1) * 1.5, upperbound = 0 + (input$mupriormean + 1) * 1.5)
}, width = 800)





generatePlot <- function(bma, input) {
  priorposteriorlikelihood.ggplot(bma, lowerbound = 0 - (input$mupriormean + 1) * 1.5, upperbound = 0 + (input$mupriormean + 1) * 1.5)
}

output$evupdate <- renderPlot({
  bma_obj <- myrvs$previousModels  # Load bma from file
  generatePlot(bma_obj, input)
}, width = 800)






