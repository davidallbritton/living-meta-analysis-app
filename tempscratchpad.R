# NOT THE REAL FILE!!!!!!!!!!!!!!!!!


tempstuff <- readRDS("tempprevmods33.RDS")
tempstuff <- readRDS("tempprevmods.RDS")

identical (tempstuff[[1]][["MA"]], tempstuff[[2]][["MA"]])
identical (tempstuff[[1]]["MA"], tempstuff[[2]]["MA"])

