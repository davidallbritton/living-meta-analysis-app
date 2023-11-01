#
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







