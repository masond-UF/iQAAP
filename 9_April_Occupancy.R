# 9 April 2020—Lab 13 "Occupancy ####
library(unmarked)
library(MuMIn)
# Notes from class discussion ####
# keep covariates constant
# don't add parameters to probability of detection, just occupancy, 
# no additive or interactive models (replace)—keep it like practice
# can't use back transform with multiple parameters. figure will give you
# predictions. the graph is predicted probability (trident)
# logit(psi) = B0, plot coefficients coming from the best model 
# (no transformations). use predict function (logstic and multiple reg (GLM) lab)
# to transform the estimates (needs to be a logit model rather than linear which
# you can do with the predict function)
# temperature is single season averages for winter
# question 1 keep winter t as probability for detection, 
# use other covariates for occupancy
# wenyi wasn't telling R what the variable is (type = State)
# new data needs to be a dataframe
# scale and center distance to main road
# if we want to determine odds ratio of estimate, we have to back transform
# the null model (for odds ratio take exponent of variable you are intersted in, if you
# want the probability you do the backtransform or prediction)

predict(pTemp.psiAgric, newdata = agricultural, type = "state", appendData = TRUE)
pred=predict(model,type="state",newdata=nwdat,appendData=TRUE))

# Single season occupancy ####
badger <- read.csv("badger_occupancy_scotland.csv", header = TRUE)
head(badger)
badger.o <- unmarkedFrameOccu(y = badger[,4:25],
															siteCovs = badger[,c(26,27,32,41,46)])
plot(badger.o)

pdot.psidot = occu(~1 ~1, data = badger.o)
pdot.psidot

backTransform(pdot.psidot, type = 'det') # estimate of p
backTransform(pdot.psidot, type = 'state') # estimate of psi

pTemp.psiTemp <- occu(~WTemp ~WTemp, data = badger.o)
summary(pTemp.psiTemp)

pTemp.psiDTMR <- occu(~WTemp ~DistMR, data = badger.o)
summary(pTemp.psiDTMR)

hist(badger.o@siteCovs$DistMR)

par(mfrow = c(1,2))
hist(badger.o@siteCovs$DistMR, main="Not scaled") 
hist(scale(badger.o@siteCovs$DistMR), main= "Scaled and centered")

mean(scale(badger.o@siteCovs$DistMR))
sd(scale(badger.o@siteCovs$DistMR))

pTemp.psiDTMR <- occu(~WTemp ~scale(DistMR), data = badger.o)
summary(pTemp.psiDTMR)

models <- fitList( 'pdot.psidot'=pdot.psidot, 
								 'pTemp.psiTemp'=pTemp.psiTemp, 
								 'pTemp.psiDTMR'=pTemp.psiDTMR )
modSel(models)
out.put <- model.sel(pTemp.psiTemp, pTemp.psiDTMR)
MA.ests <- model.avg(out.put, subset = delta < 2)
# Assignment 1 ####
# Multi-season occupancy ####