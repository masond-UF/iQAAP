# 9 April 2020—Lab 13 "Occupancy ####
library(unmarked)
library(MuMIn)
library(ggplot2)
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
hist(badger.o@siteCovs$Elev)
hist(badger.o@siteCovs$Agric)
hist(badger.o@siteCovs$Humans) 
# These don't range as extreme as other covariates

pTemp.psiELEV <- occu(~WTemp ~Elev, data = badger.o)
pTemp.psiAGRI <- occu(~WTemp ~Agric, data = badger.o)
pTemp.psiHUM <- occu(~WTemp ~Humans, data = badger.o)

models <- fitList('pdot.psidot'=pdot.psidot, 
								 'pTemp.psiTemp'=pTemp.psiTemp, 
								 'pTemp.psiDTMR'=pTemp.psiDTMR,
									'pTemp.psiELEV'=pTemp.psiELEV,
									'pTemp.psiAGRI'=pTemp.psiAGRI,
									'pTemp.psiHUM'=pTemp.psiHUM)

modSel(models)
# most parsimonious is pTemp.psiAGRI because it has lowest AIC
# and is the only with delta <2

exp(coef(pTemp.psiAGRI)[2])
# every percent increase in agricultural land use increases occupancy by 1.20

# make the figure
agricultural <- data.frame(Agric = seq(0,26,1))
agricultural$occ <- predict(pTemp.psiAGRI, newdata = agricultural, 
														type = "state", appendData = TRUE)

ggplot(d = agricultural, aes(x = Agric, y = agricultural$occ$Predicted))+
	geom_line(color = "red")+
	geom_ribbon(data=agricultural,
	aes(ymin=agricultural$occ$lower,ymax=agricultural$occ$upper),alpha=0.2,
	color = "blue", fill = "blue")+
	xlab("% Agricultural land")+
	ylab("Badger Occupancy")+
	theme_classic()+
	theme(text = element_text(size = 20))	


# Multi-season occupancy ####
y <- read.csv("lepa_y.csv", header = TRUE) # presence/absence data
head(y)
site.covs <- read.csv("lepa_site_covs.csv", header = TRUE) # covariates 
head(site.covs) # A has been scaled

plot(site.covs$x.coord., site.covs$y.coord.,
		 cex = log(site.covs$A+1), xlab = "X coordinate", ylab = "Y coordinate")

lepa_umf <- unmarkedMultFrame(y=y[,2:7], siteCovs = site.covs, numPrimary = 3)

mod_1<-colext(psiformula=~Phorophyte, gammaformula=~1, epsilonformula=~1, 
							pformula=~A+Phorophyte,data=lepa_umf)
plogis(coef(mod_1)[3])
plogis(coef(mod_1)[4])

plogis(coef(mod_1)[5])
plogis(coef(mod_1)[5]+coef(mod_1)[7])

d <- dist(cbind(site.covs$x.coord.,site.covs$y.coord.))
alpha <- 1/4.8 #1/average dispersal distance 
edis <- as.matrix(exp(-alpha*d))
diag(edis) <- 0
edis <- sweep(edis,2,site.covs$A,"*") #Finally the sumation S<-rowSums(edis)
S <- rowSums(edis)

site.covs$S <- S #adds column to site.covs 
lepa_umf <- unmarkedMultFrame(y=y[,2:7],siteCovs=site.covs,numPrimary=3)

mod_2 <- colext(psiformula=~Phorophyte,gammaformula=~S, epsilonformula=~A,
								pformula=~A+Phorophyte,data=lepa_umf) #interpret odds ratio

#connectivity and colonization
exp(coef(mod_2)[4])

#extinction and patch area
exp(coef(mod_2)[6])
# Assignment 2 ####
