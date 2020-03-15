# 13 March 2020—Lab 9: Survival analysis using CJS ####
library('RMark')
# Basics ####
data(dipper)
head(dipper)
tail(dipper)

qd <- mark(dipper)
qd$results$real

dipper.proc <- process.data(dipper, groups = c("sex"), 
														model = "CJS", begin.time = 1980)
dipper.ddl <- make.design.data(dipper.proc) 
dipper.ddl

PIMS(mark(dipper.proc, dipper.ddl, invisible = FALSE,
					run = FALSE), "Phi", simplified = FALSE)
# Defining models ####

# Phi
Phidot <- list(formula = ~1)
Phitime <- list(formula = ~time)
Phisex <- list(formula = ~sex)
Phisextime <- list(formula = ~sex + time)
Phisex.time <- list(formula = ~sex * time)

#p
pdot <- list(formula = ~1)
ptime <- list(formula = ~time)
psex <- list(formula = ~sex)
psextime <- list(formula = ~sex + time)
psex.time <- list(formula = ~sex * time)

# φ(time), p(time)
dipper.phidot.pdot<-mark(dipper.proc,dipper.ddl,
												 model.parameters=list(Phi=Phitime,p=ptime))
# φ(time), p(.)
dipper.phidot.pdot <- mark(dipper.proc, dipper.ddl,
													 model.parameters = list(Phi = Phitime, p = pdot))
# φ(time), p(sex)
dipper.phitime.ptime <- mark(dipper.proc, dipper.ddl,
														 model.parameters = list(Phi = Phitime, p = psex))
# φ(time), p(sex ∗ time)
dipper.phitime.pdot <- mark(dipper.proc, dipper.ddl,
														 model.parameters = list(Phi = Phitime, p = psex.time))
# φ(sex), p(.)
dipper.phisex.pdot <- mark(dipper.proc, dipper.ddl,
														 model.parameters = list(Phi = Phisex, p = pdot))
# φ(sex), p(sex)
dipper.phisex.psex <- mark(dipper.proc, dipper.ddl,
														 model.parameters = list(Phi = Phisex, p = psex))
# φ(sex), p(sex ∗ time)
dipper.phisex.psex.ptime <- mark(dipper.proc, dipper.ddl,
														 model.parameters = list(Phi = Phisex, p = psex.time))
# φ(sex), p(time)
dipper.phisex.ptime <- mark(dipper.proc, dipper.ddl,
														 model.parameters = list(Phi = Phisex, p = ptime))

# comparing models
dipper.table <- collect.models(type = "CJS")
dipper.table

dipper.phidot.pdot$results
dipper.phisex.pdot$results
# Assignment 1—Try this with the snail kite data ####
# Bayesian CJS ####
load("ch_simulated.RData")
known.states.cjs <- function(ch){
	state <- ch
	for (i in 1:dim(ch)[1]){
		n1 <- min(which(ch[i,] == 1))
		n2 <- max(which(ch[i,] == 1))
		state[i, n1:n2] <- 1
		state[i, n1] <- NA
	}
	state[state == 0] <- NA
	return(state)
}

head(known.states.cjs(y))

get.first <- function(x) min(which(x!=0)) 
f <- apply(y,1,get.first)

head(f)
tail(f)

jags.data <- list(y = y, f = f, nind = dim(y)[1],
									n.occasions = dim(y)[2], z = known.states.cjs(y))

sink("cjs0.txt")
cat("model {
		# priors
		mean.p~dunif(0,1)
		mean.phi~dunif(0,1)
		
		for (i in 1:nind)
			{
				for (t in f[i]:(n.occasions-1))
						{
							phi[i,t]<-mean.phi
							p[i,t]<-mean.p
							}
						}
		# likelihood
		for (i in 1:nind)
			{
					z[i, f[i]]<-1 # state at first capture must be 1
			for (t in (f[i] + 1):n.occasions)
				{
						# state
					mu1[i,t]<-phi[i, t-1]*z[i,t-1]
							z[i,t]~dbern(mu1[i,t])
					mu2[i,t]<-p[i,t-1]*z[i,t]
							y[i,t]~dbern(mu2[i,t])
		}
	}
}", fill = TRUE)
sink()

require("jagsUI")
params <- c("mean.phi", "mean.p")

# Initial values for Z
cjs.init.z <- function(ch, f){
	for (i in 1:dim(ch)[1]){
		if (sum(ch[i,]) == 1) next
		n2 <- max(which(ch[i,] == 1))
		ch[i,f[i]:n2] <- NA
	}
	for (i in 1:dim(ch)[1])
	{ ch[i,1:f[i]] <-NA
	}
	return(ch)
}





inits <- function(){list(z = cjs.init.z(jags.data$y, jags.data$f),
												 mean.phi = runif(1,0,1), mean.p = runif(1,0,1))}

m.0 <- jagsUI(data = jags.data, inits, parameters.to.save = params,
							model.file = "cjs0.txt", n.thin = 5, n.chains = 3,
							n.burnin = 10000, n.iter =20000, parallel = TRUE)
summary(m.0)
	
