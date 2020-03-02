# 2 March 2020—Lab 8: Capture Models ####
library("Rcapture")
library("FSA")
# Ornate Lizard in Chiricahua Mountains ####
# import the data
ornate_lizard <- read.csv("treelizardcapturehistories.csv", 
													header = TRUE, colClasses = c("ch" = "character"))
head(ornate_lizard)

# subset data
ornate_lizard_u1 <- ornate_lizard[ornate_lizard$hab == "u" &
																		ornate_lizard$site == "1",]
head(ornate_lizard_u1)

# convert data (we are using columns 2 and 3, which correspond to
# id and capture history).
ornate_lizard_u1_ind <- capHistConvert(ornate_lizard_u1[,2:3],
																			 id = "id", in.type = "RMark",
																			 out.type = "individual")

head(ornate_lizard_u1_ind)

# explore heterogeniety in the data (using only columns 2-14)
desc <- descriptive(ornate_lizard_u1_ind[,2:14])
desc
plot(desc)

# Model fit ####
models <- closedp.t(ornate_lizard_u1_ind[,2:14])
models

darroch_estimates <- closedpCI.t(ornate_lizard_u1_ind[,2:14],
																 m = "Mth", h = "Darroch")
plotCI(darroch_estimates)
# Bayesian parameterization ####
# data augmentation
oldata <- ornate_lizard_u1_ind[,2:14] # for rbinding we need the same col names
T <- ncol(oldata) # nunmber of columns
colnames(oldata) <- 1:T # name the columns

nz <- 1000 # add 1000 capture histories with just zero
zdata <- data.frame(array(0, dim = c(nz,T)))
colnames(zdata) <- 1:T
yaug <- rbind(oldata, zdata)

head(yaug)
tail(yaug)

library(rjags)
library(R2jags)

model0 <- function(){
	# Priors
	omega~dunif(0,1)
	p~dunif(0,1)
	
	# Likelihood
	for (i in 1:M){
		z[i]~dbern(omega)
		for (j in 1:T){
			yaug[i,j]~dbern(p.eff[i,j])
			p.eff[i,j] <- z[i]*p
		}
	}
	# Derived quantities
	N <- sum(z[])
}

# format data
data <- list(yaug = yaug, M = nrow(yaug), T=ncol(yaug))
# parameters
params <- c("N", "p", "omega")
# initial values for MCMC
inits <- function(){
	list(z = rep(1,nrow(yaug)), p = runif(0,1))
}
# run the model
jags.m0 <- jags(model.file = model0, data = data, inits = inits, params,
								n.iter = 2500, n.chains = 3)

# see model output
traceplot(jags.m0)
plot(jags.m0)

# temporal heterogeniety

modelt <- function(){
	# Priors
	omega~dunif(0,1)
	for (i in 1:T){
	p[i]~dunif(0,1)
	}
	# Likelihood
	for (i in 1:M){
		z[i]~dbern(omega)
		for (j in 1:T){
			yaug[i,j]~dbern(p.eff[i,j])
			p.eff[i,j] <- z[i]*p[j]
		}
	}
	# Derived quantities
	N <- sum(z[])
}

#format data
data<-list(yaug=yaug,M=nrow(yaug),T=ncol(yaug)) #parameters
params<-c("N","p","omega")
#initial values for MCMC
inits<-function() list(z=rep(1,nrow(yaug)),p=runif(T,0,1))
#run the model
jags.mt=jags(model.file=modelt,data=data,inits=inits,params,n.iter=2500,n.chains=3)
traceplot(jags.mt)
plot(jags.mt)

jags.mt

# Assignment 1 ####
library(ggplot2)
colSums(ornate_lizard_u1_ind)
x <- data.frame("Event" = 1:13, "Observations" = c(16, 24, 14, 23, 21, 16,
																									 20, 10, 21, 23, 23, 23, 0))

ggplot(data = x, aes(x = Event, y = Observations))+
	geom_point()+
	geom_line()+
	scale_x_continuous(breaks = 1:13)+
	theme_classic()
	

# Assignment 2 ####

