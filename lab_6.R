# Friday 14 February 2020 ####
# jagsdata is chupacabra_binomial.RData
# dat is logistic.RData
# x and y are poisson_data.RData
# Logistic regression ####
plot(dat$bodysize, dat$survive, xlab = "Body size (mm)", ylab = "Survival")

model.log = glm(dat$survive ~ dat$bodysize, family = binomial)
summary(model.log)

exp(coef(model.log)[2])

# every increase 1 mm you are 2.35 more likely to survive 
# Binomial regression ####
load("chupacabra_binomial-1.RData")
jagsdata

presence <- jagsdata$y
absence <- jagsdata$n - jagsdata$y
mod <- glm(cbind(presence, absence) ~ jagsdata$x, family = "binomial")
summary(mod)

model <- function(){
	## Specify likelihood
	for(i in 1:10){
		y[i] ~ dbin(p[i], n[i])
		logit(p[i]) <- b0 + b1*x[i]
	}
	## Specify priors
	b0 ~ dnorm(0, 0.0001)
	b1 ~ dnorm(0, 0.0001)
}

library("R2jags")

jmod <- jags(model.file = model, data = jagsdata, n.iter = 10000,
						 parameters.to.save = c("b0", "b1"))

jmod 
traceplot(jmod)
# Poisson regression ####
load("poisson_data-1.RData")
ls()
head(x)
head(y)

plot(x, y, xlab = "Patch size (ha)", ylab = "Number of bird species")

model.pois <- glm(y ~x, family = poisson)
summary(model.pois)

exp(coef(model.pois)[2])
# 17% increase in the number of birds per 1/2 ha increase in patch size
# can not extrapolate past your data (there is a limit to the number of birds)

plot(x,y,xlab="Patch size (ha)",ylab="Number of bird species")

plot(x, y, xlab = "Patch size (ha)", ylab = "Number of bird species")
preds <- predict(model.pois,
								 data.frame(x = seq(min(x), max(x), length.out = 100)),
								 type = "response")
lines(seq(min(x), max(x), length.out = 100), preds)
# Assignment 1 ####
# every increase 1 mm you are 2.35 more likely to survive 
# Assignment 2 ####
length(p_data$y)
model.pois.2 <- function(){
	## Specify likelihood 
	for(i in 1:30){
	y[i] ~ dpois(lambda[i])
	log(lambda[i]) <- b0 + b1*x[i] 
	}
	## Specify priors
	b0 ~ dnorm(0, 0.0001)
	b1 ~ dnorm(0, 0.0001)
}
p_data <- as.data.frame(cbind(x,y))
pmod <- jags(model.file = model.pois.2, data = p_data, n.iter = 10000,
						 parameters.to.save = c("b0", "b1"))

pmod 
traceplot(pmod)

exp(0.159)
