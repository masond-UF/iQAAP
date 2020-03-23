# 20 March 2020—Lab 10 ####
library(RMark)
library(ggplot2)
data(robust)
time.intervals <- c(0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0)
# Robust Model in RMark ####
S.dot <- list(formula=~1) 
p.dot <- list(formula=~1,share=TRUE) 
GammaDoublePrime.dot <- list(formula=~1)
model.0 <- mark(data=robust, model = "Robust", 
								time.intervals = time.intervals, 
								model.parameters = list(S = S.dot,
																			GammaDoublePrime = GammaDoublePrime.dot,
																			p = p.dot),threads=2) 
summary(model.0)
model.0$results$real
round(model.0$results$derived$`N Population Size`, 3)

S.time <- list(formula=~time)

#you can have p vary by session (primary period) and/or by time (secondary period)
p.time <- list(formula=~time)

#note that because share=TRUE, p = c
p.time.session <- list(formula=~-1+session:time,share=TRUE) 
p.session <- list(formula=~-1+session,share=TRUE) 
pc <- list(formula=~1) # p =! c

#Random
GammaDoublePrime.random=list(formula=~time,share=TRUE)

model.1 <- mark(data = robust, model = "Robust", time.intervals=time.intervals,
								model.parameters=list(S=S.time, GammaDoublePrime=GammaDoublePrime.random,
												 p=p.time.session),threads=2)
model.1$results$real[,1:4]
# Individual heterogeniety models in RMark ####
model.2 <- mark(data = robust, model = "RDHet", time.intervals=time.intervals,
						 model.parameters=list(S=S.time, 
						 GammaDoublePrime=GammaDoublePrime.random, p=p.dot),threads=2)
model.2$results$real
GammaPrime.dot=list(formula=~1) 
model.3=mark(data = robust, model = "RDHet",
						 time.intervals=time.intervals, 
						 model.parameters=list(S=S.dot, GammaPrime=GammaPrime.dot, 
						 GammaDoublePrime=GammaDoublePrime.dot, p=p.dot),threads=2)
model.3$results$real

model.4=mark(data = robust, model = "RDFullHet", time.intervals=time.intervals,
						 model.parameters=list(S=S.time, GammaPrime=GammaPrime.dot, 
						 GammaDoublePrime=GammaDoublePrime.dot, p=pc),threads=2)
model.4$results$real
collect.models()
# Assignment 1—Plot the model results ####
model.2 <- mark(data = robust, model = "RDHet", time.intervals=time.intervals,
								model.parameters=list(S=S.time, 
																			GammaDoublePrime=GammaDoublePrime.random, p=p.dot),threads=2)

df <- as.data.frame(model.1$results$real) # pull the results

# make S dataframe and plot
S <- df[1:4,1:4] 
S$time <- as.factor(c(1,2,3,4))

ggplot(data = S, aes(x = time, y = estimate))+
	geom_line()+
	geom_point(size = 4)+
	scale_x_discrete("Time", breaks = c(1,2,3,4))+
	ylab("S")+
	ggtitle("True Survival")+
	geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.3)+
	theme_classic(base_size = 15)


# make gamma double prime dataframe and plot
g.dbl.prime <- df[5:8,1:4]
g.dbl.prime$time <- as.factor(c(1,2,3,4))

ggplot(data = g.dbl.prime, aes(x = time, y = estimate))+
	geom_line()+
	geom_point(size = 4)+
	scale_x_discrete("Time", breaks = c(1,2,3,4))+
	ylab("γ″")+
	ggtitle("Emigration")+
	geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.3)+
	theme_classic(base_size = 15)


# make p dataframe and plot
p <- df[9:23,1:4]
p$session <- as.factor(c(1,1,2,2,3,3,3,3,4,4,4,4,4,5,5))
p$time <- as.factor(c(1,2,1,2,1,2,3,4,1,2,3,4,5,1,2))

ggplot(data = p, aes(x = time, y = estimate, group = session, color = session))+
	geom_line()+
	geom_point(size = 4)+
	scale_x_discrete("Time", breaks = c(1,2,3,4))+
	ylab("p")+
	ggtitle("Apparent probability of encounter")+
	theme_classic(base_size = 15)

# Assignment 2 ####
rm(list=ls()) #WARNING! This will delete your workspace #But is needed to use collect.models() in the end
rd.inp <- convert.inp("assignment_rd.inp")
time.intervals <- c(0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0)
rd <- process.data(data = rd.inp,
									 model = "Robust",
									 time.intervals = time.intervals)
#Define models
S.time = list(formula = ~ time)
S.dot=list(formula=~1)
p.dot=list(formula=~1)
p.session = list(formula = ~ session)
GammaDoublePrime.random = list(formula = ~time,share=TRUE) #random
GammaDoublePrime.dot = list(formula = ~1) #markovian
#gamma fixed to zero for no TE
GammaDoublePrime.zero = list(formula = ~ 1, fixed = 0)
GammaPrime.zero = list(formula = ~ 1, fixed = 0)
#######"Null" model, no TE 
model.0=mark(data = rd, model = "Robust", time.intervals=time.intervals, 
						 model.parameters=list(S=S.dot, GammaPrime=GammaPrime.zero,
						 GammaDoublePrime=GammaDoublePrime.zero, p=p.dot),threads=2)
model.0$results$real
# time
model.1 <- mark(data = rd, model = "Robust", time.intervals = time.intevals,
						 model.parameters = list(S=S.time, GammaPrime=GammaPrime.zero,
						 GammaDoublePrime=GammaDoublePrime.zero, p=p.dot),threads=2)
# time and p(session)
model.2 
# time, p(session), and markov emigration
model.3 
model.4 # time, p(session), and random emigration