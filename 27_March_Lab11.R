# 27 March 2020—Lab 11
library(RMark)
library(FSA)
library(lubridate)
library(ggplot2)
# Lesson #####
load("ms_data_3states.RData")
ms.processed <- process.data(ms,model="Multistrata") 
ms.ddl <- make.design.data(ms.processed)

ms.ddl$Psi 
ms.ddl$Psi$distance=0

#A<-->B
ms.ddl$Psi$distance[ms.ddl$Psi$stratum=="A" & 
											ms.ddl$Psi$tostratum=="B"]=1.1
ms.ddl$Psi$distance[ms.ddl$Psi$stratum=="B" 
										& ms.ddl$Psi$tostratum=="A"]=1.1
#A<-->C
ms.ddl$Psi$distance[ms.ddl$Psi$stratum=="A" & 
											ms.ddl$Psi$tostratum=="C"]=2.07
ms.ddl$Psi$distance[ms.ddl$Psi$stratum=="C" & 
											ms.ddl$Psi$tostratum=="A"]=2.07
#B<-->C
ms.ddl$Psi$distance[ms.ddl$Psi$stratum=="B" & 
											ms.ddl$Psi$tostratum=="C"]=2.3
ms.ddl$Psi$distance[ms.ddl$Psi$stratum=="C" & 
											ms.ddl$Psi$tostratum=="B"]=2.3
######Create formulas for models Psi.dot=list(formula=~1) p.dot=list(formula=~1)
S.dot=list(formula=~1) 
Psi.distance=list(formula=~distance) 
Psi.distance.time=list(formula=~distance+time) 
p.stratum=list(formula=~stratum) 
S.time=list(formula=~time)

model.list=create.model.list("Multistrata")
ms.results=mark.wrapper(model.list,data=ms.processed,ddl=ms.ddl) 
ms.results

ms.results$S.dot.p.stratum.Psi.distance$results$real
# Assignment 1 ####
# survival estimate is 0.8
# capture probability is highest at C
# A & B has lowest connectivity
# Assignment 2 ####
rm(list=ls())
gorilla <- read.csv("gorilla.csv")
gorilla_cap <- capHistConvert(gorilla[,1], in.type = "individual", out.type = "RMark")
gorilla_AtoB <- gorilla_cap[3:37,1:5]

gorilla.processed <- process.data(gorilla_cap,model="Multistrata") 
gorilla.ddl <- make.design.data(gorilla.processed)

S.dot=list(formula=~1) 
Psi.dot=list(formula=~1) 
p.dot=list(formula=~1) 
Psi.time=list(formula=~time) 
p.stratum=list(formula=~stratum) 
S.stratum=list(formula=~stratum)


model.list=create.model.list("Multistrata")
gorilla.results=mark.wrapper(model.list,data=gorilla.processed,
														 ddl=gorilla.ddl) 
gorilla.results$S.dot.p.stratum.Psi.time$results$real[3:37,1:5]
gorilla.results$S.dot.p.stratum.Psi.time$results$real[633:667,1:5]

gorilla.results$S.dot.p.stratum.Psi.time$results$real

a_b <- as.data.frame(gorilla.results$S.dot.p.stratum.Psi.time$results$real[4:37,1:4])
a_b$time <- (c(seq(1,34,1)))
b_a <- as.data.frame(gorilla.results$S.dot.p.stratum.Psi.time$results$real[634:667,1:4])
b_a$time <- (c(seq(1,34,1)))
df <- rbind(a_b, b_a)
df$group <- c("AtoB","AtoB","AtoB","AtoB","AtoB","AtoB","AtoB","AtoB",
							"AtoB","AtoB","AtoB","AtoB","AtoB","AtoB","AtoB","AtoB",
							"AtoB","AtoB","AtoB","AtoB","AtoB","AtoB","AtoB","AtoB",
							"AtoB","AtoB","AtoB","AtoB","AtoB","AtoB","AtoB","AtoB",
							"AtoB","AtoB","BtoA","BtoA",
							"BtoA","BtoA", "BtoA", "BtoA", "BtoA","BtoA", "BtoA", "BtoA",
							"BtoA","BtoA", "BtoA", "BtoA", "BtoA","BtoA", "BtoA", "BtoA",
							"BtoA","BtoA", "BtoA", "BtoA", "BtoA","BtoA", "BtoA", "BtoA",
							"BtoA","BtoA", "BtoA", "BtoA", "BtoA","BtoA", "BtoA", "BtoA")
pd <- position_dodge(0.5)

ggplot(data = df, aes(x = time, y = estimate))+
	geom_point()+
	geom_errorbar(ymin = df$lcl, ymax = df$ucl)+
	geom_jitter(aes(color = group))+
	ylab("Psi Estimate")+
	xlab("Time")+
	theme_bw()

# best model is S.dot + p.stratum + psi.time
# p is 0.08 for state A & 0.7 for state B
# no evidence that survival changes with state (wasn't significant)
# yeah the number changes (highest around time 10)

