n.true <- 60
n1 <- 23
n2 <- 22
m2 <- 9
chapman <- function(n1, n2, m2){
	nhat <-  (((n1+1)*(n2+1))/(m2+1))-1
	var <- ((n1+1)*(n2+1)*(n1-m2)*(n2-m2))/((m2+2)*(m2+1)^2)
	ci <- 1.96*sqrt(var)
	upper <- nhat+ci
	lower <- nhat-ci
	return(c(nhat,var,upper,lower))
}
estimate2 <- as.data.frame(chapman(n1,n2,m2))
estimate1 <- as.data.frame(c(181, 8736, 364.19448, -2.19448))
chapman(n1,n2,m2)
matrix(c(), nrow=3, byrow=TRUE)

estimate <- rbind(c(54.2,91.33091,72.93117, 35.46883),
									c(181, 8736, 364.19448, -2.19448))

observation <- c("estimate 1", "estimate 2")

estimate <- cbind(estimate, observation)

colnames(estimate) <- c("mean", "variance", "upper", "lower", "observation")
rownames(estimate) <- c("estimate 1", "estimate 2")

estimate <- as.data.frame(estimate, stringsAsFactors = FALSE)
estimate$mean <- as.numeric(estimate$mean)
estimate$variance <- as.numeric(estimate$variance)
estimate$upper <- as.numeric(estimate$upper)
estimate$lower <- as.numeric(estimate$lower)


var <- function(n1, n2, m2){
	var <- ((n1+1)*(n2+1)*(n1-m2)*(n2-m2))/((m2+2)*(m2+1)^2)
	return(var)
}

library(ggplot2)

str(estimate)

estimate$mean <- as.numeric(estimate$mean)
estimate$variance <- as.numeric(estimate$variance)
estimate$upper <- as.numeric(estimate$upper)
estimate$lower <- as.numeric(estimate$lower)

estimate$observation <- (estimate$observation, c("", "2"))

estimate$observation[estimate$observation == "1"] <- "2"
estimate$observation[estimate$observation == "2"] <- "1"



ggplot(data = estimate, aes(x = observation, y = mean))+
	geom_errorbar(aes(ymin = lower, ymax = upper, width = 0.15))+
	geom_point()+
	xlab("Observation")+
	ylab("Population Estimate")+
	theme_classic(base_size = 20)


