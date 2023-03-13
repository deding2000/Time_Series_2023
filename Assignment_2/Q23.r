set.seed(9999)
phi1 <- c(1.5,-0.52) 
phi2 <- c(1.5,-0.98)
sd1 <- 0.1
sd2 <- 5
n <- 300
p1 <- matrix(0,100,3)
p2 <- matrix(0,100,3)
p3 <- matrix(0,100,3)
p4 <- matrix(0,100,3)
for (i in 1:100) {
#Simulations
ts1 <- arima.sim(list(ar=phi1,ma=0), n, sd=sd1)
ts2 <- arima.sim(list(ar=phi2,ma=0), n, sd=sd1)
ts3 <- arima.sim(list(ar=phi1,ma=0), n, sd=sd2)
ts4 <- arima.sim(list(ar=phi2,ma=0), n, sd=sd2)
#Estimation of parameters
arima1 <- arima(ts1[1:n],order=c(2,0,0),include.mean=FALSE)
arima2 <- arima(ts2[1:n],order=c(2,0,0),include.mean=FALSE)
arima3 <- arima(ts3[1:n],order=c(2,0,0),include.mean=FALSE)
arima4 <- arima(ts4[1:n],order=c(2,0,0),include.mean=TRUE)
# phi_2 values
p1[i,1] <- -arima1$coef[2]
p2[i,1] <- -arima2$coef[2]
p3[i,1] <- -arima3$coef[2]
p4[i,1] <- -arima4$coef[2]
# phi_1 values
p1[i,2] <- -arima1$coef[1]
p2[i,2] <- -arima2$coef[1]
p3[i,2] <- -arima3$coef[1]
p4[i,2] <- -arima4$coef[1]
#variances
p1[i,3] <- arima1$var.coef[2,2]
p2[i,3] <- arima2$var.coef[2,2]
p3[i,3] <- arima3$var.coef[2,2]
p4[i,3] <- arima4$var.coef[2,2]
}

#Histograms
hist(p1[,1],main="Process 1", breaks = 10,
xlab=expression(phi[2]))
hist(p2[,1],main="Process 2", breaks = 10,
xlab=expression(phi[2]))
hist(p3[,1],main="Process 3", breaks = 10,
xlab=expression(phi[2]))
hist(p4[,1],main="Process 4", breaks = 10,
xlab=expression(phi[2]))
arima1$var.coef[2,2]
# 0.95 quantiles
quantile(p1[,1],probs=c(0.025,0.975))
quantile(p2[,1],probs=c(0.025,0.975))
quantile(p3[,1],probs=c(0.025,0.975))
quantile(p4[,1],probs=c(0.025,0.975))
#assuming normal distribution of the phi2s:
# t.test(p1[,1])$"conf.int"
# t.test(p2[,1])$"conf.int"
# t.test(p3[,1])$"conf.int"
# t.test(p4[,1])$"conf.int"

#effects of different phi2
plot(c(1:100),p1[,3])
mean(p1[,3])
plot(c(1:100),p2[,3])
mean(p2[,3])



#effects of different sigmas
plot(c(1:100),p1[,3])
mean(p2[,3])
plot(c(1:100),p2[,3])
mean(p4[,3])

#pair of estimates
plot(p1[,1:2])
plot(p2[,1:2])
plot(p3[,1:2])
plot(p4[,1:2])



