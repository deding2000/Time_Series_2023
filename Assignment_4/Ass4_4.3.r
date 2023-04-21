#Assignment 4

#Load data
dat <- read.csv("A4_Kulhuse.csv")
str(dat)
#    Battery, Chl, DateTime, Depth, ODO, ODOsat, pH, Sal, Temp
attach(dat)

#Missing
sum(is.na(DateTime))
sum(is.na(ODO))

Days <- seq(from = 10/24, by = 1/48, length.out = 5000) # Days from start (2017-08-24 10:00:00)

plot(Days,Sal,type='l')


A <- matrix(c(1))
B <- matrix(c(0))
C <- matrix(c(1))
Sigma1 <- matrix(c(0.01)) #System variance
Sigma2 <- matrix(c(0.005)) #Observation variance

#Question 4.3 Pure filter.
source("kalman.R")
kalman43 <- kalman(Sal,A=A,B=B,u=NULL,C=C,Sigma.1=Sigma1,Sigma.2=Sigma2,debug=FALSE,V0=Sigma1,Xhat0=Sal[1],n.ahead=1,skip=0,verbose=TRUE)

#1. Plot prediction along the data include 95% PI.
plot(Days,Sal,type="p",,col="red",xlab="Time (day)",ylab="Water Salinity [PSU] = [g/kg]")
matlines(Days,kalman43$pred[1:5000,1]+sqrt(kalman43$Sigma.yy.pred[1,1,1:5000])%*%cbind(0,-1.96,1.96),col="seagreen",lty=c(1,2,2), lwd=1)
legend("bottomright",legend=c("Observations","1-step prediciions"),col=c("tomato","seagreen"),pch=c(20,NA),bty='n',lty=c(NA,1))

#2. Plot the standardized one step prediction errors. (Prediction errors normalized with the standard error of the prediction.)
length(kalman43$pred)

errors_norm <- na.omit((kalman43$pred[1:5000,1]-Sal[1:5000]) / sqrt(kalman43$Sigma.yy.pred[1:5000]) )
plot(errors_norm,ylab="Errors normalized",type='l')

interval = c(800:950)
#3. Plot for 800-950
plot(interval,Sal[interval],type="p",,col="red",xlab="Index",ylab="Water Salinity [PSU] = [g/kg]",ylim=c(15,24))
matlines(interval,kalman43$pred[interval,1]+sqrt(kalman43$Sigma.yy.pred[1,1,interval])%*%cbind(0,-1.96,1.96),col="seagreen",lty=c(1,2,2), lwd=1)
legend("bottomright",legend=c("Observations","1-step prediciions"),col=c("tomato","seagreen"),pch=c(20,NA),bty='n',lty=c(NA,1))

errors_norm <- na.omit((kalman43$pred[interval,1]-Sal[interval]) / sqrt(kalman43$Sigma.yy.pred[interval]) )
#MAKE PLOT!!!
plot(interval,errors_norm,ylab="Errors normalized",type='l',xlab="Index")

#MAKE TABLE!!
maketable <- data.frame(Sal = Sal[5000], rec = kalman43$rec[5000],
pred = kalman43$pred[5000],
K = kalman43$K[5000],
Sigma.xx.rec = kalman43$Sigma.xx.rec[1,1,5000],
Sigma.yy.rec=kalman43$Sigma.yy.rec[5000],
kalman43$Sigma.xx.pred[5000],
kalman43$Sigma.yy.pred[5000])

maketable

