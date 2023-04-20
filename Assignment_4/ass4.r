# Q4.1
Data <- read.csv("Assignment 4/A4_Kulhuse.csv")
Sal <- Data$Sal
ODO <- Data$ODO
time <- Data$DateTime
days <- seq(from= 10/24, by =1/48, length.out = length(Sal))

plot(days, Sal, type = "l", main= "Salinity", xlab = "days from start", ylab = "[g/kg]")
plot(days,ODO, type = "l", main = "dissolved oxygen", xlab = "days from start", ylab = "[mg/L]")


min(na.omit(Sal))
min(na.omit(ODO))

# number of NA
sum(is.na(Sal))
sum(is.na(ODO))

# NA from
# 1588-1692
# ODO[1587:1693]

source('kalman.R')
source('kalman_copy.R')

# Sal_filter <- kalman(na.omit(Sal),A = matrix(1),B=0,u=NULL,C = matrix(1),Sigma.1=matrix(0.01),Sigma.2=matrix(0.005),debug=FALSE,V0=0.01,Xhat0=Sal[1],n.ahead=1,skip=0,verbose=FALSE)
# plot(na.omit(Sal) , type = "l")
# points(Sal_filter$rec, type = "l", col = "red")
# points(Sal_filter$pred, type = "l", col = "blue")

# implemented na 
Sal_filter2 <- kalman2(Sal,A = matrix(1),B=0,u=NULL,C = matrix(1),Sigma.1=matrix(0.01),Sigma.2=matrix(0.005),debug=FALSE,V0=matrix(0.01),Xhat0=Sal[1],n.ahead=1,skip=0,verbose=TRUE)
PI <- qnorm(0.025)*sqrt((Sal_filter2$Sigma.yy.pred[,,])[2:5000])
pred_error <-Sal_filter2$Sigma.yy.pred[,,][2:5000] 
pred <- Sal_filter2$pred[2:5000]

plot(Sal, type = "l")
# points(Sal_filter2$rec, type = "l", col = "red")
points(pred, type = "l", col = "green")
polygon(c(rev(2:5000), 2:5000), c(rev(pred - PI), pred +PI), col = "blue", border = NA)

# standardized errors
pred_error_stand <- (Sal[2:5000]-pred)/sqrt(pred_error)
plot(pred_error_stand, type = "l")

# more plots
plot(pred_error_stand, type = "l", xlim = c(800,950))

plot(Sal, type = "l", xlim = c(800,950),ylim = c(15,19))
polygon(c(rev(2:5000), 2:5000), c(rev(pred - PI), pred + PI), col = "#8585bc", border = NA)
points(pred, type = "l", col = "green")
points(Sal,  type = "l", xlim = c(800,950),ylim = c(15,19))

