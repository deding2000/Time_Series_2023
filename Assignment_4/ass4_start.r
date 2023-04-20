source("Time_Series_2023/Assignment_4/kalman_new.r")

# Assignment 4
##########################
# Q4.1 Loading and Plotting
DATA <- read.csv("Time_Series_2023/Assignment_4/A4_Kulhuse.csv")
Salt <- DATA$Sal
OX <- DATA$ODO
# missing observations
sum(is.na(Salt))
sum(is.na(OX))
DATA$DateTime[5000]
# Plotting
Days <- seq(from = 10/24, by = 1/48, length.out = 5000) # Days from start (2017-08-24 10:00:00)
plot(Days,DATA$Sal,type="l",xlab="Days from start",ylab="Salinity [PSU]",main="Water salinity")
plot(Days,DATA$ODO,type="l",xlab="Days from start",ylab="Oxygen [mg/L]",main = "Dissolved Oxygen")
min(na.omit(DATA$ODO))

######################################
# Q4.2
A <- matrix(1)
Y <- Salt
C <- matrix(1)
Xhat0 <- Salt[1]
V0 <- 0.01
KALMAN <- kalman2(Y,A,B=NULL,u=NULL,C,Sigma.1=0.01,Sigma.2=0.005,debug=FALSE,V0=V0,Xhat0=Xhat0,n.ahead=1,skip=0,verbose=FALSE)
plot(KALMAN$pred,type="l")
lines(Y,c="red")
