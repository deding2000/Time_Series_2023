source("Time_Series_2023/Assignment_4/kalman_new.r")
source("Time_Series_2023/Assignment_4/kalman_rem_out.r")

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
# Q4.3 - Kalman Filter
A <- matrix(1)
Y <- Salt
C <- matrix(1)
Xhat0 <- matrix(Salt[1])
V0 <- matrix(0.01)
KALMAN2 <- kalman2
KALMAN <- kalman_rem_out(Y,A,B=NULL,u=NULL,C,Sigma.1=matrix(0.01),Sigma.2=matrix(0.005),debug=FALSE,V0=V0,Xhat0=Xhat0,n.ahead=1,skip=0,verbose=TRUE)
# Number of Outliers
sum(KALMAN$Outliers)


plot(Y,type="o")
lines(KALMAN$pred,col="red")
points(KALMAN$rec,col="red")
# we use KALMAN$Sigma.yy.pred to do the prediction interval

# standardized one step prediction errors
pred_errors <- (KALMAN$pred[2:5000] - Y[2:5000])/(sqrt(KALMAN$Sigma.yy.pred[2:5000]))
plot(Days[2:5000],pred_errors,type="l",xlab="Days from start",ylab="Prediction error [PSU]",main="Standardized prediction errors")
# Zoomed in
plot(Days[801:951],pred_errors[800:950],type="l",xlab="Days from start",ylab="Prediction error [PSU]",main="Standardized prediction errors (Zoomed in)")

nll <- function(Sigma){ #Sigma[1] og Sigma[2].  Sigma <- array(c(Sigma1,Sigma2),dim=c(1,1,2))
  kl <- kalman_rem_out(Sal,A=A,B=B,u=NULL,C=C,Sigma.1=matrix(Sigma[1]),Sigma.2=matrix(Sigma[2]),debug=FALSE,V0=matrix(Sigma[1]),Xhat0=Sal[1],n.ahead=1,skip=0,verbose=TRUE)
  #using C is single index then
  
  N <- length(Sal)
  R <- na.omit( (c(C)*kl$Sigma.xx.pred[1,1,]*c(C)+c(matrix(Sigma[2])))[1:N]+0*(Sal[1:N]-kl$pred[1:N]) )
  Y_tilde <- na.omit(Sal[1:N]-kl$pred[1:N])
  1/2*sum(log(R)+Y_tilde*1/R*Y_tilde)
}
fit <- optim(c(Sigma1,Sigma2), nll,method="L-BFGS-B" , lower = 0.00005) #indsæt lower bound. #method="L-BFGS-B"
fit$convergence
fit
