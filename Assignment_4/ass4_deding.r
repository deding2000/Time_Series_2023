source("Time_Series_2023/Assignment_4/kalman(Rune).r")
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
B <- matrix(c(0))
C <- matrix(1)
Y <- Salt
Xhat0 <- matrix(Salt[1])
V0 <- matrix(0.01)
KALMAN <- kalman_rem_out(Y,A,B=NULL,u=NULL,C,Sigma.1=matrix(0.01),Sigma.2=matrix(0.005),debug=FALSE,V0=V0,Xhat0=Xhat0,n.ahead=1,skip=0,verbose=TRUE)

## Plotting
PI <- qnorm(0.025)*sqrt((KALMAN$Sigma.yy.pred[,,])[2:5000])
pred <- KALMAN$pred[2:5000]
plot(Days[801:951],Y[801:951],type="l",xlab="Days from start",ylab="Salinity [PSU]",main="Water salinity with kalman filter")
polygon(Days[2:5000],c(rev(2:5000), 2:5000), c(rev(pred - PI), pred +PI), col = "grey", border = NA)
lines(Days[801:951],KALMAN$rec[801:951], type = "l", col = "green")

# standardized one step prediction errors
pred_errors <- (KALMAN$pred[2:5000] - Y[2:5000])/(sqrt(KALMAN$Sigma.yy.pred[2:5000]))
plot(Days[2:5000],pred_errors,type="l",xlab="Days from start",ylab="Prediction error [PSU]",main="Standardized prediction errors")
# Zoomed in
plot(Days[801:951],pred_errors[800:950],xlab="Days from start",ylab="Prediction error [PSU]",main="Standardized prediction errors (Zoomed in)")

#Finale value of filter
KALMAN$rec[5000]

#####################################################
# Number of Outliers
sum(KALMAN$Outliers)

# standardized one step prediction errors
pred_errors <- (KALMAN$pred[2:5000] - Y[2:5000])/(sqrt(KALMAN$Sigma.yy.pred[2:5000]))
# Zoomed in
plot(Days[801:951],pred_errors[800:950],type="l",xlab="Days from start",ylab="Prediction error [PSU]",main="Standardized prediction errors (removed outliers)")


##############################################
#Q.45
nll <- function(Sigma){ #Sigma[1] og Sigma[2].  Sigma <- array(c(Sigma1,Sigma2),dim=c(1,1,2))
  kl <- kalman(Salt[1:800],A=A,B=B,u=NULL,C=C,Sigma.1=matrix(exp(Sigma[1])),Sigma.2=matrix(exp(Sigma[2])),debug=FALSE,V0=matrix(exp(Sigma[1])),Xhat0=Salt[1],n.ahead=1,skip=0,verbose=TRUE)
  #using C is single index then
  
  N <- length(Salt[1:800])
  R <- na.omit( (c(C)*kl$Sigma.xx.pred[1,1,]*c(C)+c(matrix(exp(Sigma[2]))))[1:N])
  Y_tilde <- na.omit(Salt[1:N]-kl$pred[1:N])
  1/2*sum(log(R)+Y_tilde*1/R*Y_tilde)
}
Sigma1 <- matrix(c(1)) #System variance
Sigma2 <- matrix(c(1)) #Observation variance
fit <- optim(c(log(Sigma1),log(Sigma2)), nll, method="L-BFGS-B")#,lower=0.005^2) #indsæt lower bound. #method="L-BFGS-B"
fit$convergence
Sigma1_opt <- exp(fit$par[1])
Sigma2_opt <- exp(fit$par[2])

