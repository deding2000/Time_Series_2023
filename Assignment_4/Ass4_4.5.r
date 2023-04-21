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

#ML estimate
Sigma <- {Sigma1,Sigma2}
Sigma
kl <- kalman(Sal,A=A,B=B,u=NULL,C=C,Sigma.1=matrix(Sigma[1]),Sigma.2=matrix(Sigma[2]),debug=FALSE,V0=matrix(Sigma[1]),Xhat0=Sal[1],n.ahead=1,skip=0,verbose=TRUE)
#using C is single index then
print(kl$Outliers)
N <- length(Sal)
R <- na.omit( (c(C)*kl$Sigma.xx.pred[1,1,]*c(C)+c(matrix(Sigma[2])))[1:N]+0*(Sal[1:N]-kl$pred[1:N]) )
Y_tilde <- na.omit(Sal[1:N]-kl$pred[1:N])

-1/2*sum(log(R+Y_tilde*1/R*Y_tilde))

#hvad gør man med NA values her så?????

source("kalman.R")
nll <- function(Sigma){ #Sigma[1] og Sigma[2].  Sigma <- array(c(Sigma1,Sigma2),dim=c(1,1,2))
  kl <- kalman_rem_out(Sal,A=A,B=B,u=NULL,C=C,Sigma.1=matrix(Sigma[1]),Sigma.2=matrix(Sigma[2]),debug=FALSE,V0=matrix(Sigma[1]),Xhat0=Sal[1],n.ahead=1,skip=0,verbose=TRUE)
  #using C is single index then
  kl$Outliers
  N <- length(Sal)
  R <- na.omit( (c(C)*kl$Sigma.xx.pred[1,1,]*c(C)+c(matrix(Sigma[2])))[1:N]+0*(Sal[1:N]-kl$pred[1:N]) )
  Y_tilde <- na.omit(Sal[1:N]-kl$pred[1:N])
  
  1/2*sum(log(R)+Y_tilde*1/R*Y_tilde)
}
fit <- optim(c(Sigma1,Sigma2), nll,method="L-BFGS-B" , lower = 0.00005) #indsæt lower bound. #method="L-BFGS-B"
fit$convergence
fit



source("kalman_rem_out.R")
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


