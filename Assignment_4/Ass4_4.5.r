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
Sigma <- c(Sigma1,Sigma2)

#ML estimate
kl <- kalman(Sal,A=A,B=B,u=NULL,C=C,Sigma.1=matrix(Sigma[1]),Sigma.2=matrix(Sigma[2]),debug=FALSE,V0=matrix(Sigma[1]),Xhat0=Sal[1],n.ahead=1,skip=0,verbose=TRUE)
#using C is single index then
print(kl$Outliers)
N <- length(Sal)
R <- na.omit( (c(C)*kl$Sigma.xx.pred[1,1,]*c(C)+c(matrix(Sigma[2])))[1:N]+0*(Sal[1:N]-kl$pred[1:N]) )
Y_tilde <- na.omit(Sal[1:N]-kl$pred[1:N])

1/2*sum(log(R+Y_tilde*1/R*Y_tilde))

#hvad gør man med NA values her så?????

source("kalman.R")
source("kalman_rem_out.R")
LowerBound <- 0.0005^2

nll1 <- function(Sigma){ #Sigma[1] og Sigma[2].  Sigma <- array(c(Sigma1,Sigma2),dim=c(1,1,2))
  kl <- kalman(Sal,A=A,B=B,u=NULL,C=C,Sigma.1=matrix(Sigma[1]),Sigma.2=matrix(Sigma[2]),debug=FALSE,V0=matrix(Sigma[1]),Xhat0=Sal[1],n.ahead=1,skip=0,verbose=TRUE)
  #using C is single index then
  N <- length(Sal)
  R <- na.omit( (c(C)*kl$Sigma.xx.pred[1,1,]*c(C)+c(matrix(Sigma[2])))[1:N]+0*(Sal[1:N]-kl$pred[1:N]) )
  Y_tilde <- na.omit(Sal[1:N]-kl$pred[1:N])
  
  1/2*sum(log(R)+Y_tilde*1/R*Y_tilde)
}
fit1 <- optim(c(Sigma1,Sigma2), nll1,method="L-BFGS-B" , lower = LowerBound) #indsæt lower bound. #method="L-BFGS-B"
fit$convergence
(sigma_new <- fit1$par)
fit1
new_kl1 <- kalman(Sal,A=A,B=B,u=NULL,C=C,Sigma.1=matrix(sigma_new[1]),Sigma.2=matrix(sigma_new[2]),debug=FALSE,V0=matrix(sigma_new[1]),Xhat0=Sal[1],n.ahead=1,skip=0,verbose=TRUE)

nll2 <- function(Sigma){ #Sigma[1] og Sigma[2].  Sigma <- array(c(Sigma1,Sigma2),dim=c(1,1,2))
  kl <- kalman_rem_out(Sal,A=A,B=B,u=NULL,C=C,Sigma.1=matrix(Sigma[1]),Sigma.2=matrix(Sigma[2]),debug=FALSE,V0=matrix(Sigma[1]),Xhat0=Sal[1],n.ahead=1,skip=0,verbose=TRUE)
  N <- length(Sal)
  R <- na.omit( (c(C)*kl$Sigma.xx.pred[1,1,]*c(C)+c(matrix(Sigma[2])))[1:N]+0*(Sal[1:N]-kl$pred[1:N]) )
  Y_tilde <- na.omit(Sal[1:N]-kl$pred[1:N])
  1/2*sum(log(R)+Y_tilde*1/R*Y_tilde)
}
fit2 <- optim(c(Sigma1,Sigma2), nll2,method="L-BFGS-B" , lower = LowerBound) #indsæt lower bound. #method="L-BFGS-B"
fit2$convergence
(sigma_new <- fit2$par)
fit

kltest <- kalman_rem_out(Sal,A=A,B=B,u=NULL,C=C,Sigma.1=matrix(Sigma[1]),Sigma.2=matrix(Sigma[2]),debug=FALSE,V0=matrix(Sigma[1]),Xhat0=Sal[1],n.ahead=1,skip=0,verbose=TRUE)
sum(kltest$Outliers)
kltest <- kalman_rem_out(Sal[1:800],A=A,B=B,u=NULL,C=C,Sigma.1=matrix(Sigma[1]),Sigma.2=matrix(Sigma[2]),debug=FALSE,V0=matrix(Sigma[1]),Xhat0=Sal[1],n.ahead=1,skip=0,verbose=TRUE)
sum(kltest$Outliers)

new_k2l <- kalman_rem_out(Sal,A=A,B=B,u=NULL,C=C,Sigma.1=matrix(sigma_new[1]),Sigma.2=matrix(sigma_new[2]),debug=FALSE,V0=matrix(sigma_new[1]),Xhat0=Sal[1],n.ahead=1,skip=0,verbose=TRUE)
sum(new_k2l$Outliers) #Outliers


#NOW for 1:800
nll800 <- function(Sigma){ #Sigma[1] og Sigma[2].  Sigma <- array(c(Sigma1,Sigma2),dim=c(1,1,2))
  kl <- kalman_rem_out(Sal[1:800],A=A,B=B,u=NULL,C=C,Sigma.1=matrix(Sigma[1]),Sigma.2=matrix(Sigma[2]),debug=FALSE,V0=matrix(Sigma[1]),Xhat0=Sal[1],n.ahead=1,skip=0,verbose=TRUE)
  N <- length(Sal[1:800])
  R <- na.omit( (c(C)*kl$Sigma.xx.pred[1,1,]*c(C)+c(matrix(Sigma[2])))[1:N]+0*(Sal[1:N]-kl$pred[1:N]) )
  Y_tilde <- na.omit(Sal[1:N]-kl$pred[1:N])
  1/2*sum(log(R)+Y_tilde*1/R*Y_tilde)
}
fit800 <- optim(c(Sigma1,Sigma2), nll800,method="L-BFGS-B" , lower = LowerBound) #indsæt lower bound. #method="L-BFGS-B"
fit800$convergence
(sigma_new <- fit800$par)
fit

kl800 <- kalman_rem_out(Sal,A=A,B=B,u=NULL,C=C,Sigma.1=matrix(sigma_new[1]),Sigma.2=matrix(sigma_new[2]),debug=FALSE,V0=matrix(sigma_new[1]),Xhat0=Sal[1],n.ahead=1,skip=0,verbose=TRUE)

interval = c(800:950)
#3. Plot for 800-950
plot(interval,Sal[interval],type="p",,col="red",xlab="Index",ylab="Water Salinity [PSU] = [g/kg]",ylim=c(15,24))
matlines(interval,kl800$pred[interval,1]+sqrt(kl800$Sigma.yy.pred[1,1,interval])%*%cbind(0,-1.96,1.96),col="seagreen",lty=c(1,2,2), lwd=1)
legend("bottomright",legend=c("Observations","1-step prediciions"),col=c("tomato","seagreen"),pch=c(20,NA),bty='n',lty=c(NA,1))

errors_norm <- na.omit((kl800$pred[interval,1]-Sal[interval]) / sqrt(kl800$Sigma.yy.pred[interval]) )
#MAKE PLOT!!!
plot(interval,errors_norm,ylab="Errors normalized",type='l',xlab="Index")



#For 800 using standard KALMAN:::


#NOW for 1:800
nll800 <- function(Sigma){ #Sigma[1] og Sigma[2].  Sigma <- array(c(Sigma1,Sigma2),dim=c(1,1,2))
  kl <- kalman(Sal[1:800],A=A,B=B,u=NULL,C=C,Sigma.1=matrix(Sigma[1]),Sigma.2=matrix(Sigma[2]),debug=FALSE,V0=matrix(Sigma[1]),Xhat0=Sal[1],n.ahead=1,skip=0,verbose=TRUE)
  N <- length(Sal[1:800])
  R <- na.omit( (c(C)*kl$Sigma.xx.pred[1,1,]*c(C)+c(matrix(Sigma[2])))[1:N]+0*(Sal[1:N]-kl$pred[1:N]) )
  Y_tilde <- na.omit(Sal[1:N]-kl$pred[1:N])
  1/2*sum(log(R)+Y_tilde*1/R*Y_tilde)
}
fit800_standard <- optim(c(Sigma1,Sigma2), nll800,method="L-BFGS-B" , lower = LowerBound) #indsæt lower bound. #method="L-BFGS-B"
fit800_standard$convergence
(sigma_new <- fit800_standard$par)
fit


kl800_standard <- kalman(Sal,A=A,B=B,u=NULL,C=C,Sigma.1=matrix(sigma_new[1]),Sigma.2=matrix(sigma_new[2]),debug=FALSE,V0=matrix(sigma_new[1]),Xhat0=Sal[1],n.ahead=1,skip=0,verbose=TRUE)

interval = c(800:950)
#3. Plot for 800-950
plot(interval,Sal[interval],type="p",,col="red",xlab="Index",ylab="Water Salinity [PSU] = [g/kg]",ylim=c(15,24))
matlines(interval,kl800_standard$pred[interval,1]+sqrt(kl800_standard$Sigma.yy.pred[1,1,interval])%*%cbind(0,-1.96,1.96),col="seagreen",lty=c(1,2,2), lwd=1)
legend("bottomright",legend=c("Observations","1-step prediciions"),col=c("tomato","seagreen"),pch=c(20,NA),bty='n',lty=c(NA,1))

errors_norm <- na.omit((kl800_standard$pred[interval,1]-Sal[interval]) / sqrt(kl800_standard$Sigma.yy.pred[interval]) )
#MAKE PLOT!!!
plot(interval,errors_norm,ylab="Errors normalized",type='l',xlab="Index")



fit1$par #kalman
fit2$par #kalman_rem_out
fit800_standard$par
fit800$par

kl1test <- kalman(Sal[1:800],A=A,B=B,u=NULL,C=C,Sigma.1=matrix(Sigma[1]),Sigma.2=matrix(Sigma[2]),debug=FALSE,V0=matrix(Sigma[1]),Xhat0=Sal[1],n.ahead=1,skip=0,verbose=TRUE)
kl2test <- kalman_rem_out(Sal[1:800],A=A,B=B,u=NULL,C=C,Sigma.1=matrix(Sigma[1]),Sigma.2=matrix(Sigma[2]),debug=FALSE,V0=matrix(Sigma[1]),Xhat0=Sal[1],n.ahead=1,skip=0,verbose=TRUE)
sum(kl2test$Outliers)

nlltest1 <- function(Sigma){ #Sigma[1] og Sigma[2].  Sigma <- array(c(Sigma1,Sigma2),dim=c(1,1,2))
  kl <- kalman(Sal[1:800],A=A,B=B,u=NULL,C=C,Sigma.1=matrix(Sigma[1]),Sigma.2=matrix(Sigma[2]),debug=FALSE,V0=matrix(Sigma[1]),Xhat0=Sal[1],n.ahead=1,skip=0,verbose=TRUE)
  N <- length(Sal[1:800])
  R <- na.omit( (c(C)*kl$Sigma.xx.pred[1,1,]*c(C)+c(matrix(Sigma[2])))[1:N]+0*(Sal[1:N]-kl$pred[1:N]) )
  Y_tilde <- na.omit(Sal[1:N]-kl$pred[1:N])
  1/2*sum(log(R)+Y_tilde*1/R*Y_tilde)
}
fit_test1 <- optim(c(Sigma1,Sigma2), nlltest1,method="L-BFGS-B" , lower = LowerBound) #indsæt lower bound. #method="L-BFGS-B"
fit_test1$convergence
fit_test1$par
fit

nlltest2 <- function(Sigma){ #Sigma[1] og Sigma[2].  Sigma <- array(c(Sigma1,Sigma2),dim=c(1,1,2))
  kl <- kalman_rem_out(Sal[1:800],A=A,B=B,u=NULL,C=C,Sigma.1=matrix(Sigma[1]),Sigma.2=matrix(Sigma[2]),debug=FALSE,V0=matrix(Sigma[1]),Xhat0=Sal[1],n.ahead=1,skip=0,verbose=TRUE)
  N <- length(Sal[1:800])
  R <- na.omit( (c(C)*kl$Sigma.xx.pred[1,1,]*c(C)+c(matrix(Sigma[2])))[1:N]+0*(Sal[1:N]-kl$pred[1:N]) )
  Y_tilde <- na.omit(Sal[1:N]-kl$pred[1:N])
  1/2*sum(log(R)+Y_tilde*1/R*Y_tilde)
}
fit_test2 <- optim(c(Sigma1,Sigma2), nlltest2,method="L-BFGS-B" , lower = LowerBound) #indsæt lower bound. #method="L-BFGS-B"
fit_test2$convergence

fit_test1$convergence
fit_test1$par
fit_test2$par

kl1 <- kalman_rem_out(Sal[1:800],A=A,B=B,u=NULL,C=C,Sigma.1=matrix(Sigma[1]),Sigma.2=matrix(Sigma[2]),debug=FALSE,V0=matrix(Sigma[1]),Xhat0=Sal[1],n.ahead=1,skip=0,verbose=TRUE)
kl2 <- kalman(Sal[1:800],A=A,B=B,u=NULL,C=C,Sigma.1=matrix(Sigma[1]),Sigma.2=matrix(Sigma[2]),debug=FALSE,V0=matrix(Sigma[1]),Xhat0=Sal[1],n.ahead=1,skip=0,verbose=TRUE)
sum(na.omit(!kl1$rec==kl2$rec))
sum(na.omit(!kl1$pred==kl2$pred))
sum(na.omit(!kl1$rec==kl2$rec))
sum(na.omit(!kl1$rec==kl2$rec))
sum(na.omit(!kl1$rec==kl2$rec))
sum(na.omit(!kl1$rec==kl2$rec))

