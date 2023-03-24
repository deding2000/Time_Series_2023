library(forecast)
rm(list = ls())
df <- read.table("A2_sales.txt", header = TRUE)
#df <- read.table("Time_Series_2023/Assignment_2/A2_sales.txt", header = TRUE)
plot(df$Sales)
mu <- 2070

#ts_sales <- ts((df$Sales-mu), freq = 4) # ts object and transform
#phi <- c(1.04,-0.2)
#Phi <- c(0.86)
#Theta <- c(-0.42)

#p <- 2
#q <- 0
#d <- 0

#D <- 0
#P <- 1
#Q <- 1

#Period = 4


#model <- arima(ts_sales ,order=c(p,d,q),seasonal = list(order=c(P,D,Q),period=Period, fixed=c(phi=phi,
 #                      Phi=Phi, Theta=Theta)))
#pred <- forecast(model,2, level = 95)
#plot(df$Sales , xlim = c(1,22), col = "black", type = "l")

#points(c(21,22), pred$mean +mu, col = "red")
#plot( c(df$Sales, pred$mean +mu), col = c(rep("black",20),rep("red",2)))

#pred$upper # prediction intervals ?
#pred$lower

#lines(ts(pred$fitted+mu,freq = 1), col = "blue") # predicted values of Yt

#library("plotrix")   
#plotCI(x = c(21,22), 
#       y = pred$mean+mu,
#       li = pred$lower+mu,
#       ui = pred$upper+mu,xlim = c(1,22),ylim = c(1700,3000),col = "red")
#lines(df$Sales , col = "black", type = "l")
#lines(ts(pred$fitted+mu,freq = 1), col = "blue") # predicted values of Yt
#legend("bottomright", legend = c("Pred 95% CI in R","Data","Fitted with R"),lwd = 3, col = c("red","black","blue"))


#y = pred$mean+mu
#li = pred$lower+mu
#ui = pred$upper+mu


#Samme ting bare med fixed mu.
ts_sales <- ts((df$Sales), freq = 4) # ts object and transform
phi <- c(1.04,-0.2)
Phi <- c(0.86)
Theta <- c(-0.42)

p <- 2
q <- 0
d <- 0

D <- 0
P <- 1
Q <- 1

Period = 4


model <- arima(ts_sales ,order=c(p,d,q),seasonal = list(order=c(P,D,Q),period=Period,include.mean=TRUE ,fixed=c(phi=phi,
                                                                                              Phi=Phi, Theta=Theta,mean=mu)))
pred <- forecast(model,2, level = 95)
plot(df$Sales , xlim = c(1,22), col = "black", type = "l")

points(c(21,22), pred$mean, col = "red")
#plot( c(df$Sales, pred$mean +mu), col = c(rep("black",20),rep("red",2)))

pred$upper # prediction intervals ?
pred$lower

lines(ts(pred$fitted,freq = 1), col = "blue") # predicted values of Yt

library("plotrix")   
plotCI(x = c(21,22), 
       y = pred$mean,
       li = pred$lower,
       ui = pred$upper,xlim = c(1,22),ylim = c(1700,3000),col = "red")
lines(df$Sales , col = "black", type = "l")
lines(ts(pred$fitted,freq = 1), col = "blue") # predicted values of Yt
legend("bottomright", legend = c("Pred 95% CI in R","Data","Fitted with R"),lwd = 3, col = c("red","black","blue"))

y = pred$mean
li = pred$lower
ui = pred$upper


#Alternativt plot med predictions
library(forecast)
fit <- model
Nile <- c(ts_sales,y)
upper <- c(fitted(fit) + 1.96*sqrt(fit$sigma2),ui)
lower <- c(fitted(fit) - 1.96*sqrt(fit$sigma2),li)
plot(df$Sales, type="n", xlim=c(1,22),ylim=range(lower,upper))
polygon(c(time(Nile),rev(time(Nile))), c(upper,rev(lower)), 
        col=rgb(0,0,0.6,0.2), border=FALSE)
#NIlines(Nile)
lines(c(fitted(fit)),col='red')
points(c(21,22),y,col='red')
#out <- (Nile < lower | Nile > upper)
#points(time(Nile)[out], Nile[out], pch=19)
lines(c(1:20),df$Sales)
legend("bottomright", legend = c("Pred w. 95% CI in R","Data"),lwd = 3, col = c("red","black"))


#Trying to make predictions analytically
#isolating Y_t and then taking the conditional expectation.
#For 2.2.1 wouldn't we need to calculate the model-predictions of all the previous Y_t's before we can predict? (Since the prediction depends on eps_{t-4}) 
#Lad t=1 og t=1..20 være kendt.
Y<-c(0,0,0,0,0,0,0,df$Sales-mu)

phi <- rev(c(1.04,-0.2,0,0.86,-0.8944,0.172))
theta <- c(1,0,0,0,-0.42)

pred_Y <- c(1:22)*0
length(pred_Y)
eps <- c(1:25)*0
k <- 5
n <- 7


for (t in 0:20) {
  eps[t+k] <- Y[t+n]+(-1)*phi%*%Y[(n+t-6):(t-1+n)]+theta[5]*eps[t-4+k]
  pred_Y[t+1] <- phi%*%Y[(n+t-5):(t+n)]+theta[5]*eps[t+k-3]
}
#predicting 22

Y<-c(Y,pred_Y[21])
t <- 21
pred_Y[22] <- phi%*%Y[(n+t-5):(t+n)]+theta[5]*eps[t+k-3]

eps <- eps[6:25]
pred_Y<-pred_Y+mu

#find Psi_1 for 2-step prediction remember to say how you found it :)
Psi_1 <- 1.04
sigma_eps2 <- 36963
a <- 0.05
pred_Y
(pred_Y_upper <- pred_Y[1:21]-qnorm(a/2)*sqrt(sigma_eps2))
(pred_Y_lower <- pred_Y[1:21]+qnorm(a/2)*sqrt(sigma_eps2))
pred_y_upper <- c(pred_Y_upper,pred_Y[22]-qnorm(a/2)*sqrt(sigma_eps2)*sqrt(1+1.04^2))
pred_y_lower <- c(pred_Y_lower,pred_Y[22]+qnorm(a/2)*sqrt(sigma_eps2)*sqrt(1+1.04^2))

#Alternativt plot med predictions
library(forecast)
Nile <- c(pred_Y)
upper <- pred_y_upper
lower <- pred_y_lower
plot(c(df$Sales),xlim=c(1,22), ylim = range(lower,upper))
polygon(c(time(Nile),rev(time(Nile))), c(upper,rev(lower)), 
        col=rgb(0,0,0.6,0.2), border=FALSE)
#NIlines(Nile)
lines(pred_Y[1:20],col='red')
#out <- (Nile < lower | Nile > upper)
#points(time(Nile)[out], Nile[out], pch=19)
lines(c(1:20),df$Sales)
points(c(21,22),pred_Y[21:22],col='red')
legend("bottomright", legend = c("Pred w. 95% CI analytical","Data"),lwd = 3, col = c("red","black"))

saveFig <- TRUE
if(saveFig == TRUE){pdf("Q22PredictionPlot.pdf", width = 10*0.8, height = 6*0.8)}
library(forecast)
Nile <- c(pred_Y)
upper <- pred_y_upper
lower <- pred_y_lower
Sales <- c(df$Sales,0,0)
Quarter <- c(df$Quarter,"2019K1","2019K2")
plot(Sales,type="n",xlim=c(1,22), ylim = range(lower,upper), xlab="Quarters")
polygon(c(time(Nile),rev(time(Nile))), c(upper,rev(lower)), 
        col=rgb(0,0,0.6,0.2), border=FALSE)
#NIlines(Nile)
lines(pred_Y[1:20],col='red')
#out <- (Nile < lower | Nile > upper)
#points(time(Nile)[out], Nile[out], pch=19)
lines(c(1:20),df$Sales)
points(c(21,22),pred_Y[21:22],col='red')
legend("bottomright", legend = c("Pred w. 95% CI analytical","Data"),lwd = 3, col = c("red","black"))

if(saveFig == TRUE){dev.off()}
saveFig <- FALSE

sum((c(pred$fitted)-df$Sales)^2)
sum((pred_Y[1:20]-df$Sales)^2)

c(pred_Y[21],pred_y_lower[21],pred_y_upper[21])
c(pred_Y[22],pred_y_lower[22],pred_y_upper[22])

#Checking values
Yp <- df$Sales-mu
k <- 0
#Check epsilon - IT IS GOOD!!!
(eps[1+k])
(eps_1 <- (df$Sales-mu)[1])
(eps[2+k])
(eps_2<-Yp[2]-1.04*Yp[1])
(eps[3+k])
(eps_3<-Yp[3]-1.04*Yp[2]+0.2*Yp[1])
(eps[4+k])
(eps_4<-Yp[4]-1.04*Yp[3]+0.2*Yp[2]+0*Yp[1])
(eps[5+k])
(eps_5<-Yp[5]-1.04*Yp[4]+0.2*Yp[3]+0*Yp[2]-0.86*Yp[1]-0.42*eps_1)
(eps[6+k])
(eps_6<-Yp[6]-1.04*Yp[5]+0.2*Yp[4]+0*Yp[3]-0.86*Yp[2]+0.8944*Yp[1]-0.42*eps_2)
(eps[7+k])
(eps_7<-Yp[7]-1.04*Yp[6]+0.2*Yp[5]+0*Yp[4]-0.86*Yp[3]+0.8944*Yp[2]-0.172*Yp[1]-0.42*eps_3)
(eps[8+k])
(eps_8<-Yp[8]-1.04*Yp[7]+0.2*Yp[6]+0*Yp[5]-0.86*Yp[4]+0.8944*Yp[3]-0.172*Yp[2]-0.42*eps_4)
(eps[12+k])
(eps_12<-Yp[12]-1.04*Yp[11]+0.2*Yp[10]+0*Yp[9]-0.86*Yp[8]+0.8944*Yp[7]-0.172*Yp[6]-0.42*eps_8)
#Check pred_Y - IT IS GOOD!!!
(pred_Y[1])
(pred_Y_1 <- 0) #eps[-3]
(pred_Y[2])
(pred_Y_2 <- 1.04*Yp[1]) #eps[-2]
(pred_Y[3])
(pred_Y_3 <- 1.04*Yp[2]-0.2*Yp[1]) #eps[-1]
(pred_Y[4])
(pred_Y_4 <- 1.04*Yp[3]-0.2*Yp[2]) #eps[0]
(pred_Y[5])
(pred_Y_5 <- 1.04*Yp[4]-0.2*Yp[3]+0*Y[2]+0.86*Yp[1] - 0.42*eps_1) 
(pred_Y[6])
(pred_Y_6 <- 1.04*Yp[5]-0.2*Yp[4]+0*Y[3]+0.86*Yp[2]-0.8944*Yp[1]- 0.42*eps_2)
(pred_Y[7])
(pred_Y_7 <- 1.04*Yp[6]-0.2*Yp[5]+0*Y[4]+0.86*Yp[3]-0.8944*Yp[2]+0.172*Yp[1]- 0.42*eps_3)
(pred_Y[16])
(pred_Y_16 <- 1.04*Yp[15]-0.2*Yp[14]+0*Y[13]+0.86*Yp[12]-0.8944*Yp[11]+0.172*Yp[10]- 0.42*eps_12)

k <- 5
#Check pred 21 and pred 22
pred_Yeps_17 <- eps[17+k]
t <- 20
eps_18 <- eps[18+k]
(phi%*%Y[(n+t-5):(t+n)]+theta[5]*eps[t+k-3])
(Yp[21] <- 1.04*Yp[20]-0.2*Yp[19]+0*Y[18]+0.86*Yp[17]-0.8944*Yp[16]+0.172*Yp[15] - 0.42*eps_17)
Y[28]<-pred_Y[21]
t<-21
(phi%*%Y[(n+t-5):(t+n)]+theta[5]*eps[t+k-3])
(Yp[22] <- 1.04*Yp[21]-0.2*Yp[20]+0*Y[19]+0.86*Yp[18]-0.8944*Yp[17]+0.172*Yp[16] - 0.42*eps_18)


