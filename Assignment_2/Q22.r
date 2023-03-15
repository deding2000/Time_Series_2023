library(forecast)
rm(list = ls())
#df <- read.table("A2_sales.txt", header = TRUE)
df <- read.table("Time_Series_2023/Assignment_2/A2_sales.txt", header = TRUE)
plot(df$Sales)
mu <- 2070

ts_sales <- ts((df$Sales-mu), freq = 4) # ts object and transform
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


model <- arima(ts_sales ,order=c(p,d,q),seasonal = list(order=c(P,D,Q),period=Period, fixed=c(phi=phi,
                       Phi=Phi, Theta=Theta)))
pred <- forecast(model,2, level = 95)
plot(df$Sales , xlim = c(1,22), col = "black", type = "l")

points(c(21,22), pred$mean +mu, col = "red")
#plot( c(df$Sales, pred$mean +mu), col = c(rep("black",20),rep("red",2)))

pred$upper # prediction intervals ?
pred$lower

lines(ts(pred$fitted+mu,freq = 1), col = "blue") # predicted values of Yt

library("plotrix")   
plotCI(x = c(21,22), 
       y = pred$mean+mu,
       li = pred$lower+mu,
       ui = pred$upper+mu,xlim = c(1,22),ylim = c(1700,3000),col = "red")
lines(df$Sales , col = "black", type = "l")
lines(ts(pred$fitted+mu,freq = 1), col = "blue") # predicted values of Yt
legend("bottomright", legend = c("Prediction with 95% CI","Data","Fitted"),lwd = 3, col = c("red","black","blue"))







#Samme ting bare med fixed mu.
rm(list = ls())
df <- read.table("A2_sales.txt", header = TRUE)
#df <- read.table("Time_Series_2023/Assignment_2/A2_sales.txt", header = TRUE)
plot(df$Sales)
mu <- 2070

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
legend("bottomright", legend = c("Prediction with 95% CI","Data","Fitted"),lwd = 3, col = c("red","black","blue"))




#Alternativt plot med predictions
library(forecast)
fit <- model
Nile <- c(ts_sales)
upper <- fitted(fit) + 1.96*sqrt(fit$sigma2)
lower <- fitted(fit) - 1.96*sqrt(fit$sigma2)
plot(df$Sales, type="n", ylim=range(lower,upper))
polygon(c(time(Nile),rev(time(Nile))), c(upper,rev(lower)), 
        col=rgb(0,0,0.6,0.2), border=FALSE)
#NIlines(Nile)
lines(c(fitted(fit)),col='red')
#out <- (Nile < lower | Nile > upper)
#points(time(Nile)[out], Nile[out], pch=19)
lines(c(1:20),df$Sales)




#Trying to make predictions analytically
#isolating Y_t and then taking the conditional expectation.
#For 2.2.1 wouldn't we need to calculate the model-predictions of all the previous Y_t's before we can predict? (Since the prediction depends on eps_{t-4}) 
#Lad t=1 og t=1..20 være kendt.
Y<-c(0,0,0,0,0,0,0,df$Sales-mu)

phi <- rev(c(1.04,-0.2,0,0.86,-0.8944,0.172))
theta <- c(1,0,0,0,-0.42)

pred_Y <- c(1:21)*0
length(pred_Y)
eps <- c(1:25)*0
k <- 5
n <- 7

for (t in 0:20) {
  eps[t+k] <- Y[t+n]+(-1)*phi%*%Y[(n+t-6):(t-1+n)]+theta[5]*eps[t-4+k]
  pred_Y[t+1] <- phi%*%Y[(n+t-5):(t+n)]+theta[5]*eps[t+k-3]
}


Yp <- df$Sales-mu



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

lines(c(1:21),pred_Y+mu,col="red")

sum((c(pred$fitted)-df$Sales)^2)
sum((pred_Y[1:20]-df$Sales)^2)
