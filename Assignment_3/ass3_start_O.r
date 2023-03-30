install.packages("marima")
#Data <- read.csv("Time_Series_2023/Assignment_3/A3Data.csv",header=TRUE)
#Data <-read.csv("/Users/OscarBP/Documents/5. DTU noter/Semester 6/02417 Time Series Analysis/Time_Series_2023/Assignment_3/A3Data.csv",header=TRUE)
Data <- read.csv("A3Data.csv",header=TRUE)
str(Data)
#Q1Q1Q1Q1Q1Q1Q1Q1Q1Q1Q1Q1Q1Q1Q1Q1Q1Q1Q1Q1Q1Q1Q1Q1Q1Q1
#Plot the quarterly average sales prices in Denmark and additional variables

pricesDK <- na.omit(Data$Denmark)
interest <- na.omit(Data$InterestRate) #interest rate
inflation <- na.omit(Data$InflationRate) #interest rate

exp_pricesDK <- exp(pricesDK)
exp_interest <- exp(interest)
exp_inflation <- exp(inflation)

log_pricesDK <- log(pricesDK)
log_interest <- log(interest)
log_inflation <- log(inflation)

log_diff_pricesDK <- log(diff(pricesDK)-min(diff(pricesDK))+1)
log_diff_interest <- log(diff(interest)-min(diff(interest))+1)
log_diff_inflation <- log(diff(inflation)-min(diff(inflation))+1)

diff_pricesDK <- diff(pricesDK)
diff_interest <- diff(interest)
diff_inflation <- diff(inflation)

exp_diff_pricesDK <- exp(diff(pricesDK))
exp_diff_interest <- exp(diff(interest))
exp_diff_inflation <- exp(diff(inflation))


multipleplots <- function(data){
  par(mfrow=c(2,2))
  plot(data)
  acf(data)
  pacf(data)
}

multipleplots(pricesDK)
multipleplots(interest)
multipleplots(inflation) # ser fin ud!

#multipleplots(exp_pricesDK) BAD
multipleplots(exp_interest) #could work
multipleplots(exp_inflation) #could work

multipleplots(log_pricesDK)
#multipleplots(log_interest) #BAD
multipleplots(log_inflation) #could work

multipleplots(diff_pricesDK)
multipleplots(diff_interest)
multipleplots(diff_inflation) #GOOD

#multipleplots(exp_diff_pricesDK) #BAD
multipleplots(exp_diff_interest) #Gør ikke mere godt end bare diff
multipleplots(exp_diff_inflation)

multipleplots(log_diff_pricesDK)
multipleplots(log_diff_interest)
multipleplots(log_diff_inflation)

#Transform the data to make it stationary!
#finaldata: 

#pricesDK
multipleplots(diff_pricesDK)

#Intersest
multipleplots(exp_interest) #could work
multipleplots(diff_interest)

#Inflation
multipleplots(inflation)

#Q4.2
#Estimate and plot Cross-Correlation
Capital <- diff(na.omit(Data$Capital))
Sealand <- diff(na.omit(Data$Sealand))
MidJutland <- diff(na.omit(Data$Capital))
Rural <- diff(na.omit(Data$Rural))

prices <- data.frame(Capital=Capital,Sealand=Sealand,MidJutland=MidJutland,Rural=Rural)
colnames(prices) <- c("Capital","Sealand","MidJutland","Rural")
prices

par(mfrow=c(2,3))
for (i in 1:4){
  for (k in 1:4){
    if (i!=k){
      ccf(prices[i],prices[k],type="correlation",plot=TRUE,main= paste(colnames(prices)[i]," & ",colnames(prices)[k]))
    }
  }
}
# De ligner alle sammen hinanden. # så bare glem det :)

#ACF and PACF
multipleplots(diff(pricesDK))
pricesDK <- log(pricesDK)
# Make arima model
fit1 <- arima(pricesDK,   order = c(2,1,1)  ,  seasonal = list(order = c(5,0,3),period=4),include.mean=FALSE)
fit1
fit2 <- arima(pricesDK,   order = c(0,1,0)  ,  seasonal = list(order = c(1,0,3),period=4),include.mean=FALSE)
fit2
fit3 <- arima(pricesDK,   order = c(0,1,0)  ,  seasonal = list(order = c(0,0,3),period=4),include.mean=FALSE)
fit3
fit4 <- arima(pricesDK,   order = c(0,1,0)  ,  seasonal = list(order = c(2,0,1),period=4),include.mean=FALSE)
fit4
fit5 <- arima(pricesDK,   order = c(0,1,0)  ,  seasonal = list(order = c(2,0,0),period=4),include.mean=FALSE)
fit5
fit6 <- arima(pricesDK,   order = c(0,1,0)  ,  seasonal = list(order = c(1,0,1),period=4),include.mean=FALSE)
fit6
fit7 <- arima(pricesDK,   order = c(0,1,0)  ,  seasonal = list(order = c(1,0,0),period=4),include.mean=FALSE)
fit7$aic
fit8 <- arima(pricesDK,   order = c(0,1,0)  ,  seasonal = list(order = c(0,0,1),period=4),include.mean=FALSE)
fit8$aic
fit9 <- arima(pricesDK,   order = c(0,1,0)  ,  seasonal = list(order = c(0,0,0),period=4),include.mean=FALSE)
fit9$aic

predict(fit9,c(1:20))
plot(c(1:20),predict(fit9,c(1:20)))
fit1$aic
fit2$aic
fit3$aic
fit4$aic
fit5$aic
fit6$aic
fit7$aic
fit8$aic
fit9$aic

plot(fit8)
plot(fit9)

fit6 <- arima(pricesDK,   order = c(0,1,0)  ,  seasonal = list(order = c(1,0,1),period=4),include.mean=FALSE)
fit6$residuals #aic = -537.9863

fit8 <- arima(pricesDK,   order = c(0,1,0)  ,  seasonal = list(order = c(0,0,1),period=2),include.mean=FALSE)
fit8$residuals # aic = 1294.645

exp(diff(log(fit8$residuals)))

sum(sign(fit8$residuals[2:length(fit8$residuals)]*fit8$residuals[1:length(fit8$residuals)-1])==-1)


fit8 <- arima(pricesDK,   order = c(1,1,0)  ,  seasonal = list(order = c(1,1,1),period=2),include.mean=FALSE)
fit8$residuals # aic = 1294.645
sum(sign(fit8$residuals[2:length(fit8$residuals)]*fit8$residuals[1:length(fit8$residuals)-1])==-1)

fit1 <- lm(log(pricesDK[1:50]) ~ interest[1:50]^2)
plot(fit1)
exp(diff(log()))

#Q4.6
library(mltools)
library(data.table)

set.seed(1)
nn <- 140000
x1 <- inflation[1:122]
x2 <- interest[1:122]
y  <- numeric(nn); y[1] <- 0
eta  <- numeric(nn); eta[1] <- 0

y <- pricesDK[1:122]

Y <- cumsum(pricesDK)
Y12 <- diffinv(y,12)[-(1:12)]
Y_1_12 <-cumsum(diffinv(y,12)[-(1:12)])
(test1 <- arima(y,order=c(1,1,0),period=2,xreg=cbind(x1,x2)))
(test2 <- arima(Y,order=c(1,1,0),xreg=cbind(cumsum(x1),cumsum(x2))))
(test3 <- arima(Y12,order=c(1,0,0),seasonal=list(order=c(0,1,0),period=12),xreg=cbind(diffinv(x1,12)[-(1:12)],diffinv(x2,12)[-(1:12)])))

(testWRONG <- arima(Y,order=c(1,1,0),xreg=cbind(x1,x2)))
(test4 <- arima(Y_1_12,order=c(1,1,0),seasonal=list(order=c(0,1,0),period=12),xreg=cbind(cumsum(diffinv(x1,12)[-(1:12)]),cumsum(diffinv(x2,12)[-(1:12)]))))


