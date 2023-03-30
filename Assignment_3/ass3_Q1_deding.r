library(forecast)
library("plotrix")  
Data <- read.csv("Time_Series_2023/Assignment_3/A3Data.csv",header=TRUE)
N <- 122
pricesDK <- na.omit(Data$Denmark)
CPH <- na.omit(Data$Capital)
Rural <- na.omit(Data$Rural)
Sealand <- na.omit(Data$Sealand)
Mitjut <- na.omit(Data$MidJutland)

interest <- na.omit(Data$InterestRate)
inflation <- na.omit(Data$InflationRate)

plot(diff(log(pricesDK)))
plot(log(pricesDK))
plot(diff(CPH))
plot(Rural)
plot(Sealand)
plot(Mitjut)
plot((diff(interest)))
plot(inflation)

acf(diff(interest)))
plot(acf(inflation))
acf(diff(pricesDK))
pacf(diff(pricesDK))

#initial guess is an (0,1,0)-(2,0,3) season 2 model
# but ended up with a (1,1,0)-(1,1,1) season 2 model
model <- arima(log(pricesDK),order=c(1,1,0), seasonal = list(order = c(1,1,1), period = 2))

#residuals
res <- model$residuals
plot(res)
acf(res)
pacf(res)
qqnorm(res)
qqline(res)
hist(res)

#sign test
sum(abs(diff(res>0)))
# lower bound
low <- (N-1)/2 - 2*sqrt((N-1)/4)
high <- (N-1)/2 + 2*sqrt((N-1)/4)

acf(diff(CPH))
pacf(diff(CPH))

plot(pacf(exp(interest)))
plot(pacf(inflation))
plot(pacf(diff(pricesDK)))

# cross correlation
ccf(diff(CPH),diff(Rural),type="correlation")
ccf(diff(CPH),diff(Sealand),type="correlation")
ccf(diff(CPH),diff(Mitjut),type="correlation")
ccf(diff(Rural),diff(Sealand),type="correlation")
ccf(diff(Rural),diff(Mitjut),type="correlation")

#prediction (6 time steps ahead)
predictions_6 <-forecast(model,6,95)
preds <- predictions_6$mean
lower <- predictions_6$lower
upper <- predictions_6$upper
plotCI(x = c(123:128), 
       y = exp(preds[1:6]),
       li = exp(lower),
       ui = exp(upper),col = "red",xlim = c(1,128),ylim = c(500,3000))
lines(exp(predictions_6$x))

#Arima X (interest seems useless but inflation is nice)
x_inflation <- cumsum(inflation)
x_interest <- interest
x_inf_new <- cumsum(c(inflation[1:124],rep(inflation[124],4)))
new_model <- Arima(log(pricesDK),xreg = c((x_inflation[1:122])),order=c(1,1,0), seasonal = list(order = c(1,1,1), period = 2))
#residuals
res2 <- new_model$residuals
plot(res2)
acf(res2)
pacf(res2)
qqnorm(res2)
qqline(res2)
hist(res2)

sum(abs(diff(res2>0)))
# lower bound
low <- (N-1)/2 - 2*sqrt((N-1)/4)
high <- (N-1)/2 + 2*sqrt((N-1)/4)

#forecast
rep(inflation[124],4)
plot(c(inflation[1:124],rep(inflation[124],4)))
x_inf_new <- cumsum(c(inflation[1:124],rep(inflation[124],4)))
plot(diff(x_inf_new))
plot(inflation)
predictions_6_new <-forecast(new_model,level=95,xreg=x_inf_new[123:128])

preds2 <- predictions_6_new$mean
lower2 <- predictions_6_new$lower
upper2 <- predictions_6_new$upper
plotCI(x = c(123:128), 
       y = exp(preds2[1:6]),
       li = exp(lower2),
       ui = exp(upper2),col = "red",xlim = c(1,128),ylim = c(500,3000))
lines(exp(predictions_6_new$x))
