library(forecast)

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

