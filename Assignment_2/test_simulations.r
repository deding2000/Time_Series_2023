theta <- c(0.8) 
phi <- c(0.8,-0.5)
n <- 200
t = c(1:n)
set.seed(223)
ts_not_inv <- arima.sim(list(ar=theta,ma=phi), n, sd=0.4)
ts_inv <- arima.sim(list(ar=theta,ma=c(0.4,-0.5)), n, sd=0.4)
plot(t,ts_not_inv[1:n],type="l")
plot(t,ts_inv[1:n],type="l",col="blue")
