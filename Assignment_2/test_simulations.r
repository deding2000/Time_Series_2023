theta <- c(0.8) #changed from minus because of the way arima.sim works
phi <- c(0.8,-0.5)
ts <- arima.sim(list(ar=theta,ma=phi), n, sd=0.4)