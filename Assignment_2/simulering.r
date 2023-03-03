#simulering
p <- 1 # AR order
q <- 0# MA order

n <- 5000

theta <- runif(p,-1/p,1/p)
phi <- runif(q,-1/q,1/q)

Y <- arima.sim(list(ar=theta,ma=phi),n)

par(mfrow=c(1,2))

acf(Y,main="ACF")
pacf(Y,main="PACF")

cat("AR parameters:",round(theta,3))
cat("MA parameters:",round(phi,3))
