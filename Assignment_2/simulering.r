#simulering 
p <- 2 # AR order
q <- 3# MA order

theta <- c(-0.8)
phi <- c(0.8,-0.5)

cat("AR parameters:",round(theta,3))
cat("MA parameters:",round(phi,3))
n <- 200
colours = list("blue","red","green","orange","purple","black","magenta","pink","chartreuse","yellow")
set.seed(1213)
y = matrix(0,10,200)
# plotting realizations
for (i in 1:10) {
    # set.seed(i)
    ts <- arima.sim(list(ar=theta,ma=phi), n, sd=0.4)
    y[i,] <- ts[1:200]
    t = c(1:200)
    if (i == 1) {
    plot(t,y[i,],type="l",col=toString(colours[i]),xlab="t",ylab="X_t")
    }
    else {
    lines(t,y[i,],type="l",col=toString(colours[i]))  
    }
}

#plotting acf and pacf
for (i in 1:10) {
    ACF <- acf(y[i,],main="ACF",plot = FALSE) 
    if (i == 1) {
    plot(ACF$lag,ACF$acf,type="h",col=toString(colours[i]),xlab="lag",ylab="ACF")
    #lines(ACF$acf, ci.type="white",col=toString(colours[i]))
    }
    else {
    lines(ACF$lag,ACF$acf,type="h",col=toString(colours[i]))  
    # lines(ACF, ci.type="white",col=toString(colours[i]))
    }

}
#confidence intervals:
ci = 0.95
abline(h=qnorm((1 + ci)/2)/sqrt(n),lty=2)
abline(h=-qnorm((1 + ci)/2)/sqrt(n),lty=2)
abline(h = 0)

for (i in 1:10) {
    PACF <- pacf(y[i,],main="ACF",plot = FALSE) 
    if (i == 1) {
    plot(PACF$lag,PACF$acf,type="h",col=toString(colours[i]),xlab="lag",ylab="PACF")
    }
    else {
    lines(PACF$lag,PACF$acf,type="h",col=toString(colours[i]))  
    }

}
abline(h=qnorm((1 + ci)/2)/sqrt(n),lty=2)
abline(h=-qnorm((1 + ci)/2)/sqrt(n),lty=2)
abline(h = 0)
