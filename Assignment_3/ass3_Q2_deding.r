library(forecast)
library("plotrix")  
library("marima")
source("Time_Series_2023/Assignment_3/res_plot_fnc.r")
source("Time_Series_2023/Assignment_3/data_loading.r")

##############################################################
# Q 4.9
par(mfrow=c(2,2))
plot(Years[1:N_prices],CPH, type = "l",col="red",main="Average CPH house prices",xlab = "Year",ylab ="Sales price per sqm")
plot(Years[1:N_prices],Rural, type = "l",col="blue",main="Average Rural house prices",xlab = "Year",ylab ="Sales price per sqm")
plot(Years[1:N_prices],Mitjut, type = "l",col="red",main="Average MidJutland house prices",xlab = "Year",ylab ="Sales price per sqm")
plot(Years[1:N_prices],Sealand, type = "l",col="red",main="Average Sealand house prices",xlab = "Year",ylab ="Sales price per sqm")

##############################################################
# Q 4.10
par(mfrow=c(2,2))
acf(diff(CPH))
acf(diff(Rural))
acf(diff(Mitjut))
acf(diff(Sealand))

par(mfrow=c(2,2))
pacf(CPH)
pacf(Rural)
pacf(Mitjut)
pacf(Sealand)




## Marima time
rural <- define.dif(log(Rural),difference=c(1,1))$dif.series
mitjut <- define.dif(log(Mitjut),difference=c(1,1))$dif.series
cph <- define.dif(log(CPH),difference=c(1,1))$dif.series
sealand <- define.dif(log(Sealand),difference=c(1,1))$dif.series
interest <- define.dif(sqrt(Interest),difference=c(1,1))$dif.series
inflation <- Inflation
mdata <- data.frame(t(cph),t(sealand),t(mitjut),t(rural),interest[1:122],inflation[1:122])

# estimated from acf and pacf plots
ar <- c(1)
ma <- c(2)
# right now CPH is not depending on the other regions
Model1 <- define.model(kvar=6, ar=ar, ma=ma, indep=c(5:6),no.dep=c(1,2,1,3,1,4))#no.dep=c(1,2,1,3,1,4))
Marima1 <- marima(mdata,means=1,
                  ar.pattern=Model1$ar.pattern, ma.pattern=Model1$ma.pattern,
                  Check=FALSE, Plot="none", penalty=1)

short.form(Marima1$ar.estimates, leading=FALSE) # print estimates
short.form(Marima1$ma.estimates, leading=FALSE)

Marima1$one.step
residuals <- Marima1$residuals
# kig på variabel x
x <- 5
res <- na.omit(residuals[1:122])
plot(res)
qqnorm(res)
qqline(res)
hist(res)
# sign test
sum(abs(diff(res>0)))
# lower bound
N <- 121 #?
low <- (N-1)/2 - 2*sqrt((N-1)/4)
high <- (N-1)/2 + 2*sqrt((N-1)/4)
