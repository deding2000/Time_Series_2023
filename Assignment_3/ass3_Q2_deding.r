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
acf(diff(log(CPH)))
acf(diff(log(Rural))
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
orgdata <- data.frame(t(log(CPH)),t(log(Sealand)),t(log(Mitjut)),t(log(Rural)),t(Interest[1:122]),Inflation[1:122])
mdata <- data.frame(t(cph),t(sealand),t(mitjut),t(rural),interest[1:122],inflation[1:122])

# estimated from acf and pacf plots
ar <- c(2)
ma <- c(4)
Model1 <- define.model(kvar=6, ar=ar, ma=ma, indep=c(6),rem.var=c(5))
#Model1 <- define.model(kvar=6, ar=ar, ma=ma, indep=c(6),rem.var=c(5),ar.rem=c(2,1,2,2,4,2,3,3,2),ma.rem=c(3,2,4,3,3,4,1,6,4))#no.dep=c(1,3,2,2,2,4))#no.dep=c(1,2,1,3,1,4))
Marima1 <- marima(mdata,means=1,
                  ar.pattern=Model1$ar.pattern, ma.pattern=Model1$ma.pattern,
                  Check=FALSE, Plot="none", penalty=2)

short.form(Marima1$ar.estimates, leading=FALSE) # print estimates
short.form(Marima1$ma.estimates, leading=FALSE)
Marima1$ar.pvalues
Marima1$ma.pvalues
Marima1$ar.stdv


# kig på variabel x
x <- 3
res <- na.omit(Marima1$residuals[x,])
residual_plots(res)
residual_sign_test(res)


fit <- (Marima1$fitted[x,])
av <- Marima1$averages[x]
mean(diff(log(Mitjut)))
par(mfrow=c(2,1))
plot(Mitjut)
fit_summed <- define.sum(t(na.omit(fit)),difference=c(1,1))$series.sum
plot(exp(fit_summed[1:122]))

Marima1$one.step
forecasting <- arma.forecast(series=log(Data[,3:8]), marima =Marima1,nstart=122,nstep=6)
plot(exp(forecasting$forecasts[x,]))
