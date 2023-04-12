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
######
#GAMMEL FREMGANGSMÅDE
# Transforming data
# rural <- define.dif(log(Rural),difference=c(1,1))$dif.series
# mitjut <- define.dif(log(Mitjut),difference=c(1,1))$dif.series
# cph <- define.dif(log(CPH),difference=c(1,1))$dif.series
# sealand <- define.dif(log(Sealand),difference=c(1,1))$dif.series
# interest <- define.dif(sqrt(Interest),difference=c(1,1))$dif.series
# inflation <- Inflation
# Creating data frames. First one is to estimat MARIMA model and second one is for forecasting
# mdata <- data.frame(t(cph),t(sealand),t(mitjut),t(rural),interest[1:122],inflation[1:122])
# orgdata <- data.frame(t(log(CPH)),t(log(Sealand)),t(log(Mitjut)),t(log(Rural)),t(Interest[1:122]),Inflation[1:122])


####
#NY FREMGANGSMÅDE (VIRKER)
Data_log <- Data
Data_log$Capital <- log(Data_log$Capital)
Data_log$Sealand<- log(Data_log$Sealand)
Data_log$MidJutland <- log(Data_log$MidJutland )
Data_log$Rural <- log(Data_log$Rural)

# Using differenced data
Data_log_dif <- define.dif(series = ts(na.omit(Data_log)),difference=c(3,1,4,1,5,1,6,1,7,1))
poly <- Data_log_dif$dif.poly
data.dif <- Data_log_dif$y.dif

# Estimated from acf and pacf plots
ar <- c(1,2)
ma <- c(2)
Model1 <- define.model(kvar=8, ar=ar, ma=ma,indep=c(8),rem.var=c(1,2,7))
Marima1 <- marima(data.dif,means=1,
                  ar.pattern=Model1$ar.pattern, ma.pattern=Model1$ma.pattern,
                  Check=FALSE, Plot="none", penalty=1)

short.form(Marima1$ar.estimates, leading=FALSE) # print estimates
short.form(Marima1$ma.estimates, leading=FALSE)
Marima1$ar.pvalues
Marima1$ma.pvalues
Marima1$ar.stdv
# Look at time series number x
# order is (quarter(1),DK(2),CPH(3),Sealand(4),Mid(5),Rural(6),Interest(7),Inflation(8))
x <- 5

#Residuals
res <- na.omit(Marima1$residuals[x,])
residual_plots(res)
residual_sign_test(res)

# Fitting and predicting
MARIMA_forecast <- arma.forecast(series=ts(Data_log), marima =Marima1,nstart=122,nstep=6,dif.poly = poly)
par(mfrow=c(2,1))
plot(MARIMA_forecast$forecasts[x,])
plot(log(Mitjut))



