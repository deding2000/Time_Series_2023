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
#started with 
# ar <- c(1,2,4)
# ma <- c(1,2)
ar <- c(1,2,4)
ma <- c(1,4)

Model1 <- define.model(kvar=8, ar=ar, ma=ma,indep=c(8),rem.var=c(1,2,7))
Marima1 <- marima(data.dif,means=1,
                  ar.pattern=Model1$ar.pattern, ma.pattern=Model1$ma.pattern,
                  Check=FALSE, Plot="none", penalty=1)

short.form(Marima1$ar.estimates, leading=FALSE) # print estimates
short.form(Marima1$ma.estimates, leading=FALSE)
Marima1$ar.pvalues
Marima1$ma.pvalues

#Residual diagnostic:
#Remember that the order is (quarter(1),DK(2),CPH(3),Sealand(4),Mid(5),Rural(6),Interest(7),Inflation(8))
res_CPH <- na.omit(Marima1$residuals[3,])
res_Sea <- na.omit(Marima1$residuals[4,])
res_Mid <- na.omit(Marima1$residuals[5,])
res_Rur <- na.omit(Marima1$residuals[6,])
res_Inf <- na.omit(Marima1$residuals[8,])

# Do the residual plot and sign test for each one:
residual_plots(res_CPH)
residual_sign_test(res_CPH)
residual_plots(res_Sea)
residual_sign_test(res_Sea)
residual_plots(res_Mid)
residual_sign_test(res_Mid)
residual_plots(res_Rur)
residual_sign_test(res_Rur)
residual_plots(res_Inf)
residual_sign_test(res_Inf)

# Also check ccf between residuals for the regions
par(mfrow=c(2,2))
ccf(res_CPH,res_Sea)
ccf(res_CPH,res_Mid)
ccf(res_CPH,res_Rur)
ccf(res_CPH,res_Inf)
par(mfrow=c(2,2))
ccf(res_Sea,res_Mid)
ccf(res_Sea,res_Rur)
ccf(res_Sea,res_Inf)
ccf(res_Mid,res_Rur)
par(mfrow=c(1,2))
ccf(res_Mid,res_Inf)
ccf(res_Rur,res_Inf)

# Fitting and predicting
MARIMA_forecast <- arma.forecast(series=ts(Data_log), marima =Marima1,nstart=122,nstep=6,dif.poly = poly)

# Plotting original time series together with fitted values
par(mfrow=c(2,2))
plot(Years[1:N_prices],CPH,main = "Capital",col="red",xlab="Year",ylab="Price pr sqm")
lines(Years[1:128],exp(MARIMA_forecast$forecasts[3,]))
plot(Years[1:N_prices],Sealand,main = "Sealand",col="green",xlab="Year",ylab="Price pr sqm")
lines(Years[1:128],exp(MARIMA_forecast$forecasts[4,]))
plot(Years[1:N_prices],Mitjut,main = "MidJutland",col="blue",xlab="Year",ylab="Price pr sqm")
lines(Years[1:128],exp(MARIMA_forecast$forecasts[5,]))
plot(Years[1:N_prices],Rural,main = "Rural",col="yellow",xlab="Year",ylab="Price pr sqm")
lines(Years[1:128],exp(MARIMA_forecast$forecasts[6,]))

# with prediction interval
par(mfrow=c(1,1))
plotCI(x = seq(from = (2024.25-0.25*(6)), to = 2024, by = 0.25), 
       y = exp(MARIMA_forecast$forecasts[3,123:128]),
       li = exp(MARIMA_forecast$forecasts[3,123:128]-2*sqrt(MARIMA_forecast$pred.var[3,3,])),
       ui = exp(MARIMA_forecast$forecasts[3,123:128]+2*sqrt(MARIMA_forecast$pred.var[3,3,])), 
       xlim = c(Years[1],2024), ylim = c(800,4800),
       col = "red",main="Capital fit",xlab = "Year",ylab ="Sales price per sqm")
points(Years[1:122],CPH,col="blue")
lines(Years[1:128],exp(MARIMA_forecast$forecasts[3,]))

par(mfrow=c(1,1))
plotCI(x = seq(from = (2024.25-0.25*(6)), to = 2024, by = 0.25), 
       y = exp(MARIMA_forecast$forecasts[4,123:128]),
       li = exp(MARIMA_forecast$forecasts[4,123:128]-2*sqrt(MARIMA_forecast$pred.var[4,4,])),
       ui = exp(MARIMA_forecast$forecasts[4,123:128]+2*sqrt(MARIMA_forecast$pred.var[4,4,])), 
       xlim = c(Years[1],2024), ylim = c(400,2400),
       col = "red",main="Sealand fit",xlab = "Year",ylab ="Sales price per sqm")
points(Years[1:122],Sealand,col="blue")
lines(Years[1:128],exp(MARIMA_forecast$forecasts[4,]))

par(mfrow=c(1,1))
plotCI(x = seq(from = (2024.25-0.25*(6)), to = 2024, by = 0.25), 
       y = exp(MARIMA_forecast$forecasts[5,123:128]),
       li = exp(MARIMA_forecast$forecasts[5,123:128]-2*sqrt(MARIMA_forecast$pred.var[5,5,])),
       ui = exp(MARIMA_forecast$forecasts[5,123:128]+2*sqrt(MARIMA_forecast$pred.var[5,5,])), 
       xlim = c(Years[1],2024), ylim = c(400,2500),
       col = "red",main="Mid Jutland fit",xlab = "Year",ylab ="Sales price per sqm")
points(Years[1:122],Mitjut,col="blue")
lines(Years[1:128],exp(MARIMA_forecast$forecasts[5,]))

par(mfrow=c(1,1))
plotCI(x = seq(from = (2024.25-0.25*(6)), to = 2024, by = 0.25), 
       y = exp(MARIMA_forecast$forecasts[6,123:128]),
       li = exp(MARIMA_forecast$forecasts[6,123:128]-2*sqrt(MARIMA_forecast$pred.var[6,6,])),
       ui = exp(MARIMA_forecast$forecasts[6,123:128]+2*sqrt(MARIMA_forecast$pred.var[6,6,])), 
       xlim = c(Years[1],2024), ylim = c(400,2000),
       col = "red",main="Rural fit",xlab = "Year",ylab ="Sales price per sqm")
points(Years[1:122],Rural,col="blue")
lines(Years[1:128],exp(MARIMA_forecast$forecasts[6,]))

# Zoomed in
par(mfrow=c(2,2))
plotCI(x = seq(from = (2024.25-0.25*(6)), to = 2024, by = 0.25), 
       y = exp(MARIMA_forecast$forecasts[3,123:128]),
       li = exp(MARIMA_forecast$forecasts[3,123:128]-2*sqrt(MARIMA_forecast$pred.var[3,3,])),
       ui = exp(MARIMA_forecast$forecasts[3,123:128]+2*sqrt(MARIMA_forecast$pred.var[3,3,])), 
       xlim = c(Years[115],2024), ylim = c(2600,4900),
       col = "red",main="CPH fit",xlab = "Year",ylab ="Sales price per sqm")
points(Years[115:122],CPH[115:122],col="blue")
lines(Years[115:128],exp(MARIMA_forecast$forecasts[3,115:128]))

plotCI(x = seq(from = (2024.25-0.25*(6)), to = 2024, by = 0.25), 
       y = exp(MARIMA_forecast$forecasts[4,123:128]),
       li = exp(MARIMA_forecast$forecasts[4,123:128]-2*sqrt(MARIMA_forecast$pred.var[4,4,])),
       ui = exp(MARIMA_forecast$forecasts[4,123:128]+2*sqrt(MARIMA_forecast$pred.var[4,4,])), 
       xlim = c(Years[115],2024), ylim = c(1400,2500),
       col = "red",main="Sealand fit",xlab = "Year",ylab ="Sales price per sqm")
points(Years[115:122],Sealand[115:122],col="blue")
lines(Years[115:128],exp(MARIMA_forecast$forecasts[4,115:128]))

plotCI(x = seq(from = (2024.25-0.25*(6)), to = 2024, by = 0.25), 
       y = exp(MARIMA_forecast$forecasts[5,123:128]),
       li = exp(MARIMA_forecast$forecasts[5,123:128]-2*sqrt(MARIMA_forecast$pred.var[5,5,])),
       ui = exp(MARIMA_forecast$forecasts[5,123:128]+2*sqrt(MARIMA_forecast$pred.var[5,5,])), 
       xlim = c(Years[115],2024), ylim = c(1500,2500),
       col = "red",main="Mid Jutland fit",xlab = "Year",ylab ="Sales price per sqm")
points(Years[115:122],Mitjut[115:122],col="blue")
lines(Years[115:128],exp(MARIMA_forecast$forecasts[5,115:128]))

plotCI(x = seq(from = (2024.25-0.25*(6)), to = 2024, by = 0.25), 
       y = exp(MARIMA_forecast$forecasts[6,123:128]),
       li = exp(MARIMA_forecast$forecasts[6,123:128]-2*sqrt(MARIMA_forecast$pred.var[6,6,])),
       ui = exp(MARIMA_forecast$forecasts[6,123:128]+2*sqrt(MARIMA_forecast$pred.var[6,6,])), 
       xlim = c(Years[115],2024), ylim = c(1000,2000),
       col = "red",main="Rural fit",xlab = "Year",ylab ="Sales price per sqm")
points(Years[115:122],Rural[115:122],col="blue")
lines(Years[115:128],exp(MARIMA_forecast$forecasts[6,115:128]))



