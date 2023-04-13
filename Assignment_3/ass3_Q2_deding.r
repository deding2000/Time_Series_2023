library(forecast)
library("plotrix")  
library("marima")
setwd("/Users/OscarBP/Documents/5. DTU noter/Semester 6/02417 Time Series Analysis")
source("Time_Series_2023/Assignment_3/res_plot_fnc.r")
source("Time_Series_2023/Assignment_3/data_loading.r")


saveFig <- TRUE
if(saveFig == TRUE){pdf("Q4.9_FourRegions_Diff_log_Plot.pdf", width = 10*0.8, height = 7*0.8)}
plot(d_log_Capital,col="red",type="o",lwd="2",main="Diff log Housing Prices in four Regions",xlab="Quarters",ylab="Diff(log(Housing Prices))")
lines(d_log_Sealand,col="darkgreen",type="o",lwd="2")
lines(d_log_MidJutland,col="blue",type="o",lwd="2")
lines(d_log_Rural,col="orange",type="o",lwd="2")
legend("topleft",
       c("diff_log_Capital","diff_log_Sealand","diff_log_MidJutland","diff_log_Rural"),
       col=c("red","darkgreen","blue","orange"),lty=1,lwd=3)
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE


##############################################################
# Q 4.9
par(mfrow=c(2,2))
plot(Years[1:N_prices],CPH, type = "l",col="red",main="Average CPH house prices",xlab = "Year",ylab ="Sales price per sqm")
lines(Years[1:N_prices],Rural, type = "l",col="blue",main="Average Rural house prices",xlab = "Year",ylab ="Sales price per sqm")
lines(Years[1:N_prices],Mitjut, type = "l",col="red",main="Average MidJutland house prices",xlab = "Year",ylab ="Sales price per sqm")
lines(Years[1:N_prices],Sealand, type = "l",col="red",main="Average Sealand house prices",xlab = "Year",ylab ="Sales price per sqm")

##############################################################
# Q 4.10
par(mfrow=c(2,2))
acf(diff(log(CPH)))
acf(diff(log(Rural)))
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
                  Check=FALSE, Plot="none", penalty=2)

write.csv()
(SAVETABLE<-data.frame(short.form(Marima1$ar.estimates, leading=FALSE))) # print estimates
write.csv(SAVETABLE,file="Time_Series_2023/Assignment_3/Q4.11Marima1_AR_estimates.csv")

SAVETABLE<-short.form(Marima1$ma, leading=FALSE)
write.csv(SAVETABLE,file="Time_Series_2023/Assignment_3/Q4.11Marima1_MA_estimates.csv")
Marima1$ar.pvalues
Marima1$ma.pvalues[,,1]

#Compare
Marima1 <- marima(data.dif,means=1,
                  ar.pattern=Model1$ar.pattern, ma.pattern=Model1$ma.pattern,
                  Check=FALSE, Plot="none", penalty=1)
Marima1$ma.pvalues[,,2]
Marima1 <- marima(data.dif,means=1,
                  ar.pattern=Model1$ar.pattern, ma.pattern=Model1$ma.pattern,
                  Check=FALSE, Plot="none", penalty=2)
Marima1$ma.pvalues[,,2]

#Residual diagnostic:
#Remember that the order is (quarter(1),DK(2),CPH(3),Sealand(4),Mid(5),Rural(6),Interest(7),Inflation(8))
res_CPH <- na.omit(Marima1$residuals[3,])
res_Sea <- na.omit(Marima1$residuals[4,])
res_Mid <- na.omit(Marima1$residuals[5,])
res_Rur <- na.omit(Marima1$residuals[6,])
res_Inf <- na.omit(Marima1$residuals[8,])

# Do the residual plot and sign test for each one:
par(mfrow=c(2,2))
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

prices <- data.frame(res_CPH=res_CPH,res_Sea=res_Sea,res_Mid=res_Mid,res_Rur=res_Rur,res_Inf=res_Inf)
colnames(prices) <- c("Capital","Sealand","MidJutland","Rural","res_Inf")
saveFig <- TRUE
if(saveFig == TRUE){pdf("Time_Series_2023/Assignment_3/Q4.12CrossCorrelation.pdf", width = 12*0.8, height = 10*0.8)}
#
acf(prices)
#
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE

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
plot(Years[1:N_prices],Sealand,main = "Sealand",col="darkgreen",xlab="Year",ylab="Price pr sqm")
lines(Years[1:128],exp(MARIMA_forecast$forecasts[4,]))
plot(Years[1:N_prices],Mitjut,main = "MidJutland",col="blue",xlab="Year",ylab="Price pr sqm")
lines(Years[1:128],exp(MARIMA_forecast$forecasts[5,]))
plot(Years[1:N_prices],Rural,main = "Rural",col="orange",xlab="Year",ylab="Price pr sqm")
lines(Years[1:128],exp(MARIMA_forecast$forecasts[6,]))


# Plotting zoomed original time series together with fitted values
par(mfrow=c(2,2))
plot(Years[78:128],CPH[78:128],main = "Capital",col="red",xlab="Year",ylab="Price pr sqm")
lines(Years[78:128],exp(MARIMA_forecast$forecasts[3,])[78:128])
plot(Years[78:128],Sealand[78:128],main = "Sealand",col="darkgreen",xlab="Year",ylab="Price pr sqm")
lines(Years[78:128],exp(MARIMA_forecast$forecasts[4,])[78:128])
plot(Years[78:128],Mitjut[78:128],main = "MidJutland",col="blue",xlab="Year",ylab="Price pr sqm")
lines(Years[78:128],exp(MARIMA_forecast$forecasts[5,])[78:128])
plot(Years[78:128],Rural[78:128],main = "Rural",col="orange",xlab="Year",ylab="Price pr sqm")
lines(Years[78:128],exp(MARIMA_forecast$forecasts[6,])[78:128])


# with prediction interval
par(mfrow=c(1,1))
plotCI(x = seq(from = (2024.25-0.25*(6)), to = 2024, by = 0.25), 
       y = exp(MARIMA_forecast$forecasts[3,123:128]),
       li = exp(MARIMA_forecast$forecasts[3,123:128]-2*sqrt(MARIMA_forecast$pred.var[3,3,])),
       ui = exp(MARIMA_forecast$forecasts[3,123:128]+2*sqrt(MARIMA_forecast$pred.var[3,3,])),col = "red",main="Capital",xlab = "Year",ylab ="Sales price per sqm")




