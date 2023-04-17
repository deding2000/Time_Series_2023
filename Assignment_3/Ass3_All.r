library(forecast)
library("plotrix")  
library("marima")
library(car)
library("LSTS")
library("data.table")
source("Time_Series_2023/Assignment_3/res_plot_fnc.r") # functions for residual checking
source("Time_Series_2023/Assignment_3/data_loading.r") # Code for loading data
###################################################################
# Q41 Plotting

# Average House Prices
par(mfrow=c(3,1))
plot(Years[1:N_prices],pricesDK, type = "l",col="red",main="Average DK house prices",xlab = "Year",ylab ="Sales price per sqm") # does not look stationary so we use the difference
plot(Years[2:N_prices],diff(pricesDK), type = "l",col="red",main="Average DK house prices (differenced)",xlab = "Year",ylab ="Sales price per sqm") # looks better but growing variance
plot(Years[2:N_prices],diff(log(pricesDK)), type = "l",col="red",main="Average DK house prices (log transformed and then differenced)",xlab = "Year",ylab ="Sales price per sqm") # looks good

# interest rates
par(mfrow=c(2,1))
plot(Years[1:N_int],Interest,type = "l",col = "blue", main="Danish interest rate",xlab = "Year", ylab="Interest Rate") #does not look stationary (or even stochastic)
plot(Years[2:N_int],diff(Interest),type = "l",col = "blue", main="Danish interest rate (differenced)",xlab = "Year", ylab="Interest Rate") # looks better

# Inflation
par(mfrow = c(1,1))
plot(Years[1:N_inf],Inflation,type="l",col="green",main="Danish inflation rate",xlab = "Year",ylab="Inflation rate") # looks rather stationary

#########################################################################3
# Q42 acf, pacf for housing prices

par(mfrow = c(3,2))
acf(pricesDK)
pacf(pricesDK)
acf(diff(pricesDK))
pacf(diff(pricesDK))
acf(diff(log(pricesDK)))
pacf(diff(log(pricesDK)))

# prewhitening
pacf(Inflation)
acf(Inflation)
## ARMA structure for inflation; AR(2)
## Estimate the model (check for convergence):
inf.fit <- arima(Inflation, order = c(2,0,0), include.mean = F)
y.fit <- arima(log(pricesDK),order = c(2,1,0), include.mean= F)
residual_plots(inf.fit$residuals) # seems okay
## Filter x and y:
x <- Inflation[1:(N_prices)]
y <- log(pricesDK)
inf.filt <- x - inf.fit$coef[1] * c(0, x[1:(length(x) - 1)]) - inf.fit$coef[2] * c(0, 0, x[1:(length(x) - 2)])
prices.filt <- y - inf.fit$coef[1] * c(0, x[1:(length(x) - 1)]) - inf.fit$coef[2] * c(0, 0, x[1:(length(x) - 2)])
## Estimate SCCF for the filtered series:
par(mfrow=c(1,1))
ccf(inf.filt, prices.filt)
print(ccf(inf.filt, prices.filt))
# trying with residuals from ARMA model:
Houseprices <-y.fit$residuals
inflation <- inf.fit$residuals
ccf(y.fit$residuals,inf.fit$residuals)
ccf(Houseprices,inflation)
# at lag -1 (very significant)
ccf(inf.filt, prices.filt, plot = FALSE)[-1] #-0.41

# now with interest
pacf(diff(Interest))
acf(diff(Interest))
## ARMA structure for interest; AR(1)
int.struct <- c(1, 0, 0)
## Estimate the model (check for convergence):
int.fit <- arima(diff(Interest), order = int.struct, include.mean = F)
residual_plots(int.fit$residuals) # seems bad but not much to do
## Filter x and y:
x <- Interest[1:(N_prices)]
y <- log(pricesDK)
int.filt <- x - int.fit$coef[1] * c(0, x[1:(length(x) - 1)])
prices.filt <- y - int.fit$coef[1] * c(0, x[1:(length(x) - 1)])
## Estimate SCCF for the filtered series:
ccf(int.filt, prices.filt) # looks very unstationary
print(ccf(int.filt, prices.filt))

# cross correlation functions
acf((diff(log(CPH))))
acf((diff(log(Sealand))))
acf((diff(log(Mitjut))))
acf((diff(log(Rural))))
pacf((diff(log(CPH))))
pacf((diff(log(Sealand))))
pacf((diff(log(Mitjut))))
pacf((diff(log(Rural))))

# cross correlation
ccf(diff(CPH),diff(Rural),type="correlation")
ccf(diff(CPH),diff(Sealand),type="correlation")
ccf(diff(CPH),diff(Mitjut),type="correlation")
ccf(diff(Rural),diff(Sealand),type="correlation")
ccf(diff(Rural),diff(Mitjut),type="correlation")

##########################################################3
# Q43
#initial guess is an (0,1,0)-(2,0,3) season 2 model
# but ended up with a (1,1,0)-(1,0,1) season 4 model
model <- arima(log(pricesDK),order=c(1,1,0), seasonal = list(order = c(1,0,1), period = 4))
# Compare with auto.arima
auto.arima(log(pricesDK))

#####################################################################3
#Q44
#residuals
residual_plots(model$residuals)
residual_hist_plot(model$residuals)
residual_sign_test(model$residuals)
###########################################################################3
#Q45
#prediction (6 time steps ahead)
predictions_6 <-forecast(model,6,95)
preds <- predictions_6$mean
lower <- predictions_6$lower
upper <- predictions_6$upper
par(mfrow=c(1,1))
plotCI(x = seq(from=2022.75,to = 2024, by = 0.25), 
       y = exp(preds[1:6]),
       li = exp(lower),
       ui = exp(upper),col = "red",xlim = c(1992.25,2024),ylim = c(500,3000),main="Average DK house prices with predictions",xlab = "Year",ylab ="Sales price per sqm")
lines(Years[1:N_prices],exp(predictions_6$x))

###################################################################
#Q46
#Arima X (interest seems useless but inflation is nice)
x_inf_new <- cumsum(c(Inflation[1:124],rep(Inflation[124],4)))
new_model <- Arima(log(pricesDK),xreg = cbind(Inflation=x_inf_new[1:122]),order=c(1,1,0), seasonal = list(order = c(1,0,1), period = 4))
#residuals
residual_plots(new_model$residuals)
residual_hist_plot(new_model$residuals)
residual_sign_test(new_model$residuals)
# check for ccf between residuals and input
plot(ccf(new_model$residuals, Inflation), main = "CCF between inflation and ARIMAX residuals")

#############################################################################
#Q47
#forecast
predictions_6_new <-forecast(new_model,level=95,xreg=cbind(Inflation=x_inf_new[123:128]))
preds2 <- predictions_6_new$mean
lower2 <- predictions_6_new$lower
upper2 <- predictions_6_new$upper
par(mfrow=c(1,1))
plotCI(x = seq(from=2022.75,to = 2024, by = 0.25), 
       y = exp(preds2[1:6]),
       li = exp(lower2),
       ui = exp(upper2),col = "red",xlim = c(1992.25,2024),ylim = c(500,3000),main="Average DK house prices with ARIMAX predictions",xlab = "Year",ylab ="Sales price per sqm")
lines(Years[1:N_prices],exp(predictions_6_new$x))

############################################################################3
#Q48
#Testing ARIMA model
nt <- 6
new_model_train <- arima(log(pricesDK[1:(122-nt)]),order=c(1,1,0), seasonal = list(order = c(1,0,1), period = 4))
predictions_6_train <-forecast(new_model_train,nt,level=95)#xreg=cbind(Inflation=x_inf_new[117:122]))
predst <- predictions_6_train$mean
lowert <- predictions_6_train$lower
uppert <- predictions_6_train$upper
par(mfrow=c(1,1))
plotCI(x = seq(from = (2022.75-0.25*(nt-1)), to = 2022.75, by = 0.25), 
       y = (predst[1:nt]),
       li = (lowert),
       ui = (uppert),col = "red",ylim=c(7.5,8.2),main="Average DK house prices with ARIMA predictions (Training)",xlab = "Year",ylab ="Sales price per sqm")
lines(Years[(N_prices-(nt-1)):N_prices],log(pricesDK[(N_prices-(nt-1)):N_prices]))
points(Years[(N_prices-(nt-1)):N_prices],log(pricesDK[(N_prices-(nt-1)):N_prices]))

#Testing ARIMAX model
new_model_train <- Arima(log(pricesDK[1:(122-nt)]),xreg = cbind(Inflation=x_inf_new[1:116]),order=c(1,1,0), seasonal = list(order = c(1,0,1), period = 4))
predictions_6_train <-forecast(new_model_train,nt,level=95,xreg=cbind(Inflation=x_inf_new[117:122]))
predst <- predictions_6_train$mean
lowert <- predictions_6_train$lower
uppert <- predictions_6_train$upper
plotCI(x = seq(from = (2022.75-0.25*(nt-1)), to = 2022.75, by = 0.25), 
       y = (predst[1:nt]),
       li = (lowert),
       ui = (uppert),col = "red",ylim=c(7.5,8.2),main="Average DK house prices with ARIMAX predictions (Training)",xlab = "Year",ylab ="Sales price per sqm")
lines(Years[(N_prices-(nt-1)):N_prices],log(pricesDK[(N_prices-(nt-1)):N_prices]))
points(Years[(N_prices-(nt-1)):N_prices],log(pricesDK[(N_prices-(nt-1)):N_prices]))
legend(1,legend=c("Prediction", "Observed"),
       col=c("red", "black"), lty=1:2, cex=0.8)

########################################################
#PART 2

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

#################################################################
# Q.41 to Q.413
## MARIMAX
# Creating data frame with logged series
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

