library(forecast)
library("plotrix")  
library("marima")
library(car)
library("LSTS")
source("Time_Series_2023/Assignment_3/res_plot_fnc.r") # for residual checking
source("Time_Series_2023/Assignment_3/data_loading.r") # loading data
###################################################################
# Q41 loading data and plotting

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
inf.struct <- c(2, 0, 0)
## Estimate the model (check for convergence):
inf.fit <- arima(Inflation, order = inf.struct, include.mean = F)
residual_plots(inf.fit$residuals) # seems okay
## Filter x and y:
x <- Inflation[1:(N_prices)]
y <- log(pricesDK)
inf.filt <- x - inf.fit$coef[1] * c(0, x[1:(length(x) - 1)]) - inf.fit$coef[2] * c(0, 0, x[1:(length(x) - 2)])
prices.filt <- y - inf.fit$coef[1] * c(0, x[1:(length(x) - 1)]) - inf.fit$coef[2] * c(0, 0, x[1:(length(x) - 2)])
## Estimate SCCF for the filtered series:
ccf(inf.filt, prices.filt)
print(ccf(inf.filt, prices.filt))
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
