library(mltools)
library(data.table)
library(marima)
library(forecast)
library("plotrix")  
library("marima")
library(car)
rm(list = ls())

#Data <- read.csv("Time_Series_2023/Assignment_3/A3Data.csv",header=TRUE)
Data <- read.csv("A3Data.csv",header=TRUE)
Denmark <- na.omit(Data$Denmark)
InterestRate <- na.omit(Data$InterestRate) #InterestRate rate
InflationRate <- na.omit(Data$InflationRate) #InterestRate rate


 
#Q4.1 Plot the quarterly average sales prices in Denmark and the additional variables, InterestRate rate and InflationRate rate.

saveFig <- TRUE
if(saveFig == TRUE){pdf("Q4.1_Denmark_CombinedPlot.pdf", width = 10*0.8, height = 10*0.8)}
#Insert Whatever needs to be saved here :))
par(mfrow=c(1,1))
par(mar=c(5, 4, 4, 6) + 0.1)
plot(Denmark,type="o",col="red",lwd="2",xlab="Quarters")
par(new=TRUE)
plot(InterestRate,type='l',col="blue", axes=FALSE,xlab="", ylab="",lwd="2")
lines(InflationRate,type='l',col="darkgreen",lwd="2")
mtext("InterestRate Rate",side=4,line=2) 
mtext("InflationRate /",side=4,line=1) 
axis(4, ylim=c(0,7000),las=1)
legend("top", legend = c("Denmark","InterestRate Rate", "InflationRate Rate"),
       lwd = 3, col = c("red","blue", "darkgreen"))
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE



saveFig <- TRUE
if(saveFig == TRUE){pdf("Q4.1Denmark_SeperatePlot.pdf", width = 10*0.8, height = 14*0.8)}
#Alternative plot
par(mfrow=c(4,1))
plot(Denmark,type="o",col="red",lwd="1",xlab="Quarters")
plot(diff(Denmark),type="o",col="violet",lwd="1",xlab="Quarters")
plot(log(Denmark),type='o',col="blue", lwd="1",xlab="Quarters")
plot(diff(log(Denmark)),type='o',col="darkgreen",lwd="1",xlab="Quarters")
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE

saveFig <- TRUE
if(saveFig == TRUE){pdf("Q4.1InterestRate_SeperatePlot.pdf", width = 10*0.8, height = 14*0.8)}
#Alternative plot
par(mfrow=c(4,1))
plot(InterestRate,type="o",col="red",lwd="1",xlab="Quarters")
plot(diff(InterestRate),type="o",col="violet",lwd="1",xlab="Quarters")
plot(log(InterestRate),type='o',col="blue", lwd="1",xlab="Quarters")
plot(diff(log(InterestRate)),type='o',col="darkgreen",lwd="1",xlab="Quarters")
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE

saveFig <- TRUE
if(saveFig == TRUE){pdf("Q4.1InflationRate_SeperatePlot.pdf", width = 10*0.8, height = 14*0.8)}
#Alternative plot
par(mfrow=c(4,1))
plot(InflationRate,type="o",col="red",lwd="1",xlab="Quarters")
plot(diff(InflationRate),type="o",col="violet",lwd="1",xlab="Quarters")
plot(log(InflationRate),type='o',col="blue", lwd="1",xlab="Quarters")
plot(diff(log(InflationRate)),type='o',col="darkgreen",lwd="1",xlab="Quarters")
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE

saveFig <- TRUE
if(saveFig == TRUE){pdf("Q4.1Denmark_SeperatePlot.pdf", width = 10*0.8, height = 14*0.8)}
#Alternative plot
par(mfrow=c(4,1))
plot(Denmark,type="o",col="red",lwd="1",xlab="Quarters")
plot(diff(Denmark),type="o",col="violet",lwd="1",xlab="Quarters")
plot(log(Denmark),type='o',col="blue", lwd="1",xlab="Quarters")
plot(diff(log(Denmark)),type='o',col="darkgreen",lwd="1",xlab="Quarters")
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE

saveFig <- TRUE
if(saveFig == TRUE){pdf("Q4.1Denmark_SeperatePlotALTernative.pdf", width = 10*0.8, height = 10*0.8)}
#Alternative plot
par(mfrow=c(2,2))
plot(Denmark,type="o",col="red",lwd="1",xlab="Quarters")
plot(diff(Denmark),type="o",col="red",lwd="1",xlab="Quarters")
plot(InterestRate,type='l',col="blue", lwd="2",xlab="Quarters")
plot(InflationRate,type='l',col="darkgreen",lwd="2",xlab="Quarters")
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE

#Denmark not stationary hence we make diff transform.
#Save the plot!
saveFig <- TRUE
if(saveFig == TRUE){pdf("name.pdf", width = 10*0.8, height = 10*0.8)}
#Insert Whatever needs to be saved here :))
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE

saveFig <- TRUE
if(saveFig == TRUE){pdf("Q4.1_differenceDenmark.pdf", width = 10*0.8, height = 10*0.8)}

#Q4.1 Plot the diff transform
par(mfrow=c(1,1))
par(mar=c(5, 4, 4, 6) + 0.1)
plot(diff(Denmark), main="Difference in PriceDK", type="o",col="red",lwd="2",xlab="Quarters",ylab="Difference in priceDK")
par(new=TRUE)
plot(InterestRate,type='l',col="blue", axes=FALSE,xlab="", ylab="",lwd="2")
lines(InflationRate,type='l',col="darkgreen",lwd="2")
mtext("InterestRate Rate",side=4,line=2) 
mtext("InflationRate /",side=4,line=1) 
axis(4, ylim=c(0,7000),las=1)
legend("top", legend = c("Denmark", "InterestRate","InflationRate Rate"),
       lwd = 3, col = c("red","blue", "darkgreen"))
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE

#Q4.2 ACF and PACF
saveFig <- TRUE
if(saveFig == TRUE){pdf("Q4.2ACF_PACF_Denmark.pdf", width = 10*0.8, height = 12*0.8)}
#
par(mfrow=c(4,2))
acf(Denmark,main="ACF of Denmark")
pacf(Denmark, main="PACF of Denmark")
acf(diff(Denmark),main="ACF of diff(Denmark)")
pacf(diff(Denmark), main="PACF of diff(Denmark)")
acf(log(Denmark),main="ACF of log(Denmark)")
pacf(log(Denmark), main="PACF of log(Denmark)")
acf(diff(log(Denmark)),main="ACF of diff(log(Denmark))")
pacf(diff(log(Denmark)), main="PACF of diff(log(Denmark))")
#
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE

#InterestRate maybe needs a rework :))
saveFig <- TRUE
if(saveFig == TRUE){pdf("Q4.2ACF_PACF_interest.pdf", width = 10*0.8, height = 8*0.8)}
#
par(mfrow=c(2,2))
acf(InterestRate,main="ACF of InterestRate")
pacf(InterestRate, main="PACF of InterestRate")
acf(diff(InterestRate),main="ACF of diff(InterestRate)")
pacf(diff(InterestRate), main="PACF of diff(InterestRate)")
#acf(log(InterestRate),main="ACF of log(InterestRate)")
#pacf(log(InterestRate), main="PACF of log(InterestRate)")
#acf(diff(log(InterestRate)),main="ACF of diff(log(InterestRate))")
#pacf(diff(log(InterestRate)), main="PACF of diff(log(InterestRate))")
#
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE

#InflationRate
saveFig <- TRUE
if(saveFig == TRUE){pdf("Q4.2ACF_PACF_inflation.pdf", width = 10*0.8, height = 12*0.8)}
#
par(mfrow=c(4,2))
acf(InflationRate,main="ACF of InflationRate")
pacf(InflationRate, main="PACF of InflationRate")
acf(diff(InflationRate),main="ACF of diff(InflationRate)")
pacf(diff(InflationRate), main="PACF of diff(InflationRate)")
acf(log(InflationRate),main="ACF of log(InflationRate)")
pacf(log(InflationRate), main="PACF of log(InflationRate)")
acf(diff(log(InflationRate)),main="ACF of diff(log(InflationRate))")
pacf(diff(log(InflationRate)), main="PACF of diff(log(InflationRate))")
#
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE


#Q4.2 cross-correlation function between four time series of the house prices:
#Estimate and plot Cross-Correlation
Capital <- diff(log(na.omit(Data$Capital)))
Sealand <- diff(log(na.omit(Data$Sealand)))
MidJutland <- diff(log(na.omit(Data$MidJutland)))
Rural <- diff(log(na.omit(Data$Rural)))

prices <- data.frame(Capital=Capital,Sealand=Sealand,MidJutland=MidJutland,Rural=Rural)
colnames(prices) <- c("Capital","Sealand","MidJutland","Rural")
saveFig <- TRUE
if(saveFig == TRUE){pdf("Q4.2CrossCorrelation.pdf", width = 12*0.8, height = 10*0.8)}
#
acf(prices)
#
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE

saveFig <- TRUE
if(saveFig == TRUE){pdf("Q4.2PartialCrossCorrelation.pdf", width = 12*0.8, height = 10*0.8)}
#
pacf(prices)
#
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE

Capital <- (diff(na.omit(Data$Capital)))
Sealand <- (diff(na.omit(Data$Sealand)))
MidJutland <- (diff(na.omit(Data$MidJutland)))
Rural <- (diff(na.omit(Data$Rural)))

prices <- data.frame(Capital=Capital,Sealand=Sealand,MidJutland=MidJutland,Rural=Rural)
colnames(prices) <- c("Capital","Sealand","MidJutland","Rural")
acf(prices)

saveFig <- TRUE
if(saveFig == TRUE){pdf("Q4.2ACF_PACF_inflation.pdf", width = 10*0.8, height = 12*0.8)}
#
acf(prices)
#
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE

########################################################################################################################
#Correlation plots???
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  Cor <- abs(cor(x, y)) # Remove abs function if desired
  txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
  if(missing(cex.cor)) {
    cex.cor <- 0.4 / strwidth(txt)
  }
  text(0.5, 0.5, txt,
       cex = 1 + cex.cor * Cor) # Resize the text by level of correlation
}
pairs(~ Capital + Sealand + MidJutland + Rural,
      upper.panel = panel.cor,    # Correlation panel
      lower.panel = panel.smooth)
########################################################################################################################

#Save the plot!
saveFig <- TRUE
if(saveFig == TRUE){pdf("Q4.2_DiffCrossCorrelation.pdf", width = 10*0.8, height = 20*0.8)}
#
par(mfrow=c(4,3))
for (i in 1:4){
  for (k in 1:4){
    if (i!=k){
      ccf(prices[i],prices[k],type="correlation",plot=TRUE,main= paste(colnames(prices)[i]," & ",colnames(prices)[k]))
    }
  }
}
#
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE


#For no diff
Capital <- (na.omit(Data$Capital))
Sealand <- (na.omit(Data$Sealand))
MidJutland <- (na.omit(Data$MidJutland))
Rural <- (na.omit(Data$Rural))
prices <- data.frame(Capital=Capital,Sealand=Sealand,MidJutland=MidJutland,Rural=Rural)
colnames(prices) <- c("Capital","Sealand","MidJutland","Rural")
acf(prices)
#Save the plot!
saveFig <- TRUE
if(saveFig == TRUE){pdf("Q4.2BAD_noCrossCorrelation.pdf", width = 10*0.8, height = 20*0.8)}
#
par(mfrow=c(4,3))
for (i in 1:4){
  for (k in 1:4){
    if (i!=k){
      ccf(prices[i],prices[k],type="correlation",plot=TRUE,main= paste(colnames(prices)[i]," & ",colnames(prices)[k]))
    }
  }
}
#
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE

#It is weak, we omit it???






#multipleplots <- function(data){
#  par(mfrow=c(2,2))
#  plot(data)
#  acf(data)
#  pacf(data)
#}

#Transform the data to make it stationary!


###########################################################################
#Q4.3 univariate model selection
par(mfrow=c(1,1))
acf(Denmark) #Clear exponential?? more like linear
pacf(Denmark) #clear lag 1, hence AR(1) part from golden table
m1 <- arima(x=Denmark,order=c(1,0,0))
tsdiag(m1) #looks like seasonal 4
m2 <- arima(x=Denmark,order=c(1,0,0),,seasonal = list(order = c(1,0,0), period = 4))
tsdiag(m2) #lag 1 is small
pacf(residuals(m2)) #lag 1 is small
m3 <- arima(x=Denmark,order=c(1,0,1),,seasonal = list(order = c(1,0,0), period = 4))
tsdiag(m3) #looks good, but residuals are increasing. -> transform
pacf(residuals(m3)) #looks good

#Univariate model selection with log transform
par(mfrow=c(1,1))
log_Denmark <- log(Denmark)
acf(log_Denmark) #Clear exponential?? more like linear
pacf(log_Denmark) #clear lag 1, hence AR(1) part from golden table
lm1 <- arima(x=log_Denmark,order=c(1,0,0))
tsdiag(lm1) #looks like seasonal 4 or maybe 2 #residuals very positive
pacf(residuals(lm1)) #looks wakko
lm2 <- arima(x=log_Denmark,order=c(1,0,0),,seasonal = list(order = c(1,0,0), period = 4))
tsdiag(lm2) #lag 1 is small
pacf(residuals(m2)) #lag 1 is small
m3 <- arima(x=log_Denmark,order=c(1,0,1),,seasonal = list(order = c(1,0,0), period = 4))
tsdiag(m3) #looks good, but residuals are increasing. -> transform
pacf(residuals(m3)) #add seasanolity AR(1)
m4 <- arima(x=log_Denmark,order=c(1,0,1),,seasonal = list(order = c(1,0,1), period = 4)) #does not work
m4 <- arima(x=log_Denmark,order=c(1,0,1),,seasonal = list(order = c(1,0,2), period = 4)) #does not work
#tsdiag(m4) #looks good
#pacf(residuals(m4)) #looks okay?

#Univariate model selection with diff transform
par(mfrow=c(1,1))
diff_Denmark <- diff(Denmark)
acf(diff_Denmark) #Clear lag 1,2,4,6,7, (8) (12) (14) (16)
pacf(diff_Denmark) #clear lag 1,2,4,13
#Start with ARIMA(2,1,2), seasonal 4 with (1,0,0)
dm1 <- arima(x=Denmark,order=c(1,1,0),seasonal = list(order = c(1,0,0), period = 4))
tsdiag(dm1) #looks nice, but residuals are increasing
pacf(residuals(dm1)) #looks nice!
#do log transform:
acf(diff(log(Denmark))) #lag 1, (2) 4, 6, 8
pacf(diff(log(Denmark))) #lag 1, 2, 4,5, 8, 12, 13, 
ldm1 <- arima(x=log(Denmark),order=c(1,1,0))
ldm1
tsdiag(ldm1) # add seasonal 4
pacf(residuals(ldm1)) # add seasonal 4

ldm2 <- arima(x=log(Denmark),order=c(1,1,0),,seasonal = list(order = c(1,0,1), period = 4))
ldm2
tsdiag(ldm2) #looks nice but lag 9
pacf(residuals(ldm2)) # looks nice but lag 9

#Is it possible to reduce it?
ldm3 <- arima(x=log(Denmark),order=c(1,1,0),,seasonal = list(order = c(1,0,1), period = 2))
ldm3
tsdiag(ldm3) #looks nice but lag 9
pacf(residuals(ldm3)) #also looks nice, but lag 9


ldm4 <- arima(x=log(Denmark),order=c(1,1,0),,seasonal = list(order = c(1,0,0), period = 4))
ldm4
tsdiag(ldm4) #looks nice but lag 9
pacf(residuals(ldm4)) #also looks nice, lag 12, 16


#narative:#narative:#narative:#narative:#narative:#narative:#narative:#narative:#narative:
#narative:#narative:#narative:#narative:#narative:#narative:#narative:#narative:#narative:
#narative:#narative:#narative:#narative:#narative:#narative:#narative:#narative:#narative:
acf(diff_Denmark) #Clear lag 1,2,4,6,7, (8) (12) (14) (16)
pacf(diff_Denmark) #clear lag 1,2,4,13
#dm1 <- arima(x=Denmark,order=c(1,1,0),seasonal = list(order = c(1,0,0), period = 4))
#Først inital diff -> residualer has increasing variance -> log transform
#Ser på ACF og PACF
acf(diff(log(Denmark))) #lag 1, (2) 4, 6, 8
pacf(diff(log(Denmark))) #lag 1, 2, 4,5, 8, 12, 13, 
# laver initiel model ldm1 <- arima(x=log(Denmark),order=c(1,1,0))
# tilføjer seasonal 4
tsdiag(ldm1) # add seasonal 4
pacf(residuals(ldm1)) # add seasonal 4
ldm2 <- arima(x=log(Denmark),order=c(1,1,0),,seasonal = list(order = c(1,0,1), period = 4))
ldm2 #MAKE TABLE!!!
tsdiag(ldm2) #looks nice but lag 9
pacf(residuals(ldm2)) # looks nice but lag 9

#initiel til plots til Q4.3 ARIMA$(1,1,0)(1,0,1)_2$ or 
deding <- arima(log(Denmark),order=c(1,1,0), seasonal = list(order = c(1,1,1), period = 2))
ldm2 <- arima(x=log(Denmark),order=c(1,1,0),,seasonal = list(order = c(1,0,1), period = 4))
tsdiag(deding) #lag 14
tsdiag(ldm2) #lag 9
pacf(residuals(ldm2)) #lag9
pacf(residuals(deding)) #lag14
#ldm2 er bedre!!! #bliver bedre med seasonal 4! med aic

deding
ldm2 #likelihood better

#WHAT WE DO IN ASSIGNMENT:::
(dlm2 <- arima(x=log(Denmark),order=c(2,1,2), seasonal = list(order = c(2,0,2), period = 2)))
length(dlm2$coef)
(dlm2 <- arima(x=log(Denmark),order=c(2,1,2), seasonal = list(order = c(2,0,2), period = 4)))
length(dlm2$coef)
(dlm2 <- arima(x=log(Denmark),order=c(2,1,2), seasonal = list(order = c(3,0,2), period = 2)))#286.84, 553.69
length(dlm2$coef)
(dlm2 <- arima(x=log(Denmark),order=c(2,1,2), seasonal = list(order = c(2,0,3), period = 2)))#286 , 552.67  
length(dlm2$coef)
(dlm2 <- arima(x=log(Denmark),order=c(1,1,2), seasonal = list(order = c(3,0,3), period = 2)))#286.84, 553.69
length(dlm2$coef)
(dlm2 <- arima(x=log(Denmark),order=c(2,1,2), seasonal = list(order = c(2,0,3), period = 2)))#286 , 552.67  
length(dlm2$coef)

(dlm2 <- arima(x=log(Denmark),order=c(2,1,2), seasonal = list(order = c(3,0,2), period = 2)))#286.84, 553.69
length(dlm2$coef)
(dlm2_BEST_COMPLEX <- arima(x=log(Denmark),order=c(3,1,2), seasonal = list(order = c(3,0,2), period = 2)))#288.49,  aic = -554.97
length(dlm2$coef)
(dlm2 <- arima(x=log(Denmark),order=c(3,1,2), seasonal = list(order = c(3,0,2), period = 2)))#288.49,  aic = -554.97
length(dlm2$coef)


#ARIMA$(1,1,0)(1,0,1)_2$ or 
(dlm2 <- arima(x=log(Denmark),order=c(1,1,0), seasonal = list(order = c(1,0,1), period = 2)))#288.49,  aic = -533.49
length(dlm2$coef)
(dlm2BEST_SIMPLE <- arima(x=log(Denmark),order=c(1,1,0), seasonal = list(order = c(1,0,1), period = 4)))#288.49,  aic = -533.49
length(dlm2$coef)
(dlm2 <- arima(x=log(Denmark),order=c(0,1,0), seasonal = list(order = c(1,0,1), period = 2)))#288.49,  aic = -533.49
length(dlm2$coef)
(dlm2 <- arima(x=log(Denmark),order=c(1,0,0), seasonal = list(order = c(1,0,1), period = 2)))#288.49,  aic = -533.49
length(dlm2$coef)
(dlm2 <- arima(x=log(Denmark),order=c(1,1,0), seasonal = list(order = c(2,0,1), period = 2)))#288.49,  aic = -533.49
length(dlm2$coef)
(dlm2 <- arima(x=log(Denmark),order=c(1,1,0), seasonal = list(order = c(1,0,2), period = 2)))#288.49,  aic = -533.49
length(dlm2$coef)
(dlm2 <- arima(x=log(Denmark),order=c(1,1,0), seasonal = list(order = c(2,0,2), period = 2)))#288.49,  aic = -533.49
length(dlm2$coef)
#Efterfølgende:

(dlm2BEST_SIMPLE <- arima(x=log(Denmark),order=c(1,1,0), seasonal = list(order = c(1,0,1), period = 4)))#288.49,  aic = -533.49

dlm2BEST_SIMPLE$aic

tsdiag(dlm2BEST_SIMPLE)

#PLOTS#PLOTS#PLOTS#PLOTS#PLOTS#PLOTS#PLOTS#PLOTS#PLOTS#PLOTS#PLOTS
#PLOTS#PLOTS#PLOTS#PLOTS#PLOTS#PLOTS#PLOTS#PLOTS#PLOTS#PLOTS#PLOTS
#PLOTS#PLOTS#PLOTS#PLOTS#PLOTS#PLOTS#PLOTS#PLOTS#PLOTS#PLOTS#PLOTS
dlm2BEST_SIMPLE
dlm2_BEST_COMPLEX

dlm2BEST_SIMPLE
dlm2_BEST_COMPLEX

final_reduced_mod <- dlm2BEST_SIMPLE #choose which model to plot:)
par(mfrow=c(2,2))
final_reduced_mod
#tsdiag(final_reduced_mod) #Standardized residuals, ACF of residuals and p-values
plot(residuals(final_reduced_mod),type="p",xlab="Quarters")
lines(cbind(1:122,0),type="l")
acf(residuals(final_reduced_mod))
Pacf(residuals(final_reduced_mod))
plot(density(residuals(final_reduced_mod)))

#SAVING:::
############################################################################################################################################
saveFig <- TRUE
if(saveFig == TRUE){pdf("Q4.4ACF_PACF_RESIDUALS.pdf", width = 10*0.8, height = 5*0.8)}
par(mfrow=c(1,2))
#final_reduced_mod
#tsdiag(final_reduced_mod) #Standardized residuals, ACF of residuals and p-values
#plot(residuals(final_reduced_mod),type="p",xlab="Quarters")
#lines(cbind(1:122,0),type="l")
acf(residuals(final_reduced_mod),main="ACF residuals")
Pacf(residuals(final_reduced_mod),main="PACF residuals")
#plot(density(residuals(final_reduced_mod)))
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE


#saveFig <- TRUE
#if(saveFig == TRUE){pdf("Q4.3InitallogDiffModel_ACF.pdf", width = 10*0.8, height = 15*0.8)}
#tsdiag(ldm1) # add seasonal 4
#if(saveFig == TRUE){dev.off()}
#saveFig <- FALSE

#LAV TABLE!!!!!#LAV TABLE!!!!!#LAV TABLE!!!!!#LAV TABLE!!!!!#LAV TABLE!!!!!#LAV TABLE!!!!!#LAV TABLE!!!!!#LAV TABLE!!!!!
#ldm2 #LAV TABLE!!!!!#LAV TABLE!!!!!#LAV TABLE!!!!!#LAV TABLE!!!!!#LAV TABLE!!!!!#LAV TABLE!!!!!#LAV TABLE!!!!!#LAV TABLE!!!!!
#LAV TABLE!!!!!#LAV TABLE!!!!!#LAV TABLE!!!!!#LAV TABLE!!!!!#LAV TABLE!!!!!#LAV TABLE!!!!!#LAV TABLE!!!!!#LAV TABLE!!!!!

#saveFig <- TRUE
#if(saveFig == TRUE){pdf("Q4.3FinallogDiffModel_RESIDUAL_ACF.pdf", width = 10*0.8, height = 15*0.8)}
#tsdiag(ldm2) #looks nice but lag 9
#if(saveFig == TRUE){dev.off()}
#saveFig <- FALSE

#saveFig <- TRUE
#if(saveFig == TRUE){pdf("Q4.3FinallogDiffModel_PACF.pdf", width = 10*0.8, height = 7*0.8)}
#pacf(residuals(ldm2)) # looks nice but lag 9
#if(saveFig == TRUE){dev.off()}
#saveFig <- FALSE
#PLOTSPLOTSPLOTSPLOTSPLOTSPLOTSPLOTSPLOTSPLOTSPLOTSPLOTSPLOTS
############################################################################################################################################

#Q4.4 residual plots
qqPlot(residuals(final_reduced_mod))

res <- final_reduced_mod$residuals
#qqnorm(res)
#qqline(res)
hist(res,n = 18)
plot(res)

#sign test
signtest<-sum(abs(diff(res>0)))
# lower bound
N <- 122
low <- (N-1)/2 - 2*sqrt((N-1)/4)
# upper bound
high <- (N-1)/2 + 2*sqrt((N-1)/4)
make_table=data.frame(Sign_Test=signtest,Low=low,High=high) 
make_table#make table


#SAVING:::
############################################################################################################################################
#Kan måske samles
plot(density(residuals(final_reduced_mod)))
final_reduced_mod
tsdiag(final_reduced_mod) #Standardized residuals, ACF of residuals and p-values
plot(residuals(final_reduced_mod),type="p",xlab="Quarters")
lines(cbind(1:122,0),type="l")
plot(res,xlab="Quarters")
acf(residuals(final_reduced_mod))
Pacf(residuals(final_reduced_mod))
plot(density(residuals(final_reduced_mod)))

saveFig <- TRUE
if(saveFig == TRUE){pdf("Q4.4RESIDUALS.pdf", width = 10*0.8, height = 7*0.8)}
par(mfrow=c(2,2))
plot(residuals(final_reduced_mod),type="p",xlab="Quarters",ylab="residuals",main="residual plot")
lines(cbind(1:122,0),type="l")
qqPlot(residuals(final_reduced_mod),ylab="residuals",main="QQplot")
hist(res,xlab="residuals")
plot(density(residuals(final_reduced_mod)),main="Density of residuals")
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE

#saveFig <- TRUE
#if(saveFig == TRUE){pdf("Q4.4_Residual_Histogram.pdf", width = 10*0.8, height = 7*0.8)}
#hist(res)

#if(saveFig == TRUE){dev.off()}
#saveFig <- FALSE


#saveFig <- TRUE
#if(saveFig == TRUE){pdf("Q4.4_Residuals.pdf", width = 10*0.8, height = 7*0.8)}
#plot(res)
#if(saveFig == TRUE){dev.off()}
#saveFig <- FALSE

#PLOTSPLOTSPLOTSPLOTSPLOTSPLOTSPLOTSPLOTSPLOTSPLOTSPLOTSPLOTS
############################################################################################################################################


###########################################################################
#Q4.5 plots
par(mfrow=c(1,1))
model <- final_reduced_mod
predictions_6 <-forecast(model,6,95)
preds <- predictions_6$mean
lower <- predictions_6$lower
upper <- predictions_6$upper
plotCI(x = c(123:128), 
       y = exp(preds[1:6]),
       li = exp(lower),
       ui = exp(upper),col = "red",xlim = c(1,128),ylim = c(500,3000),
       xlab="Quarters")
lines(exp(predictions_6$x))

#same but with smaller interval :)
plotCI(x = c(123:128), 
       y = exp(preds[1:6]),
       li = exp(lower),
       ui = exp(upper),col = "red",xlim = c(100,128),ylim = c(500,3000),
       xlab="Quarters")
lines(exp(predictions_6$x))

#SAVINGPLOTSSAVINGPLOTSSAVINGPLOTS#SAVINGPLOTSSAVINGPLOTSSAVINGPLOTS#SAVINGPLOTSSAVINGPLOTSSAVINGPLOTS
saveFig <- TRUE
if(saveFig == TRUE){pdf("Q4.5_Predictions.pdf", width = 10*0.8, height = 7*0.8)}
plotCI(x = c(123:128), 
       y = exp(preds[1:6]),
       li = exp(lower),
       ui = exp(upper),col = "red",xlim = c(1,128),ylim = c(500,3000),
       xlab="Quarters",ylab="Housing Prices")
lines(exp(predictions_6$x))
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE

saveFig <- TRUE
if(saveFig == TRUE){pdf("Q4.5_PredictionsSmallInterval.pdf", width = 10*0.8, height = 7*0.8)}
plotCI(x = c(123:128), 
       y = exp(preds[1:6]),
       li = exp(lower),
       ui = exp(upper),col = "red",xlim = c(100,128),ylim = c(1900,3000),
       xlab="Quarters",ylab="Housing Prices")
lines(exp(predictions_6$x))
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE
#SAVINGPLOTSSAVINGPLOTSSAVINGPLOTS#SAVINGPLOTSSAVINGPLOTSSAVINGPLOTS#SAVINGPLOTSSAVINGPLOTSSAVINGPLOTS

###########################################################################
#Q4.6
#Arima X (InterestRate seems useless but InflationRate is nice)
x_inflation <- cumsum(InflationRate)
x_interest <- InterestRate
x_inf_new <- cumsum(c(InflationRate[1:124],rep(InflationRate[124],4)))
#(new_model <- Arima(log(Denmark),xreg = c((x_inflation[1:122])),order=c(1,1,0), seasonal = list(order = c(1,1,1), period = 2)))
(new_model <- Arima(log(Denmark),xreg = c((x_inflation[1:122])),order=c(1,1,0), seasonal = list(order = c(1,0,1), period = 4)))
#residuals
res2 <- new_model$residuals
plot(res2) #plot residuals
hist(res2) #plot histogram of residuals
qqPlot(residuals(new_model)) #QQplot

tsdiag(new_model) #Standardized residuals, ACF of residuals and p-values
pacf(residuals(new_model))

acf(res2)
pacf(res2)
#qqnorm(res2)
#qqline(res2)
#hist(res2)

#SignTest
signtest<-sum(abs(diff(res2>0)))
low <- (N-1)/2 - 2*sqrt((N-1)/4)
high <- (N-1)/2 + 2*sqrt((N-1)/4)
make_table=data.frame(Sign_Test=signtest,Low=low,High=high) 
make_table#make table

final_reduced_mod <- 
#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING
saveFig <- TRUE
if(saveFig == TRUE){pdf("Q4.6RESIDUALS.pdf", width = 10*0.8, height = 7*0.8)}
par(mfrow=c(2,2))
plot(residuals(new_model),type="p",xlab="Quarters",ylab="residuals",main="residual plot")
lines(cbind(1:122,0),type="l")
qqPlot(residuals(new_model),ylab="residuals",main="QQplot")
hist(res,xlab="residuals")
plot(density(residuals(new_model)),main="Density of residuals")
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE

saveFig <- TRUE
if(saveFig == TRUE){pdf("Q4.6ACF_PACF_RESIDUALS.pdf", width = 10*0.8, height = 4*0.8)}
par(mfrow=c(1,2))
#final_reduced_mod
#tsdiag(final_reduced_mod) #Standardized residuals, ACF of residuals and p-values
#plot(residuals(final_reduced_mod),type="p",xlab="Quarters")
#lines(cbind(1:122,0),type="l")
acf(residuals(new_model))
Pacf(residuals(new_model))
#plot(density(residuals(final_reduced_mod)))
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE

#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING#SAVING

###########################################################################
#Q4.7 Forecasting the future house prices - II
rep(InflationRate[124],4)
plot(c(InflationRate[1:124],rep(InflationRate[124],4)))
x_inf_new <- cumsum(c(InflationRate[1:124],rep(InflationRate[124],4)))
plot(diff(x_inf_new))
plot(InflationRate)
predictions_6_new <-forecast(new_model,level=95,xreg=x_inf_new[123:128])

preds2 <- predictions_6_new$mean
lower2 <- predictions_6_new$lower
upper2 <- predictions_6_new$upper
plotCI(x = c(123:128), 
       y = exp(preds2[1:6]),
       li = exp(lower2),
       ui = exp(upper2),col = "red",xlim = c(1,128),ylim = c(500,3000))
lines(exp(predictions_6_new$x))





###########################################################################
#Q4.9 Plot quarterly sales of for each of the four regions.
Capital <- (na.omit(Data$Capital))
Sealand <- (na.omit(Data$Sealand))
MidJutland <- (na.omit(Data$MidJutland))
Rural <- (na.omit(Data$Rural))

par(mfrow=c(1,1))
plot(Capital,col="red",type="o",lwd="2")
lines(Sealand,col="darkgreen",type="o",lwd="2")
lines(MidJutland,col="blue",type="o",lwd="2")
lines(Rural,col="orange",type="o",lwd="2")
plot(Denmark,col="black",type="o")

par(mfrow=c(2,2))
plot(Capital,col="red",type="o",lwd="1")
plot(Sealand,col="darkgreen",type="o",lwd="1")
plot(MidJutland,col="blue",type="o",lwd="1")
plot(Rural,col="orange",type="o",lwd="1")


#Difference plots
dCapital <- diff(na.omit(Data$Capital))
dSealand <- diff(na.omit(Data$Sealand))
dMidJutland <- diff(na.omit(Data$MidJutland))
dRural <- diff(na.omit(Data$Rural))

par(mfrow=c(1,1))
plot(dCapital,col="red",type="o",lwd="2",main="diff")
lines(dSealand,col="darkgreen",type="o",lwd="2")
lines(dMidJutland,col="blue",type="o",lwd="2")
lines(dRural,col="orange",type="o",lwd="2")

par(mfrow=c(2,2))
plot(Capital,col="red",type="o",lwd="1")
plot(Sealand,col="darkgreen",type="o",lwd="1")
plot(MidJutland,col="blue",type="o",lwd="1")
plot(Rural,col="orange",type="o",lwd="1")

#rescaled plots
scaled_Capital <- (na.omit(Data$Capital))/max((na.omit(Data$Capital)))
scaled_Sealand <- (na.omit(Data$Sealand))/max(na.omit(Data$Sealand))
scaled_MidJutland <- (na.omit(Data$MidJutland))/max((na.omit(Data$MidJutland)))
scaled_Rural <- (na.omit(Data$Rural))/max((na.omit(Data$Rural)))

par(mfrow=c(1,1))
plot(scaled_Capital,col="red",type="o",lwd="2")
lines(scaled_Sealand,col="darkgreen",type="o",lwd="2")
lines(scaled_MidJutland,col="blue",type="o",lwd="2")
lines(scaled_Rural,col="orange",type="o",lwd="2")

par(mfrow=c(2,2))
plot(scaled_Capital,col="red",type="o",lwd="1")
plot(scaled_Sealand,col="darkgreen",type="o",lwd="1")
plot(scaled_MidJutland,col="blue",type="o",lwd="1")
plot(scaled_Rural,col="orange",type="o",lwd="1")

#rescaled plots
dscaled_Capital <- diff((na.omit(Data$Capital))/max((na.omit(Data$Capital))))
dscaled_Sealand <- diff((na.omit(Data$Sealand))/max(na.omit(Data$Sealand)))
dscaled_MidJutland <- diff((na.omit(Data$MidJutland))/max((na.omit(Data$MidJutland))))
dscaled_Rural <- diff((na.omit(Data$Rural))/max((na.omit(Data$Rural))))

par(mfrow=c(1,1))
plot(dscaled_Capital,col="red",type="o",lwd="2")
lines(dscaled_Sealand,col="darkgreen",type="o",lwd="2")
lines(dscaled_MidJutland,col="blue",type="o",lwd="2")
lines(dscaled_Rural,col="orange",type="o",lwd="2")

par(mfrow=c(2,2))
plot(dscaled_Capital,col="red",type="o",lwd="1")
plot(dscaled_Sealand,col="darkgreen",type="o",lwd="1")
plot(dscaled_MidJutland,col="blue",type="o",lwd="1")
plot(dscaled_Rural,col="orange",type="o",lwd="1")

#ACF on 4 areas
acf(Capital) 
acf(Sealand) 
acf(MidJutland)
acf(Rural) 
#all looks non stationary.


acf(dCapital) #måske 1 og 2
acf(dSealand) #måske seasonal på 4? lag på 14?
acf(dMidJutland) #seasonal på 4, negativ seasonal på 2
acf(dRural) # seasonal på 4, negativ på seasonal 2

par(mfrow=c(2,2))
pacf(dCapital) #meget lille, måske 1,2 og 11
pacf(dSealand) #ingenting, lidt på 14?
pacf(dMidJutland) #lidt neg 2, 4, neg6 og neg$14, måske 2 seasonal??
pacf(dRural) # seasonal på 2? i hvert fald for 2, 4, 6 og 8.


#Find Marimax model for house prices

