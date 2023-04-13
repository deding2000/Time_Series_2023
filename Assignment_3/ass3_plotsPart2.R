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
attach(Data)

 
#Q4.9 Plot the quarterly average sales prices in Denmark and the additional variables, InterestRate rate and InflationRate rate.
Capital <- (na.omit(Data$Capital))
Sealand <- (na.omit(Data$Sealand))
MidJutland <- (na.omit(Data$MidJutland))
Rural <- (na.omit(Data$Rural))

dCapital <- diff(Capital)
dSealand <- diff(Sealand)
dMidJutland <- diff((MidJutland))
dRural <- diff(Rural)

log_Capital<-log(Capital)
log_Sealand<-log(Sealand)
log_MidJutland<-log(MidJutland)
log_Rural<-log(Rural)

d_log_Capital<-diff(log(Capital))
d_log_Sealand<-diff(log(Sealand))
d_log_MidJutland<-diff(log(MidJutland))
d_log_Rural<-diff(log(Rural))
saveFig <- TRUE
if(saveFig == TRUE){pdf("name.pdf", width = 10*0.8, height = 10*0.8)}
#Insert Whatever needs to be saved here :))
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE

#SAVING NORMAL
saveFig <- TRUE
if(saveFig == TRUE){pdf("Q4.9_FourRegionsPlot.pdf", width = 10*0.8, height = 7*0.8)}
par(mfrow=c(1,1))
#Normal plot on top of eachother
#par(mfrow=c(1,1))
plot(Capital,col="red",type="o",lwd="2",main="Housing Prices in 4 Regions",xlab="Quarters",ylab="Housing Prices")
lines(Sealand,col="darkgreen",type="o",lwd="2")
lines(MidJutland,col="blue",type="o",lwd="2")
lines(Rural,col="orange",type="o",lwd="2")
legend("topleft",
       c("Capital","Sealand","MidJutland","Rural"),
       col=c("red","darkgreen","blue","orange"),lty=1,lwd=3)
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE


#Difference plots
#par(mfrow=c(1,1))
plot(dCapital,col="red",type="o",lwd="2",main="diff",xlab="Quarters",ylab="Diff(Housing Prices)")
lines(dSealand,col="darkgreen",type="o",lwd="2")
lines(dMidJutland,col="blue",type="o",lwd="2")
lines(dRural,col="orange",type="o",lwd="2")
legend("topleft",
       c("log_Capital","log_Sealand","log_MidJutland","log_Rural"),
       col=c("red","darkgreen","blue","orange"),lty=1,lwd=3)

#Log Plots
#par(mfrow=c(1,1))
plot(log_Capital,col="red",type="o",lwd="2",main="log",xlab="Quarters",ylab="log(Housing Prices)")
lines(log_Sealand,col="darkgreen",type="o",lwd="2")
lines(log_MidJutland,col="blue",type="o",lwd="2")
lines(log_Rural,col="orange",type="o",lwd="2")
legend("topleft",
       c("log_Capital","log_Sealand","log_MidJutland","log_Rural"),
       col=c("red","darkgreen","blue","orange"),lty=1,lwd=3)

#SAVING d_log_plot
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

#ACF with diff
acf(dCapital) #måske 1 og 2
acf(dSealand) #måske seasonal på 4? lag på 14?
acf(dMidJutland) #seasonal på 4, negativ seasonal på 2
acf(dRural) # seasonal på 4, negativ på seasonal 2

par(mfrow=c(2,2))
pacf(dCapital) #meget lille, måske 1,2 og 11
pacf(dSealand) #ingenting, lidt på 14?
pacf(dMidJutland) #lidt neg 2, 4, neg6 og neg$14, måske 2 seasonal??
pacf(dRural) # seasonal på 2? i hvert fald for 2, 4, 6 og 8.

#ACF with diff
acf(dCapital) #måske 1 og 2
acf(dSealand) #måske seasonal på 4? lag på 14?
acf(dMidJutland) #seasonal på 4, negativ seasonal på 2
acf(dRural) # seasonal på 4, negativ på seasonal 2

#ACF with diff and log transform
saveFig <- TRUE
if(saveFig == TRUE){pdf("Q4.10_ACF_and_PACF.pdf", width = 5*0.8, height = 8*0.8)}
par(mfrow=c(4,2))
acf(d_log_Capital,main="ACF diff log Capital",ylim=c(-0.22,0.4)) #meget lille, måske 1,2 og 11
pacf(d_log_Capital,main="PACF diff log Capital") #meget lille, måske 1,2 og 11

acf(d_log_Sealand,main="ACF diff log Sealand",ylim=c(-0.22,0.4)) #ingenting, lidt på 14?
pacf(d_log_Sealand,main="PACF diff log Sealand") #ingenting, lidt på 14?

acf(d_log_MidJutland,main="ACF diff log MidJutland",ylim=c(-0.22,0.4)) #lidt neg 2, 4, neg6 og neg$14, måske 2 seasonal??
pacf(d_log_MidJutland,main="PACF diff log MidJutland") #lidt neg 2, 4, neg6 og neg$14, måske 2 seasonal??

acf(d_log_Rural,main="ACF diff log Rural",ylim=c(-0.22,0.4)) # seasonal på 2? i hvert fald for 2, 4, 6 og 8.
pacf(d_log_Rural,main="PACF diff log Rural") # seasonal på 2? i hvert fald for 2, 4, 6 og 8.
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE

#Find Marimax model for house prices
