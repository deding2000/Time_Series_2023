rm(list = ls())
#Data <- read.csv("Time_Series_2023/Assignment_3/A3Data.csv",header=TRUE)
Data <- read.csv("A3Data.csv",header=TRUE)
pricesDK <- na.omit(Data$Denmark)
interest <- na.omit(Data$InterestRate) #interest rate
inflation <- na.omit(Data$InflationRate) #interest rate


 
#Q4.1 Plot the quarterly average sales prices in Denmark and the additional variables, interest rate and inflation rate.
par(mfrow=c(1,1))
par(mar=c(5, 4, 4, 6) + 0.1)
plot(pricesDK,type="o",col="red",lwd="2",xlab="Quarters")
par(new=TRUE)
plot(interest,type='l',col="blue", axes=FALSE,xlab="", ylab="",lwd="2")
lines(inflation,type='l',col="darkgreen",lwd="2")
mtext("Interest Rate",side=4,line=2) 
mtext("Inflation /",side=4,line=1) 
axis(4, ylim=c(0,7000),las=1)
legend("top", legend = c("pricesDK", "Inflation Rate","Interest Rate"),
       lwd = 3, col = c("red","blue", "darkgreen"))


#Alternative plot
par(mfrow=c(4,1))
plot(pricesDK,type="o",col="red",lwd="2",xlab="Quarters")
plot(diff(pricesDK),type="o",col="red",lwd="2",xlab="Quarters")
plot(interest,type='l',col="blue", lwd="2")
plot(inflation,type='l',col="darkgreen",lwd="2")

par(new=TRUE)
plot(interest,type='l',col="blue", axes=FALSE,xlab="", ylab="",lwd="2")
lines(inflation,type='l',col="darkgreen",lwd="2")
mtext("Interest Rate",side=4,line=2) 
mtext("Inflation /",side=4,line=1) 
axis(4, ylim=c(0,7000),las=1)
legend("top", legend = c("pricesDK", "Inflation Rate","Interest Rate"),
       lwd = 3, col = c("red","blue", "darkgreen"))



#pricesDK not stationary hence we make diff transform.
#Save the plot!
saveFig <- TRUE
if(saveFig == TRUE){pdf("name.pdf", width = 10*0.8, height = 10*0.8)}
#Insert Whatever needs to be saved here :))
if(saveFig == TRUE){dev.off()}
saveFig <- FALSE

#Q4.1 Plot the transform
par(mfrow=c(1,1))
par(mar=c(5, 4, 4, 6) + 0.1)
plot(diff(pricesDK), main="Difference in PriceDK", type="o",col="red",lwd="2",xlab="Quarters",ylab="Difference in priceDK")
par(new=TRUE)
plot(interest,type='l',col="blue", axes=FALSE,xlab="", ylab="",lwd="2")
lines(inflation,type='l',col="darkgreen",lwd="2")
mtext("Interest Rate",side=4,line=2) 
mtext("Inflation /",side=4,line=1) 
axis(4, ylim=c(0,7000),las=1)
legend("top", legend = c("pricesDK", "Inflation","Interest Rate"),
       lwd = 3, col = c("red","blue", "darkgreen"))


#Q4.2 ACF and PACF

par(mfrow=c(3,2))
acf(pricesDK,main="ACF of pricesDK")
pacf(pricesDK, main="PACF of pricesDK")
acf(diff(pricesDK),main="ACF of diff(pricesDK)")
pacf(diff(pricesDK), main="PACF of diff(pricesDK)")
acf(log(pricesDK),main="ACF of log(pricesDK)")
pacf(log(pricesDK), main="PACF of log(pricesDK)")



#Q4.2 cross-correlation function between four time series of the house prices:
#Estimate and plot Cross-Correlation
Capital <- diff(na.omit(Data$Capital))
Sealand <- diff(na.omit(Data$Sealand))
MidJutland <- diff(na.omit(Data$Capital))
Rural <- diff(na.omit(Data$Rural))

prices <- data.frame(Capital=Capital,Sealand=Sealand,MidJutland=MidJutland,Rural=Rural)
colnames(prices) <- c("Capital","Sealand","MidJutland","Rural")

par(mfrow=c(2,3))
for (i in 1:4){
  for (k in 1:4){
    if (i!=k){
      ccf(prices[i],prices[k],type="correlation",plot=TRUE,main= paste(colnames(prices)[i]," & ",colnames(prices)[k]))
    }
  }
}
#It is weak, we omit it???













multipleplots <- function(data){
  par(mfrow=c(2,2))
  plot(data)
  acf(data)
  pacf(data)
}

#Transform the data to make it stationary!
#finaldata: 

#pricesDK
multipleplots(diff(pricesDK))

#Intersest
multipleplots(exp(interest)) #could work
multipleplots(diff(interest))

#Inflation
multipleplots(inflation)


#Estimate and plot Cross-Correlation
Capital <- diff(na.omit(Data$Capital))
Sealand <- diff(na.omit(Data$Sealand))
MidJutland <- diff(na.omit(Data$MidJutland))
Rural <- diff(na.omit(Data$Rural))

prices <- data.frame(Capital=Capital,Sealand=Sealand,MidJutland=MidJutland,Rural=Rural)
colnames(prices) <- c("Capital","Sealand","MidJutland","Rural")

par(mfrow=c(2,3))
for (i in 1:4){
  for (k in 1:4){
    if (i!=k){
      ccf(prices[i],prices[k],type="correlation",plot=TRUE,main= paste(colnames(prices)[i]," & ",colnames(prices)[k]))
    }
  }
}

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

pacf(dCapital) #meget lille, måske 1,2 og 11
pacf(dSealand) #ingenting, lidt på 14?
pacf(dMidJutland) #lidt neg 2, 4, neg6 og neg$14, måske 2 seasonal??
pacf(dRural) # seasonal på 2? i hvert fald for 2, 4, 6 og 8.


#Find Marimax model for house prices
