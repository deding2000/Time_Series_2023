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

