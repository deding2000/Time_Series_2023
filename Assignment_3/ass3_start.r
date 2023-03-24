Data <- read.csv("Time_Series_2023/Assignment_3/A3Data.csv",header=TRUE)
pricesDK <- na.omit(Data$Denmark)
interest <- na.omit(Data$InterestRate)
inflation <- na.omit(Data$InflationRate)

plot(pricesDK)
plot(interest)
plot(inflation)

plot(acf(interest))
plot(acf(inflation))
plot(acf(pricesDK))
