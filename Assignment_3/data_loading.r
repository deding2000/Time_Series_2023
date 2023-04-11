Data <- read.csv("Time_Series_2023/Assignment_3/A3Data.csv",header=TRUE)
N_prices <- 122
N_inf <- 124
N_int <- 124
Years <- seq(from =1992.50,to = 2024, by = 0.25)
pricesDK <- na.omit(Data$Denmark)
CPH <- na.omit(Data$Capital)
Rural <- na.omit(Data$Rural)
Sealand <- na.omit(Data$Sealand)
Mitjut <- na.omit(Data$MidJutland)
Interest <- na.omit(Data$InterestRate)
Inflation <- na.omit(Data$InflationRate)