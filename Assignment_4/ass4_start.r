# Assignment 4
##########################
# Q4.1 Loading and Plotting
DATA <- read.csv("Assignment_4/A4_Kulhuse.csv")
Salt <- DATA$Sal
OX <- DATA$ODO
# missing observations
sum(is.na(Salt))
sum(is.na(OX))

# Plotting
Days <- seq(from = 10/24, by = 1/48, length.out = 5000) # Days from start (2017-08-24 10:00:00)
plot(Days,DATA$Sal,type="l",xlab="Days from start",ylab="Salinity [PSU]",main="Water salinity")
plot(Days,DATA$ODO,type="l",xlab="Days from start",ylab="Oxygen [mg/L]",main = "Dissolved Oxygen")
min(na.omit(DATA$ODO))