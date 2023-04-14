# Time series Assigment 3 
# Functions for residual diagnostics

residual_plots <- function(res) {
       N_res <- length(res)
       par(mfrow=c(2,2))
       plot(Years[1:N_res],res, xlab="Year",ylab="Residual value",main="Residual plot")
       qqPlot(res,main="QQ Plot",xlab="Theoritical quantiles",ylab="Sample quantiles")#main = "QQ-plot", xlab = "Theoritical quantiles", "Sample quantiles")

       acf(res,main="ACF for residuals")
       pacf(res,main="PACF for residuals")
       #sign test

}

residual_sign_test <- function(res) {
       N_res <- length(res)
       low <- (N_res-1)/2 - 2*sqrt((N_res-1)/4)
       cat("\nsign test: lower bound is ", low)
       high <- (N_res-1)/2 + 2*sqrt((N_res-1)/4)
       cat("\nsign test: upper bound is ", high)
       sign_changes <- sum(abs(diff(res>0)))
       cat("\nnumber of sign changes ", sign_changes)
}

residual_hist_plot <- function(res) {
       par(mfrow=c(1,1))
       N_res <- length(res)
       x2 <- seq(min(res), max(res), length = N_res)
       fun <- dnorm(x2, mean = 0, sd = sd(res))
       hist(res,prob=TRUE,main = "Density histogram with normal curve", xlab = "Residual value")
       lines(x2,fun,col=2,lwd=2)
}