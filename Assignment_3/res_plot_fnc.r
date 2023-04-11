residual_plots <- function(res) {
       par(mfrow=c(2,2))
       plot(res)
       qqnorm(res)
       qqline(res)
       acf(res)
       pacf(res)
       N_res <- length(res)
       #sign test
       low <- (N_res-1)/2 - 2*sqrt((N_res-1)/4)
       cat("sign test: lower bound is ", low)
       high <- (N_res-1)/2 + 2*sqrt((N_res-1)/4)
       cat("\nsign test: upper bound is ", high)
       sign_changes <- sum(abs(diff(res>0)))
       cat("\nnumber of sign changes ", sign_changes)
}
