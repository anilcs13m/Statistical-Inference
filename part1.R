library(knitr)
library(plotrix)


set.seed(123)
par(xpd=NA)


lambda <- .2; n <- 40


par(mfrow = c(2,2))

for (no_sim in c(10, 100, 1000, 10000)){

  mean_values <- NULL; mean_sds <- NULL
  
  for (i in 1:no_sim){
    values <- rexp(n, lambda)
    means <- mean(values); sds <- sd(values)
    mean_values  <- c(mean_values, means); mean_sds <- c(mean_sds, sds)
  }

  myhist <- hist(mean_values , freq = TRUE, xlim = c(2, 8), 
                 main = paste("Histogram of", no_sim, "simulations"), xlab = "Values")
}
par(mfrow = c(1,1))
myhist <- hist(mean_values , freq = FALSE, xlim = c(2, 8), ylim = c(0, .55), 
               breaks = 25, main = paste("Probability density function for", no_sim, "simulations"), 
               xlab = "Values")
avg <- mean(mean_values)
s <- sd(mean_values)
abline(v = avg , col = "steelblue", lwd = 3, lty = 2)
abline(v = 5, col = "red", lwd = 3, lty = 9)
x <- seq(min(mean_values ), max(mean_values ), length = 100) 
y <- dnorm(x, mean = avg, sd = s)
curve(dnorm(x, mean = avg, sd = s), 
      col = "gray", lwd = 3, lty = 3, add = TRUE)

legend('topright', c("Expected value", "Actual mean", "Normal distrubution"), 
       lty=1, col=c('red', 'steelblue', "gray"), bty='n', cex=.75)


sd(mean_values)
qqnorm(mean_values, col = "lightskyblue1")
qqline(mean_values)
no_sim <- 100

mean_values <- NULL; mean_sds <- NULL

for (i in 1:no_sim){
  # calculate the mean & sd of all the sample means
  values <- rexp(n, lambda)
  means <- mean(values); sds <- sd(values)
  mean_values  <- c(mean_values, means); mean_sds <- c(mean_sds, sds)
}
upper <- mean_values +  1.96 * (mean_sds/sqrt(n))
lower <- mean_values -  1.96 * (mean_sds/sqrt(n))
sum(lower < 5 & 5 < upper)/no_sim * 100


index <- c(1:no_sim)
plot(index, upper, ylim = c(0, 10), type = "n", xlab = "Index", ylab = "Mean", 
     main = "Plot of confidence interval coverage for 100 simulations")

segments(index, upper, index, lower, col = "steelblue", lwd = 3)
#ablineclip(h = 5, col = "red", lwd = 2, lty = 2)
text(-8, 5, expression(paste("", mu, "")), cex = 1.5)
ablineclip(h=5, x1 = -2.5, lty = 2, col="red")