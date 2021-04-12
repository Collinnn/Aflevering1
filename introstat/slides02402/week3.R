#####################################
### Empirical cdf for height data ###
#####################################
# Empirical cdf for sample of height data from Chapter 1
x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)
plot(ecdf(x), verticals = TRUE, main = "ecdf and cdf")

# 'True cdf' for normal distribution (with sample mean and variance)
xp <- seq(0.9*min(x), 1.1*max(x), length = 100) 
lines(xp, pnorm(xp, mean(x), sd(x)), col = 2, lw = 2) 


#####################################
### Empirical pdf for height data ###
#####################################
# Histogram for sample of height data from Chapter 1
hist(x, nclass = 6, probability = TRUE, main = "histogram and pdf")

# 'True pdf' for normal distribution (with sample mean and variance)
xp <- seq(160,200,1)
lines(xp, dnorm(xp, mean(x), sd(x)), col = 'red', lw = 2)


####################################
### Standard Normal Distribution ###
####################################
# Draw a sample of 10000 observations from a standard normal distribution 
x <- rnorm(10000, mean = 0, sd = 1)
hist(x)

####################################
### Log-normal distribution      ###
####################################
# Draw a sample of 10000 observations from a log-normal distribution with alpha = 1 and beta = 1
y <- rlnorm(10000, meanlog = 1, sdlog = 1)

par(mfrow = c(1,2))

# Plot histogram of log-normal distributed data
hist(y, nclass = 20, probability = TRUE, 
     main = 'X ~ LN(1,1)', col = 'black', xlab = 'Y')
yp <- seq(0,100,0.1)
curve(expr = dlnorm(yp,1,1), xname = 'yp', 
      col = 'red', add = TRUE, lw = 2)

# Plot histogram of ln(log-normal) distributed data
alpha <- mean(log(y))
beta <- sd(log(y))
hist(log(y), nclass = 20, probability = TRUE, 
     main = 'ln(X) ~ N(1,1)', col = 'black', xlab = 'X')
xp <- seq(-2,4,0.1)
curve(expr = dnorm(xp,alpha,beta), xname = 'xp', 
      col = 'red', add = TRUE, lw = 2)

