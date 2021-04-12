#######################################
### 'Manual' one-sample t-test in R ###
#######################################

# Enter data
x <- c(1.2, 2.4, 1.3, 1.3, 0.9, 1.0, 1.8, 0.8, 4.6, 1.4) 
n <- length(x) # sample size

# Compute 'tobs' - the observed test statistic
tobs <- (mean(x) - 0) / (sd(x) / sqrt(n))

# Compute the p-value as a tail-probability 
# in the relevant t-distribution:
2 * (1 - pt(abs(tobs), df = n-1))

####################################################
### One-sample t-test in R using t.test function ###
####################################################

t.test(x)

############################################
### Density histogram of student heights ###
############################################

# Student heights data
x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)

# Density histogram of student height data together with normal pdf
hist(x, xlab = "Height", main = "", freq = FALSE)
lines(seq(160, 200, 1), dnorm(seq(160, 200, 1), mean(x), sd(x)),
      col = "red", lw = 2)

# Density histogram of simulated data from normal distribution
# (n = 100) together with normal pdf
xr <- rnorm(100, mean(x), sd(x))
hist(xr, xlab = "Height", main = "", probability = TRUE, ylim = c(0, 0.034))
lines(seq(130, 230, 1), dnorm(seq(130, 230, 1), mean(x), sd(x)), 
      col = "red", lw = 2)

# Empirical cdf for student height data together 
# with normal cdf
plot(ecdf(x), verticals = TRUE)
xp <- seq(0.9*min(x), 1.1*max(x), length.out = 100) 
lines(xp, pnorm(xp, mean(x), sd(x)), col = "red", lw = 2) 

# Empirical cdf of simulated data from normal distribution
# (n = 100) together with normal cdf
xr <- rnorm(100,  mean(x), sd(x))
plot(ecdf(xr), verticals = TRUE)
xp <- seq(0.9*min(xr), 1.1*max(xr), length.out = 100) 
lines(xp, pnorm(xp, mean(xr), sd(xr)), col = "red", lw = 2) 

####################################
### QQ Plot of student heights   ###
####################################

# Normal q-q plot of student heights
qqnorm(x)
qqline(x)

# Compare with normal distributed data to get a feeling
make_qq_norm <- function(n)
{
  x <- rnorm(n)
  qqnorm(x)
  qqline(x)
}

make_qq_norm(10)

# Use WallyPlot function for similar purpose
library(MESS)
qqwrap <- function(x, y, ...)
{
  stdy <- (y-mean(y)) / sd(y)
  qqnorm(stdy, main = "")
  qqline(stdy)
}

x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)
wallyplot(x-mean(x), FUN = qqwrap, ylim=c(-3,3))

###########################
### Example: Radon data ###
###########################

## Reading in the data
radon <- c(2.4, 4.2, 1.8, 2.5, 5.4, 2.2, 4.0, 1.1, 1.5, 5.4, 6.3, 
           1.9, 1.7, 1.1, 6.6, 3.1, 2.3, 1.4, 2.9, 2.9)

## Histogram and q-q plot of data
par(mfrow = c(1,2))
hist(radon)
qqnorm(radon)
qqline(radon)

# Transform data using the natural logarithm
logRadon<-log(radon)

## Histogram and q-q plot of transformed data
par(mfrow = c(1,2))
hist(logRadon)
qqnorm(logRadon)
qqline(logRadon)

