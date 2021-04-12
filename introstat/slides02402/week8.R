#################################
### Linear regression example ###
### with simulated data 1     ###
#################################

# Set seed (so that simulations may be redone)  
set.seed(100)
  
# Number of data points
n <- 20
  
# Intercept, slope, and std. deviation for simulations 
beta0 <- 50
beta1 <- 200
sigma <- 90
  
# Simulated data points
x <- runif(n, -2, 4)
y <- beta0 + beta1 * x + rnorm(n, mean = 0, sd = sigma)
  
# Scatter plot of x and y
plot(x, y)
  
# Add 'true' line to the plot
lines(x, beta0 + beta1*x, col = 2)

#################################
### Linear regression example ###
### with simulated data 2     ###
#################################

# Set seed (so that simulations may be redone)  
set.seed(100)

# Generate x
x <- runif(n = 20, min = -2, max = 4)

# Simulate y
beta0 <- 50; beta1 <- 200; sigma <- 90
y <- beta0 + beta1 * x + rnorm(n = length(x), mean = 0, sd = sigma)

# From here: like for the analysis of 'real data', we have data in x and y:

# Scatter plot of y against x
plot(x, y)
  
# Find the least squares estimates, use Theorem 5.4
(beta1hat <- sum( (y - mean(y))*(x-mean(x)) ) / sum( (x-mean(x))^2 ))
(bet0hat <- mean(y) - beta1hat*mean(x))
  
# Use lm() to find the estimates
lm(y ~ x)
  
# Plot the fitted  line
abline(lm(y ~ x), col="red")

################################################
### Distribution of estimators of regression ###
### coefficients by simulation               ###
################################################

# Number of repetitions
nRepeat <- 1000

# Two vectors to save the estimates in
Beta0Hat <- numeric(nRepeat)
Beta1Hat <- numeric(nRepeat)

# Repeat the  simulation and estimation nRepeat times
for(i in 1:nRepeat){
  # Generate x
  x <- runif(n = 20, min = -2, max = 4)
  # Simulate from the linear regression model
  beta0 = 50; beta1 = 200; sigma = 90
  y <- beta0 + beta1 * x + rnorm(n = length(x), mean = 0, sd = sigma)
  # Use lm() to find the estimates
  fit <- lm(y ~ x)
  # Save the estimates
  Beta0Hat[i] <- fit$coefficients[1]
  Beta1Hat[i] <- fit$coefficients[2]
}

# See empirical distributions of the estimates
hist(Beta0Hat, probability = TRUE)
hist(Beta1Hat, probability = TRUE)

###########################################
### Linear regression: Hypothesis tests ###
### Example: Height-Weight data         ###
###########################################

# Read data into R

x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)
y <- c(65.5, 58.3, 68.1, 85.7, 80.5, 63.4, 102.6, 91.4, 86.7, 78.9)

# Fit model to data
fit <- lm(y ~ x)

# Look at model summary to find Tobs-values and p-values
summary(fit)

##################################################
### Example: Illustration of CIs by simulation ###
##################################################

# Number of repetitions (here: CIs)
nRepeat <- 1000

# Empty logical vector of length nRepeat
TrueValInCI <- logical(nRepeat)

# Repeat the simulation and estimation nRepeat times:
for(i in 1:nRepeat){
  # Generate x
  x <- runif(n = 20, min = -2, max = 4)
  # Simulate y
  beta0 = 50; beta1 = 200; sigma = 90
  y <- beta0 + beta1 * x + rnorm(n = length(x), mean = 0, sd = sigma)
  # Use lm() to fit model
  fit <- lm(y ~ x)
  # Use confint() to compute 95% CI for intercept
  ci <- confint(fit, "(Intercept)", level=0.95)
  # Was the 'true' intercept included in the interval? (covered)
  (TrueValInCI[i] <-  ci[1] < beta0  &  beta0 < ci[2])
}

# How often was the true intercept included in the CI?
sum(TrueValInCI) / nRepeat

##################################################
### Example: Confidence intervals for the line ###
##################################################

# Generate x
x <- runif(n = 20, min = -2, max = 4)

# Simulate y
beta0 = 50; beta1 = 200; sigma = 90
y <- beta0 + beta1 * x + rnorm(n = length(x), sd = sigma)

# Use lm() to fit model
fit <- lm(y ~ x)

# Make a sequence of 100 x-values
xval <- seq(from = -2, to = 6, length.out = 100)

# Use the  predict function
CI <- predict(fit, newdata = data.frame(x = xval),
              interval = "confidence",
              level = 0.95)

# Check what we got
head(CI)

# Plot the data, model fit and intervals
plot(x, y, pch = 20)
abline(fit)
lines(xval, CI[, "lwr"], lty=2, col = "red", lwd = 2)
lines(xval, CI[, "upr"], lty=2, col = "red", lwd = 2)

##################################################
### Example: Prediction intervals for the line ###
##################################################

# Generate x
x <- runif(n = 20, min = -2, max = 4)

# Simulate y
beta0 = 50; beta1 = 200; sigma = 90
y <- beta0 + beta1 * x + rnorm(n = length(x), sd = sigma)

# Use lm() to fit model
fit <- lm(y ~ x)

# Make a sequence of 100 x-values
xval <- seq(from = -2, to = 6, length.out = 100)

# Use the  predict function
PI <- predict(fit, newdata = data.frame(x = xval),
              interval = "prediction",
              level = 0.95)

# Check what we got
head(CI)

# Plot the data, model fit and intervals
plot(x, y, pch = 20)
abline(fit)
lines(xval, PI[, "lwr"], lty = 2, col = "blue", lwd = 2)
lines(xval, PI[, "upr"], lty = 2, col = "blue", lwd = 2)

##############################################
### Linear regression: Correlation and R^2 ###
### Example: Height-Weight data            ###
##############################################

# Read data into R

x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)
y <- c(65.5, 58.3, 68.1, 85.7, 80.5, 63.4, 102.6, 91.4, 86.7, 78.9)

# Fit model to data
fit <- lm(y ~ x)

# Scatter plot of data with fitted line
plot(x,y, xlab = "Height", ylab = "Weight")
abline(fit, col="red")  
  
# See summary
summary(fit)
  
# Correlation between  x and y
cor(x,y)
  
# Squared correlation is the "Multiple R-squared" from summary(fit)
cor(x,y)^2

###########################################
### Linear regression: Model validation ###
### Example: Height-Weight data         ###
###########################################

# Read data into R
x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)
y <- c(65.5, 58.3, 68.1, 85.7, 80.5, 63.4, 102.6, 91.4, 86.7, 78.9)

# Fit model to data
fit <- lm(y ~ x)

# QQ-plot of residuals
qqnorm(fit$residuals, main = "") # or "Wally plot" of residuals

# Plots of residuals against fitted values
plot(fit$fitted, fit$residuals)
