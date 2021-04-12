




par(mar=c(4,4,1,1))
x=c(168,161,167,179,184,166,198,187,191,179)
y=c(65.5, 58.3, 68.1, 85.7, 80.5, 63.4, 102.6, 91.4, 86.7, 78.9)
plot(x, y, xlab="Height", ylab="Weight", cex=1.5, col="blue", type="n")
text(x, y, 1:10, cex=1.5, col="blue")
par(mar=c(4,4,1,1))
x=c(168,161,167,179,184,166,198,187,191,179)
y=c(65.5, 58.3, 68.1, 85.7, 80.5, 63.4, 102.6, 91.4, 86.7, 78.9)
plot(x, y, xlab="Height", ylab="Weight", cex=1.5, col="blue", type="n")
text(x, y, 1:10, cex=1.5, col="blue")
abline(lm(y~x))
summary(lm(y ~ x))
par(mar=c(4,4,1,1))
hwdata <- data.frame(cbind(x, y))
myfit <- lm(y ~ x, data = hwdata)
hwdata_and_fit <- data.frame(hwdata, predict(myfit, interval="confidence"))
suppressWarnings(hwdata_and_fit <- data.frame(hwdata_and_fit, predict(myfit, interval="prediction")))

plot(x, y, xlab="Height", ylab="Weight", cex=1.5, col="blue", type="n")
text(x, y, 1:10, cex=1.5, col="blue")
abline(lm(y~x))

with(hwdata_and_fit, lines(sort(x), sort(lwr), col = 2))
with(hwdata_and_fit, lines(sort(x), sort(upr), col = 2))
with(hwdata_and_fit, lines(sort(x), sort(lwr.1), col = 3))
with(hwdata_and_fit, lines(sort(x), sort(upr.1), col = 3))

################################################################
 ## Simulate a  linear model with  normally distrubuted
 ## errors and estimate the parameters

## FiRST MAKE DATA:
## Generates x
x <- runif(n=20, min=-2, max=4)
## Simulate y
beta0=50; beta1=200; sigma=90
y <- beta0 + beta1 * x + rnorm(n=length(x), mean=0, sd=sigma)

## FROM HERE: as  real data analysis, we have th data in x and y:
## A scatter plot of x and y
plot(x, y)

## Find the  least squares estimates, use Theorem 5.4
(beta1hat <- sum( (y-mean(y))*(x-mean(x)) ) / sum( (x-mean(x))^2 ))
(beta0hat <- mean(y) - beta1hat*mean(x))

## Use lm() to find the estimates
lm(y ~ x)

## Plot the fitted  line
abline(lm(y ~ x), col="red")
################################################################
## See how the  parameter estimates are distributed

## Number of repeats
nRepeat <- 1000

## Two vectors to save the estimates in
Beta0Hat <- numeric(nRepeat)
Beta1Hat <- numeric(nRepeat)

## Repeat the  simulation and the estimation nRepeat times
for(i in 1:nRepeat){
  ## Generate x
  x <- runif(n=20, min=-2, max=4)
  ## Simulate the linear regression model
  beta0=50; beta1=200; sigma=90
  y <- beta0 + beta1 * x + rnorm(n=length(x), mean=0, sd=sigma)
  ## Use lm() to find the estimates
  fit <- lm(y ~ x)
  ## Save the estimates
  Beta0Hat[i] <- fit$coefficients[1]
  Beta1Hat[i] <- fit$coefficients[2]
}

## See their empirical distribution
par(mfrow=c(1,2))
hist(Beta0Hat, probability=TRUE)
hist(Beta1Hat, probability=TRUE)

## Gå lige tilbage til slides and kør næste slide

## See the estimate of the standard deviation of the   parameter estimates
summary(fit)
################################################################
## Hypothesis tests om signifikante parametre

## Generate x
x <- runif(n=20, min=-2, max=4)
## Simulate Y
beta0=50; beta1=200; sigma=90
y <- beta0 + beta1 * x + rnorm(n=length(x), mean=0, sd=sigma)

## Use lm() to find the estimates
fit <- lm(y ~ x)

## See summary - what we need
summary(fit)
################################################################
## Make confidence intervals for the parameters

## number of repeats
nRepeat <- 100

## Did we catch the correct parameter
TrueValInCI <- logical(nRepeat)

## Repeat the simulation and estimation nRepeat times:
for(i in 1:nRepeat){
  ## Generate x
  x <- runif(n=20, min=-2, max=4)
  ## Simulate y
  beta0=50; beta1=200; sigma=90
  y <- beta0 + beta1 * x + rnorm(n=length(x), mean=0, sd=sigma)

  ## Use lm() to find the estimates
  fit <- lm(y ~ x)
  
  ## Luckily  R can compute the  confidence interval (level=1-alpha)
  (ci <- confint(fit, "(Intercept)", level=0.95))
  
  ## Was the correct parameter value "caught" by the interval? (covered)
  (TrueValInCI[i] <-  ci[1] < beta0  &  beta0 < ci[2])
}

## How often did this happen?
sum(TrueValInCI) / nRepeat
################################################################
## Example of confidence interval for the line

## Make a sequence of x values
xval <- seq(from=-2, to=6, length.out=100)

## Use the  predict function
CI <- predict(fit, newdata=data.frame(x=xval),
interval="confidence",
level=.95)

## Check what we got
head(CI)

## Plot the data, model fit and intervals
plot(x, y, pch=20)
abline(fit)
lines(xval, CI[, "lwr"], lty=2, col="red", lwd=2)
lines(xval, CI[, "upr"], lty=2, col="red", lwd=2)
################################################################
## Example with prediction interval

## Make a sequence of x values
xval <- seq(from=-2, to=6, length.out=100)

## Use the  predict function
PI <- predict(fit, newdata=data.frame(x=xval),
interval="prediction",
level=.95)

## Check what we got
head(CI)

## Plot the data, model fit and intervals
plot(x, y, pch=20)
abline(fit)
lines(xval, PI[, "lwr"], lty=2, col="blue", lwd=2)
lines(xval, PI[, "upr"], lty=2, col="blue", lwd=2)
################################################################

## Generates x
x <- runif(n=20, min=-2, max=4)
## Simulate y
beta0=50; beta1=200; sigma=90
y <- beta0 + beta1 * x + rnorm(n=length(x), mean=0, sd=sigma)

## Scatter plot
plot(x,y)

## Use lm() to find the estimates
fit <- lm(y ~ x)

## The "true"  line
abline(beta0, beta1)
## Plot of fit
abline(fit, col="red")  

## See summary
summary(fit)

## Correlation between  x and y
cor(x,y)

## Squared  becomes the "Multiple R-squared" from summary(fit)
cor(x,y)^2
################################################################
fit <- lm(y ~ x)
par(mfrow = c(1, 2))
qqnorm(fit$residuals)
plot(fit$fitted, fit$residuals)

