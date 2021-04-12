## Read data
x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)
y <- c(65.5, 58.3, 68.1, 85.7, 80.5, 63.4, 102.6, 91.4, 86.7, 78.9)

## Calculate averages 
xbar <- mean(x)
ybar <- mean(y)

## Parameters estimates
Sxx <- sum((x - xbar)^2)
beta1hat <- sum((x - xbar)*(y - ybar)) / Sxx
beta0hat <- ybar - beta1hat * xbar

D <- data.frame(x=x, y=y)
fitStudents <- lm(y ~ x, data=D)
summary(fitStudents)

  set.seed(124)
  n <- 10; k <- 500
  beta0 <- 10;  beta1 <- 3;  sigma <- 5
  x <- seq(-2, 5, length=n)
  y <- matrix(0, ncol=k, nrow=n)
  y <- y + beta0 + beta1*x + rnorm(n*k, sd=sigma)

 b0 <- numeric(k);  b1 <- numeric(k)
 for(i in 1:k){
   b <- coef(lm(y[ ,i] ~ x))
   b0[i] <- b[1]
   b1[i] <- b[2]
 }
 c(mean(b0),  mean(b1))

par(mfrow=c(1,2))
hist(b0, prob=TRUE, main="Empirical density of $\\hat{\\beta}_0$")
lines(beta0*c(1, 1),c(0, 300), col=2, lwd=2)
hist(b1, prob=TRUE, main="Empirical density of $\\hat{\\beta}_1$")
lines(beta1*c(1, 1),c(0, 300), col=2, lwd=2,)

# NOGET HELT ANDET DATA  x <- c(-2.00, -1.22, -0.44, 0.33, 1.11, 1.89, 2.67, 3.44, 4.22, 5.00)
#  y <- c(-2.93, 6.52, 4.85, 12.06, 20.46, 19.39, 21.50, 19.19, 23.65, 31.04)
#  mx <- mean(x)
#  my <- mean(y)
#  Sxx <- sum((x - mx)^2)
#  beta1 <- sum((x - mx)*(y - my))/Sxx;
#  beta0 <- my - beta1*mx; 
beta0 <- coef(fitStudents)[1]
beta1 <- coef(fitStudents)[2]
e <- y - (beta0 + beta1 * x)
n <- length(e)
sigma <- sqrt(sum(e^2) / (n - 2))
sigma.beta0 <- sqrt(sigma^2 * (1 / n + xbar^2 / Sxx))
sigma.beta1 <- sqrt(sigma^2 / Sxx) 
c(sigma, sigma.beta0, sigma.beta1)

set.seed(124)
n <- 10; k <- 500
beta0 <- 10;  beta1 <- 3; sigma <- 5
x <- seq(-2, 5, length=n)
Sxx <- (n-1)*var(x)
c(mean(x), Sxx)
y <- matrix(0, ncol=k, nrow=n)
y <- y + beta0 + beta1*x + rnorm(n*k,sd=sigma)

b0 <- numeric(k);  b1 <- numeric(k)
sigma <- numeric(k)
for(i in 1:k){
  fit <- lm(y[ ,i] ~ x)
  b <- coef(fit)
  b0[i] <- b[1]
  b1[i] <- b[2]
  sigma[i] <- summary(fit)$sigma
 }
 c(var(b0), var(b1), mean(sigma))

set.seed(124)
x <- seq(0, 1, length=10)
y <- 1 + x + rnorm(10)
## Fit the model (estimate parameter)
fit <- lm(y ~ x)
## Print summary of model fit
summary(fit)
## Residual standard deviation
sigma <- summary(fit)$sigma 
## Estimated standard deviation of parameters
summary(fit)$coefficients[ ,2]

set.seed(124)
k <- 500
sigma.beta <- matrix(nrow=k,ncol=2)
sigma <- numeric(k);
n <- seq(3, k+2)
for(i in 1:k){
  x <- seq(0,1,length=n[i])
  y <- 1+x+rnorm(n[i])
  fit <- lm(y ~ x)
  sigma[i] <- summary(fit)$sigma
  sigma.beta[i, ] <- summary(fit)$coefficients[ ,2]
}

Sxx<-n*(n+1)/12/(n-1)

par(mfrow=c(1,3), mar=c(3.25, 2.5, 0, 0.75), cex=0.85)
plot(n,sigma^2,pch=19,cex=0.5,xlab="$n$",ylab="$\\hat{\\sigma}^2$")
lines(c(0,max(n)),c(1,1),col=2,lwd=2)
plot(n,sigma.beta[ ,1],ylim=c(0,max(sigma.beta[ ,1])),pch=19,cex=0.5,xlab="$n$",ylab="$\\hat{\\sigma}_{\\beta_0}$")
lines(n,sqrt(1/n+1/4*1/Sxx),col=2,lwd=2)
plot(n,sigma.beta[ ,2],ylim=c(0,max(sigma.beta[ ,2])),pch=19,cex=0.5,xlab="$n$",ylab="$\\hat{\\sigma}_{\\beta_1}$")
lines(n,sqrt(1/Sxx),col=2,lwd=2)

qt(0.975,df=10-2)    

p.v0 <- 2 * (1 - pt(abs(-6.35), df=10-2))
p.v1 <- 2 * (1 - pt(abs(1.07), df=10-2))
c(p.v0,p.v1)

#  x <- c(-2.00, -1.22, -0.44, 0.33, 1.11, 1.89, 2.67, 3.44, 4.22, 5.00)
#  y <- c(-2.93, 6.52, 4.85, 12.06, 20.46, 19.39, 21.50, 19.19, 23.65, 31.04)
#  fit <- lm(y~x)
confint(fitStudents, level=0.95) 

predict(fitStudents, newdata=data.frame(x=200), interval="confidence", 
        level=0.95) 
predict(fitStudents, newdata=data.frame(x=200), interval="prediction", 
        level=0.95) 

set.seed(1234)

## The number of observations and the parameters
n <- 30
beta0 <- 10;  beta1 <- 3; sigma <- 0.5
## Generate some input values
x <- runif(n, -10, 10)
## Simulate output values
y <- beta0 + beta1*x + rnorm(n, sd=sigma)
## Fit a simple linear regression model to the sample
fit <- lm(y ~ x)

## The number of new observations
k <- 10000
## Generate k new input values
xnew <- runif(k, -10, 10)
## Calculate the prediction intervals for the new input values
PI <- predict(fit, newdata=data.frame(x=xnew), interval="pred")
## Simulate new output observations
ynew <- beta0 + beta1*xnew + rnorm(k, sd=sigma)
## Calculate the fraction of times the prediction interval covered the new observation
sum(ynew > PI[ ,"lwr"] & ynew < PI[ ,"upr"]) / k

## The number of simulated samples
k <- 10000
## Repeat the sampling k times
covered <- replicate(k, {
  ## The number of observations and the parameters
  n <- 30
  beta0 <- 10;  beta1 <- 3; sigma <- 0.5
  ## Generate some input values
  x <- runif(n, -10, 10)
  ## Simulate output values
  y <- beta0 + beta1*x + rnorm(n, sd=sigma)
  ## Fit a simple linear regression model to the sample
  fit <- lm(y ~ x)
  
  ## Generate a new input value
  xnew <- runif(1, -10, 10)
  ## Calculate the prediction interval for the new value
  PI <- predict(fit, newdata=data.frame(x=xnew), interval = "pred")
  ## Simulate a single new observation
  ynew <- beta0 + beta1*xnew + rnorm(1, sd=sigma)
  ## Check if the new observation was inside the interval
  ynew > PI[1,"lwr"] & ynew < PI[1,"upr"]
})
## The fraction of covered new observations
sum(covered)/k

## Data
X <- cbind(1, x)
n <- length(y)
## Parameter estimates and variance
beta     <- solve(t(X) %*% X) %*% t(X) %*% y
e        <- y - X %*% beta
s        <- sqrt(sum(e^2) / (n - 2))
Vbeta    <- s^2 * solve(t(X) %*% X)
sbeta    <- sqrt(diag(Vbeta))
T.stat   <- beta / sbeta
p.value  <- 2 * (1 - pt(abs(T.stat), df = n-2))
## Print the results
coef.mat <- cbind(beta, sbeta, T.stat, p.value); 
colnames(coef.mat) <- c("Estimates","Std.Error","t.value","p.value")
rownames(coef.mat) <- c("beta0", "beta1")
coef.mat;  s

## Prediction and confidence interval
xnew  <- matrix(c(1, 200), ncol=2)
ynew  <- xnew %*% beta
Vconf <- xnew %*% Vbeta %*% t(xnew)
Vpred <- Vconf + s^2
sqrt(c(Vconf, Vpred))

cor(x, y)^2

#  fit <- lm(y ~ x)
coef(fitStudents)[2]^2 * var(x) / var(y)

set.seed(124)
par(mfrow=c(1,2))
n <-100
x1 <- seq(1, 10, length=n)
y <- x1 + rnorm(n)
fit <- lm(y ~ x1)
qqnorm(fit$residuals, pch=19, cex=0.5)
qqline(fit$residuals)
plot(fit$fitted.values, fit$residuals, pch=19, cex=0.5,
xlab="Fitted values ($\\hat{y}_i$)", ylab="Residuals ($e_i$)")

x1 <- seq(1, 10, length=n)
x2 <- seq(1, 10, length=n)^2
y <- x1 + 0.5 * x2 + rnorm(n)
fit <- lm(y ~ x1)
par(mfrow=c(1,2))
qqnorm(fit$residuals, pch=19, cex=0.5)
qqline(fit$residuals)
plot(fit$fitted.values, fit$residuals, pch=19, cex=0.5,
xlab="Fitted values ($\\hat{y}_i$)", ylab="Residuals ($e_i$)")

x1 <- seq(4, 10, length=100)
y <- exp( 0.2 * x1 + rnorm(length(x1), sd = 0.15))
fit <- lm(y ~ x1)
par(mfrow=c(1,2))
qqnorm(fit$residuals, pch=19, cex=0.5)
qqline(fit$residuals)
plot(fit$fitted.values, fit$residuals, pch=19, cex=0.5,
xlab="Fitted values ($\\hat{y}_i$)", ylab="Residuals ($e_i$)")

y <- log(y)
fit <- lm(y ~ x1)
par(mfrow=c(1,2))
qqnorm(fit$residuals, pch=19, cex=0.5)
qqline(fit$residuals)
plot(fit$fitted.values, fit$residuals, pch=19, cex=0.5,
xlab="Fitted values ($\\hat{y}_i$)", ylab="Residuals ($e_i$)")
