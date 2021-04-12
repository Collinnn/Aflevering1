################################################################
## Example: Normal and t probabilities and quantiles
##
## The P(T>1.96) probability  for n=10
1 - pt(1.96, df = 9)
## The P(Z>1.96) probability
1 - pnorm(1.96)
## The P(T>1.96) probability  for n-values, 10, 20, ... ,50
1 - pt(1.96, df = seq(9, 49, by = 10))
## The P(T>1.96) probability  for n-values, 100, 200, ... ,500
1 - pt(1.96, df = seq(99, 499, by = 100))

## The standard normal 0.975% quantile
qnorm(0.975)
## The t-quantiles for n-values: 10, 20, ... ,50
## (rounded to 3 decimal points)
qt(0.975, df = seq(9, 49, by = 10))
## The t-quantiles for n-values: 100, 200, ... ,500 
## (rounded to 3 decimal points)
qt(0.975, df = seq(99, 499, by = 100))



################################################################
## Example: Student heights
##
## The t-quantiles for n=10:
qt(0.975, 9)



################################################################
## Example: Student heights
##
## The 99% confidence interval for the mean
x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)
n <- length(x)
mean(x) - qt(0.995, df = 9) * sd(x) / sqrt(n)
mean(x) + qt(0.995, df = 9) * sd(x) / sqrt(n)


## The 99% confidence interval for the mean
t.test(x, conf.level = 0.99)

set.seed(1234)


################################################################
## Example: Central Limit Theorem in practice
##
par(mfrow=c(2,2))
xlim <- c(0,1)
## Number of simulated samples
k <- 1000

## Number of observations in each sample
n <- 1
## Simulate k samples with n observations
## Note, the use of replicate: it repeats the second argument (here k times)
Xbar <- replicate(k, runif(n))
hist(Xbar, col="blue", main="n=1", xlab="Sample means", xlim=xlim)
## Increase the number of observations in each sample
## Note, the use of apply here: it takes the mean on the 2nd dimension 
## (i.e. column) of the matrix returned by replicate
n <- 2
Xbar <- apply(replicate(k, runif(n)), 2, mean)
hist(Xbar, col="blue", main="n=2", xlab="Sample means", xlim=xlim)
## Increase the number of observations in each sample
n <- 6
Xbar <- apply(replicate(k, runif(n)), 2, mean)
hist(Xbar, col="blue", main="n=6", xlab="Sample means", xlim=xlim)
## Increase the number of observations in each sample
n <- 30
Xbar <- apply(replicate(k, runif(n)), 2, mean)
hist(Xbar, col="blue", main="n=30", xlab="Sample means", xlim=xlim)

## We set a seed to be able get the same results
set.seed(12345.6789)


################################################################
## Example: Simulating many confidence intervals

## Simulate 1000 samples of n=50 observations, and
## calculate a CI from each sample
k <- 1000
ThousandCIs <- replicate(k, t.test(rnorm(n=50, mean=1, sd=1))$conf.int)
## Count how often 1 is covered
sum(ThousandCIs[1,] < 1 & 1 < ThousandCIs[2,])



################################################################
## Example: The chi-square-distribution
##
## The chisquare-distribution with df=9 (the density)
x <- seq(0, 35, by = 0.1)
plot(x, dchisq(x, df = 9), type = "l", ylab="Density")

## Reading the data into R:
xA <- c(.7,-1.6,-.2,-1.2,-1,3.4,3.7,.8,0,2)
xB <- c(1.9,.8,1.1,.1,-.1,4.4,5.5,1.6,4.6,3.4)
dif <- xB-xA
dif
t.test(dif)



################################################################
## Example: Sleeping medicine

## Enter sleep difference observations
x <- c(1.2, 2.4, 1.3, 1.3, 0.9, 1.0, 1.8, 0.8, 4.6, 1.4) 
n <- length(x)
## Compute the tobs - the observed test statistic
tobs <- (mean(x) - 0) / (sd(x) / sqrt(n))
tobs
## Compute the p-value as a tail-probability in the t-distribution
pvalue <- 2 * (1-pt(abs(tobs), df=n-1))
pvalue


t.test(x)



################################################################
## Example: Student heights

## The height sample
x <- c(168,161,167,179,184,166,198,187,191,179)

## Using histograms
par(mfrow=c(1,3), mar=c(4,3,1,1))
hist(x, xlab="Height", main="")
hist(x, xlab="Height", main="", breaks=8)
hist(x, xlab="Height", main="", breaks=2)


## Plot the empirical cdf
plot(ecdf(x), verticals = TRUE)
## Plot the best normal cdf
xseq <- seq(0.9*min(x), 1.1*max(x), length.out = 100) 
lines(xseq, pnorm(xseq, mean(x), sd(x))) 


## The expected quantiles in a 0 to 1 uniform distribution 
n <- length(x)
## They have equal distance
pseq <- (1:n-0.5)/n
## Plot the expected normal distribution quantiles
plot(x=qnorm(p=pseq), y=sort(x), xlab="Normal quantiles", 
     ylab="Sample quantiles")
## Mark the 1st and 3rd quantiles with crosses
points(x=qnorm(p=c(0.25,0.75)), y=quantile(x,probs=c(0.25,0.75)), 
       pch=3, col="red")
## Add a straight line through the 1st and 3rd quantiles
qqline(x)

set.seed(89473)

## Simulate 100 normal distributed observations
xsim <- rnorm(100,  mean(x), sd(x))
## Do the q-q normal plot with inbuilt functions
qqnorm(xsim)
qqline(xsim)

set.seed(98)

## Do the Wally plot

## To install the MESS package (run only once)
#install.packages("MESS")
## Load the MESS package
library(MESS)

## Define the plotting function
qqwrap <- function(x, y, ...){
  stdy <- (y-mean(y))/sd(y)
  qqnorm(stdy, main="", ...)
  qqline(stdy)}
## Do the Wally plot
wallyplot(x-mean(x), FUN=qqwrap, ylim=c(-3,3))



################################################################
## Example: Radon in houses
##
## Reading in the sample
radon <- c(2.4, 4.2, 1.8, 2.5, 5.4, 2.2, 4.0, 1.1, 1.5, 5.4, 6.3,
           1.9, 1.7, 1.1, 6.6, 3.1, 2.3, 1.4, 2.9, 2.9)
## A histrogram and q-q plot
par(mfrow = c(1,2))
hist(radon)
qqnorm(radon, ylab = "Sample quantiles", xlab = "Normal quantiles")
qqline(radon)


## Transform using the natural logarithm
logRadon <- log(radon)
par(mfrow = c(1,2))
hist(logRadon)
qqnorm(logRadon, ylab = "Sample quantiles", xlab = "Normal quantiles")
qqline(logRadon)


## A confidence interval and t-test
t.test(logRadon, conf.level=0.95)

## Back transform to original scale, now we get the median!
exp(0.9644)

## And the confidence interval on the original scale
exp(c(0.7054, 1.2234))



################################################################
## Example: Nutrition study
##
## Load the two samples
xA <- c(7.53, 7.48, 8.08, 8.09, 10.15, 8.4, 10.88, 6.13, 7.9)
xB <- c(9.21, 11.51, 12.79, 11.85, 9.97, 8.79, 9.69, 9.68, 9.19)
## Summary statistics
c(mean(xA), mean(xB))
c(var(xA), var(xB))
c(length(xA), length(xB))

t.test(xB,xA)
## Check computations with 3 decimal points:
ms=round(c(mean(xA), mean(xB)),3)
vs=round(c(var(xA), var(xB)),3)
ns=c(length(xA), length(xB))
(ms[2]-ms[1])/sqrt(vs[1]/ns[1]+vs[2]/ns[2])
nu=((vs[1]/ns[1]+vs[2]/ns[2])^2)/((vs[1]/ns[1])^2/(ns[1]-1)+(vs[2]/ns[2])^2/(ns[2]-1))
nu


## Keep the summary statistics
ms <- c(mean(xA), mean(xB))
vs <- c(var(xA), var(xB))
ns <- c(length(xA), length(xB))
## The observed statistic
t_obs <- (ms[2]-ms[1])/sqrt(vs[1]/ns[1]+vs[2]/ns[2])
## The degrees of freedom
nu <- ((vs[1]/ns[1]+vs[2]/ns[2])^2)/
  ((vs[1]/ns[1])^2/(ns[1]-1)+(vs[2]/ns[2])^2/(ns[2]-1))
## Print the result
t_obs
nu


## The probability of observed greater that t_obs
1 - pt(t_obs, df = nu)


## Use the built in function for t-test
t.test(xB, xA)


## The critical values for the test
qt(0.975, df = 15.99)



################################################################
## Example: Overlapping confidence intervals?
##
## The confidence intervals and joining the lower and upper limits
CIA <- t.test(xA)$conf.int
CIB <- t.test(xB)$conf.int
lower <- c(CIA[1], CIB[1])
upper <- c(CIA[2], CIB[2])
## The barplot of means WITH CIs added using gplots-package
## First install the package with: install.packages("gplots")
library(gplots)
barplot2(c(mean(xA),mean(xB)), plot.ci=TRUE, ci.l=lower, ci.u=upper,
         col = 2:3)

## This chunk makes the plot, since gplots is not working on the compute server
CIA <- t.test(xA)$conf.int
CIB <- t.test(xB)$conf.int
lower <- c(CIA[1], CIB[1])
upper <- c(CIA[2], CIB[2])
##
barplot(c(mean(xA),mean(xB)), col = 2:3, xlim=c(0,2.5), ylim=c(0,11.5))
arrows(0.7,lower[1],0.7,lower[2], code=3, angle=90, length=0.3)
arrows(1.4+0.5,upper[1],1.4+0.5,upper[2], code=3, angle=90, length=0.3)     


## The confidence intervals 
CIA 
CIB 



################################################################
## Example: Sleeping medicine
##
## Read the samples
x1 <- c(.7,-1.6,-.2,-1.2,-1,3.4,3.7,.8,0,2)
x2 <- c(1.9,.8,1.1,.1,-.1,4.4,5.5,1.6,4.6,3.4)
## Take the differences
dif <- x2 - x1
## t-test on the differences
t.test(dif)


## Give both samples, but make paired t-test
t.test(x2, x1, paired = TRUE)


## WRONG analysis
t.test(x1, x2)


## The sample variances of each sample and of the differences
var(x1)
var(x2)
var(x1-x2)


## The sample size for power=0.80
power.t.test(power=0.8, delta=4, sd=12.21, sig.level=0.05,
             type="one.sample")


## The power for n=50
power.t.test(n=50, delta=4, sd=12.21, sig.level=0.05,
             type="one.sample")


## The detectable effect size for n=50 and power=0.80
power.t.test(n=50, power=0.80, sd=12.21, sig.level=0.05, 
             type="one.sample")



################################################################
## Example: Two-sample power and sample size computations in R
##
## Finding the power of detecting a group difference of 2 
## with sigma=1 for n=10
power.t.test(n=10, delta=2, sd=1, sig.level=0.05)


## Finding the sample size for detecting a group difference of 2 
## with sigma=1 and power=0.9
power.t.test(power=0.90, delta=2, sd=1, sig.level=0.05)


## Finding the detectable effect size (delta) 
## with sigma=1, n=10 and power=0.9
power.t.test(power=0.90, n=10, sd=1, sig.level=0.05)







par(mfrow = c(3, 3))
par(cex=0.8)
for (i in 1:9){
    xr <- rnorm(9)
    qqnorm(xr, main="")
    qqline(xr)
}
