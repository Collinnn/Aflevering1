################################################################
## Example: Simulation of rolling a dice
##
## Make a random draw from (1,2,3,4,5,6) with equal probability
## for each outcome
sample(1:6, size=1)


## Simulate a fair dice

## Number of simulated realizations
n <- 30
## Draw independently from the set (1,2,3,4,5,6) with equal probability
xFair <- sample(1:6, size=n, replace=TRUE)
## Count the number of each outcome using the table function
table(xFair)
## Plot the pdf
par(mfrow=c(1,2))
plot(rep(1/6,6), type="h", col="red", ylim=c(0,1), lwd=10)
## Plot the empirical pdf
lines(table(xFair)/n, lwd=4)
## Plot the cdf
plot(cumsum(rep(1/6,6)), ylim=c(0,1), lwd=10, type="h", col="red")
## Add the empirical cdf
lines(cumsum(table(xFair)/n), lwd=4, type="h")


## Simulate an unfair dice

## Number of simulated realizations
n <- 30
## Draw independently from the set (1,2,3,4,5,6) with higher
## probability for a six
xUnfair <- sample(1:6, size=n, replace=TRUE, prob=c(rep(1/7,5),2/7))
## Plot the pdf
plot(c(rep(1/7,5),2/7), type="h", col="red", ylim=c(0,1), lwd=10)
## Plot the empirical density function
lines(table(xUnfair)/n, lwd=4)
## Plot the cdf
plot(cumsum(c(rep(1/7,5),2/7)), ylim=c(0,1), lwd=10, type="h", col="red")
## Add the empirical cdf
lines(cumsum(table(xUnfair)/n), lwd=4, type="h")



################################################################
## Random numbers and seed in R
##
## The random numbers generated depends on the seed

## Set the seed
set.seed(127)
## Generate a (pseudo) random sequence
sample(1:10)
## Generate again and see that new numbers are generated
sample(1:10)
## Set the seed and the same numbers as before just after the 
## seed was set are generated
set.seed(127)
sample(1:10)

    

################################################################
## Example: Simulate and estimate the mean
##
## Simulate a fair dice

## Number of realizations
n <- 30
## Simulate rolls with a fair dice
xFair <- sample(1:6, size=n, replace=TRUE)
## Calculate the sample mean
sum(xFair)/n
## or
mean(xFair)


## Simulate an unfair dice

## n realizations
xUnfair <- sample(1:6, size=n, replace=TRUE, prob=c(rep(1/7,5),2/7))
## Calculate the sample mean
mean(xUnfair)



################################################################
## Example: Simulate and estimate the sample variance
##
## Simulate a fair dice and calculate the sample variance

## Number of realizations
n <- 30
## Simulate
xFair <- sample(1:6, size=n, replace=TRUE)
## Calculate the distance for each sample to the sample mean
distances <- xFair - mean(xFair)
## Calculate the average of the squared distances
sum(distances^2)/(n-1)
## Or use the built in function
var(xFair)


## Plot the pdf of the six-sided dice and the four-sided dice
plot(rep(1/6,6), type="h", col="red")
plot(rep(1/4,4), type="h", col="blue")


## Calculate the means and variances of the dices

## The means
muXSixsided <- sum((1:6)*1/6)  # Six-sided
muXFoursided <- sum((1:4)*1/4)  # Four-sided
## The variances
sum((1:6-muXSixsided)^2*1/6)
sum((1:4-muXFoursided)^2*1/4)



################################################################
## Example: simulation with a binomial distribution
##
## Simulate a binomial distributed experiment

## Number of flips
nFlips <- 10
## The possible outcomes are (0,1,...,nFlips)
xSeq <- 0:nFlips
## Use the dbinom() function which returns the pdf, see ?dbinom
pdfSeq <- dbinom(xSeq, size=nFlips, prob=1/2)
## Plot the density
plot(xSeq, pdfSeq, type="h")



################################################################
## Example: Simulate 30 successive dice rolls
##  
## Simulate 30 successive dice rolls
Xfair <- sample(1:6, size=30, replace=TRUE)
## Count the number sixes obtained
sum(Xfair==6)
## This is equivalent to
rbinom(1, size=30, prob=1/6)



################################################################
## Example: Lottery probabilities using the hypergeometric distribution
##
## The probability of getting x numbers of the sheet in 25 drawings

## Number of successes in the population
a <- 8
## Size of the population
N <- 90
## Number of draws
n <- 25
## Plot the pdf, note: parameters names are different in the R function
plot(0:8, dhyper(x=0:8,m=a,n=N-a,k=n), type="h")



################################################################
## Example: Poisson rate scaling
##
## Probability of no goals in 10 minutes

## The Poisson pdf
dpois(x=0, lambda=3.4/9)



################################################################
## Example: Poisson distributed random variable
##
## Simulate a Poisson random variable

## The mean rate of events per interval
lambda <- 4
## Number of realizations
n <- 1000
## Simulate
x <- rpois(n, lambda)
## Plot the empirical pdf
plot(table(x)/n)
## Add the pdf to the plot
lines(0:20, dpois(0:20,lambda), type="h", col="red")



################################################################
## Example: The normal pdf
##
## Play with the normal distribution
  
## The mean and standard deviation
muX <- 0
sigmaX <- 1
## A sequence of x values
xSeq <- seq(-6, 6, by=0.1)
##
pdfX <- 1/(sigmaX*sqrt(2*pi)) * exp(-(xSeq-muX)^2/(2*sigmaX^2))
## Plot the pdf
plot(xSeq, pdfX, type="l", xlab="$x$", ylab="f(x)")



################################################################
## Example: R functions for the normal distribution
##  
## Do it for a sequence of x values
xSeq <- c(-3,-2,1,0,1,2,3)
## The pdf
dnorm(xSeq, mean=0, sd=1)
## The cdf
pnorm(xSeq, mean=0, sd=1)
## The quantiles
qnorm(c(0.01,0.025,0.05,0.5,0.95,0.975,0.99), mean=0, sd=1)
## Generate random normal distributed realizations
rnorm(n=10, mean=0, sd=1)
## Calculate the probability that that the outcome of X is between a and b
a <- 0.2
b <- 0.8
pnorm(b) - pnorm(a)
## See more details by running "?dnorm"



################################################################
## Example: Exponential distributed time intervals
##
## Simulate exponential waiting times

## The rate parameter: events per time
lambda <- 4
## Number of realizations
n <- 1000
## Simulate
x <- rexp(n, lambda)
## The empirical pdf
hist(x, probability=TRUE)
## Add the pdf to the plot
curve(dexp(xseq,lambda), xname="xseq", add=TRUE, col="red")


## Check the relation to the Poisson distribution
## by counting the events in each interval

## Sum up to get the running time
xCum <- cumsum(x)
## Use the hist function to count in intervals between the breaks,
## here 0,1,2,...
tmp <- hist(xCum, breaks=0:ceiling(max(xCum)))
## Plot the discrete empirical pdf
plot(table(tmp$counts)/length(tmp$counts))
## Add the Poisson pdf to the plot
lines(0:20, dpois(0:20,lambda), type="h", col="red")



################################################################
## Example: Random numbers in R
##
## Generate 100 normal distributed values
rnorm(100, mean=2, sd=3)
## Similarly, generate 100 uniform distributed values from 0 to 1 and 
## put them through the inverse normal cdf
qnorm(runif(100), mean=2, sd=3)



################################################################
## Example: Simulating the exponential distribution
##
## Three equivalent ways of simulating the exponential distribution
## with lambda=1/2
re1 <- -2*log(1-runif(10000))

re2 <- qexp(runif(10000), 1/2)

re3 <- rexp(10000, 1/2)

## Check the means and variances of each
c(mean(re1), mean(re2), mean(re3)) 

c(var(re1), var(re2), var(re3)) 



################################################################
## Simulation of chi^2-distribution
##
## Simulate 10 realizations from a standard normal distributed variable
n <- 10
rnorm(n)
## Now repeat this 200 times and calculate the sum of squares each time
## Note: the use of the function replicate: it repeats the
##       expression in the 2nd argument k times, see ?replicate
k <- 200
x <- replicate(k, sum(rnorm(n)^2))
## Plot the epdf of the sums and compare with the theoretical chisquare pdf
par(mfrow=c(1,2))
hist(x, freq=FALSE)
curve(dchisq(xseq,df=n), xname="xseq", add=TRUE, col="red")
## and the ecdf compared to the cdf
plot(ecdf(x))
curve(pchisq(xseq,df=n), xname="xseq", add=TRUE, col="red")



################################################################
## Example: Milk dose machines
##
## Chi-square milk dosing precision

## The sample size
n <- 20
## The claimed deviation
sigma <- 0.02
## The observed sample standard deviation
s <- 0.03
## Calculate the chi-square statistic
chiSq <- (n-1)*s^2 / sigma^2
## Use the cdf to calculate the probability of getting the observed 
## sample standard deviation or higher
1 - pchisq(chiSq, df=n-1)



################################################################
## Example: Relation between normal and chi^2
##
## Set simulate parameters
nu <- 8; k <- 200
## Generate the simulated realizations
z <- rnorm(k)
y <- rchisq(k, df=nu)
x <- z/sqrt(y/nu)
## Plot
par(mfrow=c(1,2))
hist(x, freq = FALSE)
curve(dt(xseq, df = nu), xname="xseq", add=TRUE, col="red")
plot(ecdf(x))
curve(pt(xseq, df = nu), xname="xseq", add=TRUE, col="red")



################################################################
## Example: Simulation of t-distribution
##
## Simulate
n <- 8; k <- 200; mu <- 1; sigma <- 2
## Repeat k times the simulation of a normal dist. sample:
## return the values in a (n x k) matrix
x <- replicate(k, rnorm(n, mean=mu, sd=sigma))
xbar <- apply(x, 2, mean)
s <- apply(x, 2, sd)
tobs <- (xbar - mu)/(s/sqrt(n))
## Plot
par(mfrow=c(1,2))
hist(tobs, freq = FALSE)
curve(dt(xseq, df=n-1), xname="xseq", add=TRUE, col="red")
plot(ecdf(tobs))
curve(pt(xseq, df=n-1), xname="xseq", add=TRUE, col="red")

    

################################################################
## Example: Electric car driving distance
##
## Calculate the probability of getting the sample mean under the 
## conditions that the claim is actually the real mean

## A test of 10 cars was carried out
n <- 10
## The claim is that the real mean is 400 km
muX <- 400
## From the sample the sample mean was calculated to
xMean <- 393
## And the sample deviation was
xSD <- 14
## Use the cdf to calculate the probability of obtaining this 
## sample mean or a lower value
pt( (xMean-muX) / (xSD/sqrt(n)), df=n-1)



################################################################
## Example: t-distribution
##
## Plot the t-distribution for different sample sizes

## First plot the standard normal distribution
curve(dnorm(x), xlim=c(-5,5), xlab="x", ylab="Density")
## Add the t-distribution for 30 observations
curve(dt(x,df=30-1), add=TRUE, col=2)
## Add the t-distribution for 15, 5 and 2 observations
curve(dt(x,df=15-1), add=TRUE, col=3)
curve(dt(x,df=5-1), add=TRUE, col=4)
curve(dt(x,df=2-1), add=TRUE, col=5)
## Add a legend
legend("topright", c("Norm.",paste("n=",c(30,15,5,2))), lty=1, col=1:6, cex=0.8)



################################################################
## Example: F-distribution
##
## Simulate
nu1 <- 8; nu2 <- 10; k <- 200
u <- rchisq(k, df=nu1)
v <- rchisq(k, df=nu2)
fobs <- (u/nu1) / (v/nu2)
## Plot
par(mfrow=c(1,2))
hist(fobs, freq = FALSE)
curve(df(x, df1=nu1, df2=nu2), add=TRUE, col="red")
plot(ecdf(fobs))
curve(pf(x, df1=nu1, df2=nu2), add=TRUE, col="red")



################################################################
## Example: Relation between normal and F-distribution
##
## Simulate
n1 <- 8; n2 <- 10; k <- 200
mu1 <- 2; mu2 <- -1
sigma1 <- 2; sigma2 <- 4
s1 <- replicate(k, sd(rnorm(n1, mean=mu1, sd=sigma1)))
s2 <- replicate(k, sd(rnorm(n2, mean=mu2, sd=sigma2)))
fobs <- (s1^2 / sigma1^2) / (s2^2 / sigma2^2)
## Plot
par(mfrow=c(1,2))
hist(fobs, freq=FALSE)
curve(df(xseq, df1=n1-1, df2=n2-1), xname="xseq", add=TRUE, col="red")
plot(ecdf(fobs))
curve(pf(xseq, df1=n1-1, df2=n2-1), xname="xseq", add=TRUE, col="red")
