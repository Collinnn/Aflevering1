set.seed(345)
################################################################
## Example: Rectangular plates
##
## Number of simulations 
k <- 10000 
## Simulate X, Y and then A
X <- rnorm(k, 2, 0.01) 
Y <- rnorm(k, 3, 0.02) 
A <- X*Y 


## The mean and std. deviation of the simulated values
mean(A) 
sd(A) 

mean(abs(A-6)>0.1)


 

################################################################
## One-sample confidence interval for mu
##
## Read the data 
x <- c(32.6, 1.6, 42.1, 29.2, 53.4, 79.3, 2.3 , 4.7, 13.6, 2.0)
n <- length(x) 
## Set the number of simulations
k <- 100000
## 1. Simulate 10 exponentials with the right mean k times
set.seed(9876)
simsamples <- replicate(k, rexp(10,1/26.08))
## 2. Compute the mean of the 10 simulated observations k times
simmeans <- apply(simsamples, 2, mean)
## 3. Find the two relevant quantiles of the k simulated means
quantile(simmeans, c(0.025, 0.975)) 


## Histogram of the simulated means
hist(simmeans, col="blue", nclass=30, cex.main=0.8)

set.seed(9876)


################################################################
## Confidence interval for the median assuming an exponential distribution
##
## Load the data
x <- c(32.6, 1.6, 42.1, 29.2, 53.4, 79.3, 2.3 , 4.7, 13.6, 2.0)
n <- length(x)
## Set the number of simulations
k <- 100000
## 1. Simulate k samples of n=10 exponentials with the right mean
simsamples <- replicate(k, rexp(n,1/26.08))
## 2. Compute the median of the n=10 simulated observations k times:
simmedians <- apply(simsamples, 2, median)
## 3. Find the two relevant quantiles of the k simulated medians:
quantile(simmedians, c(0.025, 0.975)) 


## See the simulated medians
hist(simmedians, col="blue", nclass=30, cex.main=0.8)

Q3 <- function(x){ quantile(x, 0.75) }

## load in the data
x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)
n <- length(x)
 ## Set the number of simulations:
k <- 100000
## 1. Simulate k samples of n=10 normals with the right mean and variance:
set.seed(9876)
simsamples <- replicate(k, rnorm(n, mean(x), sd(x)))
## 2. Compute the Q3 of the n=10 simulated observations k times:
simQ3 <- apply(simsamples, 2, Q3)
## 3. Find the two relevant quantiles of the k simulated medians:
quantile(simQ3, c(0.005, 0.995)) 

hist(simQ3, col="blue", cex.main=0.8)

set.seed(9876)


################################################################
## CI for the difference of two means from exponential distributed data
##
## Read the data
x <- c(32.6, 1.6, 42.1, 29.2, 53.4, 79.3, 2.3 , 4.7, 13.6, 2.0)
y <- c(9.6, 22.2, 52.5, 12.6, 33.0, 15.2, 76.6, 36.3, 110.2, 18.0, 
       62.4, 10.3)
n1 <- length(x)
n2 <- length(y)
## Set the number of simulations
k <- 100000

## 1. Simulate k samples of each n1=10 and n2=12 exponentials 
##    with the right means
simXsamples <- replicate(k, rexp(n1,1/mean(x)))
simYsamples <- replicate(k, rexp(n2,1/mean(y))) 
## 2. Compute the difference between the simulated means k times
simDifmeans <- apply(simXsamples,2,mean) - apply(simYsamples,2,mean) 
## 3. Find the two relevant quantiles of the k simulated differences 
##    in sample means
quantile(simDifmeans, c(0.025, 0.975), cex.main=0.8) 


## The histogram of the simulated sample mean differences
hist(simDifmeans, col="blue", nclass=25, cex.main=0.8)



################################################################
## Nutrition study: comparing medians assuming normal distributions
##
## Read the data
xA <- c(7.53, 7.48, 8.08, 8.09, 10.15, 8.4, 10.88, 6.13, 7.9)
xB <- c(9.21, 11.51, 12.79, 11.85, 9.97, 8.79, 9.69, 9.68, 9.19)
nA <- length(xA)
nB <- length(xB)

set.seed(9843)

## Set the number of simulations
k <- 100000
## 1. Simulate k samples of each nA=9 and nB=9 exponentials with the 
## right means and standard deviations
simAsamples <- replicate(k, rnorm(nA, mean(xA), sd(xA)))
simBsamples <- replicate(k, rnorm(nB, mean(xB), sd(xB)))
 
## 2. Compute the difference between the simulated medians k times
simDifmedians <- apply(simAsamples, 2, median) - apply(simBsamples, 2, median) 
## 3. Find the two relevant quantiles of the k simulated differences of means
quantile(simDifmedians, c(0.025, 0.975)) 

 

################################################################
## Example: Women's cigarette consumption
##
## Read and calculate the differences for each woman before and after
x1 <-  c(8, 24, 7, 20, 6, 20, 13, 15, 11, 22, 15) 
x2 <-  c(5, 11, 0, 15, 0, 20, 15, 19, 12, 0, 6) 
dif <- x1-x2 
dif 


t(replicate(5, sample(dif, replace=TRUE)))


## Number of simulated samples
k <- 100000 
## Simulate
simsamples <- replicate(k, sample(dif, replace=TRUE)) 
## Calculate the mean of each simulated sample
simmeans <- apply(simsamples, 2, mean) 
## Quantiles of the differences gives the CI
quantile(simmeans, c(0.025,0.975)) 

 
## The 95% CI for the median change
k <- 100000 
simsamples <- replicate(k, sample(dif, replace = TRUE)) 
simmedians <- apply(simsamples, 2, median) 
quantile(simmedians, c(0.025,0.975)) 


 
################################################################
## Example: Teeth and bottle
##
## Reading in "no bottle" group
x <- c(9, 10, 12, 6, 10, 8, 6, 20, 12) 
## Reading in "yes bottle" group
y <- c(14,15,19,12,13,13,16,14,9,12) 

## Number of simulations
k <- 100000 
## Simulate each sample k times
simxsamples <- replicate(k, sample(x, replace=TRUE))
simysamples <- replicate(k, sample(y, replace=TRUE)) 
## Calculate the sample mean differences
simmeandifs <- apply(simxsamples,2,mean) - apply(simysamples,2,mean)  
## Quantiles of the differences gives the CI
quantile(simmeandifs, c(0.025,0.975)) 


## CI for the median differences
simmediandifs <- apply(simxsamples,2,median)-apply(simysamples,2,median)  
quantile(simmediandifs, c(0.005,0.995)) 



################################################################
## Bootstrapping - a further perspective
##
## Install the bootstrap package
install.packages("bootstrap")


## Calculate the 95% CI for the Teech and bottle example above
library(bootstrap) 
quantile(bootstrap(dif,k,mean)$thetastar, c(0.025,0.975)) 



################################################################
## Example: Bootstrapping the mean mu with the boot package
##
## Read and calculate the differences for each woman before and after
x1 <- c(8,24,7,20,6,20,13,15,11,22,15) 
x2 <- c(5,11,0,15,0,20,15,19,12,0,6) 
dif <- x1-x2 


## Define function for calculating the mean of the d indexes
samplemean <- function(x, d){ mean(x[d]) }


## Call the new function
mean(dif)
samplemean(dif,1:11)
samplemean(dif,c(1,3))
dif
dif[c(1,3)]
mean(dif[c(1,3)])


## Load the boot package
library(boot)

## Non-parametric bootstrap of the mean difference:
k <- 10000
meandifboot <- boot(dif, samplemean, k) 
plot(meandifboot)


## Percentile bootstrap CI:
boot.ci(meandifboot, type="perc")

## Bias Corrected Accelerated CI:
boot.ci(meandifboot, type="bca")


## Define a function for taking the median in the needed format
samplemedian <- function(x, d) {
  return(median(x[d]))
}
## Non-parametric bootstrap of the x1 median
b <- boot(x1,samplemedian,k) 
boot.ci(b, type="bca")

## Non-parametric bootstrap of the Dif coef. of var
samplecoefvar <- function(x, d) {
  return(sd(x[d])/mean(x[d]))
}
##
b <- boot(dif,samplecoefvar,k) 
boot.ci(b, type="bca")

## Example with data frame and two variables

## Making our own data for the example - into a data frame:
x <- runif(100)
y <- x + 2*runif(100)
D <- data.frame(x, y)
head(D)
plot(D)
cor(D$x,D$y)


## The correlation function on the right form:
mycor  <- function(D, d) {
  E <- D[d,]
  return(cor(E$x, E$y))
}

## Check:
mycor(D, 1:100)
mycor(D, 1:15)

## Doing the bootstrap on the data frame:
b <-  boot(D, mycor, 10000)
boot.ci(b, type="bca")

## Bootstrapping an R-Squared from an MLR using 
## the data set mtcars from the boot package:
## (And showing how to send EXTRA stuff to your function)

# function to obtain R-Squared from the data 
# AND working for ANY model fit you want!!

rsq <- function(formula, data, d) {
  fit <- lm(formula, data=data[d,])
  return(summary(fit)$r.square)
} 

## Bootstrapping with 1000 replications 
b <- boot(mtcars, rsq, 1000, formula=mpg~wt+disp)
plot(b)

boot.ci(b, type="bca")
