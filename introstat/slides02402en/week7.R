




xseq <- seq(-5, 10, len=200)
##
plot(xseq, pexp(xseq, 1/2), type="l", xlab="Exponential outcomes", ylab="Uniform outcomes")
set.seed(123)
us <- runif(5)
arrows(rep(-5,5), us, qexp(us, 1/2), us, lty=3, length=0.2)
arrows(qexp(us, 1/2), us, qexp(us, 1/2), 0, lty=3, length=0.2)
set.seed(345)
k = 10000 # Number of simulations 
X = rnorm(k, 2, 0.01) 
Y = rnorm(k, 3, 0.02) 
A = X*Y 
mean(A) 
sd(A) 
mean(abs(A-6)>0.1)
## Set the number of simulations:
k <- 100000
## 1. Simulate 10 exponentials with the right mean k times:
set.seed(9876.543)
simsamples <- replicate(k, rexp(10, 1/26.08))
## 2. Compute the mean of the 10 simulated observations k times:
simmeans <- apply(simsamples, 2, mean)
## 3. Find the two relevant quantiles of the k simulated means:
quantile(simmeans, c(0.025, 0.975)) 
hist(simmeans, col="blue", nclass=30)
## Set the number of simulations:
k <- 100000
## 1. Simulate 10 exponentials with the right mean k times:
set.seed(9876.543)
simsamples <- replicate(k, rexp(10, 1/26.08))
## 2. Compute the median of the n=1010 simulated observations k times:
simmedians <- apply(simsamples, 2, median)
## 3. Find the two relevant quantiles of the k simulated medians:
quantile(simmedians, c(0.025, 0.975)) 
hist(simmedians, col="blue", nclass=30)
## Read in the heights data:
x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)
n <- length(x)
## Define a Q3-function:
Q3 <- function(x){ quantile(x, 0.75)}
 ## Set the number of simulations:
k <- 100000
## 1. Simulate k samples of n=10 normals with the right mean and variance:
set.seed(9876.543)
simsamples <- replicate(k, rnorm(n, mean(x), sd(x)))
## 2. Compute the Q3 of the n=10 simulated observations k times:
simQ3s <- apply(simsamples, 2, Q3)
## 3. Find the two relevant quantiles of the k simulated medians:
quantile(simQ3s, c(0.005, 0.995)) 
## Day 1 data:
x <- c(32.6, 1.6, 42.1, 29.2, 53.4, 79.3, 
       2.3 , 4.7, 13.6, 2.0)
## Day 2 data:
y <- c(9.6, 22.2, 52.5, 12.6, 33.0, 15.2, 
       76.6, 36.3, 110.2, 18.0, 62.4, 10.3)
n1 <- length(x)
n2 <- length(y)
## Set the number of simulations:
k <- 100000
## 1. Simulate k samples of each n1=10 and n2=12 
## exponentials with the right means:
set.seed(9876.543)
simXsamples <- replicate(k, rexp(n1, 1/mean(x)))
simYsamples <- replicate(k, rexp(n2, 1/mean(y)))
## 2. Compute the difference between the simulated 
## means k times:
simDifmeans <- apply(simXsamples, 2, mean) - 
                    apply(simYsamples, 2, mean) 
## 3. Find the two relevant quantiles of the 
## k simulated differences of means:
quantile(simDifmeans, c(0.025, 0.975)) 
x1 <-  c(8, 24, 7, 20, 6, 20, 13, 15, 11, 22, 15) 
x2 <-  c(5, 11, 0, 15, 0, 20, 15, 19, 12, 0, 6) 
dif <- x1-x2 
dif
mean(dif)
t(replicate(5, sample(dif, replace = TRUE)))
k = 100000 

simsamples = replicate(k, sample(dif, replace = TRUE)) 
simmeans = apply(simsamples, 2, mean) 
quantile(simmeans, c(0.025,0.975)) 
k = 100000 
simsamples = replicate(k, sample(dif, replace = TRUE)) 
simmedians = apply(simsamples, 2, median) 
quantile(simmedians, c(0.025,0.975)) 
## Reading in no group: 
 x <- c(9, 10, 12, 6, 10, 8, 6, 20, 12) 
## Reading in yes group: 
 y <- c(14,15,19,12,13,13,16,14,9,12) 

k <- 100000 
simxsamples <- replicate(k, sample(x, replace = TRUE))
simysamples <- replicate(k, sample(y, replace = TRUE)) 
simmeandifs <- apply(simxsamples, 2, mean)-
                           apply(simysamples, 2, mean)  
quantile(simmeandifs, c(0.025,0.975)) 
k <- 100000 
simxsamples <- replicate(k, sample(x, replace = TRUE))
simysamples <- replicate(k, sample(y, replace = TRUE)) 
simmediandifs <- apply(simxsamples, 2, median)-
                        apply(simysamples, 2, median)  
quantile(simmediandifs, c(0.005,0.995)) 

