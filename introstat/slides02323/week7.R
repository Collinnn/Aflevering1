
################################
## Simuler fra nogle fordelinger
## Sæt et seed for at få samme udfald hver gang
set.seed(123)
## 100 realisationer fra binomialfordelingen:
## Antal successer fra 25 trækninger med 0.2 sandsynlighed for succes
rbinom(n=100, size=25, prob=0.2)
## Exponential fordelte
hist(rexp(n=100, rate=2), prob=TRUE)
## Plot the theoretical pdf
xseq <- seq(0,10,len=1000)
lines(xseq, dexp(xseq, rate=2))


################################
## Sæt et seed for at få samme udfald hver gang
set.seed(345)
## Antal simuleringer
k = 10000
## Simuler længderne af siderne
x = rnorm(k, 2, 0.01) 
y = rnorm(k, 3, 0.02) 
## Beregn arealet for hver simulering
area = x*y
## Beregn statistikker
## (stikprøve)Gennemsnit
mean(area) 
## (stikprøve)spredning
sd(area)
## Fraktion af arealer afviger mere end 0.1
sum(abs(area-6) > 0.1) / k


################################
## Beregn variansen af arealet med simulering
## Antal simuleringer
k = 10000
## Simuler længderne af siderne
x = rnorm(k, 2, 0.01) 
y = rnorm(k, 3, 0.02) 
## Beregn arealet for hver simulering
area = x*y
## Beregn variansen af de simulerede arealer
var(area)
 


################################
## Beregn konfidensinterval for middelværdien med simulering
## Sæt seed hvis sammen resultat ønskes
set.seed(758)
## Første gang: Simuler 10 observationer og beregn gennemsnit
simSample <- rexp(10, 1/26.08)
mean1 <- mean(simSample)
## Anden gang: Simuler 10 observationer og beregn gennemsnit
simSample <- rexp(10, 1/26.08)
mean2 <- mean(simSample)
## Tredje gang: Simuler 10 observationer og beregn gennemsnit
simSample <- rexp(10, 1/26.08)
mean3 <- mean(simSample)
## Gør det 100000 gange!
 


################################
## Beregn konfidensinterval for middelværdien med simulering
## Set the number of simulations:
k <- 100000
set.seed(321)
## 1. Simulate 10 exponentials k times and keep in a matrix:
simSamples <- replicate(k, rexp(10, 1/26.08))
## 2. Compute the mean of the 10 simulated observations k times:
simMeans <- apply(simSamples, 2, mean)
## 3. Find the two relevant quantiles of the k simulated means:
quantile(simMeans, c(0.025, 0.975)) 
hist(simMeans, col="blue", nclass=30)
abline(v=quantile(simMeans, c(0.025, 0.975)), col="red")


################################
## Beregn konfidensinterval for medianen med parametrisk bootstrapping
## Set the number of simulations:
k <- 100000
set.seed(543)
## 1. Simulate 10 exponentials with the right mean k times:
simSamples <- replicate(k, rexp(10, 1/26.08))
## 2. Compute the median of the n=10 simulated observations k times:
simMedians <- apply(simSamples, 2, median)
## 3. Find the two relevant quantiles of the k simulated medians:
quantile(simMedians, c(0.025, 0.975)) 
hist(simMedians, col="blue", nclass=30)
abline(v=quantile(simMedians, c(0.025, 0.975)), col="red")


################################
## Konfidensinterval for den øvre kvartil (Q_3) i en normalfordeling
## Read in the heights data:
x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)
n <- length(x)
## Define a (upper-quartile) Q3-function:
Q3 <- function(x){ quantile(x, 0.75)}
## Set the number of simulations:
k <- 100000
set.seed(543)
## 1. Simulate k samples of n=10 normals with the right mean and variance:
simSamples <- replicate(k, rnorm(n, mean(x), sd(x)))
## 2. Compute the Q3 of the n=10 simulated observations k times:
simQ3s <- apply(simSamples, 2, Q3)
## 3. Find the two relevant quantiles of the k simulated medians:
quantile(simQ3s, c(0.005, 0.995)) 


################################
## Konfidensinterval for the forskellen mellem to exponentielle middelværdier
## Day 1 data:
x <- c(32.6, 1.6, 42.1, 29.2, 53.4, 79.3, 
       2.3 , 4.7, 13.6, 2.0)
## Day 2 data:
y <- c(9.6, 22.2, 52.5, 12.6, 33.0, 15.2, 
       76.6, 36.3, 110.2, 18.0, 62.4, 10.3)
## Keep sample sizes
n1 <- length(x)
n2 <- length(y)


################################
## Konfidensinterval for the forskellen mellem to exponentielle middelværdier
## Set the number of simulations:
k <- 100000
set.seed(987)
## 1. Simulate k samples of each n1=10 and n2=12 
## exponentials with the right means:
simxSamples <- replicate(k, rexp(n1, 1/mean(x)))
simySamples <- replicate(k, rexp(n2, 1/mean(y)))
## 2. Compute the difference between the simulated 
## means k times:
simDifMeans <- apply(simxSamples, 2, mean) - 
                    apply(simySamples, 2, mean) 
## 3. Find the two relevant quantiles of the 
## k simulated differences of means:
quantile(simDifMeans, c(0.025, 0.975)) 


################################
## Parret test af middelværdiforskel med ikke-parametrisk bootstrapping
## Input the two cigaret use samples
x1 <-  c(8, 24, 7, 20, 6, 20, 13, 15, 11, 22, 15) 
x2 <-  c(5, 11, 0, 15, 0, 20, 15, 19, 12, 0, 6) 
## Calculate the difference
dif <- x1 - x2 
dif
## And the sample mean
mean(dif)


################################
## Resample several times
sample(dif, replace = TRUE)
sample(dif, replace = TRUE)
sample(dif, replace = TRUE)
sample(dif, replace = TRUE)


################################
## Resample calculate mean statistic many time, and find 95% confidence interval
k = 100000 
simSamples = replicate(k, sample(dif, replace = TRUE)) 
## Take the mean for every resample
simMeans = apply(simSamples, 2, mean) 
## Take the two quantiles to get the confidence interval
quantile(simMeans, c(0.025,0.975)) 


################################
## Konfidensintervallet for ændringen af median cigaretforbruget
## Simulate many times
k = 100000 
simsamples = replicate(k, sample(dif, replace = TRUE)) 
## Take the median for each resample
simMedians = apply(simsamples, 2, median) 
## Take the two quantiles to get the confidence interval
quantile(simMedians, c(0.025,0.975)) 


################################
## Tandsundhed og flaskebrug. Konfidensinterval for forskel i middelværdi
## Reading in no group: 
x <- c(9,10,12,6,10,8,6,20,12) 
## Reading in yes group: 
y <- c(14,15,19,12,13,13,16,14,9,12) 
## Resample many times
k <- 100000 
simxSamples <- replicate(k, sample(x, replace = TRUE))
simySamples <- replicate(k, sample(y, replace = TRUE)) 
## Take the mean for each time and subtract
simmeandifs <- apply(simxSamples, 2, mean) -
                           apply(simySamples, 2, mean)  
## Take the two quantiles to get the confidence interval
quantile(simmeandifs, c(0.025,0.975)) 


################################
## Tandsundhed og flaskebrug. Konfidensinterval for forskel i median
## Resample many times
k <- 100000 
simxSamples <- replicate(k, sample(x, replace = TRUE))
simySamples <- replicate(k, sample(y, replace = TRUE)) 
## Take the difference in medians
simmedianDiffs <- apply(simxSamples, 2, median)-
                        apply(simySamples, 2, median)  
## Take the two quantiles to get the confidence interval
quantile(simmedianDiffs, c(0.005,0.995)) 
