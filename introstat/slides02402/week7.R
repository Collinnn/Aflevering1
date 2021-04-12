######################################################################
### Simulating from the exponential distribution with lambda = 0.5 ###  
### using the U(0,1)-distribution                                  ###
######################################################################

xseq <- seq(-5, 10, len = 200)
plot(xseq, pexp(xseq, 1/2), type = "l", xlab = "Exponential outcomes", ylab = "Uniform outcomes", cex.lab = 2)
set.seed(123)
us <- runif(5)
arrows(rep(-5,5), us, qexp(us, 1/2), us, lty = 3, length = 0.2)
arrows(qexp(us, 1/2), us, qexp(us, 1/2), 0, lty = 3, length = 0.2)

##################################
### Simulation: Area of plates ###
##################################

set.seed(345)

k = 10000 # Number of simulations 
X = rnorm(k, 2, 0.01) 
Y = rnorm(k, 3, 0.02) 
A = X*Y 

mean(A) 
var(A) 
mean(abs(A - 6) > 0.1)

########################################################################
### Simulations: Mean of 10 exponential distributed random variables ###
########################################################################

set.seed(9876)

# Number of simulations
k <- 100000

# Simulate 10 exponentials with the 'right' mean k times
sim_samples <- replicate(k, rexp(10, 1/26.08))

# Compute the mean of the 10 simulated observations k times
sim_means <- apply(sim_samples, 2, mean)

# Find relevant quantiles of the k simulated means
quantile(sim_means, c(0.025, 0.975)) 

# Make histogram of simulated means
hist(sim_means, col = "blue", nclass = 30, main = "", prob = TRUE, xlab = "Simulated means")

##########################################################################
### Simulations: Median of 10 exponential distributed random variables ###
##########################################################################

set.seed(9876)

# Number of simulations
k <- 100000

# Simulate 10 exponentials with the 'right' mean k times
sim_samples <- replicate(k, rexp(10, 1/26.08))

# Compute the median of the 10 simulated observations k times
sim_medians <- apply(sim_samples, 2, median)

# Find relevant quantiles of the k simulated medians
quantile(sim_medians, c(0.025, 0.975)) 

# Make histogram of simulated medians
hist(sim_medians, col = "blue", nclass = 30, main = "", prob = TRUE, xlab = "Simulated medians")

######################################################
### Simulation: CI for Q3 of a normal distribution ###
######################################################

set.seed(9876)

# Heights data
x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)
n <- length(x)

# Define a Q3-function
Q3 <- function(x){ quantile(x, 0.75)}

# Set number of simulations
k <- 100000

# Simulate k samples of n = 10 normals with the 'right' mean and variance
sim_samples <- replicate(k, rnorm(n, mean(x), sd(x)))

# Compute the Q3 of the n = 10 simulated observations k times
simQ3s <- apply(sim_samples, 2, Q3)

# Find the two relevant quantiles of the k simulated Q3s
quantile(simQ3s, c(0.005, 0.995)) 

#################################################################################
### Simulations: Confidence interval for difference between exponential means ###
#################################################################################

set.seed(9876)

# Day 1 data
x <- c(32.6, 1.6, 42.1, 29.2, 53.4, 79.3,  2.3 , 4.7, 13.6, 2.0)
n1 <- length(x)

# Day 2 data
y <- c(9.6, 22.2, 52.5, 12.6, 33.0, 15.2, 76.6, 36.3, 110.2, 
       18.0, 62.4, 10.3)
n2 <- length(y)

# Set number of simulations:
k <- 100000

# Simulate k samples of each n1 = 10 and n2 = 12 exponentials 
# with the 'right' means

simX_samples <- replicate(k, rexp(n1, 1/mean(x)))
simY_samples <- replicate(k, rexp(n2, 1/mean(y)))

# Compute the difference between the simulated means k times
sim_dif_means <- apply(simX_samples, 2, mean) - 
  apply(simY_samples, 2, mean) 

# Find the relevant quantiles of the k simulated differences of means:
quantile(sim_dif_means, c(0.025, 0.975)) 

##############################################
### Example: Womens' cigarette consumption ###
##############################################

# Data 
x1 <-  c(8, 24, 7, 20, 6, 20, 13, 15, 11, 22, 15) 
x2 <-  c(5, 11, 0, 15, 0, 20, 15, 19, 12, 0, 6) 

# Compute differences
dif <- x1-x2 
dif

# Compute average difference
mean(dif)

# 95% CI for mean by non-parametric bootstrap
k = 100000 
sim_samples = replicate(k, sample(dif, replace = TRUE)) 
sim_means = apply(sim_samples, 2, mean) 
quantile(sim_means, c(0.025,0.975)) 

# 95% CI for median by non-parametric bootstrap
k = 100000 
sim_samples = replicate(k, sample(dif, replace = TRUE)) 
sim_medians = apply(sim_samples, 2, median) 
quantile(sim_medians, c(0.025,0.975)) 

#############################
### Example: Tooth health ###
#############################

# Reading in data
x <- c(9, 10, 12, 6, 10, 8, 6, 20, 12)
y <- c(14,15,19,12,13,13,16,14,9,12) 

# 95% CI for mean difference by non-parametric bootstrap
k <- 100000 
simx_samples <- replicate(k, sample(x, replace = TRUE))
simy_samples <- replicate(k, sample(y, replace = TRUE)) 
sim_mean_difs <- apply(simx_samples, 2, mean)-
                           apply(simy_samples, 2, mean)  
quantile(sim_mean_difs, c(0.025,0.975)) 

# 99% CI for median difference by non-parametric bootstrap
k <- 100000 
simx_samples <- replicate(k, sample(x, replace = TRUE))
simy_samples <- replicate(k, sample(y, replace = TRUE)) 
sim_median_difs <- apply(simx_samples, 2, median)-
                        apply(simy_samples, 2, median)  
quantile(sim_median_difs, c(0.005,0.995)) 
