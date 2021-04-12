#################################################################
### Simulation study: (Empirical) distribution of sample mean ###
#################################################################

# 'True' mean and standard deviation
mu <- 178
sigma <- 12

# Sample size
n <- 10

# Simulate normal distributed X_i for n = 10
x <- rnorm(n = n, mean = mu, sd = sigma)
round(x)

# Empirical density
hist(x, prob = TRUE, col = 'blue')

# Compute sample mean
mean(x)

# Repeat the simulated sampling many times (1000 samples)
mat <- replicate(1000, rnorm(n = n, mean = mu, sd = sigma))

# Compute the sample mean for each sample
xbar <- apply(mat, 2, mean)
xbar

# See the distribution of the sample means
hist(xbar, prob = TRUE, col = 'blue', nclass = 10, xlab = "sample mean",
     main = "Empirical distribution of the sample mean")

# Empirical mean of sample means
mean(xbar)


#####################################################################
### Density function for t-distribution with 9 degrees of freedom ###
### plotted together with that of a standard normal distribution  ###
#####################################################################

x <- seq(-4, 4, by = 0.01)
plot(x, dt(x, df = 9), type = "l", col = "red", ylab = "Density(x)")
lines(x, dnorm(x), type = "l")
text(2.5, 0.3,"Black: N(0,1)")
text(3, 0.1,"Red: t(9)", col = "red")

#############################################################
### Example: Heights                                      ###
### Computing a 99% CI for mu using the 't.test' function ###
#############################################################

# Data
x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)

# 99% CI for mu
t.test(x, conf.level = 0.99)

################################################################################
### CLT Example: Mean of independent and U(0,1)-distributed random variables ###
################################################################################

n <- 1 # Sample size
k <- 1000 # No. of samples (i.e. no. of means to be computed)

# Simulations from U(0,1)-distribution (k = 1000 samples, each of size n = 1)
u <- matrix(runif(k*n), ncol = n)

# Empirical density of means
hist(apply(u, 1, mean), col = "blue", main = "n = 1", xlab = "Means", prob = TRUE)

n <- 2 # Sample size
k <- 1000 # No. of samples (i.e. no. of means to be computed)

# Simulations from U(0,1)-distribution (k = 1000 samples, each of size n = 2)
u <- matrix(runif(k*n), ncol = n)

# Empirical density of means
hist(apply(u, 1, mean), col = "blue", main = "n = 2", xlab = "Means", xlim = c(0,1), prob = TRUE)

n <- 6 # Sample size
k <- 1000 # No. of samples (i.e. no. of means to be computed)

# Simulations from U(0,1)-distribution (k = 1000 samples, each of size n = 6)
u <- matrix(runif(k*n), ncol = n)

# Empirical density of means
hist(apply(u, 1, mean), col = "blue", main = "n = 6", xlab = "Means", xlim = c(0,1), prob = TRUE)

n <- 30 # Sample size
k <- 1000 # No. of samples (i.e. no. of means to be computed)

# Simulations from U(0,1)-distribution (k = 1000 samples, each of size n = 30)
u <- matrix(runif(k*n), ncol = n)

# Empirical density of means
hist(apply(u, 1, mean), col = "blue", main = "n = 30", xlab = "Means", xlim = c(0,1), prob = TRUE)


#####################################################################
### Simulation study: (Empirical) distribution of sample variance ###
#####################################################################

# 'True' mean and standard deviation
mu <- 178
sigma <- 12

# Sample size
n <- 10

# Simulate normal distributed X_i for n = 10
x <- rnorm(n = n, mean = mu, sd = sigma)
round(x)

# Empirical density
hist(x, prob = TRUE, col = 'blue')

# Compute sample mean
mean(x)

# Repeat the simulated sampling many times (1000 samples)
mat <- replicate(1000, rnorm(n = n, mean = mu, sd = sigma))

# Compute the sample variance for each sample
xbar <- apply(mat, 2, var)
xbar

# See the empirical distribution of the sample variances
hist((n-1)*xbar/sigma^2, prob = TRUE, col = 'blue', nclass = 30,
     main = "Empirical pdf of sample variance", xlab = "sample variance")

# Compare with ChiSquare pdf
xp <- seq(0,40,0.1)
lines(xp, dchisq(xp,df=n-1), col = "red", lw = 3)
text(25, 0.06,"ChiSquare(df=9)", col = "red")
