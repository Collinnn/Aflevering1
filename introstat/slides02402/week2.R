######################################
##### Repeat: Sample variance ########
######################################
x <- c(168,170,180,172,182,181)
#x1 <- c(3,4,1,2)
var_x <- sum((x-mean(x))^2) / (length(x)-1)
var(x)


#####################
### A random draw ###
#####################
# Set random seed ensuring that the random outcome is reproducible
set.seed(1)

# One random draw from (1,2,3,4,5,6) 
# with equal probability for each outcome
sample(1:6, size = 1)


##################################
### Empirical density function ###
### 'Fair dice' example        ###
##################################
# Set random seed ensuring that the random outcome is reproducible
set.seed(2)

# Number of simulated realizations (sample size)
n <- 30

# n independent random draws from the set (1,2,3,4,5,6) 
# with equal probability of each outcome
xFair <- sample(1:6, size = n, replace = TRUE)
xFair

# Count number of each outcome using the 'table' function
table(xFair)

# Plot the empirical pdf
plot(table(xFair)/n, lwd = 10, ylim = c(0,1), xlab = "x", 
     ylab = "Density f(x)")
# Add the true pdf to the plot
lines(rep(1/6,6), lwd = 4, type = "h", col = 2)
# Add a legend to the plot
legend("topright", c("Empirical pdf","True pdf"), lty = 1, col = c(1,2), 
       lwd = c(5, 2), cex = 0.8)


##################################
### Empirical density function ###
### 'Unfair dice' example      ###
##################################
# Set random seed ensuring that the random outcome is reproducible
set.seed(3)

# Number of simulated realizations (sample size)
n <- 30

# n independent random draws from the set (1,2,3,4,5,6) 
# with higher probability of getting a six
xUnfair <- sample(1:6, size = n, replace = TRUE, prob = c(rep(1/7,5),2/7))
xUnfair

# Plot the empirical pdf
plot(table(xUnfair)/n, lwd = 10, ylim = c(0,1), xlab = "x", 
     ylab = "Density f(x)")
# Add the true pdf to the plot
lines(c(rep(1/7,5),2/7), lwd = 4, type = "h", col = 2)
# Add a legend to the plot
legend("topright", c("Empirical pdf","True pdf"), lty = 1, col = c(1,2), 
       lwd = c(5, 2), cex = 0.8)


###############################################
### Simulating from a binomial distribution ###
###############################################
# Set random seed ensuring that the random outcome is reproducible
set.seed(4)

## Probability of success
p <- 0.1

## Number of repetitions
nRepeat <- 30

## Simulate Bernoulli experiment 'nRepeat' times
tmp <- sample(c(0,1), size = nRepeat, prob = c(1-p,p), replace = TRUE)

# Compute 'x'
sum(tmp)

## Or: Use the binomial distribution simulation function 
rbinom(1, size = 30, prob = p)


########################################################
### Example: Simulating number of six'es (fair dice) ###
########################################################
# Set random seed ensuring that the random outcome is reproducible
set.seed(5)

# Number of simulated realizations (sample size)
n <- 30

# n independent random draws from the set (1,2,3,4,5,6) 
# with equal probability for each outcome
xFair <- sample(1:6, size = n, replace = TRUE)

# Count the number of six'es
sum(xFair == 6)

## Do the same using 'rbinom()' instead
rbinom(n = 1, size = 30, prob = 1/6)


#################################
### Example: The binomial cdf ###
#################################
pbinom(q = 5, size = 10, prob = 0.6)
# Get help with:
?pbinom


###########################
### Example 1           ###
###########################
# Probability that all six errors are corrected within the same day
0.7^6
dbinom(x = 6, size = 6, prob = 0.7)


###########################
### Example 2           ###
###########################
# The probability that at least one of them has scratches
n <- 3
N <- 10
a <- 2
x <- 0
1 - choose(a,x)*choose(N-a,n-x)/choose(N,n)
1 - dhyper(x = x, m = a, n = N-a, k = n)


###########################
### Example 3           ###
###########################
# Probability of getting at most two patients per day
ppois(2, 0.3)

# Probability of getting at most 1 patient in three days
# You can simply scale lambda to 0.9 patients / 3 days
ppois(1, 0.9)


###########################
### Sample mean         ###
### 'Fair dice' example ###
###########################
# Set random seed ensuring that random outcome is reproducible
set.seed(6)

# Number of simulated realizations (sample size)
n <- 30

# Sample independently from the set (1,2,3,4,5,6)
# with equal probability of outcomes
xFair <- sample(1:6, size = n, replace = TRUE)

# Compute the sample mean
mean(xFair)


###########################
### Sample variance     ###
### 'Fair dice' example ###
###########################
# Set random seed ensuring that random outcome is reproducible
set.seed(7)

# Number of simulated realizations (sample size)
n <- 30

# Sample independently from the set (1,2,3,4,5,6)
# with equal probability of outcomes
xFair <- sample(1:6, size = n, replace = TRUE)

# Compute the sample variance
var(xFair)

