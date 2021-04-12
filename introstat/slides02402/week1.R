#################################
## Selected summary statistics ##
#################################

# Sample: Student heights
z <- c(185, 184, 194, 180, 182)

# Ordered heights
sort(z)

# Sample mean
mean(z)

# Sample median
median(z)

# Sample variance
var(z)

# Sample standard deviation
sd(z)

# 25%, 50%, 75% percentiles
quantile(z, probs = c(.25, .50, .75), type = 2)

# IQR
IQR(z, type = 2)


##################
## Scatter plot ##
##################

# Heights
x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)

# Weights
y <- c(65.5, 58.3, 68.1, 85.7, 80.5, 63.4, 102.6, 91.4, 86.7, 78.9)

# Scatter plot of weight against height
plot(x, y, xlab = "Height", ylab = "Weight", cex = 1.5, col = "blue", type = "n")
text(x, y, 1:10, cex = 1.5, col = "blue")
abline(v = mean(x), h = mean(y), lty = 2)
text(mean(x) + 3, 60, expression(bar(x) == 178), cex = 1.5)
text(163.5, mean(y) + 2.5, expression(bar(y) == 78.1), cex = 1.5)


################################
## Covariance and correlation ##
################################

# Sample covariance between height and weight
s_xy <- cov(x, y)
s_xy

# Sample standard deviations for height and weight
s_x <- sd(x)
s_x

s_y <- sd(y)
s_y

# Sample correlation between height and weight
s_xy/(s_x * s_y) # Method 1
cor(x ,y) # Method 2


###########################
## Example: Correlations ##
###########################

# Ensures that "randomness" is reproducible
set.seed(1234) 

# Simulate data
n <- 200
x <- runif(n)
y1 <- x + rnorm(n, sd = 0.1)
y2 <- -x + rnorm(n, sd = 0.5)
y3 <- rnorm(n)
y4 <- sin(pi*x) + rnorm(n, sd = 0.1)

# Make scatter plots
par(mfrow = c(2,2), mar = c(3.5, 3.5, 1.5, 0.5))
plot(x, y1, pch = 19, cex = 0.5, ylab = "y", main = expression(r%~~%0.95), cex.main = 2)
plot(x, y2, pch = 19, cex = 0.5, ylab = "y", main = expression(r%~~%-0.5), cex.main = 2)
plot(x, y3, pch = 19, cex = 0.5, ylab = "y", main = expression(r%~~%0), cex.main = 2)
plot(x, y4, pch = 19, cex = 0.5, ylab = "y", main = expression(r%~~%0), cex.main = 2)

par(mfrow = c(1,1))

#############
## R Intro ##
#############

# Adding numbers in the console
2 + 3

# Assigning a number to a variable
x <- 3
x

# Assigning a vector to a variable
x <- c(1, 4, 6, 2); x

# A vector of integers from 1 to 10
( x <- 1:10 )

# Height data from before
x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)

# Sample mean
mean(x)

# Sample median
median(x)

# Sample variance
var(x)

# Sample standard deviation
sd(x)

# Sample quartiles
quantile(x, type = 2)

# Sample quantiles 0%, 10%,..,90%, 100%
quantile(x, probs = seq(0, 1, by = 0.10), type = 2)

# A histogram of the heights
hist(x)

# A density histogram of the heights
hist(x, prob = TRUE, col = "red", nclass = 8)

# Empirical cumulative distribution function of the heights
plot(ecdf(x), verticals = TRUE)

# Basic box plot of the heights ('range = 0' makes it "basic")
boxplot(x, range = 0, col = "red", main = "Basic box plot")
text(1.3, quantile(x), c("Minimum", "Q1", "Median", "Q3", "Maximum"), col = "blue")

# Modified box plot of heights with an additional extreme observation (235 cm).
# The modified version is the default.
boxplot(c(x, 235), col = "red", main = "Modified box plot")
text(1.3, quantile(c(x, 235)), c("Minimum", "Q1", "Median", "Q3", "Maximum"), col = "blue")


