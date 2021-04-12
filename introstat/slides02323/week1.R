
################################
# Eksempel med terningeforsøgsdesign 
# Andel (sandsynlighed) vi vil teste for
p <- 1/6
# Signifikansniveau (hvor ofter vil vi lave denne fejl: 
#   Terningen er fair, men vi konkluderer den ikke er fair)
alpha <- 0.05
# Fejlmargen vi vil tillade (kaldet præcisionen, margin of error)
ME <- 0.01
# Beregn antal gange vi skal slå med terningen
p * (1-p) * (qnorm(1-alpha/2)/ME)^2


################################
# Basic R
# Adding numbers in the console
2+3
y <- 3
# Define a vector
x <- c(1, 4, 6, 2)
x
# A sequence from 1 to 10
x <- 1:10
x


################################
# Sample Mean and Median (data from book)
x <- c(168,161,167,179,184,166,198,187,191,179)
mean(x)
median(x)
# Sample variance and standard deviation
var(x)
sd(x)
# Sample quartiles
quantile(x, type=2)
# Sample quantiles 0%, 10%,..,90%, 100%:
quantile(x, probs=seq(0, 1, by=0.10), type=2)


################################
# Plotting
# A histogram of the heights:
hist(x)
# A density histogram (empirical distribution) of the heights:
hist(x, freq=FALSE, col="red", nclass=8)
# Empirical cumulated distribution function (ecdf)
plot(ecdf(x), verticals=TRUE)
# A basic boxplot of the heights: (range=0 makes it "basic")
boxplot(x, range=0, col="red", main="Basic boxplot")
text(1.3, quantile(x), c("Minimum","Q1","Median","Q3","Maximum"),
     col="blue")
# A modified boxplot of the heights with an 
# extreme observation, 235cm added:
# The modified version is the default
boxplot(c(x,235), col="red", main="Modified boxplot")
text(1.3, quantile(c(x,235)), c("Minimum","Q1","Median","Q3",
     "Maximum"),col="blue")
