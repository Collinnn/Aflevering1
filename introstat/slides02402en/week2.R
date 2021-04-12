




  sample(1:6, size=1)
    ## Number of simulated realizations
    n <- 30

    ## Draw independently from the set (1,2,3,4,5,6) with 
    ## equal probability
    xFair <- sample(1:6, size=n, replace=TRUE)

    ## Print the values
    xFair
    
    ## Count the number of each outcome using the table function
    table(xFair)
    
    ## Plot the empirical pdf
    plot(table(xFair)/n, lwd=10, ylim=c(0,1), xlab="x", ylab="Density")
    
    ## Add the pdf to the plot
    lines(rep(1/6,6), lwd=4, type="h", col=2)
    ## Add a legend to the plot
    legend("topright", c("Empirical pdf","pdf"), lty=1, col=c(1,2), 
           lwd=c(5,2), cex=0.8)
## Simuler en ikke-fair terning

## Number of simulated realizations
n <- 30

## Draw independently from the set (1,2,3,4,5,6) with 
## higher probability for a six
xUnfair <- sample(1:6, size=n, replace=TRUE, prob=c(rep(1/7,5),2/7))

## Plot the empirical density function
plot(table(xUnfair)/n, lwd=10, ylim=c(0,1), xlab="x", ylab="Density")

## Add the pdf to the plot
lines(c(rep(1/7,5),2/7), lwd=4, type="h", col=2)

## Add a legend
legend("topright", c("Empirical pdf","pdf"), lty=1, col=c(1,2), lwd=c(5,2))
## Simulate a binomial distribution

## Probability  of success
p <- 0.1
## Number of repeats
nRepeat <- 30
## Simulate Bernoulli experiment nRepeat times
tmp <- sample(c(0,1), size=nRepeat, prob=c(1-p,p), replace=TRUE)
## x is now
sum(tmp)

## Make similar with binomial distribution simulation function 
rbinom(1, size=30, prob=p)

################
## Fair dice example

## Number of simulated realizations
n <- 30
## Sample independent from the set (1,2,3,4,5,6) with same probabilities
xFair <- sample(1:6, size=n, replace=TRUE)
## Count the number of 6'es
sum(xFair == 6)

## Make similar with rbinom()
rbinom(n=1, size=30, prob=1/6)
## Example 1: The probability for at alle 6 indraporterede fejl udbedres samme dag?
## Example 1: The probability for at 2 eller færre fejl bliver udbedret samme dag?
## Example 2: The probability for at få mindst en harddisk med skrammer?
## Example 3.1: The probability for at højst 2 patienter indlægges samme dag?
## Example 3.2: The probability for at præcis 2 patienter indlægges samme dag?
## Example 3.3: The probability for at mindst 1 patient indlægges en dag?
## Example 3.4: The probability for at præcis 1 patient indlægges hver tredje dag?
## Binomial distribution function

pbinom(q=5, size=10, prob=0.6)
## Get the hep with
?pbinom
## Simulate a  fair dice

## NUmber of simulated realizations
n <- 30
## Sample independently from the set (1,2,3,4,5,6) with 
## equal probability
xFair <- sample(1:6, size=n, replace=TRUE)

## Find the sample mean
mean(xFair)
  ## Simulate a  fair dice

  ## NUmber of simulated realizations
  n <- 30
  ## Sample independently from the set (1,2,3,4,5,6) with 
  ## equal probability
  xFair <- sample(1:6, size=n, replace=TRUE)

  ## Find the sample variance
var(xFair)

