






##  # Getting the Bang and Olufsen data from the lmerTest-package:
library(lmerTest) # (Developed by us)
data(TVbo)

# Each of 8 assessors scored each of 12 combinations 2 times
# Let's look at only a single picture and one of the two reps:
# And let us look at the sharpness
TVbosubset <- subset(TVbo,Picture==1 & Repeat==1)[,c(1, 2, 9)]

sharp <- matrix(TVbosubset$Sharpness, nrow=8, byrow=T)
colnames(sharp) <- c("TV3", "TV2", "TV1")
rownames(sharp) <- c("Person 1", "Person 2", "Person 3", 
                     "Person 4", "Person 5", "Person 6", 
                     "Person 7", "Person 8")

library(xtable)
xtable(sharp)


##  # Getting the Bang and Olufsen data from the lmerTest-package:
library(lmerTest) # (Developed by us)
data(TVbo)

# Each of 8 assessors scored each of 12 combinations 2 times
# Let's look at only a single picture and one of the two reps:
# And let us look at the sharpness
TVbosubset <- subset(TVbo,Picture==1 & Repeat==1)[,c(1, 2, 9)]

sharp <- matrix(TVbosubset$Sharpness, nrow=8, byrow=T)
colnames(sharp) <- c("TV3", "TV2", "TV1")
rownames(sharp) <- c("Person 1", "Person 2", "Person 3", 
                     "Person 4", "Person 5", "Person 6", 
                     "Person 7", "Person 8")

library(xtable)
xtable(sharp)
####################################
## Input data and plot

## Observations
y <- c(2.8, 3.6, 3.4, 2.3,
       5.5, 6.3, 6.1, 5.7,
       5.8, 8.3, 6.9, 6.1)

## treatments (Groups, varieties)
treatm <- factor(c(1, 1, 1, 1,
                   2, 2, 2, 2,
                   3, 3, 3, 3))

## blocks (persons, fields)
block <- factor(c(1, 2, 3, 4, 
                  1, 2, 3, 4,
                  1, 2, 3, 4))

## for later formulas
(k <- length(unique(treatm)))
(l <- length(unique(block)))

## Plots
par(mfrow=c(1,2))

## Plot histogramms by treatments
plot(treatm, y, xlab="Treatments", ylab="y")
## Plot histogrammer by  blocks
plot(block, y, xlab="Blocks", ylab="y")
####################################
## Compute estimates of parameters in the model

## Sample mean
(muHat <- mean(y))
## Sample mean for each treatment
(alphaHat <- tapply(y, treatm, mean) - muHat)
## Sample mean for each  Block
(betaHat <- tapply(y, block, mean) - muHat)
################################
## Compute the  total variation, sum of squares SST

## SST for the example
(SST <- sum( (y - muHat)^2 ))
################################
## Compute Variance explained by the treatment part of the model

## Sum of squares  of treatment SS(Tr)
    (SSTr <- l * sum(alphaHat^2))
################################
## Compute Variance explained by the block  part of the model

## Sum of squares for blocks SS(Bl) for the example 
(SSBl <- k * sum(betaHat^2))
################################
## Compute residual variance after the model fit: 
## Residual sum of squares SSE
(SSE <- SST - SSTr - SSBl)
################################
## Plot the F distribution and see the critical value for treatments

## Remember, this is "under H0" (that is we compute as if H0 is true):
## Sequence for plot
xseq <- seq(0, 10, by=0.1)
## Plot the density of the  F distribution
plot(xseq, df(xseq, df1=k-1, df2=(k-1)*(l-1)), type="l")
##The critical value for significance level 5 %
cr <- qf(0.95, df1=k-1, df2=(k-1)*(l-1))
## Mark it in the plot 
abline(v=cr, col="red") 
## The value of the  test statistic
  (Ftr <- (SSTr/(k-1)) / (SSE/((k-1)*(l-1))))
## The p-value hence is:
(1 - pf(Ftr, df1=k-1, df2=(k-1)*(l-1)))
################################  
## Plot the F distribution  and see the critical value

## Remember, this is "under H0" (that is we compute as if H0 is true):
## Sequence for plot
xseq <- seq(0, 10, by=0.1)
## Plot the density of the  F distribution
plot(xseq, df(xseq, df1=l-1, df2=(k-1)*(l-1)), type="l")
##The critical value for significance level 5 %
cr <- qf(0.95, df1=l-1, df2=(k-1)*(l-1))
## Mark it in the plot 
abline(v=cr, col="red") 
## The value of the  test statistic
(Fbl <- (SSBl/(l-1)) / (SSE/((k-1)*(l-1))))
## The p-value hence is:
(1 - pf(Fbl, df1=l-1, df2=(k-1)*(l-1)))
################################
## All this can be found using anova() and lm()

anova(lm(y ~ treatm + block))
################################
## Check assumption of  homogeneous variance


## Save the  fit
fit <- lm(y ~ treatm + block)
## Box plot
par(mfrow=c(1,2))
plot(treatm, fit$residuals, y, xlab="Treatment")
## Box plot
plot(block, fit$residuals, xlab="Block")
################################
## Check the assumption  of  normality of residuals

## qq-normal plot of residuals
qqnorm(fit$residuals)
qqline(fit$residuals)

## Or with a  Wally plot
require(MESS)
qqwrap <- function(x, y, ...) {qqnorm(y, main="",...);
  qqline(y)}
## Can we see a deviating qq-norm plot?
wallyplot(fit$residuals, FUN = qqwrap)

