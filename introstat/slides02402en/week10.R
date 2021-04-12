




## 
## 
##  # Getting the Bang and Olufsen data from the lmerTest-package:
## library(lmerTest) # (Developed by us)
## data(TVbo)
## head(TVbo)
## # Defining the factor identifying the 12 TVset and Picture combs:
## TVbo$TVPic <- factor(TVbo$TVset:TVbo$Picture)
## # Each of 8 assessors scored each of 12 combinations 2 times
## # Averaging the two replicates for each Assessor and TVpic:
## library(doBy)
## TVbonoise <- summaryBy(Noise ~ Assessor + TVPic, data = TVbo,
##                        keep.names = T)
## # One-way ANOVA of the Noise: (Not the correct analysis!!)
## anova(lm(Noise ~ TVPic, data = TVbonoise))
## # Two-way ANOVA of the Noise: (Much better analysis - next week)
## anova(lm(Noise ~ Assessor + TVPic, data = TVbonoise))
####################################
## Input data and plot

## Observations
y <- c(2.8, 3.6, 3.4, 2.3,
       5.5, 6.3, 6.1, 5.7,
       5.8, 8.3, 6.9, 6.1)

## Groups (treatments)
treatm <- factor(c(1, 1, 1, 1,
                   2, 2, 2, 2,
                   3, 3, 3, 3))

## Plot
par(mfrow=c(1,2))
plot(as.numeric(treatm), y, xlab="Treatment", ylab="y")
##
plot(treatm, y, xlab="Treatment", ylab="y")
################################
## Compute the  total variation, sum of squares SST

## SST for the example
(SST <- sum( (y - mean(y))^2 ))
################################
## Compute residual variance after the model fit: 
## Residual sum of squares SSE

## Data in each Group
y1 <- y[1:4]
y2 <- y[5:8]
y3 <- y[9:12]
## SSE
(SSE <- sum( (y1 - mean(y1))^2 ) + 
        sum( (y2 - mean(y2))^2 ) + 
        sum( (y3 - mean(y3))^2 ))
################################
## Compute the variance explained by the model: 
## Sum of squares  of treatment SS(Tr)

## By simple subtraction:
(SSTr <- SST - SSE)
## Or by the defining formula:
4 * (mean(y1) - mean(y))^2 + 4 * (mean(y2) - mean(y))^2 + 4 * (mean(y3) - mean(y))^2
################################
## All this is computed by  anova() and lm()

anova(lm(y ~ treatm))
## Number of Groups
k <- 3
## Number in each  Group
ni <- 10
## Simulate data from model with 3 means
yModel1 <- rep( c(4, 5, -3), each=ni) + rnorm(ni*k, sd=1)
## Simulate data from model with 3 other means
yModel2 <- rep( c(1, 3, 1), each=ni) + rnorm(ni*k, sd=1)
## 3 Groups
group <- rep(1:k, each=ni)
## Plot them
par(mfrow=c(1,2))
plot(group, yModel1, ylim=range(yModel1,yModel2))
plot(group, yModel2, ylim=range(yModel1,yModel2))

## Compute SST: total variance, which is highest?
(SST1 <- sum( (yModel1 - mean(yModel1))^2 ))
(SST2 <- sum( (yModel2 - mean(yModel2))^2 ))

## Compute SSE: total residual variation, which is highest?
(SSE1 <- sum(tapply(yModel1, group, function(x){ sum((x - mean(x))^2) })))
(SSE2 <- sum(tapply(yModel2, group, function(x){ sum((x - mean(x))^2) })))

## ################################
## ## Plot the F distribution  and see the critical value
## 
## ## Remember, this is "under H0" (that is we compute as if H0 is true):
## ## Number of Groups
## k <- 3
## ## number of observations
## n <- 12
## ## Sequence for plot
## xseq <- seq(0, 10, by=0.1)
## ## Plot the density of the  F distribution
## plot(xseq, df(xseq, df1=k-1, df2=n-k), type="l")
## ##The critical value for significance level 5 %
## cr <- qf(0.95, df1=k-1, df2=n-k)
## ## Mark it in the plot
## abline(v=cr, col="red")
## ## The value of the  test statistic
## (F <- (SSTr/(k-1)) / (SSE/(n-k)))
## ## The p-value hence is:
## (1 - pf(F, df1=k-1, df2=n-k))
################################
## Plot the F distribution  and see the critical value

## Remember, this is "under H0" (that is we compute as if H0 is true):
## Number of Groups
k <- 3
## number of observations
n <- 12
## Sequence for plot
xseq <- seq(0, 10, by=0.1)
## Plot the density of the  F distribution
plot(xseq, df(xseq, df1=k-1, df2=n-k), type="l")
##The critical value for significance level 5 %
cr <- qf(0.95, df1=k-1, df2=n-k)
## Mark it in the plot 
abline(v=cr, col="red") 
## The value of the  test statistic
(F <- (SSTr/(k-1)) / (SSE/(n-k)))
## The p-value hence is:
(1 - pf(F, df1=k-1, df2=n-k))
################################
## All this is computed by  anova() and lm()

anova(lm(y ~ treatm))
################################
## Check assumption of  homogeneous variance

## Box plot
plot(treatm,y)
################################
## Check the assumption  of  normality of residuals

## qq-normal plot of residuals
fit1 <- lm(y ~ treatm)
qqnorm(fit1$residuals)
qqline(fit1$residuals)

## Or with a  Wally plot
require(MESS)
qqwrap <- function(x, y, ...) {qqnorm(y, main="",...);
qqline(y)}
## Can we see a deviating qq-norm plot?
wallyplot(fit1$residuals, FUN = qqwrap)
## 
## 
##  # Getting the Bang and Olufsen data from the lmerTest-package:
## library(lmerTest) # (Developed by us)
## data(TVbo)
## head(TVbo)
## # Defining the factor identifying the 12 TVset and Picture combs:
## TVbo$TVPic <- factor(TVbo$TVset:TVbo$Picture)
## # Each of 8 assessors scored each of 12 combinations 2 times
## # Averaging the two replicates for each Assessor and TVpic:
## library(doBy)
## TVbonoise <- summaryBy(Noise ~ Assessor + TVPic, data = TVbo,
##                        keep.names = T)
## # One-way ANOVA of the Noise: (Not the correct analysis!!)
## anova(lm(Noise ~ TVPic, data = TVbonoise))
## # Two-way ANOVA of the Noise: (Much better analysis - next week)
## anova(lm(Noise ~ Assessor + TVPic, data = TVbonoise))

