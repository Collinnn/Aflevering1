########################################################
### Example: One-way and two-way ANOVA with B&O data ###
########################################################

# Get the B&O data from the lmerTest-package
library(lmerTest)
data(TVbo)
head(TVbo) # First rows of the data

# Define factor identifying the 12 TV set and picture combinations 
TVbo$TVPic <- factor(TVbo$TVset:TVbo$Picture)
 
# Each of 8 assessors scored each of the 12 combinations twice.
# Average the two replicates for each assessor and combination of 
# TV set and picture
library(doBy)
TVbonoise <- summaryBy(Noise ~ Assessor + TVPic, data = TVbo, 
                       keep.names = T)

# One-way ANOVA of the noise (not the correct analysis!)
anova(lm(Noise ~ TVPic, data = TVbonoise))

# Two-way ANOVA of the noise (better analysis, week 12)
anova(lm(Noise ~ Assessor + TVPic, data = TVbonoise))

##############################################
### Simple example: Plots of data by group ###
##############################################

# Input data
y <- c(2.8, 3.6, 3.4, 2.3,
       5.5, 6.3, 6.1, 5.7,
       5.8, 8.3, 6.9, 6.1)

## Define treatment groups
treatm <- factor(c(1, 1, 1, 1,
                   2, 2, 2, 2,
                   3, 3, 3, 3))

## Plot data by treatment groups
par(mfrow = c(1,2))
plot(y ~ as.numeric(treatm), xlab = "Treatment", ylab = "y")
boxplot(y ~ treatm, xlab = "Treatment", ylab = "y")

###############################################
###  Plot F-distribution and critical value ###
###############################################

# Remember, this is "under H0" (i.e. we compute as if H0 is true)

# Number of groups
k <- 3

# Total number of observations
n <- 12

# Sequence for plot
xseq <- seq(0, 10, by = 0.1)

# Plot density of the F-distribution
plot(xseq, df(xseq, df1 = k-1, df2 = n-k), type = "l")

# Plot critical value for significance level 5%
cr <- qf(0.95, df1 = k-1, df2 = n-k)
abline(v = cr, col = "red") 

############################################
### One-way ANOVA using anova() and lm() ###
############################################

anova(lm(y ~ treatm))

###############################
### One-way ANOVA 'by hand' ###
###############################

k <- 3; n <- 12  # Number of groups k, total number of observations n

# Total variation, SST
(SST <- sum( (y - mean(y))^2 ))

# Residual variance after model fit, SSE
y1 <- y[1:4]; y2 <- y[5:8]; y3 <- y[9:12]

(SSE <- sum( (y1 - mean(y1))^2 ) + 
        sum( (y2 - mean(y2))^2 ) + 
        sum( (y3 - mean(y3))^2 ))

# Variance explained by the model, SS(Tr)
(SSTr <- SST - SSE)

# Test statistic
(Fobs <- (SSTr/(k-1)) / (SSE/(n-k)))

# P-value
(1 - pf(Fobs, df1 = k-1, df2 = n-k))

########################
### Model validation ###
########################

# Check assumption of homogeneous variance using, e.g., 
# a box plot.
plot(treatm, y)

# Check normality of residuals using a normal QQ-plot
fit1 <- lm(y ~ treatm)
qqnorm(fit1$residuals)
qqline(fit1$residuals)


