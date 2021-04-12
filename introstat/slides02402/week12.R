###########################
### Bang & Olufsen data ###
###########################

# Get the B&O data from the lmerTest-package
library(lmerTest)
data(TVbo)

# Each of 8 assessors scored each of 12 combinations 2 times.
# Take a look at the sharpness scores for one single picture
# and one of the two repetitions
TVbo_sub <- subset(TVbo, Picture == 1 & Repeat == 1)[, c(1, 2, 9)]
sharp <- matrix(TVbo_sub$Sharpness, nrow = 8, byrow = T)
colnames(sharp) <- c("TV3", "TV2", "TV1")
rownames(sharp) <- c("Person 1", "Person 2", "Person 3", 
                     "Person 4", "Person 5", "Person 6", 
                     "Person 7", "Person 8")
library(xtable)
xtable(sharp)

###############################
### Input and plot toy data ###
###############################

# Observations
y <- c(2.8, 3.6, 3.4, 2.3,
       5.5, 6.3, 6.1, 5.7,
       5.8, 8.3, 6.9, 6.1)

# Treatments (groups, varieties)
treatm <- factor(c(1, 1, 1, 1,
                   2, 2, 2, 2,
                   3, 3, 3, 3))

# Blocks (persons, fields)
block <- factor(c(1, 2, 3, 4, 
                  1, 2, 3, 4,
                  1, 2, 3, 4))

# No. of treatments and no. of blocks (for later formulas)
(k <- length(unique(treatm)))
(l <- length(unique(block)))

# Box plots by treatment
plot(treatm, y, xlab = "Treatment", ylab = "y")

# Box plots by  block
plot(block, y, xlab = "Block", ylab="y")

####################################################
### Compute estimates of parameters in the model ###
####################################################

# Sample mean
(mu_hat <- mean(y))

# Sample mean deviation for each treatment
(alpha_hat <- tapply(y, treatm, mean) - mu_hat)

# Sample mean deviation for each block
(beta_hat <- tapply(y, block, mean) - mu_hat)

########################################################
### Compute the  total variation, sum of squares SST ###
########################################################

# SST for the example data
(SST <- sum((y - mu_hat)^2))

#####################################################################
### Compute variance explained by the treatment part of the model ###
#####################################################################

# Sum of squares  of treatment SS(Tr) for the example data
(SSTr <- l * sum(alpha_hat^2))

#################################################################
### Compute variance explained by the block part of the model ###
#################################################################

# Sum of squares for blocks SS(Bl) for the example data
(SSBl <- k * sum(beta_hat^2))

#####################################################
### Compute residual variance after the model fit ###
#####################################################

# Residual sum of squares SSE
(SSE <- SST - SSTr - SSBl)

##########################################
### Hypothesis test: Treatment effect? ###
##########################################

# Plot density of relevant F-distribution. Remember that this is "under H0" 
# (computed as if H0 were true)
xseq <- seq(0, 10, by = 0.1)
plot(xseq, df(xseq, df1 = k-1, df2 = (k-1)*(l-1)), type = "l")

# Show critical value (5% signif. level) for test of treatment hypothesis
critical_value <- qf(0.95, df1 = k-1, df2 = (k-1)*(l-1))
abline(v = critical_value, col = "red") 

# Compute value of the  test statistic
(FTr <- (SSTr/(k-1)) / (SSE/((k-1)*(l-1))))

# Compute p-value for the test
1 - pf(FTr, df1 = k-1, df2 = (k-1)*(l-1))

######################################
### Hypothesis test: Block effect? ###
######################################

# Plot density of relevant F-distribution. Remember that this is "under H0" 
# (computed as if H0 were true)
xseq <- seq(0, 10, by = 0.1)
plot(xseq, df(xseq, df1 = l-1, df2 = (k-1)*(l-1)), type = "l")

# Show critical value (5% signif. level) for test of treatment hypothesis
critical_value <- qf(0.95, df1 = l-1, df2 = (k-1)*(l-1))
abline(v = critical_value, col = "red") 

# Compute value of the  test statistic
(FBl <- (SSBl/(l-1)) / (SSE/((k-1)*(l-1))))

# Compute p-value for the test
1 - pf(FBl, df1 = l-1, df2 = (k-1)*(l-1))

###################################################
### Two-way ANOVA output using anova() and lm() ###
###################################################

anova(lm(y ~ treatm + block))

############################
### Model validation:    ###
### Variance homogeneity ###
############################

# Save the  fitted model
fit <- lm(y ~ treatm + block)

# Make box plots of residuals
par(mfrow = c(1,2))
plot(treatm, fit$residuals, xlab = "Treatment")
plot(block, fit$residuals, xlab = "Block")

#########################
### Model validation: ###
### Normality         ###
#########################

# Normal QQ-plot of the residuals
qqnorm(fit$residuals)
qqline(fit$residuals)


