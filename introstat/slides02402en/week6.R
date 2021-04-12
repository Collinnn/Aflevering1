






################################
## p-value for nutrition study example
1 - pt(3.01, df = 15.99)
x=seq(-3.5,3.5,by=0.01)
plot(x,dt(x,9),type="l",xaxt="n",xlab="",ylab="",yaxt="n")
polygon(c(qt(0.975,9),seq(qt(0.975,9),4, by=0.01),4, qt(0.975,9)),c(0,dt(seq(qt(0.975,9),4, by=0.01),9),0, 0),col="pink")
polygon(c(-qt(0.975,9),seq(-qt(0.975,9),-4, by=-0.01),-4,-qt(0.975,9)),c(0,dt(seq(-qt(0.975,9),-4, by=-0.01),9),0,0),col="pink")
text(0,0.1,"Acceptance",cex=2)
text(-3,0.05,"Rejection",cex=2)
text(3,0.05,"Rejection",cex=2)
axis(1, at=c(0), labels=expression(bar(x)-bar(y)), cex.axis=2)


################################
## Read the two-sample in R
xA=c(7.53, 7.48, 8.08, 8.09, 10.15, 8.4, 10.88, 6.13, 7.9)
xB=c(9.21, 11.51, 12.79, 11.85, 9.97, 8.79, 9.69, 9.68, 9.19)
## A two sample Welch t-test
t.test(xB, xA)


################################
## Read the two samples
x1=c(.7,-1.6,-.2,-1.2,-1,3.4,3.7,.8,0,2)
x2=c(1.9,.8,1.1,.1,-.1,4.4,5.5,1.6,4.6,3.4)
## Take the difference to get a paired t-test
dif=x2-x1
## Calculate the test and results
t.test(dif)
## Another way to calculate the paired setup
t.test(x2, x1, paired=TRUE)


################################
## Q-Q plot for each sample
par(mfrow=c(1,2))
qqnorm(xA, main="Hospital A")
qqline(xA)
qqnorm(xB, main="Hospital B")
qqline(xB)

################################
## Multiple (simulated) Q-Q plots for each sample
require(MESS)
fit1 <- lm(xA ~ 1)
qqnorm.wally <- function(x, y, ...) { qqnorm(y, ...); qqline(y, ...)}
wallyplot(fit1, FUN=qqnorm.wally, main="")
## Multiple (simulated) Q-Q plots for each sample
fit1 <- lm(xB ~ 1)
qqnorm.wally <- function(x, y, ...) { qqnorm(y, ...); qqline(y, ...)}
wallyplot(fit1, FUN=qqnorm.wally, main="")
power.t.test(n = 40, delta = 4, sd = 12.21, 
             type = "one.sample")
power.t.test(power = .80, delta = 4, sd = 12.21, 
             type = "one.sample")


################################
## Power calculation for two-sample
power.t.test(n = 10, delta = 2, sd = 1, sig.level = 0.05)


################################
## Sample size calculation for two-sample
power.t.test(power = 0.90, delta = 2, sd = 1, sig.level = 0.05)


################################
## Detectable effect size calculation for two-sample
power.t.test(power = 0.90, n = 10, sd = 1, sig.level = 0.05)

