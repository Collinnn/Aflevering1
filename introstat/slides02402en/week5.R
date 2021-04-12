




2 * (1-pt(4.67, 9))
## Enter data:
x <- c(1.2, 2.4, 1.3, 1.3, 0.9, 1.0, 1.8, 0.8, 4.6, 1.4) 
n <- length(x)
## Compute the tobs - the observed test statistic:
tobs <- (mean(x) - 0) / (sd(x) / sqrt(n))
## Compute the p-value as a tail-probability 
## in the t-distribution:
pvalue <- 2 * (1-pt(abs(tobs), df=n-1))
pvalue
t.test(x)
x=seq(-3.5,3.5,by=0.01)
plot(x,dt(x,9),type="l",xaxt="n",xlab="",ylab="",yaxt="n")
polygon(c(qt(0.975,9),seq(qt(0.975,9),4, by=0.01),4, qt(0.975,9)),c(0,dt(seq(qt(0.975,9),4, by=0.01),9),0, 0),col="pink")
polygon(c(-qt(0.975,9),seq(-qt(0.975,9),-4, by=-0.01),-4,-qt(0.975,9)),c(0,dt(seq(-qt(0.975,9),-4, by=-0.01),9),0,0),col="pink")
text(0,0.1,"Acceptance",cex=2)
text(-3,0.05,"Rejection",cex=2)
text(3,0.05,"Rejection",cex=2)
axis(1, at=c(-qt(0.975,9),qt(0.975,9)), labels=c(expression(t[0.025]),expression(t[0.975])), cex.axis=2)
axis(1, at=c(0), labels=0, cex.axis=2)
x=seq(-3.5,3.5,by=0.01)
plot(x,dt(x,9),type="l",xaxt="n",xlab="",ylab="",yaxt="n")
polygon(c(qt(0.975,9),seq(qt(0.975,9),4, by=0.01),4, qt(0.975,9)),c(0,dt(seq(qt(0.975,9),4, by=0.01),9),0, 0),col="pink")
polygon(c(-qt(0.975,9),seq(-qt(0.975,9),-4, by=-0.01),-4,-qt(0.975,9)),c(0,dt(seq(-qt(0.975,9),-4, by=-0.01),9),0,0),col="pink")
text(0,0.1,"Acceptance",cex=2)
text(-3,0.05,"Rejection",cex=2)
text(3,0.05,"Rejection",cex=2)
axis(1, at=c(-qt(0.975,9),qt(0.975,9)), labels=c(expression(bar(x)-t[0.025]*s/sqrt(n)),expression(bar(x)+t[0.975]*s/sqrt(n))), cex.axis=2)
axis(1, at=c(0), labels=expression(bar(x)), cex.axis=2)
x <- c(168,161,167,179,184,166,198,187,191,179)
hist(x, xlab="Height", main="", freq = FALSE)
lines(seq(160, 200, 1), dnorm(seq(160, 200, 1), mean(x), sd(x)))
xr <- rnorm(100, mean(x), sd(x))
hist(xr, xlab="Height", main="", freq = FALSE)
lines(seq(130, 230, 1), dnorm(seq(130, 230, 1), mean(x), sd(x)))
par(mar = c(3.5, 3.5, 1.5, 0.5))
plot(ecdf(x), verticals = TRUE)
xp <- seq(0.9*min(x), 1.1*max(x), length.out = 100) 
lines(xp, pnorm(xp, mean(x), sd(x))) 
par(mar = c(3.5, 3.5, 1.5, 0.5))
xr <- rnorm(100,  mean(x), sd(x))
plot(ecdf(xr), verticals = TRUE)
xp <- seq(0.9*min(xr), 1.1*max(xr), length.out = 100) 
lines(xp, pnorm(xp, mean(xr), sd(xr))) 
par(mar = c(3.5, 3.5, 1.5, 0.5))
qqnorm(x)
qqline(x)
par(mar = c(3.5, 3.5, 1.5, 0.5))
## READING IN THE DATA
radon<-c(2.4, 4.2, 1.8, 2.5, 5.4, 2.2, 4.0, 1.1, 1.5, 5.4, 6.3,
        1.9, 1.7, 1.1, 6.6, 3.1, 2.3, 1.4, 2.9, 2.9)
##A HISTOGRAM AND A QQ-PLOT
par(mfrow=c(1,2))
hist(radon)
qqnorm(radon,ylab = 'Sample quantiles',xlab = "Normal quantiles")
qqline(radon)
par(mar = c(3.5, 3.5, 1.5, 0.5))
par(mfrow=c(1,2))
radon<-c(2.4, 4.2, 1.8, 2.5, 5.4, 2.2, 4.0, 1.1, 1.5, 5.4, 6.3,
        1.9, 1.7, 1.1, 6.6, 3.1, 2.3, 1.4, 2.9, 2.9)
##TRANSFORM USING NATURAL LOGARITHM
logRadon<-log(radon)

hist(logRadon)
qqnorm(logRadon,ylab = 'Sample quantiles',xlab = "Normal quantiles")
qqline(logRadon)

