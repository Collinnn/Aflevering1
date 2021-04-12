




## ## Mean
## mu <- 178
## ## Standard deviation
## sigma <- 12
## ## Sample size
## n <- 10
## ## Simulate  normally distributed  X_i
## x <- rnorm(n=n, mean=mu, sd=sigma)
## ## See the realizations
## x
## ## Empirical density
## hist(x, prob=TRUE, col='blue')
## ## Find the sample mean
## mean(x)
## ##  Find the sample variance
## var(x)
## ## Repeat the simulated sampling many times
## mat <- replicate(100, rnorm(n=n, mean=mu, sd=sigma))
## ## Find the sample mean for each of them
## xbar <- apply(mat, 2, mean)
## ## Now we have many realizations of the sample mean
## xbar
## ## See their distribution
## hist(xbar, prob=TRUE, col='blue')
## ## There mean
## mean(xbar)
## ## and sample variance
## var(xbar)
x=seq(-4,4, by=0.01)
plot(x,dt(x,9), type="l", col=2)
lines(x,dnorm(x), type="l")
text(2.5,0.3,"Black: standard normal")
text(3,0.1,"Red: t(9)", col = 2)
x=seq(-4,4, by=0.01)
plot(x,dt(x,9), type="l", col=2)
lines(x,dnorm(x), type="l")
text(2.5,0.3,"Black: standard normal")
text(3,0.1,"Red: t(9)", col = 2)
polygon(c(2,seq(2,4, by=0.01),4,2),c(0,dt(seq(2,4, by=0.01),9),0, 0),col="pink")
text(3.5,0.04, "P(T>2)=0.038",col=2)
x=seq(-4,4, by=0.01)
plot(x,dt(x,9), type="l", col=2)
lines(x,dnorm(x), type="l")
text(2.5,0.3,"Black: standard normal")
text(2.8,0.1,"Red: t(9)", col = 2)
polygon(c(2,seq(2,4, by=0.01),4,2),c(0,dnorm(seq(2,4, by=0.01)),0, 0),col="grey")
text(3.3,0.04, "P(Z>2)=0.023")
## The t-quantiles for n=10:
 qt(0.975,9)
 qt(0.995,9)
x <- c(168,161,167,179,184,166,198,187,191,179)
t.test(x,conf.level=0.99)
par(mar = c(4.5, 4.5, 3.5, 0.5))

n=1

k=1000

u=matrix(runif(k*n),ncol=n)

hist(apply(u,1,mean),col="blue",main="n=1",xlab="Means")
par(mar = c(4.5, 4.5, 3.5, 0.5))

n=2

k=1000

u=matrix(runif(k*n),ncol=n)

hist(apply(u,1,mean),col="blue",main="n=2",xlab="Means")
par(mar = c(4.5, 4.5, 3.5, 0.5))

n=6

k=1000

u=matrix(runif(k*n),ncol=n)

hist(apply(u,1,mean),col="blue",main="n=6",xlab="Means")
par(mar = c(4.5, 4.5, 3.5, 0.5))

n=30

k=1000

u=matrix(runif(k*n),ncol=n)

hist(apply(u,1,mean),col="blue",main="n=30",xlab="Means", nclass=15)
x=seq(-3.5,3.5,by=0.01)
plot(x,dt(x,9),type="l",xaxt="n",xlab="",ylab="",yaxt="n")
polygon(c(qt(0.975,9),seq(qt(0.975,9),4, by=0.01),4, qt(0.975,9)),c(0,dt(seq(qt(0.975,9),4, by=0.01),9),0, 0),col="pink")
polygon(c(-qt(0.975,9),seq(-qt(0.975,9),-4, by=-0.01),-4,-qt(0.975,9)),c(0,dt(seq(-qt(0.975,9),-4, by=-0.01),9),0,0),col="pink")
text(0,0.1,expression(P(group("|",frac(bar(X)-mu,S/sqrt(n)),"|")<t[0.975])), cex=1.3)
axis(1, at=c(-qt(0.975,9),qt(0.975,9)), labels=c(expression(t[0.025]),expression(t[0.975])))
axis(1, at=c(0), labels=0)
 x <- seq(0, 20, by = 0.1)

 plot(x, dchisq(x, df = 9), type = "l")
qchisq(c(0.025, 0.975), df = 19)
qchisq(c(0.025, 0.975), df = 9)

