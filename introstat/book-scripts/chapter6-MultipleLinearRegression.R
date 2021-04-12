## Read data
x1 <- c(0.083, 0.409, 0.515, 0.397, 0.223, 0.292, 0.584, 0.491, 0.923, 
        0.280, 0.772, 0.857, 0.758, 0.850, 0.409, 0.055, 0.578, 0.745, 
        0.886, 0.031)
x2 <- c(0.625, 0.604, 0.077, 0.414, 0.343, 0.202, 0.840, 0.266, 0.831, 
        0.385, 0.821, 0.308, 0.440, 0.865, 0.111, 0.970, 0.192, 0.939, 
        0.149, 0.318)
y <- c(0.156, 1.234, 0.490, 1.649, 0.500, 0.395, 1.452, 0.416, 1.390, 
       0.234, 1.574, 0.349, 1.287, 1.709, 0.323, 1.201, 1.210, 1.787, 
       0.591, 0.110)

## Parameter estimation
fit <- lm(y ~ x1 + x2)

## Summary of fit (parameter estimates, standard error, p-values, etc.)
summary(fit)

-0.118+c(-1,1)*qt(0.975,df=17)*0.212
##     0.827+c(-1,1)*qt(0.975,df=17)*0.304
##     1.239+c(-1,1)*qt(0.975,df=17)*0.293

confint(fit, level = 0.95)

## New data
Xnew <- data.frame(x1 = c(0.5, 1), x2 = c(0.5, 1))

## Prediction
pred <- predict(fit, newdata = Xnew, se = TRUE)
pred

## Average of the independent variables in the original data
## c(mean(x1), mean(x2))

## Confidence interval
predict(fit, newdata = Xnew, interval = "confidence", level = 0.95)
## Prediction interval
predict(fit, newdata = Xnew, interval = "prediction", level = 0.95)

s3d<-scatterplot3d(x=x1,y=x2,z=y,pch=19,box=FALSE,color="white",
                 angle=40,xlab=expression(x[1]),
          ylab=expression(x[2]),zlab="$\\hat{\\sigma}_{\\hat{y}}$",zlim=c(0,0.5),ylim=c(0,1),xlim=c(0,1))
my.lm <- lm(y ~ x1 + x2)
#s3d$plane3d(my.lm, lty.box = "solid",col=gray(0.5))

n<-50
x1.plot<-seq(0,1,length=n)
x2.plot<-seq(0,1,length=n)

x2.plot<-seq(0,1,length=5)
for(i in 1:length(x2.plot)){
  x2.tmp<-rep(x2.plot[i],n)
  se<-predict(my.lm,newdata=data.frame(x1=x1.plot,
                 x2=x2.tmp),se.fit=TRUE)$se.fit
  se2<-sqrt(se^2+summary(my.lm)$sigma^2)
  s3d$points(x=x1.plot,y=x2.tmp,z=se,type="l",col="blue")
  s3d$points(x=x1.plot,y=x2.tmp,z=se2,type="l",col="red")
}

x2.plot<-seq(0,1,length=n)
x1.plot<-seq(0,1,length=5)
for(i in 1:length(x2.plot)){
  x1.tmp<-rep(x1.plot[i],n)
  se<-predict(my.lm,newdata=data.frame(x1=x1.tmp,
                 x2=x2.plot),se.fit=TRUE)$se.fit
  se2<-sqrt(se^2+summary(my.lm)$sigma^2)
  s3d$points(x=x1.tmp,y=x2.plot,z=se,type="l",col="blue")
  s3d$points(x=x1.tmp,y=x2.plot,z=se2,type="l",col="red")
}

## Confidence interval
predict(fit, newdata=Xnew, interval="confidence", level=1-alpha)
## Prediction interval
predict(fit, newdata=Xnew, interval="prediction", level=1-alpha)

set.seed(1234)
n <- 200
x <- runif(n)
y <- sin(pi*x) + rnorm(n,sd=0.1)

fit1 <- lm(y ~ x)
confint(fit1) 

x1 <- x; x2 <- x^2
fit2 <- lm(y ~ x1 + x2)
confint(fit2)

par(mfrow=c(1,2))
plot(fit1$fitted.values,fit1$residuals,pch=19,cex=0.5, xlab="fit1\\$fitted.values", ylab="fit1\\$residuals")
plot(fit2$fitted.values,fit2$residuals,pch=19,cex=0.5, xlab="fit2\\$fitted.values", ylab="fit2\\$residuals")

xplot <- seq(0,1,by=0.01)
plot(x, y, pch=19, cex=0.5, xlab="$x$", ylab="$y$")
lines(xplot,predict(fit1,newdata=data.frame(x=xplot)),col="green",lwd=2)
lines(xplot,predict(fit2,newdata=data.frame(x1=xplot,x2=xplot^2)),col="red",lwd=2)

set.seed(200)
n <- 100
x1 <- runif(n)
x2 <- x1 + rnorm(n, sd=0.01)
y <- x1 + x2 + rnorm(n, sd=0.5)
par(mfrow=c(1,2))
plot(x1, y, pch=19, cex=0.5, xlab=expression(x[1]))
plot(x2, y, pch=19, cex=0.5, xlab=expression(x[2]))

confint(lm(y ~ x1 + x2)) 

summary(lm(y ~ x2)) 

set.seed(200)
par(mfrow=c(1,2))
n <- 100
x1 <- runif(n)
x2 <- runif(n)
y <- x1 + 2*x2^2 + rnorm(n,sd=0.125)
plot(x1, y, pch=19, cex=0.5)
plot(x2, y, pch=19, cex=0.5)

par(mfrow=c(1,3), mar=c(4,3,0.2,0.2), cex=0.8)
fit <- lm(y ~ x1 + x2)
plot(fitted.values(fit), residuals(fit), pch=19, cex=0.7)
plot(x1, residuals(fit), pch=19, cex=0.7)
plot(x2, residuals(fit), pch=19, cex=0.7)

par(mfrow=c(1,3), mar=c(4,3,0.2,0.2), cex=0.8)
x3 <- x2^2
fit <- lm(y ~ x1 + x2 + x3)
plot(fitted.values(fit), residuals(fit), pch=19, cex=0.7)
plot(x1, residuals(fit), pch=19, cex=0.7)
plot(x2, residuals(fit), pch=19, cex=0.7)

summary(fit)
