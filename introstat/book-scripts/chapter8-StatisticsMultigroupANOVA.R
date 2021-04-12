y <- c(2.8, 3.6, 3.4, 2.3,
     5.5, 6.3, 6.1, 5.7,
     5.8, 8.3, 6.9, 6.1)
treatm <- factor(c(1, 1, 1, 1,
                 2, 2, 2, 2,
                 3, 3, 3, 3))
plot(as.numeric(treatm), y, pch=19, xlim=c(0.5, 3.5), axes=FALSE, xlab="Treatment", ylab="", cex=0.7) 
lines(c(0,4),c(1,1)*mean(y),col=2)
lines(c(1,1),c(mean(y),mean(y[treatm==1])),col="blue")
lines(c(2,2),c(mean(y),mean(y[treatm==2])),col="blue")
lines(c(3,3),c(mean(y),mean(y[treatm==3])),col="blue")
box()
axis(1,at=c(1,2,3),labels=c("1","2","3"))
axis(2,at=c(mean(y),mean(y[treatm==1]),mean(y[treatm==2]),mean(y[treatm==3])),labels=c("$\\hat{\\mu}$","$\\hat{\\mu}_{1}$","$\\hat{\\mu}_{2}$","$\\hat{\\mu}_{3}$"), las=1)
points(c(1,2,3),c(mean(y[treatm==1]),mean(y[treatm==2]),mean(y[treatm==3])),pch=19,col=2,cex=1.2)
text(1,(mean(y)+mean(y[treatm==1]))/2,labels="$\\hat{\\alpha}_1$",adj=c(-1,0),col="blue")
text(2,(mean(y)+mean(y[treatm==2]))/2,labels="$\\hat{\\alpha}_2$",adj=c(-1,0),col="blue")
text(3,(mean(y)+mean(y[treatm==3]))/2,labels="$\\hat{\\alpha}_3$",adj=c(-1,0),col="blue")
text(1,min(y),labels="$y_{1,j}$",adj=c(-1,0),col=1)

y <- c(2.8, 3.6, 3.4, 2.3,
       5.5, 6.3, 6.1, 5.7,
       5.8, 8.3, 6.9, 6.1)
treatm <- factor(c(1, 1, 1, 1,
                   2, 2, 2, 2,
                   3, 3, 3, 3))
plot(treatm,y)

mu <- mean(y)
muis <- tapply(y, treatm, mean)
alpha <- muis - mu
mu
muis
alpha

tapply(y, treatm, var)

SST <- sum((y - mu)^2)
SSE <- sum((y[treatm==1] - muis[1])^2)+
       sum((y[treatm==2] - muis[2])^2)+
       sum((y[treatm==3] - muis[3])^2)
SSTr <- 4 * sum(alpha^2)
c(SST, SSE, SSTr)

vars <- tapply(y, treatm, var)
(12-3)*mean(vars)

x<-seq(0,5,by=0.01)
plot(x,df(x,df1=2,df2=9),type="l", xlab="$x$", ylab="pdf")
lines(x,df(x,df1=4,df2=9),col=2)

F <- (SSTr/(3 - 1)/(SSE/(12 - 3)))
pv <- 1 - pf(F, df1 = 3 - 1, df2 = 12 - 3)
c(F , pv)

anova(lm(y ~ treatm))

muis[1] - muis[2] + c(-1, 1) *  
  qt(0.975, df = 12 - 3) * sqrt(SSE/(12 - 3) * (1/4 + 1/4))

tobs <- (muis[1] - muis[2])/sqrt(SSE/(12 - 3) * (1/4 + 1/4))
2 * (1 - pt(abs(tobs), 9))

alphaBonf <- 0.05/3 
## A-B
alpha[1] - alpha[2] + c(-1, 1) *  
  qt(1-alphaBonf/2, df = 12 - 3) * sqrt(SSE/(12 - 3) * (1/4 + 1/4))
## A-C
alpha[1] - alpha[3] + c(-1, 1) * 
  qt(1-alphaBonf/2, df = 12 - 3) * sqrt(SSE/(12 - 3) * (1/4 + 1/4))
## B-C
alpha[2] - alpha[3] + c(-1, 1) * 
  qt(1-alphaBonf/2, df = 12 - 3) * sqrt(SSE/(12 - 3) * (1/4 + 1/4))

c(qt(1 - alphaBonf/2, 9), qt(0.975, 9))

residuals <- lm(y ~ treatm)$residuals
qqnorm(residuals)
qqline(residuals)
residuals

D <- data.frame(
  strength=c(44.6, 52.8, 53.1, 51.5, 48.2, 50.5, 58.3, 50.0, 53.7, 40.8,
             46.3, 55.4, 54.4, 50.5, 44.5, 48.5, 57.4, 55.3, 54.4, 43.9,
             45.2, 58.1, 50.6, 47.5, 45.9, 52.3, 54.6, 53.4, 47.8, 42.5),
  plastictype=factor(rep(1:5, 6))
)
plot(D$plastictype, D$strength, xlab="Plastictype", ylab="Strength")
fit <- lm(strength ~ plastictype, data=D)
anova(fit)

library(xtable)
print(xtable(anova(fit)))

qqnorm(fit$residuals)
qqline(fit$residuals)

set.seed(138)
library(MESS)
qqwrap <- function(x, y, ...){
  stdy <- (y-mean(y))/sd(y)
  qqnorm(stdy, main="", ...)
  qqline(stdy)}
wallyplot(fit$residuals, FUN=qqwrap, ylim=c(-3,3))

tapply(D$strength, D$plastictype, mean)

LSD_0.005 <- qt(0.9975, 25) * sqrt(2*6.74/6)
LSD_0.005

y <- c(2.8, 3.6, 3.4, 2.3,
       5.5, 6.3, 6.1, 5.7,
       5.8, 8.3, 6.9, 6.1)
treatm <- factor(c(1, 1, 1, 1,
                   2, 2, 2, 2,
                   3, 3, 3, 3))
block <- factor(c(1, 2, 3, 4, 
                  1, 2, 3, 4,
                  1, 2, 3, 4))

mu <- mean(y)
alpha <- tapply(y, treatm, mean) - mu
beta <- tapply(y, block, mean)  - mu 
mu
alpha
beta

SSBl <- 3 * sum(beta^2)
SSE <- SST - SSTr - SSBl
c(SST, SSE, SSTr, SSBl)                    

## Test statistics
Ftr <- SSTr / (3-1) / ( SSE / ((3-1) * (4-1)))
Fbl <- SSBl / (4-1) / ( SSE / ((3-1) * (4-1)))
## p-values
pv.tr <- 1 - pf(Ftr, df1=3-1, df2=(3-1)*(4-1))
pv.bl <- 1 - pf(Fbl, df1=4-1, df2=(3-1)*(4-1))
c(Ftr, Fbl)
c(pv.tr, pv.bl)

D <- data.frame(treatm, block, y)
fit <- lm(y ~ treatm + block, data=D)
anova(fit)

muis[1] - muis[2] + c(-1,1) * qt(0.975, df=(4-1)*(3-1)) * 
  sqrt(SSE/((4-1)*(3-1)) * (1/4+1/4))

tobs <- (muis[1] - muis[2])/sqrt(SSE/6 * (1/4 + 1/4))
2 * (1 - pt(abs(tobs), df=6))

alphaBonf <- 0.05/3 
## A vs. B
alpha[1] - alpha[2] + c(-1, 1) *  
  qt(1-alphaBonf/2, df = 6) * sqrt(SSE/6 * (1/4 + 1/4))
## A vs. C
alpha[1] - alpha[3] + c(-1, 1) * 
  qt(1-alphaBonf/2, df = 6) * sqrt(SSE/6 * (1/4 + 1/4))
## B vs. C
alpha[2] - alpha[3] + c(-1, 1) * 
  qt(1-alphaBonf/2, df = 6) * sqrt(SSE/6 * (1/4 + 1/4))

qqnorm(fit$residuals)
qqline(fit$residuals)
fit$residuals

par(mfrow=c(1,2))
plot(D$treatm, fit$residuals, xlab="Treatment", ylab="Residuals")
plot(D$block, fit$residuals, xlab="Block", ylab="Residuals")

## Collecting the data in a data frame
D <- data.frame(
  y=c(22.5, 24.3, 24.9, 22.4,
      21.5, 21.3, 23.9, 18.4,
      22.2, 21.9, 21.7, 17.9),
  car=factor(c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4)),
  tire=factor(c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3))
)

par(mfrow=c(1,2))
plot(D$tire, D$y, xlab="Tire", ylab="y")
plot(D$car, D$y, xlab="Car", ylab="y")

fit <- lm(y ~ car + tire, data=D)

anova(fit)

qqnorm(fit$residuals)
qqline(fit$residuals)

par(mfrow=c(1,2))
plot(D$car, fit$residuals, xlab="Car", ylab="Residuals")
plot(D$tire, fit$residuals, xlab="Tire", ylab="Residuals")

tapply(D$y, D$tire, mean)

LSD_bonf <- qt(1-0.05/6, df=6) * sqrt(2*1.19/4)
LSD_bonf
