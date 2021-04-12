




par(mar = c(3.5, 3.5, 1.5, 0.5))

x <- c(168,161,167,179,184,166,198,187,191,179)

plot(ecdf(x), verticals = TRUE)

xp <- seq(0.9*min(x), 1.1*max(x), length.out = 100) 

lines(xp, pnorm(xp, mean(x), sd(x))) 
punif(30,0,30)-punif(20,0,30)
1-punif(30,0,30)
pnorm(-2)
pnorm(-2)
z=seq(-4,4,by=0.01)
plot(z,dnorm(z),type="l",xaxt="n",yaxt="n")
polygon(c(-2,seq(-2,-4.3, by=-0.01),-4.4,-2),c(0,dnorm(seq(-2,-4.3, by=-0.01)),0,0),col="pink")
axis(1, at=-3:3, labels=-3:3)
1-pnorm(2)
z=seq(-4,4,by=0.01)
plot(z,dnorm(z),type="l",xaxt="n",yaxt="n")
polygon(c(2,seq(2,4.3, by=0.01),4.4,2),c(0,dnorm(seq(2,4.3, by=0.01)),0,0),col="pink")
axis(1, at=-3:3, labels=-3:3)
pnorm(1)-pnorm(-1)
z=seq(-4,4,by=0.01)
plot(z,dnorm(z),type="l",xaxt="n",yaxt="n")
polygon(c(1,seq(1,-4.3, by=-0.01),-4.4,1),c(0,dnorm(seq(1,-4.3, by=-0.01)),0,0),col="red")
axis(1, at=-3:3, labels=-3:3)
pnorm(1)-pnorm(-1)
z=seq(-4,4,by=0.01)
plot(z,dnorm(z),type="l",xaxt="n",yaxt="n")
polygon(c(1,seq(1,-4.3, by=-0.01),-4.4,1),c(0,dnorm(seq(1,-4.3, by=-0.01)),0,0),col="red")

polygon(c(-1,seq(-1,-4.3, by=-0.01),-4.4,-1),c(0,dnorm(seq(-1,-4.3, by=-0.01)),0,0),col="grey")


axis(1, at=-3:3, labels=-3:3)
1-pnorm(300, m = 280, s = 10)
1-pnorm((300-280)/10)
z=seq(-4,4,by=0.01)
plot(z,dnorm(z),type="l",xaxt="n",yaxt="n")
polygon(c(2,seq(2,4.3, by=0.01),4.4,2),c(0,dnorm(seq(2,4.3, by=0.01)),0,0),col="pink")
axis(1, at=-3:3, labels=seq(250,310, by=10))
1-pnorm(300, m = 290, s = 4)
z=seq(-4,4,by=0.01)
plot(z,dnorm(z),type="l",xaxt="n",yaxt="n")
polygon(c(2.5,seq(2.5,4.3, by=0.01),4.4,2.5),c(0,dnorm(seq(2.5,4.3, by=0.01)),0,0),col="pink")
axis(1, at=-3:3, labels=seq(278,302, by=4))
qnorm(c(0.025, 0.975), m = 290, s = 4)
1-pexp(2, rate = 1/2)
z=seq(0,8,by=0.01)

plot(z,dexp(z, 1/2),type="l", main="Exp(2) - distribution")

polygon(c(2,seq(2,8, by=0.01),8,2),c(0,dexp(seq(2,8, by=0.01),1/2),0,0),col="pink")
text(3,0.07,"P(X>2)")
text(3,0.03,"=0.37")
polygon(c(2,seq(2,0, by=-0.01),0,2),c(0,dexp(seq(2,0, by=-0.01),1/2),0,0),col="grey")
text(1,0.1,"P(X<2)")
text(1,0.05,"=0.63")
## z=seq(0,8,by=0.01)
## 
## 
## plot(z,dexp(z, 1/2),type = "l", main = "Exp(2) - distribution")
## 
## 
## polygon(c(2, seq(2, 8, by = 0.01), 8, 2),
## 
##         c(0, dexp(seq(2, 8, by = 0.01), 1/2), 0, 0),
## 
##         col = "pink")
## 
## 
## text(3,0.07,"P(X>2)")
## 
## text(3,0.03,"=0.37")
## 
## 
## polygon(c(2, seq(2, 0, by = -0.01), 0, 2),
## 
##         c(0, dexp(seq(2, 0, by =- 0.01), 1/2), 0, 0),
## 
##         col = "grey")
## 
## 
## text(1,0.1,"P(X<2)")
## 
## text(1,0.05,"=0.63")
dpois(0,1)
exp(-1)
dpois(0,5)
1-pnorm(4000, m = 3850, s = sqrt(5500))
1-pnorm(4000, m = 3850, s = 550)

