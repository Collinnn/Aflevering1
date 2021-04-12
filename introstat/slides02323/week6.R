
################################
## p-værdi for nulhypotese om ingen forskel mellem sygeplejeskers energiforbrug
2 * (1 - pt(3.01, df = 15.99))
options(digits=6)


################################
## t-test for forskel i middelværdi på sygeplejeskers energiforbrug
xA <- c(7.53, 7.48, 8.08, 8.09, 10.15, 8.4, 10.88, 6.13, 7.9)
xB <- c(9.21, 11.51, 12.79, 11.85, 9.97, 8.79, 9.69, 9.68, 9.19)
## Default i t.test() er H_0: mu_1 = mu_2 (ingen forskel i middelværdi)
t.test(xB, xA)


################################
## Det parrede setup: Tag forskellen og brug one-sample test
x1 <- c(.7,-1.6,-.2,-1.2,-1,3.4,3.7,.8,0,2)
x2 <- c(1.9,.8,1.1,.1,-.1,4.4,5.5,1.6,4.6,3.4)
dif <- x2-x1
t.test(dif)


################################
## Eller angiv at testen er parret med "paired=TRUE"
t.test(x2, x1, paired=TRUE)


################################
## Check af normalitetsantagelsen med q-q plots
par(mfrow=c(1,2))
qqnorm(xA, main="Hospital A")
qqline(xA)
qqnorm(xB, main="Hospital B")
qqline(xB)


################################
## Check af normalitetsantagelsen med q-q plots og Wally-plot
## Brug pakken MESS (install.packages("MESS"))
library(MESS)
## Define the plotting function
qqwrap <- function(x, y, ...){
  stdy <- (y-mean(y))/sd(y)
  qqnorm(stdy, main="", ...)
  qqline(stdy)}
## Do the Wally plot
wallyplot(xA, FUN=qqwrap, ylim=c(-3,3))


################################
## Check af normalitetsantagelsen med q-q plots og Wally-plot
## Do the Wally plot
wallyplot(xB, FUN=qqwrap, ylim=c(-3,3))
