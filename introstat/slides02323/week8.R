
################################################################
## Simuler en lineær model med normalfordelt afvigelse og estimer parametrene
## FØRST LAV DATA:
## Generer n værdier af input x som uniform fordelt
x <- runif(n=20, min=-2, max=4)
## Simuler lineær regressionsmodel
beta0=50; beta1=200; sigma=90
y <- beta0 + beta1 * x + rnorm(n=length(x), mean=0, sd=sigma)
## HERFRA ligesom virkeligheden, vi har dataen i x og y:
## Et scatter plot af x og y
plot(x, y)
## Udregn least squares estimaterne, brug Theorem 5.4
(beta1hat <- sum( (y-mean(y))*(x-mean(x)) ) / sum( (x-mean(x))^2 ))
(beta0hat <- mean(y) - beta1hat*mean(x))
## Brug lm() til at udregne estimaterne
lm(y ~ x)
## Plot den estimerede linie
abline(lm(y ~ x), col="red")
## Tilføj den "rigtige" line
abline(a=beta0, b=beta1)
legend("bottomright", c("Estimerede linie","Rigtige linie"), lty=1, col=c(2,1))


################################################################
## Se hvordan parameterestimaterne er fordelt
## Antal gentagelser
nRepeat <- 1000
## To vektorer til at gemme estimaterne i
Beta0Hat <- numeric(nRepeat)
Beta1Hat <- numeric(nRepeat)
## Gentag simuleringen og estimeringen nRepeat gange
for(i in 1:nRepeat){
  ## Generer x
  x <- runif(n=20, min=-2, max=4)
  ## Simuler lineær regressionsmodel
  beta0=50; beta1=200; sigma=90
  y <- beta0 + beta1 * x + rnorm(n=length(x), mean=0, sd=sigma)
  ## Brug lm() til at udregne estimaterne
  fit <- lm(y ~ x)
  ## Gem estimaterne
  Beta0Hat[i] <- fit$coefficients[1]
  Beta1Hat[i] <- fit$coefficients[2]
}
## Se deres empiriske fordeling
par(mfrow=c(1,2))
hist(Beta0Hat, probability=TRUE)
hist(Beta1Hat, probability=TRUE)
## Gå lige tilbage til slides og kør næste slide
## Se estimatet af standardafvigelsen på parameter estimaterne
summary(fit)


################################################################
## Hypotesetests for signifikante parametre
## Generer x
x <- runif(n=20, min=-2, max=4)
## Simuler Y
beta0=50; beta1=200; sigma=90
y <- beta0 + beta1 * x + rnorm(n=length(x), mean=0, sd=sigma)
## Brug lm() til at udregne estimaterne
fit <- lm(y ~ x)
## Se summary, deri står hvad vi har brug for
summary(fit)


################################################################
## Lav konfidensintervaller for parametrene
## Antal gentagelser
nRepeat <- 100
## Fangede vi den rigtige parameter
TrueValInCI <- logical(nRepeat)
## Gentag simuleringen og estimeringen nRepeat gange
for(i in 1:nRepeat){
  ## Generer x
  x <- runif(n=20, min=-2, max=4)
  ## Simuler y
  beta0=50; beta1=200; sigma=90
  y <- beta0 + beta1 * x + rnorm(n=length(x), mean=0, sd=sigma)
  ## Brug lm() til at udregne estimaterne
  fit <- lm(y ~ x)
  
  ## Heldigvis kan R beregne konfidensintervallet (level=1-alpha)
  (ci <- confint(fit, "(Intercept)", level=0.95))
  
  ## Var den rigtige parameterværdi "fanget" af intervallet?
  (TrueValInCI[i] <-  ci[1] < beta0  &  beta0 < ci[2])
}
## Hvor ofte blev den rigtige værdi "fanget"?
sum(TrueValInCI) / nRepeat


################################################################
## Eksempel med konfidensinterval for linien
## Lav en sekvens af x værdier
xval <- seq(from=-2, to=6, length.out=100)
## Brug predict funktionen
CI <- predict(fit, newdata=data.frame(x=xval),
interval="confidence",
level=.95)
## Se lige hvad der kom
head(CI)
## Plot data, model og intervaller
plot(x, y, pch=20)
abline(fit)
lines(xval, CI[, "lwr"], lty=2, col="red", lwd=2)
lines(xval, CI[, "upr"], lty=2, col="red", lwd=2)


################################################################
## Eksempel med prædiktionsinterval
## Lav en sekvens a x værdier
xval <- seq(from=-2, to=6, length.out=100)
## Beregn interval for hvert x
PI <- predict(fit, newdata=data.frame(x=xval),
interval="prediction",
level=.95)
## Se lige hvad der kom tilbage
head(PI)
## Plot data, model og intervaller
plot(x, y, pch=20)
abline(fit)
lines(xval, PI[, "lwr"], lty=2, col="blue", lwd=2)
lines(xval, PI[, "upr"], lty=2, col="blue", lwd=2)
options(digits=3)
options(digits=5)
options(digits=2)


################################################################
## Korrelation
## Generer x
x <- runif(n=20, min=-2, max=4)
## Simuler y
beta0=50; beta1=200; sigma=90
y <- beta0 + beta1 * x + rnorm(n=length(x), mean=0, sd=sigma)
## Scatter plot
plot(x,y)
## Brug lm() til at udregne estimaterne
fit <- lm(y ~ x)
## Den rigtige linie
abline(beta0, beta1)
## Plot fittet
abline(fit, col="red")  
## Se summary, deri står hvad vi har brug for
summary(fit)
## Korrelation mellem x og y
cor(x,y)
## Kvadreret er den "Multiple R-squared" fra summary(fit)
cor(x,y)^2


################################################################
## Model validering: residual analysis
fit <- lm(y1 ~ x)
par(mfrow = c(1, 2))
qqnorm(fit$residuals)
qqline(fit$residuals)
plot(fit$fitted, fit$residuals, xlab="Fitted values", ylab="Residuals")


################################################################
## Model validering: residual analysis
fit <- lm(y4 ~ x)
par(mfrow = c(1, 2))
qqnorm(fit$residuals)
qqline(fit$residuals)
plot(fit$fitted, fit$residuals, xlab="Fitted values", ylab="Residuals")
