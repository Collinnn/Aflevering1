
####################################
## Input data og plot
## Observationer
y <- c(2.8, 3.6, 3.4, 2.3,
       5.5, 6.3, 6.1, 5.7,
       5.8, 8.3, 6.9, 6.1)
## Grupper (behandlinger)
treatm <- factor(c(1, 1, 1, 1,
                   2, 2, 2, 2,
                   3, 3, 3, 3))
## Plot som punkter
par(mfrow=c(1,2))
plot(as.numeric(treatm), y, xlab="Treatment", ylab="y")
## Plot som box-plot
plot(treatm, y, xlab="Treatment", ylab="y")


################################
## Beregn den totale variation, kvadratsummen SST
## SST for eksemplet
(SST <- sum( (y - mean(y))^2 ))


################################
## Beregn varians tilbage efter modellen: Residual kvadratsummen SSE
## Data i hver gruppe
y1 <- y[1:4]
y2 <- y[5:8]
y3 <- y[9:12]
## SSE
(SSE <- sum( (y1 - mean(y1))^2 ) + 
        sum( (y2 - mean(y2))^2 ) + 
        sum( (y3 - mean(y3))^2 ))


################################
## Beregn varians forklaret af modellen: Kvadratsummen for grupperingen SS(Tr)
## Ved bare at trække fra
(SSTr <- SST - SSE)
## Eller ved 
4 * (mean(y1) - mean(y))^2 + 4 * (mean(y2) - mean(y))^2 + 4 * (mean(y3) - mean(y))^2


################################
## Plot F fordeling og se kritisk værdi
## Husk, dette er under H0 (altså vi regner som om H0 er sand):
## Antal grupper
k <- 3
## Antal punkter
n <- 12
## Sekvens til plot
xseq <- seq(0, 10, by=0.1)
## Plot F fordelingens tæthedsfunktion
plot(xseq, df(xseq, df1=k-1, df2=n-k), type="l", xlab="x", ylab="f(x)")
## Kritisk værdi for signifikans niveau 5 %
cr <- qf(0.95, df1=k-1, df2=n-k)
## Tegn den i plottet
abline(v=cr, col="red") 
## Test statistikkens værdi
(Fobs <- (SSTr/(k-1)) / (SSE/(n-k)))
## p-værdien er da
(1 - pf(Fobs, df1=k-1, df2=n-k))


################################
## Alt dette beregnes med lm() og anova()
anova(lm(y ~ treatm))


################################
## Check antagelse om homogen varians
## Box plot
plot(treatm,y)


################################
## Check antagelse om normal fordelte afvigelser
## qq-normal plot af residualer
fit1 <- lm(y ~ treatm)
qqnorm(fit1$residuals)
qqline(fit1$residuals)
## Eller med et Wally plot
library(MESS)
qqwrap <- function(x, y, ...) {qqnorm(y, main="",...); qqline(y)}
## Kan vi se et afvigende qq-norm plot?
wallyplot(fit1$residuals, FUN = qqwrap)
