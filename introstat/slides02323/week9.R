
################################
## Indlæs data, plot
## Se info om data
?airquality
## Indlæs data
Air <- airquality
## Fjern rækker hvor der er mindst en NA værdi
Air <- na.omit(Air)
## Fjern en outlier
Air <- Air[-which(Air$Ozone == 1), ]
## Se lige på empirisk tæthedsfunktion
hist(Air$Ozone, probability=TRUE, xlab="Ozon", main="")
## Koncentrationer er positive og meget højre-skæv fordeling, derfor log transformer
Air$logOzone <- log(Air$Ozone)
## Bedre epdf?
hist(Air$logOzone, probability=TRUE, xlab="log Ozon", main="")
## Lav en tid (R tidsklasse, se ?POSIXct)
Air$t <- ISOdate(1973, Air$Month, Air$Day)
## Behold kun nogle af kolonnerne
Air <- Air[ ,c(7,4,3,2,8)]
## Nye navne på kolonnerne
names(Air) <- c("logOzone","temperature","wind","radiation","t")
## Hvad er der i Air?
str(Air)
Air
head(Air)
tail(Air)
## Typisk vil man starte med et pairs plot
pairs(Air, panel = panel.smooth, main = "airquality data")


################################
## Fit simpel lineær regressionsmodel  
## Start med at sige at vi har 20 datapunkter
Air20 <- Air[1:20, ]
## Se på sammenhængen mellem log(ozon) og temperatur
plot(Air20$temperature, Air20$logOzone, xlab="Temperatur", ylab="log Ozon")
## Korrelation 
cor(Air20$logOzone, Air20$temperature)


####
## Se om der er signifikant korrelation med 20 observationer
summary(lm(logOzone ~ temperature, data=Air20))


####
## Tilføj en vektor med normalfordelte tilfældige værdier
## Sæt et seed for at få samme resultat hver gang
set.seed(2157)
## Er der signifikant lineær sammenhæng (korrelation)?
Air20$noise <- rnorm(20)
plot(Air20$noise, Air20$logOzone, xlab="Noise", ylab="log Ozon")
cor(Air20$noise, Air20$logOzone)


####
## Se om der er signifikant korrelation med 20 observationer
summary(lm(logOzone ~ noise, data=Air20))


################################
## Med når alle observationer inkluderes
## Er der signifikant lineær sammenhæng (korrelation)?
plot(Air$temperature, Air$logOzone, xlab="Temperatur", ylab="log Ozon")
cor(Air$temperature, Air$logOzone)


################################
## Se om der er signifikant korrelation med ALLE 110 OBSERVATIONER
summary(lm(logOzone ~ temperature, data=Air)) 
set.seed(324983)
## Tilføj en vektor med normalfordelte tilfældige værdier
## Er der signifikant lineær sammenhæng (korrelation)?
Air$noise <- rnorm(nrow(Air))
plot(Air$noise, Air$logOzone, xlab="Noise", ylab="log Ozon")
cor(Air$noise, Air$logOzone)


########
## Test om der er signifikant korrelation med ALLE 110 OBSERVATIONER
summary(lm(logOzone ~ noise, data=Air))


################################
## Med de to andre klimavariabler
## Simpel lineær regressionsmodel med vindhastigheden
plot(Air$wind, Air$logOzone, xlab="Vindhastighed", ylab="log Ozon")
cor(Air$wind, Air$logOzone)
summary(lm(logOzone ~ wind, data=Air))
## Simpel lineær regressionsmodel med indstrålingen
plot(Air$radiation, Air$logOzone, ylab="log Ozon", xlab="Indstraaling")
cor(Air$radiation, Air$logOzone)
summary(lm(logOzone ~ radiation, data=Air))


################################
## Udvid modellen
## Forward selection:
## Tilføj vind, indstråling eller støj input til modellen
summary(lm(logOzone ~ temperature + wind, data=Air))
summary(lm(logOzone ~ temperature + radiation, data=Air))
summary(lm(logOzone ~ temperature + noise, data=Air))
## Tilføj indstråling eller støj input til modellen
summary(lm(logOzone ~ temperature + wind + radiation, data=Air))
summary(lm(logOzone ~ temperature + wind + noise, data=Air))
## Udvid yderligere med støj input?
summary(lm(logOzone ~ temperature + wind + radiation + noise, data=Air))


################################
## Backward selection
## Fit den fulde model
summary(lm(logOzone ~ temperature + wind + radiation + noise, data=Air))
## Fjern det mest ikke-signifikante input, er alle nu sigifikante?
summary(lm(logOzone ~ temperature + wind + radiation, data=Air))


################################
## Simulate an MLR model and plot the estimated plane surface in 3d
## Simulate 20 data points from an MLR model with 2 inputs
set.seed(124)
n <- 20
x1 <- runif(n)
x2 <- runif(n)
y <- 1 + x1 + (-0.5)*x2 + rnorm(n,sd=0.1)
## Plot the points
require(rgl)
spheres3d(x1, x2, y, size=10, col="red", radius=0.02)
aspect3d(c(1,1,1))
axes3d(c('x--','y--','z--'))         
mtext3d('x1', edge=c('x--'), line=2)
mtext3d('x2', edge=c('y--'), line=2)
mtext3d('y', edge=c('z--'), line=2)
## Estimate a plane
fit <- lm(y ~ x1 + x2)
## Make predictions for a grid to see the estimated plane
nplot <- 20  
x1plot <- seq(min(x1),max(x1),len=nplot)
x2plot <- seq(min(x2),max(x2),len=nplot)
yprd <- outer(x1plot, x2plot, function(x1,x2){predict(fit, data.frame(x1=x1, x2=x2))})
## 'jet.colors' is "as in Matlab", alternatives see ?rainbow
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
"#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
## Use 100 different colors
colors <- jet.colors(100)
## Set the colors for z values
color <- colors[(yprd-min(yprd))/(max(yprd)-min(yprd))*100]
rgl.viewpoint(fov=40, theta=0, phi=-90)  
## Make a surface with jet colors and grid
surface3d(x1plot, x2plot, yprd, color=color, alpha=0.5)
surface3d(x1plot, x2plot, yprd, front="lines", back="lines", alpha=0.5)  


################################
## Antagelse om normalfordelte residualer
## Gem det udvalgte fit
fitSel <- lm(logOzone ~ temperature + wind + radiation,data=Air)
## qq-normalplot
qqnorm(fitSel$residuals)
qqline(fitSel$residuals)


################################
## Plot residualerne vs. prædikterede værdier      
plot(fitSel$fitted.values, fitSel$residuals, 
     xlab="Prædikteret værdi", ylab="Residualer")


################################
## Plot residualerne vs. de forklarende variabler
pairs(cbind(fitSel$residuals, Air[,c("temperature","wind",
              "radiation")]), panel = panel.smooth)


################################
## Udvid modellen med passende kurvelineær regression
## Lav den kvadrerede vind
Air$windSq <- Air$wind^2
## Tilføj den til modellen
fitWindSq <- lm(logOzone ~ temperature + wind + windSq + radiation, data=Air)
summary(fitWindSq)
## Gør tilsvarende for temperatur
Air$temperatureSq <- Air$temperature^2
## Tilføj
fitTemperatureSq <- lm(logOzone ~ temperature + temperatureSq + wind + radiation, data=Air)
summary(fitTemperatureSq)
## Gør tilsvarende for indstråling
Air$radiationSq <- Air$radiation^2
## Tilføj  
fitRadiationSq <- lm(logOzone ~ temperature + wind + radiation + radiationSq, data=Air)
summary(fitRadiationSq)
## Hvilken en var bedst!?
summary(fitWindSq)
summary(fitTemperatureSq)
## Her kunne man prøve at udvide yderligere
fitWindSqTemperatureSq <- lm(logOzone ~ temperature + temperatureSq + wind + windSq + radiation, data=Air)
summary(fitWindSqTemperatureSq)
## Model kontrol
qqnorm(fitWindSq$residuals)
qqline(fitWindSq$residuals)
plot(fitWindSq$fitted.values, fitWindSq$residuals, pch=19)


################################
## Plot residualerne vs. de forklarende variabler
pairs(cbind(fitWindSq$residuals, Air[,c("temperature","wind","radiation")]), panel=panel.smooth)


################################
## Konfidens- og prædiktionsintervaller for den kurvelineære model
## Generer et nyt data.frame med konstant temperatur og instråling, men varierende vindhastighed
wind <- seq(1,20.3,by=0.1)
setTemperature <- 78
setRadiation <- 186
AirForPred <- data.frame(temperature=setTemperature, wind=wind, windSq=wind^2, radiation=setRadiation)
## Udregn konfidens- og prædiktionsintervaller (-bånd)
## Læg mærke til at der transformeres tilbage
CI <- exp(predict(fitWindSq, newdata=AirForPred, interval="confidence", level=0.95))
PI <- exp(predict(fitWindSq, newdata=AirForPred, interval="prediction", level=0.95))
## Plot them
Air$ozone <- exp(Air$logOzone)
plot(Air$wind, Air$ozone, ylim=range(CI,PI,Air$ozone), xlab="", ylab="")
title(xlab="Vindhastighed (MpH)", ylab="Ozon (ppb)", main=paste("Ved temperatur =",setTemperature, "F og indstraaling = ",setRadiation,"Langleys"))
lines(wind, CI[,"fit"])
lines(wind, CI[,"lwr"], lty=2, col=2)
lines(wind, CI[,"upr"], lty=2, col=2)  
lines(wind, PI[,"lwr"], lty=2, col=3)
lines(wind, PI[,"upr"], lty=2, col=3)
## legend
legend("topright", c("Prædiktion","95% konfidensbånd","95% prædiktionsbånd"), lty=c(1,2,2), col=1:3)
  
  


################################
## Se hvad der sker hvis inputs er stærkt korrelerede
## Lav en variabel, som er meget korreleret f.eks. endnu en vindmåling
set.seed(367)
Air$wind2 <- Air$wind + rnorm(nrow(Air), sd=1)
cor(Air$wind, Air$wind2)
plot(Air$wind, Air$wind2)
## Tilføj den til modellen
fitWind2 <- lm(logOzone ~ temperature + wind + wind2 + radiation, data=Air)
summary(fitWind2)
## Sammenlign med modellen med kun den ene
fitWind <- lm(logOzone ~ temperature + wind + radiation, data=Air)
summary(fitWind)
