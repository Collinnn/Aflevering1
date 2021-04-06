# Indlæs 'soenderborg2_data.csv' filen med data
D <- read.table("soenderborg2_data.csv", sep = ";", header = TRUE)


# Lav 't' om til en datovariabel i R
D$t <- as.Date(D$t, format = "%d/%m/%Y")

# Udvælg data for perioden 15. okt 2009 til 15. apr 2010 for de fire huse
D_model <- subset(D, ("2009-10-15" <= t & t < "2010-04-16") & 
                  (houseId %in% c(3, 5, 10, 17)))


# Fjern observationer med manglende værdier
D_model <- na.omit(D_model)


# Estimer multipel lineær regressionsmodel
fit <- lm(Q ~ Ta + G, data = D_model)

# Vis estimerede parametre mm.
summary(fit)


# Plots til modelkontrol

# Observationer mod fittede værdier
plot(fit$fitted.values, D_model$Q, xlab = "Fittede værdier",     
       ylab = "Varmeforbrug")

# Residualer mod hver af de forklarende variable
plot(D_model$FORKLARENDE_VARIABEL, fit$residuals, 
        xlab = "INDSÆT TEKST", ylab = "Residualer")

# Residualer mod fittede værdier
plot(fit$fitted.values, fit$residuals, xlab = "Fittede værdier", 
     ylab = "Residualer")

# Normal QQ-plot af residualerne
qqnorm(fit$residuals, ylab = "Residualer", xlab = "Z-scores", 
       main = "")
qqline(fit$residuals)


# Konfidensintervaller for modellens koefficienter
confint(fit, level = 0.95)


# Udvælg valideringsdatasæt
D_test <- subset(D, (t == "2008-12-06" & houseId == 3)|
                          (t == "2009-02-22" & houseId == 5)|
                          (t == "2009-03-12" & houseId == 10)|
                          (t == "2009-04-01" & houseId == 17))

# Prædiktioner og 95% prædiktionsintervaller
pred <- predict(SLUTMODEL, newdata = D_test, interval = "prediction", 
              level = 0.95)

# Observerede værdier sammen med prædiktioner
cbind(id = D_test$houseId, Q = D_test$Q, pred)

