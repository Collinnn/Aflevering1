setwd("D:/DTU/Statistik/Aflevering1/Aflevering1")
getwd()

#Reads table in csv from folder. 
D <- read.table("soenderborg2_data.csv", sep = ";", header = TRUE)
# Dimensions of D (number of rows and columns)
dim(D)
# Column/variable names
names(D)
## The first rows/observations
head(D)
# The last rows/observations
tail(D)
# Selected summary statistics
summary(D)
# Another type of summary of the dataset
str(D)
#Dataen er 454 forskellige observiation til 5 forskellige værdier. Heri t,AGG,VAW,IWN,SPY.
#t er en string, de 4 andre er doubles.
#Atal non Applicable
sum(is.na(D$t))
sum(is.na(D$houseId))
sum(is.na(D$Q))
sum(is.na(D$Ta))
sum(is.na(D$G))

# Make 't' a date variable in R
D$t <- as.Date(D$t, format = "%d/%m/%Y")
# Choose data from 15 Oct 2009 to 15 Apr 2010 for the four houses
D_model <- subset(D, ("2009-10-15" <= t & t < "2010-04-16") &
                    (houseId %in% c(3, 5, 10, 17)))
# Remove observations with missing values
D_model <- na.omit(D_model)

udvalgte <- c("Q","Ta","G")
plotcolors <-c("Red","Green","Blue")
q<-0
par(mfrow = c(1, 3))
for(i in udvalgte){
  q<- q+1
  x<-D_model[, i]
  hist(x,main=i,col=plotcolors[q])
}


q<-0
par(mfrow=c(1,3))
for(i in udvalgte){
  q<- q+1
  x<-D_model[, i]
  boxplot(x,main=i,col=plotcolors[q])
}

q<-0
udvalgte2 <- c("Ta","G")
par(mfrow = c(2, 1))
for(i in udvalgte2){
  q<- q+1
  x<-D_model[, i]
  plot(D_model$Q,x,main=i,col=plotcolors[q])
}

quantile(D_model$Q,c(.25,.50,.75))
sum(!is.na(D_model$Q))
mean(D_model$Q)
sd(D_model$Q)

quantile(D_model$Ta,c(.25,.50,.75))
sum(!is.na(D_model$Ta))
mean(D_model$Ta)
sd(D_model$Ta)

quantile(D_model$G,c(.25,.50,.75))
sum(!is.na(D_model$G))
mean(D_model$G)
sd(D_model$G)

fit <- lm(Q ~ Ta + G, data = D_model)
summary(fit)

plot(fit$fitted.values, D_model$Q, xlab = "Fitted values",
     ylab = "Heat consumption")
# Residuals against each of the explanatory variables
plot(D_model$EXPLANATORY_VARIABLE, fit$residuals,
     xlab = "INSERT TEXT", ylab = "Residuals")
# Residuals against fitted values
plot(fit$fitted.values, fit$residuals, xlab = "Fitted values",
     ylab = "Residuals")
# Normal QQ-plot of the residuals
qqnorm(fit$residuals, ylab = "Residuals", xlab = "Z-scores",
       main = "")
qqline(fit$residuals)

