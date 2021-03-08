
#Set directory to yours
setwd("D:/DTU/Statistik/Aflevering1/Aflevering1")
getwd()

#Reads table in csv from folder. 
D <- read.table("finans1_data.csv",header=TRUE,sep=";", as.is=TRUE)
#Sets different values
D <- D[ ,c("t","AGG","VAW","IWN","SPY")]
#An exchange-traded fund (ETF)!!!!!!


##Vises også i højre side under Enviroment ##

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



## Histogram describing the empirical density of the weekly returns from
## AGG (histogram of weekly returns normalized to have an area of 1)
AGG <- D$AGG ##Only reason is so the name isn't D$AGG
hist(AGG, xlab="Return (AGG)", prob=TRUE,col="DARKBLUE",border="BLUE")

VAW <- D$VAW ##Only reason is so the name isn't D$AGG
hist(VAW, xlab="Return (AGG)", prob=TRUE,col="DARKRED",border="BLUE")
qqnorm(VAW)
qqline(VAW)
IWN <- D$IWN ##Only reason is so the name isn't D$AGG
hist(IWN, xlab="Return (AGG)", prob=TRUE,col="DARKGREY",border="BLUE")
qqnorm(IWN)
qqline(IWN)

SPY <- D$SPY ##Only reason is so the name isn't D$AGG
hist(SPY, xlab="Return (AGG)", prob=TRUE,col="DARKGREEN",border="BLUE")
qqnorm(SPY)
qqline(SPY)