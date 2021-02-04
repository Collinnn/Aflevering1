
#Set directory to yours
setwd("D:/DTU/Statistik/Aflevering1/Aflevering1")
getwd()

#Reads table in csv from folder. 
D <- read.table("finans1_data.csv",header=TRUE,sep=";", as.is=TRUE)
#Sets different values
D <- D[ ,c("t","AGG","VAW","IWN","SPY")]



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

## Histogram describing the empirical density of the weekly returns from
## AGG (histogram of weekly returns normalized to have an area of 1)
AGG <- D$AGG
hist(AGG, xlab="Return (AGG)", prob=TRUE)


