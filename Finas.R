
#Set directory to yours
setwd("D:/DTU/Statistik/Aflevering1/Aflevering1")
getwd()

#Reads table in csv from folder. 
D <- read.table("soenderborg1_data.csv",header=TRUE,sep=";", as.is=TRUE)



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
#Atal non Applicable
sum(is.na(D$t))
sum(is.na(D$Ta))
sum(is.na(D$G))
sum(is.na(D$Ws))
sum(is.na(D$Q1))
sum(is.na(D$Q2))
sum(is.na(D$Q3))
sum(is.na(D$Q4))

## Histogram describing the empirical density of the weekly returns from
## AGG (histogram of weekly returns normalized to have an area of 1)
Q1 <- D$Q1 ##Only reason is so the name isn't D$AGG
hist(Q1, xlab="Varmeforbrug (hus 1)", prob=TRUE)


#Histogramplot
hist(Q1, breaks=10, col="red", xlab="Varmefobrug(hus1) i kiloWatt per dag",ylab="Antal gange",main="Density Histogram with Normal Curve")
#Sat t som dato og hvis det 
D$t <- as.Date(x=D$t, format="%Y-%m-%d")
summary(D$t)
#plot af de 4 forskellige huse
plot(D$t, D$Q1, type="l", xlim=as.Date(c("2008-10-02","2010-10-01")),ylim=c(0,9), xlab="Dato over Q1,Q2,Q3,Q4", ylab="Varmeforbrug", col="2")
lines(D$t, D$Q2, col=3)
lines(D$t, D$Q3, col=4)
lines(D$t, D$Q4, col=5)
## Tilføj legend
legend("topright", legend=paste0("Q",c(1,2,3,4)), lty=1, col=2:5)

#boxplot
Dsel <- subset(D, "2010-01-01" <= t & t < "2010-3-01")
boxplot(Dsel[ ,c("Q1","Q2","Q3","Q4")],xlab="4 forskellige huse", ylab="Varmeforbrug",col=(c(2,3,4,5)),main="Varmeforbruget over 4 huse")



#Tabel generation
Tbl <- data.frame()
udvalgte <- c("Q1","Q2","Q3","Q4")
for(i in udvalgte){

x <- Dsel[, i]

Tbl[i, "obs."] <- sum(!is.na(x))
Tbl[i, "mean"] <- mean(x, na.rm=TRUE)

Tbl[i, "var"] <- var(x, na.rm=TRUE)
Tbl[i, "sd"] <- sd(x, na.rm=TRUE)
Tbl[i, "Q1"] <- quantile(x, na.rm=TRUE,probs =0.25,type=2)
Tbl[i, "Q2"] <- quantile(x, na.rm=TRUE,probs =0.5,type=2)
Tbl[i, "Q3"] <- quantile(x, na.rm=TRUE,probs =0.75,type=2)
}
Tbl

#Tabel af plots
par(mfrow = c(2, 2))
for(i in udvalgte){
  x <- Dsel[, i]
 qqnorm(x)
qqline(x)
}
#Flere tabeller
Tab <- data.frame()
for(i in udvalgte){
  x <- Dsel[, i]
  Tab[i, "Nedre"] <- mean(x, na.rm= TRUE)-qt(p=1-0.05/2,df=sum(!is.na(x))-1) * sd(x,na.rm=TRUE) / sqrt(sum(!is.na(x)))
 
  Tab[i, "Øvre"] <-mean(x, na.rm= TRUE)+qt(p=1-0.05/2,df=sum(!is.na(x))-1) * sd(x,na.rm=TRUE) / sqrt(sum(!is.na(x)))
}
t.test(Dsel$Q1, conf.level=0.95)$conf.int
t.test(Dsel$Q2, conf.level=0.95)$conf.int
t.test(Dsel$Q3, conf.level=0.95)$conf.int
t.test(Dsel$Q4, conf.level=0.95)$conf.int
Tab

#Tabeller
Tabs <- data.frame()
for(i in "Q1"){
  x <- Dsel[, i]
  f <-(mean(x, na.rm= TRUE) - 0.238e1) /sd(x,na.rm=TRUE) * sqrt(sum(!is.na(x)))
  Tabs[i, "p værdi"] <-2*(1-pt(f,sum(!is.na(x)))) 
}
f
-2*(1-pt(abs(f),df=sum(!is.na(x))-1))

#Udregning af ligning til opgave H
mean(x,na.rm=TRUE)
sd(x,na.rm=TRUE)



#Print tabel
Tabs
t.test(Dsel$Q1, mu=2.38)$p.value
t.test(Dsel$Q2, mu=2.38)$p.value
t.test(Dsel$Q3, mu=2.38)$p.value
t.test(Dsel$Q4, mu=2.38)$p.value

#definationer
s1<-sd(Dsel$Q1,na.rm=T)
s2<-sd(Dsel$Q2,na.rm=T)
n1<-sum(!is.na(Dsel$Q1))
n2<-sum(!is.na(Dsel$Q2))

#De 2 ligninger for welch udregning
v<-(s1^2/n1+s2^2/n2)^2/(s1^4/n1^2/(n1-1)+s2^4/n2^2/(n2-1))
v
t<-(mean(Dsel$Q1,na.rm=T)-mean(Dsel$Q2,na.rm=T))/sqrt(s1^2/n1+s2^2/n2)
t
n1
n2
s1
s2

2*(1-pt(abs(t),df = v))
#Testing for both
t.test(Dsel$Q1,Dsel$Q2)




#Selecting only specifik parts
De<-D[,c("t","Q1","G")]
sum(De$Q1,na.rm=T)
De<-na.omit(De)
sum(De$Q1)
sum(!is.na(De$Q1))
head(De)
mean(De$Q1)
sd(De$Q1)
mean(De$G)
sd(De$G)

(1/509-1)*((2.1-2.371653)/1.483319)*((8.553098-143.8487)/105.9259)

#Correlation equation 
(1/(sum(!is.na(De$Q1))-1)*sum((De$Q1-mean(De$Q1))*(De$G-mean(De$G))))/(sd(De$G)*sd(De$Q1))

plot(D$Q1,D$G,xlab= "Varmeforbrug hus 1(Kw/dag)",ylab = "Grundstråling(W/m^2)")

#Given correlation
cor(D[, c("Q1","G")], use="pairwise.complete.obs")

