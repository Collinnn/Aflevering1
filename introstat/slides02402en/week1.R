




x <- c(185, 184, 194, 180, 182)
n=length(x)
cat(paste0("n=",n))
cat("\\begin{equation*}\n")
Digits <- 1
ch1 <- paste0("\\ioverline{x} = \\frac{1}{", length(x), "}")
ch2 <- paste(as.character(x), collapse= " + ")
ch3 <- paste0(" = ", round(mean(x), Digits))
cat(ch1, "(", ch2, ")", ch3, "\n", sep="")
cat("\\end{equation*}\n")
ch2 <- paste(as.character(x[order(x)]))
cat(ch2,"\n")
if (n/2-ceiling(n/2)<0) cat(paste0("the middle (since n is uneven)(",ceiling(n/2),"'th)")) else cat(paste0("the mean of the two middle ones (since n is even) (",ceiling(n/2),"'th and ",ceiling((n+1)/2),"'th)"))
cat(median(x)) 
cat(median(c(x,235)))
cat(mean(c(x,235))) 
cat(paste0("     n=",n,": "))
cat(x)
## n is assumed to be between 5 and 8 (incl)
cat("\\begin{equation*}\n")
ch1 <- paste0("\\frac{1}{", n-1, "}")
mch2=rep("0",n)
for (i in 1:n) mch2[i] <- paste0("(",x[i]," - ",round(mean(x),1),")^2")
ch2 <- paste(mch2[1:4], collapse= " + ")
ch2b <- paste(mch2[5:n], collapse= " + ")
ch3 <- paste0(" = ", round(var(x), 3))
cat(ch1, "(", ch2,"\n", sep="")
cat("\\end{equation*}\n")
cat("\\begin{equation*}\n")
cat(" + ",ch2b,")","\n", sep="")
cat("\\end{equation*}\n")
cat("\\begin{equation*}\n")
cat(ch3, "\n", sep="")
cat("\\end{equation*}\n")
## n is assumed to be between 5 and 8 (incl)
rvar=as.character(round(var(x), 3))
rsd=as.character(round(sd(x), 3))
cat("\\begin{equation*}\n")
cat("s=\\sqrt{",rvar,"} = ",rsd,"\n")
cat("\\end{equation*}\n")
cat(paste0("     n=",n,": "))
cat(x)
ch2 <- paste(as.character(x[order(x)]))
cat(ch2,"\n")
mynp=as.character(0.25*n)
cat("$np=",mynp,"$:\n")
cat("$Q_1=",as.character(quantile(x,probs=0.25,type=2)),"$\n")
ch2 <- paste(as.character(x[order(x)]))
cat(ch2,"\n")
mynp=as.character(0.75*n)
cat("$np=",mynp,"$:\n")
cat("$Q_3=",as.character(quantile(x,probs=0.75,type=2)),"$\n")
x=c(168,161,167,179,184,166,198,187,191,179)
y=c(65.5, 58.3, 68.1, 85.7, 80.5, 63.4, 102.6, 91.4, 86.7, 78.9)
plot(x, y, xlab="Height", ylab="Weight", cex=1.5, col="blue", type="n")
text(x, y, 1:10, cex=1.5, col="blue")
abline(v=mean(x), h=mean(y), lty=2)
text(mean(x)+1, 60, expression(bar(x)==178), cex=1.5)
text(163.5, mean(y)+2.5, expression(bar(y)==78.1), cex=1.5)
      set.seed(1234)
      n <- 200
      x <- runif(n)
      y1 <- x + rnorm(n,sd=0.1)
      y2 <- -x + rnorm(n,sd=0.5)
      y3 <- rnorm(n)
      y4 <- sin(pi*x) + rnorm(n,sd=0.1)
      #cor(cbind(x,y1,y2,y3,y4))
      
      par(mfrow=c(2,2))
      par(mar = c(3.5, 3.5, 1.5, 0.5))
      plot(x,y1,pch=19,cex=0.5,ylab="y", main=expression(r%~~%0.95))
      plot(x,y2,pch=19,cex=0.5,ylab="y", main=expression(r%~~%-0.5))
      plot(x,y3,pch=19,cex=0.5,ylab="y", main=expression(r%~~%0))
      plot(x,y4,pch=19,cex=0.5,ylab="y", main=expression(r%~~%0))
## Adding numbers in the console
2+3
y <- 3
x <- c(1, 4, 6, 2)
x
x <- 1:10
x
## Sample Mean and Median (data from eNote)
x <- c(168,161,167,179,184,166,198,187,191,179)
mean(x)
median(x)
## Sample variance and standard deviation
var(x)
sd(x)
## Sample quartiles
quantile(x,type=2)
## Sample quantiles 0%, 10%,..,90%, 100%:
quantile(x,probs=seq(0, 1, by=0.10),type=2)
## A histogram of the heights:
 hist(x)
## A density histogram of the heights:
hist(x,freq=FALSE,col="red",nclass=8)
plot(ecdf(x),verticals=TRUE)
## A basic boxplot of the heights: (range=0 makes it "basic")
boxplot(x,range=0,col="red",main="Basic boxplot")
text(1.3,quantile(x),c("Minimum","Q1","Median","Q3","Maximum"),
     col="blue")
## A modified boxplot of the heights with an 
## extreme observation, 235cm added:
## The modified version is the default
boxplot(c(x,235),col="red",main="Modified boxplot")
text(1.3,quantile(c(x,235)),c("Minimum","Q1","Median","Q3"
                              ,"Maximum"),col="blue")

