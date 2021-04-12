################################################################
## R basic operations
##
## Add two numbers in the console
2+3


## Assign the value 3 to y
y <- 3


## Concatenate numbers to a vector
x <- c(1, 4, 6, 2)
x


## A sequence from 1 to 10
x <- 1:10
x


## Sequence with specified steps
x <- seq(0, 1, by=0.1)
x



################################################################
## Summary statistics in
##
## Sample Mean and Median
x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)
mean(x)
median(x)


## Sample variance and standard deviation
var(x)
sqrt(var(x))
sd(x)


## Sample quartiles
quantile(x, type=2)


## Sample quantiles 0%, 10%,..,90%, 100%:
quantile(x, probs=seq(0, 1, by=0.10), type=2)



################################################################
## Histogram in R
##
## A histogram of the heights
hist(x)



################################################################
## Empirical density in R
##
## A density histogram or empirical density of the heights
hist(x, prob=TRUE, col="red", nclass=8)



################################################################
## Cumulative distribution plot in R
##
## Empirical cumulative distribution plot
plot(ecdf(x), verticals=TRUE)



################################################################
## Box plot in R
##
## A basic box plot of the heights (range=0 makes it "basic")
boxplot(x, range=0, col="red", main="Basic box plot")
## Add the blue text
text(1.3, quantile(x), c("Minimum","Q1","Median","Q3","Maximum"),
     col="blue")


## Add an extreme value and box plot
boxplot(c(x, 235), col="red", main="Modified box plot")
boxplot(c(x, 235), col="red", main="Basic box plot", range=0)


## Box plot with two groups
Males <-  c(152, 171, 173, 173, 178, 179, 180, 180, 182, 182, 182, 185,
            185 ,185, 185, 185 ,186 ,187 ,190 ,190, 192, 192, 197)
Females <-c(159, 166, 168 ,168 ,171 ,171 ,172, 172, 173, 174 ,175 ,175,
            175, 175, 175, 177, 178)
boxplot(list(Males, Females), col=2:3, names=c("Males", "Females"))



################################################################
## Read and explore data in R
##
## Read the data (note that per default sep="," but here semicolon)
studentheights <- read.table("studentheights.csv", sep=";", dec=".",
                             header=TRUE)

options(digits=7)

## Have a look at the first 6 rows of the data
head(studentheights)
## Get an overview
str(studentheights)
## Get a summary of each column/variable in the data
summary(studentheights)

options(digits=digits)


## Box plot for each gender
boxplot(Height ~ Gender, data=studentheights, col=2:3)



################################################################
## Explore data included in R
##
## See information about the mtcars data
?mtcars


## To make 2 plots
par(mfrow=c(1,2))
## First the default version
plot(mtcars$wt, mtcars$mpg, xlab="wt", ylab="mpg")
## Then a nicer version
plot(mpg ~ wt, xlab="Car Weight (1000lbs)", data=mtcars,
     ylab="Miles pr. Gallon", col=factor(am),
     main="Inverse fuel usage vs. size")
## Add a legend to the plot
legend("topright", c("Automatic transmission","Manual transmission"), 
       col=c("black","red"), pch=1, cex=0.7)



################################################################
## Bar plots and Pie charts in R
##
## Barplot
barplot(table(studentheights$Gender), col=2:3)


## Pie chart
pie(table(studentheights$Gender), cex=1, radius=1)







options(width=60)
## Reading the data into R
before <- c(9.1, 8.0, 7.7, 10.0, 9.6, 7.9, 9.0, 7.1, 8.3, 9.6,
            8.2, 9.2, 7.3, 8.5, 9.5)
after  <- c(8.2, 6.4, 6.6, 8.5, 8.0, 5.8, 7.8, 7.2, 6.7, 9.8,
            7.1, 7.7, 6.0, 6.6, 8.4)
## Making ordered vectors using the 'sort' function
sortedBefore <- sort(before)
sortedAfter <- sort(after)
## Printing the ordered vectors
sortedBefore
sortedAfter
## Printing the 8th observation in these vectors
sortedBefore[8]
sortedAfter[8]
