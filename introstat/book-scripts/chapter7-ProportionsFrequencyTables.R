par(mfrow=c(2,3), mar=c(3,3,1,1)+0.1)
par(cex=0.8)

plotit <- function(n, p){
    plot(0:n, dbinom(0:n, n, p), type="h", xlab="", ylab="",
         main=paste0("$np = ",n*p,"$, $n(1-p) = ",n*(1-p),"$"))
}

plotit(n=6, p=0.5)
plotit(n=9, p=1/3)
plotit(n=18, p=1/3)
plotit(n=30, p=1/3)
plotit(n=45, p=1/3)
plotit(n=100, p=0.3)


## TESTING THE PROBABILITY = 0.5 WITH A TWO-SIDED ALTERNATIVE
## WE HAVE OBSERVED 518 OUT OF 1154
## WITHOUT CONTINUITY CORRECTIONS

prop.test(x=518, n=1154, p = 0.5, correct = FALSE)

## TESTING THAT THE PROBABILITIES FOR THE TWO GROUPS ARE EQUAL
## CALCULATING 99% CONFINDECE INTERVAL
prop.test(x=c(23,35), n=c(57,167), correct=FALSE, conf.level=0.99)

#READING THE LE INTO R 

pill.study <- matrix(c(23, 35, 34, 132), ncol = 2, byrow = TRUE)
rownames(pill.study) <- c("Blood Clot", "No Clot")
colnames(pill.study) <- c("Pill", "No pill")
pill.study
# CHI2 TEST FOR TESTING THE PROBABILITIES FOR THE TWO GROUPS ARE EQUAL

chisq.test(pill.study, correct = FALSE)

#IF WE WANT THE EXPECTED NUMBERS SAVE result IN AN OBJECT
chi <- chisq.test(pill.study, correct = FALSE)

#THE EXPECTED VALUES
chi$expected

#READING THE :LE INTO  R
poll <- matrix(c(79, 91, 93, 84, 66, 60, 37, 43, 47), ncol = 3, 
               byrow = TRUE)
colnames(poll) <- c("4 weeks", "2 weeks", "1 week")
rownames(poll) <- c("Cand1", "Cand2", "Undecided")

#COLUMN PERCENTAGES
colpercent<-prop.table(poll, 2)
colpercent

barplot(t(colpercent), beside = TRUE, col = 2:4, las = 1, 
        ylab = "Percent each week", xlab = "Candidate", 
        main = "Distribution of Votes")
legend( legend = colnames(poll), fill = 2:4,"topright", cex = 0.7)

## Testing same distribution in the three populations
chi <- chisq.test(poll, correct = FALSE)
chi

## Expected values
chi$expected

## READING THE LE INTO  R 
results <- matrix(c(23, 60, 29, 28, 79, 60, 9, 49, 63), ncol = 3, 
                  byrow = TRUE)
colnames(results) <- c("MathBad", "MathAve", "MathGood")
rownames(results) <- c("EngBad", "EngAve", "EngGood")

#PERCENTAGES
prop.table(results)

#ROW TOTALS
margin.table(results, 1)

#COLUMN TOTALS
margin.table(results, 2)

#TESTING INDEPENDENCE BETWEEN ENGLISH AND MATHS RESULTS
chi <- chisq.test(results, correct = FALSE)
chi

#EXPECTED VALUES
chi$expected
