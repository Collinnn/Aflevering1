




x=seq(-4,4, by=0.01)
plot(x,dnorm(x), type="l")
polygon(c(1.96,seq(1.96,4, by=0.01),4,1.96),c(0,dnorm(seq(1.96,4, by=0.01)),0, 0),col="grey")
polygon(c(-1.96,seq(-1.96,-4, by=-0.01),-4,-1.96),c(0,dnorm(seq(-1.96,-4, by=-0.01)),0, 0),col="grey")
text(3.3,0.05, "P(Z>1.96)=0.025")
text(-3.3,0.05, "P(Z<-1.96)=0.025")
qchisq(0.95, 1)
# TESTING THE PROBABILITY = 0.5 WITH A TWO-SIDED ALTERNATIVE
# WE HAVE OBSERVED 518 OUT OF 1154
# WITHOUT CONTINUITY CORRECTIONS

prop.test(518, 1154, p = 0.5, correct = FALSE)
#READING THE TABLE INTO R 
pill.study<-matrix(c(23, 34, 35, 132), ncol = 2, byrow = TRUE)
colnames(pill.study) <- c("Blood Clot", "No Clot")
rownames(pill.study) <- c("Pill", "No pill")
       
# TESTING THAT THE PROBABILITIES FOR THE TWO GROUPS ARE EQUAL
prop.test(pill.study, correct = FALSE)
# CHI2 TEST FOR TESTING THE PROBABILITIES FOR THE TWO GROUPS ARE EQUAL
chisq.test(pill.study, correct = FALSE)
#IF WE WANT THE EXPECTED NUMBERS SAVE THE TEST IN AN OBJECT
chi <- chisq.test(pill.study, correct = FALSE)
#THE EXPECTED VALUES
chi$expected
#READING THE TABLE INTO  R
poll <-matrix(c(79, 91, 93, 84, 66, 60, 37, 43, 47), 
              ncol = 3, byrow = TRUE)
colnames(poll) <- c("4 weeks", "2 weeks", "1 week")
rownames(poll) <- c("Cand1", "Cand2", "Undecided")

#COLUMN PERCENTAGES
colpercent<-prop.table(poll, 2)
colpercent
# Plotting percentages 
par(mar=c(5,4,4.1,2)+0.1)
barplot(t(colpercent), beside = TRUE, col = 2:4, las = 1, 
        ylab = "Percent each week", xlab = "Candidate", 
        main = "Distribution of Votes")
legend( legend = colnames(poll), fill = 2:4,"topright", cex = 0.5)
par(mar=c(5,4,4,2)+0.1)
#TESTING SAME DISTRIBUTION IN THE THREE POPULATIONS
chi <- chisq.test(poll, correct = FALSE)
chi

#EXPECTED VALUES
chi$expected

