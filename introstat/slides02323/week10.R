
################################
## Single proportion
## Testing the probability = 0.5 with a two-sided alternative
## We have observed 518 out of 1154
## Without continuity corrections
prop.test(x=518, n=1154, p = 0.5, correct = FALSE)


################################
## Pill study: two proportions
## Reading the table into R
pill.study <- matrix(c(23, 34, 35, 132), ncol = 2)
rownames(pill.study) <- c("Blood Clot", "No Clot")
colnames(pill.study) <- c("Pill", "No pill")
       
## Testing that the probabilities for the two groups are equal
prop.test(t(pill.study), correct = FALSE)
## Or simply directly by
prop.test(x=c(23,35), n=c(57,167), correct = FALSE)


################################
## Pill study: two proportions, chi-square test
## Chi2 test for testing the probabilities for the two groups are equal
chisq.test(pill.study, correct = FALSE)
## If we want the expected numbers save the test in an object
chi <- chisq.test(pill.study, correct = FALSE)
## The expected values
chi$expected


################################
## Poll study: contingency table, chi-square test
## Reading the table into  r
poll <-matrix(c(79, 91, 93, 84, 66, 60, 37, 43, 47), ncol = 3, byrow = TRUE)
colnames(poll) <- c("4 weeks", "2 weeks", "1 week")
rownames(poll) <- c("Cand1", "Cand2", "Undecided")
## Column percentages
colpercent <- prop.table(poll, 2)
colpercent
# Plotting percentages 
par(mar=c(5,4,4.1,2)+0.1)
barplot(t(colpercent), beside = TRUE, col = 2:4, las = 1, 
        ylab = "Percent each week", xlab = "Candidate", 
        main = "Distribution of Votes")
legend( legend = colnames(poll), fill = 2:4,"topright", cex = 0.5)
par(mar=c(5,4,4,2)+0.1)


################################
## Testing same distribution in the three populations
chi <- chisq.test(poll, correct = FALSE)
chi
## Expected values
chi$expected
