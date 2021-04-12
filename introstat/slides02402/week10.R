#################################################################
### Testing hypotheses about one proportion using prop.test() ###
#################################################################

prop.test(10, 100, p = 0.5, correct = FALSE)

##################################################################
### Testing hypotheses about two proportions using prop.test() ###
##################################################################

# Read data table into R
pill.study <- matrix(c(23, 34, 35, 132), 
                     ncol = 2, byrow = TRUE)
colnames(pill.study) <- c("Blood Clot", "No Clot")
rownames(pill.study) <- c("Pill", "No pill")

# Show data table
pill.study

# Test whether probabilities are equal for the two groups
prop.test(pill.study, correct = FALSE)

###################################################################
### Testing hypotheses about two proportions using chisq.test() ###
###################################################################

# Test whether probabilities are equal for the two groups
chisq.test(pill.study, correct = FALSE)

# Expected values
chisq.test(pill.study, correct = FALSE)$expected

###################################################################
### Testing hypotheses in contingency tables using chisq.test() ###
###################################################################

# Read data table into R
poll <-matrix(c(79, 91, 93, 84, 66, 60, 37, 43, 47), 
              ncol = 3, byrow = TRUE)
colnames(poll) <- c("4 weeks", "2 weeks", "1 week")
rownames(poll) <- c("Cand1", "Cand2", "Undecided")

# Show data table
poll

# Show column percentages
prop.table(poll, 2)

# Plot probabilities
barplot(t(prop.table(poll, 2)), beside = TRUE, col = 2:4, las = 1, ylim = c(0, 0.5),
        ylab = "Percent", xlab = "Candidate", 
        main = "Distribution of votes")
legend(legend = colnames(poll), fill = 2:4, "topright")

# Testing for same distribution in the three populations
chisq.test(poll, correct = FALSE)

# Expected values
chisq.test(poll, correct = FALSE)$expected


