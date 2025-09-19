# Install packages
install.packages("rcompanion")
library("rcompanion")

## Section 17.6
## R In Focus: Chi Square Goodness of Fit
## Learning Objective: Compute the chi-square goodness-of-fit test and evaluate the assumptions using R

## First, return to section 17.4 of the textbook
## What are the 2 assumptions needed to compute a chi-square goodness-of-fit test? 

## Read through example 17.1 regarding the study on frequency of dreaming and ability to recall dreams
## What is the null hypothesis? 
## What is the alternate hypothesis? 

## We can compute the test statistic by hand or by using R
## Let's use R

## First, create a table of recall (did recall, did not recall, unsure)
recall <- c(58, 12, 10)
recall

## Are the 2 assumptions met? Explain why or why not.

## If the assumptions are met, we can then compute chi square goodness of fit
## To do this, we need to input our observed N and expected distribution

chisq.test(recall, p=c(.8, .1, .1))

## What is the test statistic value? 

## What is our p-value? 

## Do we reject or fail to reject the null hypothesis? 

## Let's visualize our data by creating a bar graph
obs.matrix <- matrix(c(58, 12, 10, 64, 8, 8), nrow=2, ncol=3, byrow=TRUE)
obs.matrix
colnames(obs.matrix) <- c("Full Recall", "Did Not Recall", "Unsure")
rownames(obs.matrix) <- c("Observed Values", "Expected Values")
obs.table.recall <- as.table(obs.matrix)
obs.table.recall

barplot(obs.table.recall, main="Dream Recall",
        xlab="Recall?", col=c("darkolivegreen","darkgoldenrod1"), beside=TRUE, 
        legend.text = c("Observed", "Expected"))


## Section 17.11
## R In Focus: Chi Square Test of Independence
## Learning Objective: Compute the chi-square test for independence and evaluate assumptions using R

## First, return to example 17.2 (p. 739) of the textbook
## What are the 2 assumptions needed to compute a chi-square goodness-of-fit test? 

## Read through example 17.2 regarding the study on the effectiveness of individual and family counseling in different settings
## We hypothesize that patients will be more likely to complete therapy when family is involved in counseling rather than in individual therapy
## We measure the relation between type of counseling (family vs individual) and counseling outcome (completion vs premature termination)

## What is the null hypothesis? 
## What is the alternate hypothesis? 
## Have we met our 2 assumptions (what are they and how do you know we've met them)? 

## We can compute the test statistic by hand or by using R
## Let's use R

## First, let's create a matrix of our data 

obs.matrix.ind <- matrix(c(22, 12, 31, 45), ncol=2, byrow=TRUE)
obs.matrix.ind

## Note: if we specified byrow argument as FALSE, data would be entered into the matrix down the first column (on left) and data would have finished in second column (on right)

obs.matrix.ind <- matrix(c(22,12,31,45), ncol=2, byrow=FALSE)

## However, we want the data to look like the first obs.matrix

## Let's name our columns and rows

colnames(obs.matrix.ind) <- c("Completion", "Premature Termination")
rownames(obs.matrix.ind) <- c("Family", "Individual")

## Now convert matrix to a table and view table

obs.table.ind <- as.table(obs.matrix.ind)
obs.table.ind

chisq.test(obs.table.ind)

## Note, these results do not converge with what we did by hand. 
## This is because the default in R is to do a continuity correction. 
## We don't want that, so we can make the argument false. 

chisq.test(obs.table.ind, correct=F)

## What is the value of the test statistic? 
## What is the p-value? 
## Do we reject or fail to reject the null hypothesis? Why? 

# What is the effect size (Cramer's V)? 
cramerV(obs.table.ind)


## Let's visualize our data by creating a bar graph
barplot(obs.table.ind, main="Effect of Therapy Type on Therapy Completion",
        xlab="Completion?", col=c("cornflowerblue","darksalmon"),
        legend = rownames(obs.table.ind), beside=TRUE)

