## R In Focus: The One-Way Within-Subjects ANOVA

## Learning Objective 13.7: Compute the one-way within subjects ANOVA, evaluate the assumptions, and select an appropriate post hoc test using R 

## Load Packages
install.packages("semTools")
library("semTools")
install.packages("rstatix")
library("rstatix")
install.packages("car")
library("car")
install.packages("effectsize")
library("effectsize")
install.packages("multcomp")
library("multcomp")
install.packages("emmeans")
library("emmeans")
install.packages("sciplot")
library("sciplot")

## Read in Dataset 
stroop <- Stroop.csv

## Create Factors
condition <- as.factor(stroop$Condition)
participant <- as.factor(stroop$Participant)

## First, Evaluate Assumptions
## One-Way Within-Subjects ANOVAs are parametric statistics
## The data must be on an interval or ratio scale of measurement

## Three Other Assumptions

## 1 Methodological: Independence within groups

## 2 Statistical: Normality and Sphericity

## Testing Normality of Dependent Variable
## Null Hypothesis: Data are normally distributed
## Alternative Hypothesis: Data are not normally distributed

## Check skewness (distribution symmetry) and kurtosis (peakedness of distribution) of each mood group
congruent <- subset(stroop, condition == "Congruent")
incongruent <- subset(stroop, condition == "Incongruent")
blackink <- subset(stroop, condition == "Black Ink")

skew(congruent$ReactionTime)
kurtosis(congruent$ReactionTime)
describe(congruent$ReactionTime)

skew(incongruent$ReactionTime)
kurtosis(incongruent$ReactionTime)
describe(incongruent$ReactionTime)


skew(blackink$ReactionTime)
kurtosis(blackink$ReactionTime)
describe(blackink$ReactionTime)

### Report skewness and kurtosis of each wine cost level (inexpensive, moderate, expensive)

## Check using Shapiro-Wilk test of normality

shapiro.test(congruent$ReactionTime)
shapiro.test(incongruent$ReactionTime)
shapiro.test(blackink$ReactionTime)

## Do skewness, kurtosis, and Shapiro-Wilk tests of normality reject or fail to reject the null hypothesis? 
## Is assumption of normality supported? 

## Now, test assumption of sphericity
## We'll use Mauchly's test of sphericity
## Null Hypothesis: variances of the differences between all pairs of groups are equal
## Alternative Hypothesis: differences between all pairs of groups are not equal

sphericity <- anova_test(data = stroop, dv = ReactionTime, wid = participant, within = condition)
mauchly_results <- sphericity$`Mauchly's Test for Sphericity`
mauchly_results

## What is the p-value for Mauchly's test? 
## Do we reject or fail to reject the null hypothesis? 
## Is the assumption of sphericity met? 

## Now, we compute the one-way within subjects ANOVA

withinmodel <- anova_test(data = stroop, dv = ReactionTime, wid = participant, within = condition, effect.size = "pes")
get_anova_table(withinmodel)

### Report degrees of freedom numerator and denominator
### Report test statistic (F)
### Report p-value
### Report Effect Sizes--value of partial eta-squared

## Now, conduct Bonferroni post hoc test showing pairwise comparisons

pairwise.t.test(stroop$ReactionTime,stroop$condition, p.adj = "bonf")

## Note that the output from R differs slightly from the output in the textbook
## However, the finding remains the same


## Let's visualize the data!

## We can create a line plot of the data

lineplot.CI(data = stroop,
            x.factor = condition,
            response = ReactionTime,
            xlab = "Condition",
            ylab = "Reaction Time (seconds)")

#dot represents mean of the cell
#error bars represent mean +/- standard error of the mean


