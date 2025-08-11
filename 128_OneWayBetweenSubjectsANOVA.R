## Section 12.8 
## R In Focus: The One-Way Between Subjects ANOVA

## Learning Objective 12.8: Compute the one-way between-subjects ANOVA; Evaluate the assumptions; Select an appropriate post hoc test using R

## Load Packages
install.packages("semTools")
library("semTools")
install.packages("car")
library("car")
install.packages("effectsize")
library("effectsize")
install.packages("MOTE")
library("MOTE")


## Read in Dataset 12.8

ExerciseData <- read.csv("ExerciseData.csv", header=TRUE)
group_character <- as.factor(ExerciseData$group)

## First, Evaluate Assumptions
## One-Way Between-Subjects ANOVAs are parametric statistics
## The data must be on an interval or ratio scale of measurement

## Four Other Assumptions

## 2 Methodological: Random Sampling and Independence

## 2 Statistical: Normality and Homogeneity of Variance

## Testing Normality of Dependent Variable

## Check skewness (distribution symmetry) and kurtosis (peakedness of distribution) of each mood group

lowex <- subset(ExerciseData, group == "lowintensity")
modex <- subset(ExerciseData, group == "moderateintensity")
control <- subset(ExerciseData, group == "control")

skew(lowex$mood)
kurtosis(lowex$mood)

skew(modex$mood)
kurtosis(modex$mood)

skew(control$mood)
kurtosis(control$mood)

### Report skewness and kurtosis of each exercise level (Low-intensity, moderate-intensity, and control)

## Check using Shapiro-Wilk test of normality

shapiro.test(lowex$mood)
shapiro.test(modex$mood)
shapiro.test(control$mood)

## Report Shapiro-Wilk stat for each exercise level.

### Do the skewness, kurtosis, and Shapiro-Wilk test suggest a normal distribution for the three levels? 

## Test Homogeneity of Variance Assumption
## Levene's test: Null hypothesis is that variances are equal
## If we reject the null, hypotehsis, there is evidence that this assumption is violated (variances are NOT equal)

leveneTest(mood~group_character, data=ExerciseData)

### Does Levene Test support or reject the null hypothesis? 

## Now, we compute one-way ANOVA

result <- aov(mood~group_character, data=ExerciseData)
summary(result)

## What is the Effect Size? 
## Eta-Squared
eta_squared(result)

## Omega-Squared--for this line of code, fill in the correct values
  ## dfm (degrees of freedom between)
  ## dfe (degrees of freedom within)
  ## Fvalue (F statistic)
  ## n (sample size)

omega.F(dfm = , dfe = ,
        Fvalue = , n = , a = .05)

### Report the Between Group Sum of Squares, Within Groups Sum of Squares, and Total Sum of Squares
### Report degrees of freedom numerator and denominator
### Report test statistic (F)
### Report p-value
### Report Effect Sizes--value of eta-squared and omega squared
### Is there a significant difference between groups? 

## Perform Tukey HSD post hoc test
TukeyHSD(result, conf.level=.95)

### Is difference between low and moderate intensity significant? 
### Is difference between low and control significant? 
### Is difference between moderate and control significant? 

### Ultimately, what between groups difference is driving the finding that there is a significant difference between group means?







