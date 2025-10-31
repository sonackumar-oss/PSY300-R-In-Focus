install.packages("psych")
library("psych")
install.packages("car")
library("car")

## Pearson Correlation Coefficient

# Create Data Frame
mood <- data.frame(Mood= c(6,4,7,4,2,5,3,1), 
                   Eating= c(480,490,500,590,600,400,545,650))

mood

# Assumptions

## Assumption of Normality
describe(mood)

shapiro.test(mood$Mood)
shapiro.test(mood$Eating)

# can also do qq plot (quantile quantile plot). if points are pretty much on straight line, this indicates normality
# this plot compares theoretical quantiles (of a theoretical normal distribution) with the sample quantiles
# if the theoretical and sample quantiles are similar, they'll fall closely along the qq line, indicating normality
qqnorm(mood$Mood)
qqline(mood$Mood)

qqnorm(mood$Eating)
qqline(mood$Eating)

## Assumptions of Homoscedasticity and Linearity

# For linearity, we expect graph to be approximately linear
plot(mood$Mood, mood$Eating, main='Scatter Plot for Linearity Check', xlab='Mood', ylab='Eating')
abline(model)

# For homoscedasticity, we expect the variability of data points to be similar as we move away from the y-axis; in other words, we expect the variability of data points to have a similar (homo) variance or scatter (scedasticity) along the regression line (x-axis).
# For linearity, we also expect data points to be randomly dispersed around the regression line (x-axis); data points should not show nonlinear patterns. 
# in this case, we plot the residuals
# residual = sample datapoint - predicted datapoint

model <- lm(Eating ~ Mood, data = mood)
residuals <- resid(model)
fitted_values <- fitted(model)

plot(fitted_values, residuals, xlab='Fitted Values', ylab='Residuals', main='Residual Plot')
abline(h=0, col='red')

# Checking for outliers 

boxplot(mood$Mood, main='Boxplot for Outlier Detection')
boxplot(mood$Eating, main='Boxplot for Outlier Detection')

# Conduct Pearson correlation

cor.test(mood$Mood,mood$Eating,method=c("pearson"),
         alternative=c("two.sided"), conf.level=.95)

# What is effect size (coefficient of determination)? 


## Spearman Correlation 
# Allows us to test relation between two ranked variables 

ranks <- data.frame(Food=c(1,1,3,4,5,6,7,8),
                    Water=c(1,3,2,6,4,7,8,5))
ranks

## Assumption that two variable have a monotonic relationship
# We can test this with a scatterplot matrix 

pairs(ranks[,c("Food","Water")])

## Test
cor.test(ranks$Food,ranks$Water,method=c("spearman"),
         alternative=c("two.sided"),conf.level=.95)

# The error message is telling us that there are tied ranks, which Spearman isn't always great with.
# We can set the exact argument to FALSE to correct for this

cor.test(ranks$Food,ranks$Water,method=c("spearman"),
         alternative=c("two.sided"),conf.level=.95, exact=F)



## Point-Biserial Correlation
# Used when we have one continuous and one categorical variable

comedy <- data.frame(gender=c(1,1,1,1,1,2,2,2,2,2,2,2), 
                     laughter=c(23,9,12,12,29,32,10,8,20,12,24,34))
comedy

comedy$gender <- as.factor(comedy$gender)


## Normality Assumption for continuous variable
describe(comedy$laughter)
shapiro.test(comedy$laughter)

## Equal Variances Assumption for dichotomous variable
leveneTest(laughter~gender, data=comedy)

## Correlation
comedy$gender <- as.numeric(comedy$gender)
cor.test(comedy$laughter,comedy$gender)

# What is effect size? 

