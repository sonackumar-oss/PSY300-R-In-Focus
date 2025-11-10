####Simple Linear Regression####

###packages###
#if any are not installed, install them
library(package = "psych")
library(package = "lsr") # will likely need to install
library(package = "ggplot2")
library(package = "tidyverse")
library(package = "jtools") #will likely need to install
library(package = "car")
library(package = "sciplot") #will likely need to install
library(package = "performance") #will likely need to install
library(package = "lmtest") #will likely need to install

####import data####
therapy <- read.csv("~/Downloads/16_7_regression.csv")
therapy <- read.csv("~/Library/CloudStorage/OneDrive-Personal/Davidson VAP/Teaching/Fall 2025 Teaching/Research Design & Stats Fall 2025/Datasets/16_7_regression.csv")

#check data types of the variables
str(therapy)

####evaluate assumptions####

#####linearity#####
#####homoscedasticity#####
#####normality of errors#####
#####independence of errors#####

#review descriptive statistics for each variable
describe(therapy) #get an idea of M and SD for each variable

#graph correlation to see if it seems roughly linear
#plot(df$IV, df$DV)
plot(therapy$session, therapy$symptoms)

####Compute Linear Regression Model####
reg.model <- lm(formula = DV ~ IV, 
                data = df)

reg.model <- lm(symptoms ~ sessions, 
                data = therapy)
summary(reg.model)

#coefficients section - estimate column gives the y intercept and slop of the regression line
#residual standard error = measure of the accuracy of predictions made using the regression

####check assumptions####
#for regression we check the assumptions after we run the model because we need to create the model object to test the assumptions

#visualize assumption tests
check_model(reg.model)

# Plot residuals -> what can this tell us? 
residuals <- resid(reg.model)
fitted_values <- fitted(reg.model)

plot(fitted_values, residuals, xlab='Fitted Values', ylab='Residuals', main='Residual Plot')
abline(h=0, col='red')

#homoscedasticity
# Breusch-Pagan test
check_heteroscedasticity(reg.model)

# For linearity, we also expect graph to be approximately linear
plot(therapy$sessions, therapy$symptoms, main='Scatter Plot for Linearity Check', xlab='Sessions', ylab='Symptoms')
abline(reg.model)

#normality of residuals (errors)
#review graph titled "homogeneity of residuals - should fall along line
#Shapiro-Wilk test on residuals
check_normality(reg.model)

## other ways to test normality--qqplots
qqnorm(therapy$sessions)
qqline(therapy$sessions)

qqnorm(therapy$symptoms)
qqline(therapy$symptoms)


#independence of errors
#Durbin-Watson test
dwtest(formula = reg.model)

####visualize data####

reg.plot <- ggplot(therapy, aes(x = sessions, y = symptoms)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_apa() + 
  labs(title = "Linear Regression of Symptoms of Sessions",
       x = "Sessions",
       y = "Symptoms")
reg.plot
