####Multiple Linear Regression####

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
library(package = "predict3d") #will likely need to download

####import data####
consumer <- read.csv("~/Workspace/PSY300B/Ng Working Folder/Regression/16_12_mult_regression.csv")

#check data types of the variables
str(consumer)

####evaluate assumptions####
#for regression we check many of the assumptions after we run the model because we need to create the model object to test the assumptions

####normality#### 
#not a technical assumption, but we test anyways because if this is violated, we likely will violate elsewhere
#review descriptive statistics for each variable
print(describe(consumer, type = 2), digits=3) #get an idea of M and SD for each variable

#shapiro-wilk test for each DV
#shapiro.test(df$DV)
shapiro.test(consumer$age)
shapiro.test(consumer$DisIncome)
shapiro.test(consumer$sales)

#####linearity#####
# For linearity, we expect graph to be approximately linear
#look at two plots - one for each IV

#age
plot(consumer$age, consumer$sales, main='Scatter Plot for Linearity Check', xlab='', ylab='')

#DisIncome
plot(consumer$DisIncome, consumer$sales , main='Scatter Plot for Linearity Check', xlab='', ylab='')

#*the rest of the assumptions, we will check after we run the model

####outliers####

#boxplot(df$DV, main='Title')

boxplot(consumer$age)
boxplot(consumer$DisIncome)
boxplot(consumer$sales)

####Compute Linear Regression Model####
#reg.model <- lm(formula = DV ~ IV1 + IV2, 
#                data = df)

reg.model1 <- lm(sales ~ age + DisIncome, 
                data = consumer)
summary(reg.model1)

#but we don't want a raw b, we want our betas
#reg.model.betas <- standardCoefs(reg.model)
reg.model.betas <- standardCoefs(reg.model1)
reg.model.betas

#what if we flip the order of IVs?
#should it matter/differ?

reg.model2 <- lm(sales ~ DisIncome + age, 
                data = consumer)
summary(reg.model2)

#but we don't want a raw b, we want our betas
#reg.model.betas <- standardCoefs(reg.model)
reg.model.betas2 <- standardCoefs(reg.model2)
reg.model.betas2

#what if we looked at models with only one predictor - would they differ?

reg.model3 <- lm(sales ~ age,
                 data = consumer)

reg.model4 <- lm(sales ~ DisIncome,
                 data = consumer)

summary(reg.model1)
summary(reg.model2)
summary(reg.model3)
summary(reg.model4)


####Return to Assumption Check####

#function to check tests for the regression
check_model(reg.model) #it's ok if this doesn't run

#####homoscedasticity#####
check_heteroscedasticity(reg.model)

#####normality of errors#####
#Shapiro-Wilk test on residuals
check_normality(reg.model)

#####independence of errors#####
#Durbin-Watson test
dwtest(formula = reg.model)

#####multicollinearity####
#Variance Inflation Factor
#graph scatterplot to see if IVs look related
plot(consumer$age, consumer$DisIncome) 

vif(reg.model) #does this fall within threshold of 4?


####visualize data####
#it's difficult to visualize a multiple regression - many papers do not show a visual depiction

#one method is to look at separate regression graphs for each predictor with the DV. this is helpful, but doesn't depict both predictors at once.

reg.plot1 <- ggplot(consumer, aes(x = age, y = sales)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_apa() + 
  labs(title = "Linear Regression of Sales on Age",
       x = "Age",
       y = "Sales")
reg.plot1

reg.plot2 <- ggplot(consumer, aes(x = DisIncome, y = sales)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_apa() + 
  labs(title = "Linear Regression of Sales on Disposible Income",
       x = "Disposible Income",
       y = "Sales")
reg.plot2

#we can also make a regression plot with multiple regression lines to depict the ways that IV1 influences the DV at different levels of IV2 
#want to use a new package, predict3d
#might need to reinstall latest version of ggplot2 before this will run as well as the predict3d package

predict3d::ggPredict(reg.model,
                     pred = IV1,
                     modx = IV2,
                     digits = 1, #number of digits to round to
                     se = TRUE, #show SE band around prediction line
                     show.point = TRUE, #show scatterplot points
                     show.error = FALSE, #show estimate error for individual points
                     show.text = TRUE, #shows equation of the line
                     facet.modx = FALSE, #breaks up the 3 lines into 3 different plots
                     facetbycol = FALSE) #shows 3 different plots as columns 

#the three lines shown represent the mean(moderator) +/- 1SD

predict3d::ggPredict(reg.model,
                     pred = age,
                     modx = DisIncome,
                     digits = 1, #number of digits to round to
                     se = TRUE, #show SE band around prediction line
                     show.point = TRUE, #show scatterplot points
                     show.error = FALSE, #show estimate error for individual points
                     show.text = TRUE, #shows equation of the line
                     facet.modx = FALSE, #breaks up the 3 lines into 3 different plots
                     facetbycol = FALSE) #shows 3 different plots as columns 

#can flip axes and color by switching which is specified as pred and modx
predict3d::ggPredict(reg.model,
                     pred = DisIncome,
                     modx = age,
                     digits = 1, #number of digits to round to
                     se = TRUE, #show SE band around prediction line
                     show.point = TRUE, #show scatterplot points
                     show.error = FALSE, #show estimate error for individual points
                     show.text = TRUE, #shows equation of the line
                     facet.modx = FALSE, #breaks up the 3 lines into 3 different plots
                     facetbycol = FALSE) #shows 3 different plots as columns

#note that this graph works for a multiple regression with 2 predictors, however, it would not work with > 2 predictors
#you can use the first method of visual depiction if you have > 2 predictor variables