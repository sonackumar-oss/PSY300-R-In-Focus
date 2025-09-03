###One-Sample T-test###

###packages###
#if any are not installed, install them
library(package = "psych")
library(package = "lsr") # will likely need to install
library(package = "ggplot2")
library(package = "tidyverse")
library(package = "jtools") #will likely need to install

####import data####
care <- read.csv("~/Workspace/PSY300B/PSY300B_F25/privitera_infocus/9_8_one_sample_t.csv") #In the Environment window, Import > From Text (base) > Select File > Name the file whatever you want. Copy and paste the line of code from the Console window.

#check data types of the variables
str(care)

####evaluate assumptions####

  #####normality#####
#skewness
#kurtosis
describe(care$health) #view skew and kurtosis information in those columns, do not match SPSS in book
describe(care$health, type=2) #different method of calculating skew and kurtosis, matches SPSS, we will use this
print(describe(care$health, type=2), digits=3) #change digits if want more decimal places
psych::describe(care$health, type=2) #this tells R to specifically look to the package psych for the funciton describe. Sometimes there are multiple packages with functions with different names. R can get confused between the different functions with the same name.

#skewness within +/- 1.0 
#kurtosis within +/- 2.0

#does it pass normality for skewness? kurtosis?

#Shapiro-Wilk test for normality
shapiro.test(care$health)

#does it pass Shapiro-Wilk test?

####compute one-sample t-test####
#t.test(x = df$dv, mu = value)
t.test(x = care$health, mu = 77.43)
#confidence interval included in output

#Cohen's D effect size
#cohensD(x = df$dv, mu = value)
cohensD(x = care$health, mu = 77.43)

#alternative way to calculate one-sample t-test
#oneSampleTTest(df$dv, mu = value, one.sided = FALSE, conf.level = 0.95)
oneSampleTTest(care$health, mu = 77.43, one.sided = FALSE, conf.level = 0.95) # this gives you the same result as the T-test and Cohen's D test, but in a different format

####visualize data####
#below we build up our visualization via histogram

health.histo1 <-  
  ggplot(data= care, 
         mapping = aes(x = health)) +
  
  geom_histogram() 
  

health.histo2<-  
  ggplot(data= care, 
         mapping = aes(x = health)) +
  
  geom_histogram() +
  
  theme_minimal() 


health.histo3<-  
  
  ggplot(data= care, 
         mapping = aes(x = health)) +
  
  geom_histogram() +
  
  theme_apa() 


health.histo4<-  
  
  ggplot(data = care, 
         mapping = aes(x = health)) +
  
  geom_histogram(binwidth = 5,
                 fill = "#7463AC",
                 color = "white") +
  
  theme_apa() +
  
  labs(title = "title",
       x = "x-axis",
       y = "y-axis")

#compare each histogram to see what is changing each iteration. 

#for geom_histogram, play around with binwidth, fill, and color. try changing theme_apa to theme_minimal. Try changing the title and labels for the axes under labs.


