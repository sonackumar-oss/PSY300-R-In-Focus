###Independent-Sample T-test###

###packages###
#if any are not installed, install them
library(package = "psych")
library(package = "lsr") # will likely need to install
library(package = "ggplot2")
library(package = "tidyverse")
library(package = "jtools") #will likely need to install
library(package = "car")

####import data####
buffet <- read.csv("~/Workspace/PSY300B/PSY300B_F25/privitera_infocus/10_8_independent_t.csv") #In the Environment window, Import > From Text (base) > Select File > Name the file whatever you want. Copy and paste the line of code from the Console window.

#check data types of the variables
str(buffet)

#convert to factor
buffet$groups<-as.factor(buffet$groups)

####evaluate assumptions####

#####normality#####
#skewness
#kurtosis
#when there are different groups, we want to calculate separately for each group
#describeBy(df$DV, group = df$IV)
describeBy(buffet$intake, group = buffet$groups) #not match SPSS
print (describeBy(buffet$intake, group = buffet$groups, type = 2), digits = 3) #view skew and kurtosis information in those columns, matches SPSS

#skewness within +/- 1.0 
#kurtosis within +/- 2.0

#does each group pass normality for skewness? kurtosis?

#Shapiro-Wilk test for normality
#conduct separately for each group
#break up data into two groups, one for each level

#df_subset <- filter(df, IV == "IVlevel")
slowly <- filter(buffet, groups == "slowly") #filters data to only include relevant rows of data based on the criteria. Puts this subset of data in a new dataframe, which you name.
fast <- filter(buffet, groups == "fast")

#shapiro-wilk test on each group
shapiro.test(fast$intake)
shapiro.test(slowly$intake)

#do both groups pass Shapiro-Wilk test?

####Equality of variances####
#Levene's test
#leveneTest(y = DV ~ IV, data = df)
leveneTest(y = intake ~ groups, center = mean, data = buffet) #the default is to center this metric around the median, we want to center around the mean to match SPSS output from your book

####compute independent sample t-test####
#t.test(formula = data$DV ~ data$IV)
t.test(formula = buffet$intake ~ buffet$groups, var.equal = TRUE) #conducts and returns results of an independent sample t-test assuming the variances are equal (what the Levene test tested for), otherwise the default is to use Welch's which does not assume equal variance
#confidence interval included in output

#Cohen's D effect size
#cohensD(x = DV ~ IV,
#        data = df,
#        method = "pooled") #use pooled if assumption of equal variances is met

cohensD(x = intake ~ groups,
        data = buffet,
        method = "pooled")

#alternative way to calculate independent sample t-test
#same output as indepdent samples t-test and cohen's D test
# independentSampleTTest(DV ~ IV, 
#                        data = df, 
#                        var.equal = TRUE,
#                        one.sided = FALSE, 
#                        conf.level = 0.95)

independentSamplesTTest(intake ~ groups,
                        data = buffet,
                        var.equal = TRUE,
                        one.sided = FALSE,
                        conf.level = 0.95)

####visualize data####

#histograms of each group 
buffet$intakeN<-as.numeric(buffet$intake) #to create a histogram of the intake variable, it must be treated as a numeric data type

intake.histo <- ggplot(data = buffet, mapping = aes(x = intake)) +
  
  geom_histogram(bins = 10, 
                 fill = "#7463AC",
                 color = "white") +
  
  facet_grid(cols = vars(groups), #use the groups in variable groups for the different panels
             labeller = as_labeller(c(fast = "Fast", slowly = "Slowly"))) + #relabel the different panels with formatted names
  
  theme_apa() + 
  
  labs(title = "Histogram of calorie intake by eating speed",
       x = "Calorie intake",
       y = "Participants")

#bar graph comparing the two groups with error bars

# data.plot <- data.frame(
#  Group = c("Group A", "Group B"),
#  Mean = c(mean.t1, mean.t2),
#  sem = c(SEM.t1, SEM.t2)
# )

#get the data from the earlier descriptive statistics you looked at
#what do you run to get these descriptive statistics?
buffet.data.plot <- data.frame(
  Group = c("Fast", "Slowly"),
  Mean = c(650, 600),
  sem = c(53.229, 63.246)
)

intake.bar <- ggplot(data = buffet.data.plot) + 
                    geom_bar(aes(x = Group, y = Mean), 
                             stat = "identity", 
                             fill = c("red", "blue"),
                             color = "white") +
                    geom_errorbar(aes(x = Group, ymin = Mean - sem, ymax = Mean + sem), 
                                  width = .2) +
             theme_apa() + 
             labs(title = "Mean calorie intake by eating speed",
                  x = "Eating speed",
                  y = "Mean calorie intake")



