####Non-Parametric Ordinal Tests####
#packages
library(package = "psych")
library(package = "lsr") 
library(package = "ggplot2")
library(package = "tidyverse")
library(package = "jtools") 
library(package = "car")
library(package = "sciplot") 
library(package = "performance") l
library(package = "lmtest") 
library("BSDA") #likely need to install

####Wilcoxon Sign Ranks T Test####
#Hypothesis: People will like a new art installment in their community less the first time they see it than after one year of exposure.
#DV: Ratings from 1 (dislike very much) to 7 (like very much).

#import data
art <- 18_wilcoxon

#check structure
str(art)

#descriptive statistics
describe(art, type=2)

#check if data is normal - if so, do a related samples t-test
shapiro.test(art$visit1)
shapiro.test(art$visit2)

#statistical test
# wilcox.test(x = df$DV(Condition1),
#             y = df$DV(Condition2),
#             paired = TRUE, #stays constant
#             exact = FALSE) #stays constant

wilcox.test(x = art$visit1,
            y = art$visit2,
            paired = TRUE,
            exact = FALSE)

####Mann-Whitney U Test####
#aka  Wilcoxon rank sum test
#Hypothesis: Children wearing masks on Halloween will take more candy from unattended bowls than will children without masks.
#DV: Pieces of candy taken.

#import data
halloween <- 18_mannwhitneyu
#check structure
str(halloween)

halloween$costume <- as.factor(halloween$costume)

#descriptive statistics
describeBy(halloween$candy, group = halloween$costume, type = 2)

#check assumptions- if normal, do indpendent samples t-test
mask <- filter(halloween, costume == "mask")
no_mask <- filter(halloween, costume == "no_mask")

shapiro.test(mask$candy)
shapiro.test(no_mask$candy)

#statistical test
# DV.by.IV <- wilcox.test(formula = df$DV ~ df$IV,
#                                 paired = FALSE) #stays constant

candy.by.costume <- wilcox.test(formula = halloween$candy ~ halloween$costume,
                                paired = FALSE)

candy.by.costume

#look at median of each group to compare

####Kruskal Wallis H Test####
#Hypothesis: Girls' performance on a math exam will be affected by whether they hear a message that confirms or challenges the stereotype that women are bad at math.
#DV: Math exam scores from 0 to 100.

#import data
math <- read.csv("~/Workspace/PSY300B/Ng Working Folder/NonParametric/kruskalwallisH.csv")

#check structure
str(math)

math$condition <- as.factor(math$condition)

#descriptive statistics 
describeBy(math$score, group = math$condition, type = 2)

#check assumptions - normality - if normal, do one-way between subjects anova
confirm <- filter(math, condition == "confirm")
challenge <- filter(math, condition == "challenge")
control <- filter(math, condition == "control")

shapiro.test(confirm$score)
shapiro.test(challenge$score)
shapiro.test(control$score)

#statistical test
# DV.by.IV <- kruskal.test(formula = DV ~ IV,
#                                    data = df)

score.by.condition <- kruskal.test(formula = score ~ condition,
                                     
                                     data = math)
score.by.condition
