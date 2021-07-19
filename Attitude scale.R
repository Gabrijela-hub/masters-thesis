
library(psych)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(FSA)
library(rstatix)
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")

# load the data
cookies <- read.csv("Cookies Data Set.csv")

# remove participants that didn't complete Attitudes questionnaire
cookies <- cookies[rowSums(is.na(cookies)) < 20,]
# N = 1166

Subset_Attitudes <- cookies[ , 12:20]

# SUM of Attitudes scale
cookies$Attitudes_Sum <- rowSums(Subset_Attitudes)

# Average of Attitudes scale
cookies$Attitudes_Average <- rowMeans(Subset_Attitudes)

# Benefits subscale score (average of Health, Environment, Animal Welfare)
cookies$Benefits_Attitudes <- rowMeans(Subset_Attitudes[,1:3])

# Satisfaction subsacale score (average of Tasty, Enjoyable, Filling)
cookies$Satisfaction_Attitudes <- rowMeans(Subset_Attitudes %>% select(4:5,8))

# Practicality subscale score (average of Convenient, Cheap, Local)
cookies$Practicality_Attitudes <- rowMeans(Subset_Attitudes %>% select(6:7,9))

# create Attitudes data frame

Attitude_Scale_DF <- cookies %>% 
                        
                        # select Attutude items and scale results
                         subset(., select = c(11,12:20, 44:48)) %>% 

                        # keep only complete cases
                        .[complete.cases(.), ]
# N = 1166 

                        
alpha(Attitude_Scale_DF[,2:10])

# Cronbach Alpha for the entire scale alpha = 0.88

omega(Attitude_Scale_DF[,2:10])

# Omega Hierarchical (one factor) = 0.75
# Omega Total (3 factors) = 0.92


# reliability: Benefits
alpha(Attitude_Scale_DF[,2:4])
# Cronbach Alpha = 0.8

# reliability: Satisfaction
alpha(Attitude_Scale_DF %>% select(5:6,9))
# Cronbach Alpha = 0.89

# reliability: Practicality
alpha(Attitude_Scale_DF %>% select(7:8,10))
# Cronbach Alpha = 0.71


# descriptives for Attitudes subscales
descriptives_benefits <- describeBy(Attitude_Scale_DF$Benefits_Attitudes, 
                                    Attitude_Scale_DF$Diet_Type)
descriptives_satisfaction <- describeBy(Attitude_Scale_DF$Satisfaction_Attitudes, 
                                        Attitude_Scale_DF$Diet_Type)
descriptives_practicality <- describeBy(Attitude_Scale_DF$Practicality_Attitudes, 
                                        Attitude_Scale_DF$Diet_Type)

attitudes_descriptives <- map_df(list(descriptives_benefits,
                                      descriptives_satisfaction,
                                      descriptives_practicality),
                                 rbind)

attitudes_descriptives
# plotting variables
hist(Attitude_Scale_DF$Benefits_Attitudes)
hist(Attitude_Scale_DF$Satisfaction_Attitudes)
hist(Attitude_Scale_DF$Practicality_Attitudes)
# distributions don't match normal distibution


# checking homogeneity of variances
bartlett.test(Diet_Type~Benefits_Attitudes, Attitude_Scale_DF)
bartlett.test(Diet_Type~Satisfaction_Attitudes, Attitude_Scale_DF)
bartlett.test(Diet_Type~Practicality_Attitudes, Attitude_Scale_DF)
# variances between Diet_Types are unequal

#  visualisation
ggboxplot(Attitude_Scale_DF, x = "Diet_Type", y = "Benefits_Attitudes", 
          color = "Diet_Type", 
          order = c("1", "2", "3"),
          ylab = "Benefits", xlab = "Diet Type")

ggboxplot(Attitude_Scale_DF, x = "Diet_Type", y = "Satisfaction_Attitudes", 
          color = "Diet_Type", 
          order = c("1", "2", "3"),
          ylab = "Satisfaction", xlab = "Diet Type")

ggboxplot(Attitude_Scale_DF, x = "Diet_Type", y = "Practicality_Attitudes", 
          color = "Diet_Type", 
          order = c("1", "2", "3"),
          ylab = "Practicality", xlab = "Diet Type")

# Kruskal-Wallis rank sum test
kruskal.test(Diet_Type~Benefits_Attitudes, data=Attitude_Scale_DF)
kruskal.test(Diet_Type~Satisfaction_Attitudes, data=Attitude_Scale_DF)
kruskal.test(Diet_Type~Practicality_Attitudes, data=Attitude_Scale_DF)
# all significant

# effect sizes; eta squared
kruskal_effsize(Diet_Type~Benefits_Attitudes, data=Attitude_Scale_DF)
kruskal_effsize(Diet_Type~Satisfaction_Attitudes, data=Attitude_Scale_DF)
kruskal_effsize(Diet_Type~Practicality_Attitudes, data=Attitude_Scale_DF)

#effect sizes
kruskal_effzis

#Dunn's post-coc tets with Bonferroni correction

# Benefits
dunnTest(Benefits_Attitudes~Diet_Type, 
         data=Attitude_Scale_DF,
         method="bonferroni")
# vegetarians(2) and vegans (3) do not differ

# Satisfaction
dunnTest(Satisfaction_Attitudes~Diet_Type, 
         data=Attitude_Scale_DF,
         method="bonferroni")
# vegetarians(2) and vegans (3) do not differ

# Practicality
dunnTest(Practicality_Attitudes~Diet_Type, 
         data=Attitude_Scale_DF,
         method="bonferroni")
# significant difference between all dietary groups







