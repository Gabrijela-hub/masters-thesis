
library(psych)
library(plyr)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(FSA)

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")

# data frame containing only Attitude items
Attitude_Scale_DF <- cookies[,12:20]


alpha(Attitude_Scale_DF)

# Cronbach Alpha for the entire scale alpha = 0.88

omega(Attitude_Scale_DF)

# Omega Hierarchical (one factor) = 0.75
# Omega Total (3 factors) = 0.92


# SUM of Attitudes scale
cookies$Attitudes_Sum <- rowSums(Attitude_Scale_DF)

# Average of Attitudes scale
cookies$Attitudes_Average <- rowMeans(Attitude_Scale_DF)

# Benefits subscale score (average of Health, Environment, Animal Welfare)
cookies$Benefits_Attitudes <- rowMeans(Attitude_Scale_DF[,1:3])

# Satisfaction subsacale score (average of Tasty, Enjoyable, Filling)
cookies$Satisfaction_Attitudes <- rowMeans(Attitude_Scale_DF %>% select(4:5,8))

# Practicality subsacale score (average of Convenient, Cheap, Local)
cookies$Practicality_Attitudes <- rowMeans(Attitude_Scale_DF %>% select(6:7,9))

# descriptives for Attitudes subscales
describe(cookies$Benefits_Attitudes)
describe(cookies$Satisfaction_Attitudes)
describe(cookies$Practicality_Attitudes)
# skewness and curtosis acceptable; approximation of normal distribution


# checking homogeneity of variances
bartlett.test(Diet_Type~Benefits_Attitudes, data=cookies)
bartlett.test(Diet_Type~Satisfaction_Attitudes, data=cookies)
bartlett.test(Diet_Type~Practicality_Attitudes, data=cookies)
# variances between Diet_Types are unequal

#  visualisation
ggboxplot(cookies, x = "Diet_Type", y = "Benefits_Attitudes", 
          color = "Diet_Type", 
          order = c("1", "2", "3"),
          ylab = "Benefits", xlab = "Diet Type")

ggboxplot(cookies, x = "Diet_Type", y = "Satisfaction_Attitudes", 
          color = "Diet_Type", 
          order = c("1", "2", "3"),
          ylab = "Satisfaction", xlab = "Diet Type")

ggboxplot(cookies, x = "Diet_Type", y = "Practicality_Attitudes", 
          color = "Diet_Type", 
          order = c("1", "2", "3"),
          ylab = "Practicality", xlab = "Diet Type")

# Kruskal-Wallis rank sum test
kruskal.test(Diet_Type~Benefits_Attitudes, data=cookies)
kruskal.test(Diet_Type~Satisfaction_Attitudes, data=cookies)
kruskal.test(Diet_Type~Practicality_Attitudes, data=cookies)
# all significant

#Dunn's post-coc tets with Bonferroni correction

# Benefits
dunnTest(Benefits_Attitudes~Diet_Type, 
         data=cookies,
         method="bonferroni")
# vegetarians(2) and vegans (3) do not differ

# Satisfaction
dunnTest(Satisfaction_Attitudes~Diet_Type, 
         data=cookies,
         method="bonferroni")
# vegetarians(2) and vegans (3) do not differ

# Practicality
dunnTest(Practicality_Attitudes~Diet_Type, 
         data=cookies,
         method="bonferroni")
# significant difference between all dietary groups


omnivores 





