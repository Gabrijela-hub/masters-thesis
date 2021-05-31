
library(psych)
library(dplyr)
library(ggpubr)

# load the data
cookies <- read.csv("Cookies Data Set.csv")

omnivores <- cookies %>% 
                        # select omnivores
                        .[.$Diet_Type==1,] %>%
  
                        # get complete cases for the experiment
                        .[complete.cases(.[ ,1:31]), ]

# reliability of Acceptability
omega(omnivores[ , 22:28])


# Acceptability and average acceptability added                        
omnivores$Acceptabiliy <- rowSums(omnivores[ , 22:28])

omnivores$Acceptabiliy_Average <- rowMeans(omnivores[ , 22:28])

#data frame for Hypothesis 1 in the experiment

experiment_1 <- omnivores %>% 
                              # select control and vegan group
                              .[which(.$Manipulation_Groups==1 |
                                      .$Manipulation_Groups==2), ]
                              

#data frame for Hypothesis 2 in the experiment

experiment_2 <- omnivores %>% 
                              # select vegan,  vegan_health and vegan_animals groups
                              .[which(.$Manipulation_Groups==2 |
                                      .$Manipulation_Groups==3 | 
                                      .$Manipulation_Groups==4), ]
# plotting variables

hist(experiment_1$Acceptabiliy_Average)
hist(experiment_1$WTP)
hist(experiment_1$Expected_price)
hist(experiment_1$Likely_To_Buy)
     

# removing outliers in open-ended items
outliers_expected_price_1 <- boxplot(experiment_1$Expected_price, plot=FALSE)$out
experiment_1 <- experiment_1[-which(experiment_1$Expected_price %in%
                                      outliers_expected_price_1),]

outliers_WTP_1 <- boxplot(experiment_1$WTP, plot=FALSE)$out
experiment_1 <- experiment_1[-which(experiment_1$WTP %in%
                                      outliers_WTP_1),]

# plot again
hist(experiment_1$Expected_price)
hist(experiment_1$WTP)

# descriptive statistics
describe(experiment_1$Acceptabiliy_Average)
describe(experiment_1$WTP)
describe(experiment_1$Expected_price)
describe(experiment_1$Likely_To_Buy)

# checking equality of variance

bartlett.test(Acceptabiliy_Average~Manipulation_Groups, experiment_1)
bartlett.test(WTP~Manipulation_Groups, experiment_1)
bartlett.test(Expected_price~Manipulation_Groups, experiment_1)
bartlett.test(Likely_To_Buy~Manipulation_Groups, experiment_1)
# only Expected price deviates slighlty; parametrics can be used


# subsetting by experimental groups

control_exp_1 <- experiment_1[experiment_1$Manipulation_Groups==1,]
vegan_exp_1 <- experiment_1[experiment_1$Manipulation_Groups==2,]


# t-tests

acceptability_t_exp_1 <- t.test(control_exp_1$Acceptabiliy_Average, 
                                vegan_exp_1$Acceptabiliy_Average, 
                                var.equal = TRUE)

wtp_t_exp_1 <- t.test(control_exp_1$WTP, 
                      vegan_exp_1$WTF, paired=FALSE, 
                      var.equal = TRUE)

expected_price_t_exp_1 <- t.test(control_exp_1$Expected_price, 
                                 vegan_exp_1$Expected_price, 
                                 var.equal = TRUE)

likely_to_buy_t_exp_1 <- t.test(control_exp_1$Likely_To_Buy, 
                                vegan_exp_1$Likely_To_Buy, 
                                var.equal = TRUE)


library(broom)
library(purrr)
t_tests_exp_1 <- map_df(list(acceptability_t_exp_1,
                          wtp_t_exp_1, 
                          expected_price_t_exp_1,
                          likely_to_buy_t_exp_1),
                        tidy)

rownames(t_tests_exp_1) <- c("Acceptability", 
                             "WTP", 
                             "Expected_Price", 
                             "Likely_To_Buy")





which(colnames(cookies)=="Overall_Acceptability")



