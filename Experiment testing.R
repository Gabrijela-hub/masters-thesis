
library(psych)
library(dplyr)
library(ggpubr)
library(broom)
library(purrr)
library(rstatix)
library(effectsize)


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


# EXPERIMENT 1 ====

#data frame for Hypothesis 1 in the experiment

experiment_1 <- omnivores %>% 
                              # select control and vegan group
                              .[which(.$Manipulation_Groups==1 |
                                      .$Manipulation_Groups==2), ]
# N = 415

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
# N = 400

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

acceptability_t_test <- t.test(control_exp_1$Acceptabiliy_Average, 
                                vegan_exp_1$Acceptabiliy_Average, 
                                var.equal = TRUE)

wtp_t_test <- t.test(control_exp_1$WTP, 
                      vegan_exp_1$WTP,
                      var.equal = TRUE)

expected_price_t_test <- t.test(control_exp_1$Expected_price, 
                                 vegan_exp_1$Expected_price, 
                                 var.equal = TRUE)

likely_to_buy_t_test <- t.test(control_exp_1$Likely_To_Buy, 
                                vegan_exp_1$Likely_To_Buy, 
                                var.equal = TRUE)

# effect sizes

cohen_acceptability <- cohens_d(control_exp_1$Acceptabiliy_Average, 
         vegan_exp_1$Acceptabiliy_Average)

cohen_wtp <- cohens_d(control_exp_1$WTP, 
                                vegan_exp_1$WTP)

cohen_expected_price<- cohens_d(control_exp_1$Expected_price, 
                                vegan_exp_1$Expected_price)

cohen_likely_to_buy <- cohens_d(control_exp_1$Likely_To_Buy, 
                      vegan_exp_1$Likely_To_Buy)


cohen_d_df <- rbind(cohen_acceptability,
                  cohen_wtp,
                  cohen_expected_price,
                  cohen_likely_to_buy)
cohen_d_df

# put t-test results in a df

t_tests <- map_df(list(acceptability_t_test,
                       wtp_t_test, 
                       expected_price_t_test,
                       likely_to_buy_t_test),
                  tidy)

t_tests$Cohens_d <- cohen_d_df$Cohens_d

rownames(t_tests) <- c("Acceptability", 
                       "WTP", 
                       "Expected_Price", 
                       "Likely_To_Buy")

# get descriptives by group for experiment 1
descriptives_acc <- describeBy(experiment_1$Acceptabiliy_Average, group = experiment_1$Manipulation_Groups)
descriptives_wtp <- describeBy(experiment_1$WTP, group = experiment_1$Manipulation_Groups)
descriptives_expected <- describeBy(experiment_1$Expected_price, group = experiment_1$Manipulation_Groups)
descriptives_likely <- describeBy(experiment_1$Likely_To_Buy, group = experiment_1$Manipulation_Groups)


exp_1_descriptives <- map_df(list(descriptives_acc,
                            descriptives_wtp,
                            descriptives_expected,
                            descriptives_likely), rbind)

exp_1_descriptives

library("Hmisc")

correlations_exp_1 <- rcorr(as.matrix(subset(experiment_1, select = c(29:31,45))))
correlations_exp_1

test = experiment_1[experiment_1$Manipulation_Groups==2,]

rcorr(as.matrix(subset(test, select = c(29:31,45))))

# EXPERIMENT 2 ====

#data frame for Hypothesis 2 in the experiment

experiment_2 <- omnivores %>% 
                              # select vegan,  vegan_health and vegan_animals groups
                              .[which(.$Manipulation_Groups==2 |
                                      .$Manipulation_Groups==3 | 
                                      .$Manipulation_Groups==4), ]

# N = 601

# plotting variables

hist(experiment_2$Acceptabiliy_Average)
hist(experiment_2$WTP)
hist(experiment_2$Expected_price)
hist(experiment_2$Likely_To_Buy)

# remove outliers
outliers_expected_price_2 <- boxplot(experiment_2$Expected_price, plot=FALSE)$out
experiment_2 <- experiment_2[-which(experiment_2$Expected_price %in%
                                      outliers_expected_price_2),]

outliers_WTP_2 <- boxplot(experiment_2$WTP, plot=FALSE)$out
experiment_2 <- experiment_2[-which(experiment_2$WTP %in%
                                      outliers_WTP_2),]
# N = 525


outliers_expected_price_2
# plot again
hist(experiment_2$Expected_price)
hist(experiment_2$WTP)


# checking equality of variance

bartlett.test(Acceptabiliy_Average~Manipulation_Groups, experiment_2)
bartlett.test(WTP~Manipulation_Groups, experiment_2)
bartlett.test(Expected_price~Manipulation_Groups, experiment_2)
bartlett.test(Likely_To_Buy~Manipulation_Groups, experiment_2)
# variances equal


acceptability_anova <- aov(Acceptabiliy_Average~
                           Manipulation_Groups, 
                           data = experiment_2)

WTP_anova <- aov(WTP~
                 Manipulation_Groups, 
                 data = experiment_2)

expected_price_anova <- aov(Expected_price~
                            Manipulation_Groups, 
                            data = experiment_2)

likely_to_buy_anova <- aov(Likely_To_Buy~
                           Manipulation_Groups, 
                           data = experiment_2)


anova_summary(WTP_anova)

anova_exp_2 <- map_df(list(acceptability_anova, 
                           WTP_anova,
                           expected_price_anova,
                           likely_to_buy_anova), 
                      anova_summary)
# not significant

descriptives_acc_exp2 <- describeBy(experiment_2$Acceptabiliy_Average,
                                    group = experiment_2$Manipulation_Groups)
descriptives_wtp_exp2 <- describeBy(experiment_2$WTP,
                                    group = experiment_2$Manipulation_Groups)
descriptives_expected_exp2 <- describeBy(experiment_2$Expected_price,
                                         group = experiment_2$Manipulation_Groups)
descriptives_likely_exp2 <- describeBy(experiment_2$Likely_To_Buy, 
                                       group = experiment_2$Manipulation_Groups)


exp_2_descriptives <- map_df(list(descriptives_acc_exp2,
                                  descriptives_wtp_exp2,
                                  descriptives_expected_exp2,
                                  descriptives_likely_exp2), 
                             rbind)
exp_2_descriptives
  
cor(x=vegan_exp_1$WTP, y=vegan_exp_1$Likely_To_Buy, method = c("pearson")) 
cor(x=vegan_exp_1$WTP, y=vegan_exp_1$Expected_price, method = c("pearson")) 
cor(x=vegan_exp_1$Likely_To_Buy, y=vegan_exp_1$Expected_price, method = c("pearson")) 


library(Hmisc)

# correlations for the vegan group

rcorr(as.matrix(vegan_exp_1[28:31]),type="pearson")



