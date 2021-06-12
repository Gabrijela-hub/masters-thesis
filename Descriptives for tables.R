
# sample descriptives

library(dplyr)
library(pastecs)

descriptives_df <- cookies %>% 
  # select soc-demo data and Attitude scale
  subset(., select = c(7:20))
  
# N = 1166 

# SOC_DEMO DESCRIPTIVES ====

# table with descriptives for soc-demo data
sample_descriptives <- stat.desc(descriptives_df[1:7])
sample_frequencies <- apply(descriptives_df[1:7],2,table)

sample_descriptives

# get proportions for soc-demo variables
gender_descriptives <-  table(descriptives_df$Gender)
education_descriptives <- table(descriptives_df$Education)
diet_descriptives <- table(descriptives_df$Diet_Type)

#save proportions into a data frame
percentages <- map_df(list(gender_descriptives,
                        education_descriptives,
                        diet_descriptives), 
                   prop.table)

# setting row names (not ideal)
rownames(percentages) <- c('Gender', 'Education', 'Diet')

round(sample_descriptives,2)

# OMNIVORE ATTITUDES DESCRIPTIVES ====

omnivores_df <- cookies[cookies$Diet_Type==1,]

omnivores_attitude_descriptives <- stat.desc(omnivores_df$Attitudes_Average)
omnivores_attitude_descriptives 

omnivore_attitudes_descriptives <- stat.desc(omnivores_df[46:48])

