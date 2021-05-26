
# read data
cookies <- read.csv("Istraživanje o keksima with names.csv")
# N = 1322


# remove participants under the age od 18 
cookies <- cookies[cookies$Age>17,]
# N = 1305

# remove participants that didn't complete Attitudes questionnaire
cookies <- cookies[rowSums(is.na(cookies)) < 20,]
# N = 1166

# set seed
set.seed(50)


# create dummy variable containing 0 and 1 at random
dummy.set <- rbinom(nrow(cookies),1,0.5)

# split data in 2 sets
EFA <- cookies[dummy.set==0,]
CFA <- cookies[dummy.set==1,]

# write subsets as csv
write.csv(EFA, "Data for EFA.csv")
write.csv(CFA, "Data for CFA.csv")


