
library(lavaan)
library(psych)

CFA <- read.csv("Data for CFA.csv")

# defining 1-factor model
One_Factor <- ' Attitudes =~ Attitudes_Healthy + Attitudes_Environment + Attitudes_Animal_Welfare + Attitudes_Tasty + Attitudes_Enjoyable + Attitudes_Convenient + Attitudes_Cheap + Attitudes_Filling + Attitudes_Local'


# fitting 1-factor model
Fit_One_Factor <- cfa(model = One_Factor, data = CFA)

summary(Fit_One_Factor, fit.measures=TRUE, standardized=TRUE)

# defining 3-factor model
Three_Factor <-'
Benefits =~ Attitudes_Healthy + Attitudes_Environment + Attitudes_Animal_Welfare
Satisfaction =~ Attitudes_Tasty + Attitudes_Enjoyable + Attitudes_Filling
Practicality =~ Attitudes_Convenient + Attitudes_Cheap + Attitudes_Local
'

# fitting 3-factor model
Fit_Three_Factor <- cfa(model = Three_Factor, data = CFA)

summary(Fit_Three_Factor, fit.measures=TRUE, standardized=TRUE)



#defining bifactor model

Bifactor <-'
Benefits =~ Attitudes_Healthy + Attitudes_Environment + Attitudes_Animal_Welfare
Satisfaction =~ Attitudes_Tasty + Attitudes_Enjoyable + Attitudes_Filling
Practicality =~ Attitudes_Convenient + Attitudes_Cheap + Attitudes_Local
Attitudes =~ Attitudes_Healthy + Attitudes_Environment + Attitudes_Animal_Welfare + Attitudes_Tasty + Attitudes_Enjoyable + Attitudes_Convenient + Attitudes_Cheap + Attitudes_Filling + Attitudes_Local
'

# fitting bifactor model

Fit_Bifactor <- cfa(model = Bifactor, data = CFA, orthogonal=T)
summary(Fit_Bifactor, fit.measures=TRUE, standardized=TRUE)

# comparing fit of the 3 models
anova(Fit_One_Factor, Fit_Three_Factor, Fit_Bifactor)

# conclusion: bifactor solution is the most fitting

# bifactor model visualisaton

library(semPlot)


labels <- c('Health',
            'Environment',
            'Animals',
            'Taste',
            'Enjoyment',
            'Satiety',
            'Convenient',
            'Cheap',
            'Local', 
            'Benefits',
            'Satisfaction',
            'Practicality',
            'Attitudes')

semPaths(Fit_Bifactor, 
         label.scale = FALSE, 
         sizeMan2 = 4,
         sizeLat = 10,
         sizeMan = 10, layout = "tree2", nodeLabels=labels, bifactor = c("Attitudes"), rotation=2)


# factor loadings
inspect(Fit_Bifactor,what="std")$lambda
