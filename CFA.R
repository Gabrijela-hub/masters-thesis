

library(lavaan)
library(psych)

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

