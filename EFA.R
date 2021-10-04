
EFA <- read.csv("Data for EFA.csv")

# PRINCIPAL COMPONENT ANALYSIS

jmv::pca(
  data = EFA,
  vars = vars(Attitudes_Healthy, Attitudes_Environment, Attitudes_Animal_Welfare, Attitudes_Tasty, Attitudes_Enjoyable, Attitudes_Convenient, Attitudes_Cheap, Attitudes_Filling, Attitudes_Local),
  nFactors = 3,
  rotation = "oblimin",
  screePlot = TRUE,
  factorCor = TRUE,
  factorSummary = TRUE,
  kmo = TRUE,
  bartlett = TRUE)

# conclusion: one factor