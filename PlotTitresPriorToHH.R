data_titres_rel_HH <- read.csv("y:/ian/johnesthresholds/johnesproper/data/individualfarms/Combined_TestResultsLong.csv")

ggplot(data_titres_rel_HH[data_titres_rel_HH$TestBeforeHH == 1 & data_titres_rel_HH$AgeAtLastTest >= 90,], aes(x = AgeAtTest, y = value)) +
  geom_point() +
  ylim(0,200) +
  labs(title = "Pre-HH Titres of Ultimately HH/H-H animals (Last Test >= 90m old)")


ggplot(data_titres_rel_HH[data_titres_rel_HH$EverHH == 0 & data_titres_rel_HH$AgeAtLastTest >= 90,], aes(x = AgeAtTest, y = value)) +
  geom_point() +
  ylim(0,200) +
  labs(title = "Titres of Never HH animals (Last Test >= 90m old)")


