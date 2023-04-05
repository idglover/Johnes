#######Herd Johne's Data######

ggplot(data_combi[data_combi$Target_4 != "U",], aes(x = propposavg, y = as.numeric(Target_4)-1)) +
  geom_smooth()

model_LR_univar <- glm(Target_4 ~ propposavg, data = data_combi[data_combi$Target_4 != "U",], family = "binomial")


ggplot(data_combi[data_combi$Target_4 != "U",], aes(x = meantitreavg, y = as.numeric(Target_4)-1)) +
  geom_smooth()

model_LR_univar <- glm(Target_4 ~ meantitreavg, data = data_combi[data_combi$Target_4 != "U",], family = "binomial")
model_LR_univar <- glm(Target_4 ~ meantitreavg + I(meantitreavg^2), data = data_combi[data_combi$Target_4 != "U",], family = "binomial")



ggplot(data_combi[data_combi$Target_4 != "U",], aes(x = meantitrenegcowsavg, y = as.numeric(Target_4)-1)) +
  geom_smooth()

model_LR_univar <- glm(Target_4 ~ meantitrenegcowsavg, data = data_combi[data_combi$Target_4 != "U",], family = "binomial")



#######Siblings######

ggplot(data_combi[data_combi$Target_4 != "U",], aes(x = as.factor(nsiblings), y = as.numeric(Target_4)-1)) +
  geom_bar(stat = "summary", fun.y = "mean")

model_LR_univar <- glm(Target_4 ~ nsiblings, data = data_combi[data_combi$Target_4 != "U",], family = "binomial")


ggplot(data_combi[data_combi$Target_4 != "U",], aes(x = as.factor(nsiblingsstatus2birth), y = as.numeric(Target_4)-1)) +
  geom_bar(stat = "summary", fun.y = "mean")

model_LR_univar <- glm(Target_4 ~ as.factor(nsiblingsstatus2birth), data = data_combi[data_combi$Target_4 != "U",], family = "binomial")

ggplot(data_combi[data_combi$Target_4 != "U",], aes(x = as.factor(nsiblingsstatus3birth), y = as.numeric(Target_4)-1)) +
  geom_bar(stat = "summary", fun.y = "mean")

model_LR_univar <- glm(Target_4 ~ as.factor(nsiblingsstatus3birth), data = data_combi[data_combi$Target_4 != "U",], family = "binomial")


ggplot(data_combi[data_combi$Target_4 != "U",], aes(x = as.factor(propsiblingsstatus2birth), y = as.numeric(Target_4)-1)) +
  geom_bar(stat = "summary", fun.y = "mean")

model_LR_univar <- glm(Target_4 ~ propsiblingsstatus2birth, data = data_combi[data_combi$Target_4 != "U",], family = "binomial")

ggplot(data_combi[data_combi$Target_4 != "U",], aes(x = as.factor(propsiblingsstatus3birth), y = as.numeric(Target_4)-1)) +
  geom_bar(stat = "summary", fun.y = "mean")

model_LR_univar <- glm(Target_4 ~ propsiblingsstatus3birth, data = data_combi[data_combi$Target_4 != "U",], family = "binomial")


#######Dam/Granddam status#########

ggplot(data_combi[data_combi$Target_4 != "U",], aes(x = as.factor(damstatusbirth), y = as.numeric(Target_4)-1)) +
  geom_bar(stat = "summary", fun.y = "mean")

model_LR_univar <- glm(Target_4 ~ as.factor(damstatusbirth), data = data_combi[data_combi$Target_4 != "U",], family = "binomial")

data_combi$damstatusbirth_cat <- as.factor(data_combi$damstatusbirth)
levels(data_combi$damstatusbirth_cat) <- c(levels(data_combi$damstatusbirth_cat), "Other")
data_combi$damstatusbirth_cat[data_combi$damstatusbirth_cat != "1" & data_combi$damstatusbirth_cat != "2" & data_combi$damstatusbirth_cat != "3"] <- "Other"
data_combi$damstatusbirth_cat <- droplevels(data_combi$damstatusbirth_cat, c("4", "5", "6", "7", "8"))

model_LR_univar <- glm(Target_4 ~ as.factor(damstatusbirth_cat), data = data_combi[data_combi$Target_4 != "U",], family = "binomial")

ggplot(data_combi[data_combi$Target_4 != "U",], aes(x = as.factor(granddamstatusbirth), y = as.numeric(Target_4)-1)) +
  geom_bar(stat = "summary", fun.y = "mean")

model_LR_univar <- glm(Target_4 ~ as.factor(granddamstatusbirth), data = data_combi[data_combi$Target_4 != "U",], family = "binomial")

data_combi$granddamstatusbirth_cat <- as.factor(data_combi$granddamstatusbirth)
levels(data_combi$granddamstatusbirth_cat) <- c(levels(data_combi$granddamstatusbirth_cat), "Other")
data_combi$granddamstatusbirth_cat[data_combi$granddamstatusbirth_cat != "1" & data_combi$granddamstatusbirth_cat != "2" & data_combi$granddamstatusbirth_cat != "3"] <- "Other"
data_combi$granddamstatusbirth_cat <- droplevels(data_combi$granddamstatusbirth_cat, c("4", "5", "6", "7", "8"))

model_LR_univar <- glm(Target_4 ~ as.factor(granddamstatusbirth_cat), data = data_combi[data_combi$Target_4 != "U",], family = "binomial")

#######Aunts######

ggplot(data_combi[data_combi$Target_4 != "U",], aes(x = as.factor(naunts), y = as.numeric(Target_4)-1)) +
  geom_bar(stat = "summary", fun.y = "mean")

model_LR_univar <- glm(Target_4 ~ naunts, data = data_combi[data_combi$Target_4 != "U",], family = "binomial")

ggplot(data_combi[data_combi$Target_4 != "U",], aes(x = as.factor(ngreataunts), y = as.numeric(Target_4)-1)) +
  geom_bar(stat = "summary", fun.y = "mean")

model_LR_univar <- glm(Target_4 ~ ngreataunts, data = data_combi[data_combi$Target_4 != "U",], family = "binomial")



ggplot(data_combi[data_combi$Target_4 != "U",], aes(x = as.factor(propauntsstatus2birth), y = as.numeric(Target_4)-1)) +
geom_bar(stat = "summary", fun.y = "mean")

model_LR_univar <- glm(Target_4 ~ propauntsstatus2birth, data = data_combi[data_combi$Target_4 != "U",], family = "binomial")



ggplot(data_combi[data_combi$Target_4 != "U",], aes(x = as.factor(propauntsstatus3birth), y = as.numeric(Target_4)-1)) +
  geom_bar(stat = "summary", fun.y = "mean")

model_LR_univar <- glm(Target_4 ~ propauntsstatus3birth, data = data_combi[data_combi$Target_4 != "U",], family = "binomial")


ggplot(data_combi[data_combi$Target_4 != "U",], aes(x = as.factor(propgreatauntsstatus2birth), y = as.numeric(Target_4)-1)) +
  geom_bar(stat = "summary", fun.y = "mean")

model_LR_univar <- glm(Target_4 ~ propgreatauntsstatus2birth, data = data_combi[data_combi$Target_4 != "U",], family = "binomial")




ggplot(data_combi[data_combi$Target_4 != "U",], aes(x = as.factor(propgreatauntsstatus3birth), y = as.numeric(Target_4)-1)) +
  geom_bar(stat = "summary", fun.y = "mean")

model_LR_univar <- glm(Target_4 ~ propgreatauntsstatus3birth, data = data_combi[data_combi$Target_4 != "U",], family = "binomial")




#######Proximal Calves######

ggplot(data_combi[data_combi$Target_4 != "U",], aes(x = nproximalcalves, y = as.numeric(Target_4)-1)) +
  geom_smooth()

model_LR_univar <- glm(Target_4 ~ nproximalcalves, data = data_combi[data_combi$Target_4 != "U",], family = "binomial")

ggplot(data_combi[data_combi$Target_4 != "U",], aes(x = nvaguelyproximalcalves, y = as.numeric(Target_4)-1)) +
  geom_smooth()

model_LR_univar <- glm(Target_4 ~ nvaguelyproximalcalves, data = data_combi[data_combi$Target_4 != "U",], family = "binomial")


#######Proximal Dams#######

ggplot(data_combi[data_combi$Target_4 != "U",], aes(x = nproximaldams, y = as.numeric(Target_4)-1)) +
  geom_smooth()

model_LR_univar <- glm(Target_4 ~ nproximaldams, data = data_combi[data_combi$Target_4 != "U",], family = "binomial")



ggplot(data_combi[data_combi$Target_4 != "U",], aes(x = nvaguelyproximaldams, y = as.numeric(Target_4)-1)) +
  geom_smooth()

model_LR_univar <- glm(Target_4 ~ nvaguelyproximaldams, data = data_combi[data_combi$Target_4 != "U",], family = "binomial")


ggplot(data_combi[data_combi$Target_4 != "U",], aes(x = as.factor(nproximaldamsstatus2birth), y = as.numeric(Target_4)-1)) +
  geom_bar(stat = "summary", fun.y = "mean")

model_LR_univar <- glm(Target_4 ~ nproximaldamsstatus2birth, data = data_combi[data_combi$Target_4 != "U",], family = "binomial")




ggplot(data_combi[data_combi$Target_4 != "U",], aes(x = as.factor(nproximaldamsstatus3birth), y = as.numeric(Target_4)-1)) +
  geom_bar(stat = "summary", fun.y = "mean")

model_LR_univar <- glm(Target_4 ~ nproximaldamsstatus3birth, data = data_combi[data_combi$Target_4 != "U",], family = "binomial")



ggplot(data_combi[data_combi$Target_4 != "U",], aes(x = as.factor(propproximaldamsstatus2birth), y = as.numeric(Target_4)-1)) +
  geom_bar(stat = "summary", fun.y = "mean")

model_LR_univar <- glm(Target_4 ~ propproximaldamsstatus2birth, data = data_combi[data_combi$Target_4 != "U",], family = "binomial")



ggplot(data_combi[data_combi$Target_4 != "U",], aes(x = as.factor(propproximaldamsstatus3birth), y = as.numeric(Target_4)-1)) +
  geom_bar(stat = "summary", fun.y = "mean")


model_LR_univar <- glm(Target_4 ~ propproximaldamsstatus3birth, data = data_combi[data_combi$Target_4 != "U",], family = "binomial")

ggplot(data_combi[data_combi$Target_4 != "U",], aes(x = as.factor(nvaguelyproximaldamsstatus2birth), y = as.numeric(Target_4)-1)) +
  geom_bar(stat = "summary", fun.y = "mean")

model_LR_univar <- glm(Target_4 ~ nvaguelyproximaldamsstatus2birth, data = data_combi[data_combi$Target_4 != "U",], family = "binomial")

ggplot(data_combi[data_combi$Target_4 != "U",], aes(x = as.factor(nvaguelyproximaldamsstatus3birth), y = as.numeric(Target_4)-1)) +
  geom_bar(stat = "summary", fun.y = "mean")

model_LR_univar <- glm(Target_4 ~ nvaguelyproximaldamsstatus3birth, data = data_combi[data_combi$Target_4 != "U",], family = "binomial")

ggplot(data_combi[data_combi$Target_4 != "U",], aes(x = as.factor(propvaguelyproximaldamsstatus2birth), y = as.numeric(Target_4)-1)) +
  geom_bar(stat = "summary", fun.y = "mean")

model_LR_univar <- glm(Target_4 ~ propvaguelyproximaldamsstatus2birth, data = data_combi[data_combi$Target_4 != "U",], family = "binomial")


ggplot(data_combi[data_combi$Target_4 != "U",], aes(x = as.factor(propvaguelyproximaldamsstatus3birth), y = as.numeric(Target_4)-1)) +
  geom_bar(stat = "summary", fun.y = "mean")

model_LR_univar <- glm(Target_4 ~ propvaguelyproximaldamsstatus3birth, data = data_combi[data_combi$Target_4 != "U",], family = "binomial")





