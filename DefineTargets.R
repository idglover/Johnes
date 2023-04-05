data_combi$Target_4 <- ifelse(data_combi$ntests >=5 & data_combi$ageatlasttest >= 70 & data_combi$nonehigh == 1, "0", "U")
data_combi$Target_4 <- ifelse(data_combi$posstatusanypoint == 1, "1", data_combi$Target_4)
data_combi$Target_4 <- as.factor(data_combi$Target_4)


data_combi$Target_Meyer <- ifelse(data_combi$ntests >= 9 & grepl("H", substr(data_combi$profile, nchar(data_combi$profile)-7, nchar(data_combi$profile))) == FALSE, "0", "U")
data_combi$Target_Meyer <- ifelse(data_combi$ntests >= 3 &  substr(data_combi$profile, nchar(data_combi$profile) - 1, nchar(data_combi$profile)) == "HH", "1", data_combi$Target_Meyer)
data_combi$Target_Meyer <- as.factor(data_combi$Target_Meyer)




####OLD CODE####
data_combi_clean_cat$Target_1 <- ifelse(data_combi_clean_cat$nTests >=5 & data_combi_clean_cat$AgeAtLastTest >= 70 & data_combi_clean_cat$HAnyPoint == 0, "0", "U")
data_combi_clean_cat$Target_1 <- ifelse(data_combi_clean_cat$HAnyPoint == 1, "1", data_combi_clean_cat$Target_1)
data_combi_clean_cat$Target_1 <- as.factor(data_combi_clean_cat$Target_1)


data_combi_clean_cat$Target_2 <- ifelse(data_combi_clean_cat$nTests >=5 & data_combi_clean_cat$AgeAtLastTest >= 70 & data_combi_clean_cat$HHAnyPoint == 0, "0", "U")
data_combi_clean_cat$Target_2 <- ifelse(data_combi_clean_cat$HHAnyPoint == 1, "1", data_combi_clean_cat$Target_2)
data_combi_clean_cat$Target_2 <- as.factor(data_combi_clean_cat$Target_2)

data_combi_clean_cat$Target_3 <- ifelse(data_combi_clean_cat$nTests >=5 & data_combi_clean_cat$AgeAtLastTest >= 70 & data_combi_clean_cat$PosStatusAnyPoint == 0, "0", "U")
data_combi_clean_cat$Target_3 <- ifelse(data_combi_clean_cat$PosStatusAnyPoint == 1, "1", data_combi_clean_cat$Target_3)
data_combi_clean_cat$Target_3 <- as.factor(data_combi_clean_cat$Target_3)





data_combi_clean$Target_1 <- ifelse(data_combi_clean$nTests >=5 & data_combi_clean$AgeAtLastTest >= 70 & data_combi_clean$HAnyPoint == 0, "0", "U")
data_combi_clean$Target_1 <- ifelse(data_combi_clean$HAnyPoint == 1, "1", data_combi_clean$Target_1)
data_combi_clean$Target_1 <- as.factor(data_combi_clean$Target_1)


data_combi_clean$Target_2 <- ifelse(data_combi_clean$nTests >=5 & data_combi_clean$AgeAtLastTest >= 70 & data_combi_clean$HHAnyPoint == 0, "0", "U")
data_combi_clean$Target_2 <- ifelse(data_combi_clean$HHAnyPoint == 1, "1", data_combi_clean$Target_2)
data_combi_clean$Target_2 <- as.factor(data_combi_clean$Target_2)

data_combi_clean$Target_3 <- ifelse(data_combi_clean$nTests >=5 & data_combi_clean$AgeAtLastTest >= 70 & data_combi_clean$PosStatusAnyPoint == 0, "0", "U")
data_combi_clean$Target_3 <- ifelse(data_combi_clean$PosStatusAnyPoint == 1, "1", data_combi_clean$Target_3)
data_combi_clean$Target_3 <- as.factor(data_combi_clean$Target_3)





ggplot(data_combi_clean[data_combi_clean$Target_3 != "U",], aes(x = Target_3)) +
  geom_bar(stat = "count")

summary(as.factor(data_combi_clean$Target_3[data_combi_clean$Target_3 != "U"]))


