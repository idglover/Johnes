####SPLT TRAIN EVALUATE TEST#####

data_model <- data_combi_clean_cat[data_combi_clean_cat$Target_3 != "U",]
data_model$Target_3 <- droplevels(data_model$Target_3, c("U"))
levels(data_model$Target_3) <- c(levels(data_model$Target_3),"T","F")
data_model$Target_3[data_model$Target_3 == 1] <- "T"
data_model$Target_3[data_model$Target_3 == 0] <- "F"
data_model$Target_3 <- droplevels(data_model$Target_3, c("1","0"))

data_model$Target_3_binary <- ifelse(data_model$Target_3 == "T",1,0)
write.csv(data_model, "Y:/Ian/JohnesThresholds/JohnesProper/Data/Python/data_FactorNA.csv")





set.seed(1981)
trainRowNumbers <- createDataPartition(data_model$Target_2, p = 0.8, list = FALSE)
data_model_train <- data_model[trainRowNumbers,]
data_model_test <- data_model[-trainRowNumbers,]






data_model_train <- data_model_train[,c(7,11,22,457,458,483,484,467,468,475,476,456),]

data_model_test <- data_model_test[,c(7,11,22,457,458,483,484,467,468,475,476,456),]

write.csv(data_model_train, "Y:/Ian/JohnesThresholds/JohnesProper/Data/Python/data_train_birth_FactorNA.csv")
write.csv(data_model_test, "Y:/Ian/JohnesThresholds/JohnesProper/Data/Python/data_test_birth_FactorNA.csv")




data_model_NA <- data_combi_clean[data_combi_clean$Target_2 != "U",]
data_model_NA$Target_2 <- droplevels(data_model_NA$Target_2, c("U"))
levels(data_model_NA$Target_2) <- c(levels(data_model_NA$Target_2),"T","F")
data_model_NA$Target_2[data_model_NA$Target_2 == 1] <- "T"
data_model_NA$Target_2[data_model_NA$Target_2 == 0] <- "F"
data_model_NA$Target_2 <- droplevels(data_model_NA$Target_2, c("1","0"))

data_model_NA$Target_2_binary <- ifelse(data_model_NA$Target_2 == "T",1,0)




set.seed(1981)
trainRowNumbers <- createDataPartition(data_model_NA$Target_2, p = 0.8, list = FALSE)
data_model_NA_train <- data_model_NA[trainRowNumbers,]
data_model_NA_test <- data_model_NA[-trainRowNumbers,]


MI_model <- preProcess(data_model_NA_train, method = "knnImpute")

MI_model

data_model_NA_imputed <- predict(MI_model, newdata = data_model_NA_train)




write.csv(data_model_NA, "Y:/Ian/JohnesThresholds/JohnesProper/Data/Python/data_NA.csv")













fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5, sampling = "up", classProbs = TRUE, savePredictions = "final", summaryFunction = twoClassSummary)


model_glm_1 <- train(as.factor(Target_2) ~ ., data = data_model_train[data_model_train$Target_2 != "U",], method = "glm", trControl = fitControl)
model_regLR_1 <- train(as.factor(Target_2) ~ ., data = data_model_train[data_model_train$Target_2 != "U",], method = "regLogistic", trControl = fitControl)



model_MARS_1 <- train(as.factor(Target_2) ~ ., data = data_model_train[data_model_train$Target_2 != "U",], method = "earth", trControl = fitControl)
model_RF_1 <- train(as.factor(Target_2) ~ ., data = data_model_train[data_model_train$Target_2 != "U",], method = "ranger", trControl = fitControl)
model_SVMLin_1 <- train(as.factor(Target_2) ~ ., data = data_model_train[data_model_train$Target_2 != "U",], method = "svmLinear", trControl = fitControl)
model_SVMRad_1 <- train(as.factor(Target_2) ~ ., data = data_model_train[data_model_train$Target_2 != "U",], method = "svmRadial", trControl = fitControl)
model_SVMPoly_1 <- train(as.factor(Target_2) ~ ., data = data_model_train[data_model_train$Target_2 != "U",], method = "svmPoly", trControl = fitControl)
model_ADA_1 <- train(as.factor(Target_2) ~ ., data = data_model_train[data_model_train$Target_2 != "U",], method = "adaboost", trControl = fitControl)



models_compare <- resamples(list(GLM=model_glm_1, RegLR = model_regLR_1, MARS = model_MARS_1, RF=model_RF_1, SVMLinear = model_SVMLin_1, SVMRadial = model_SVMRad_1, SVMPoly = model_SVMPoly_1, model_ADA_1))
summary(models_compare)



#data_model_multivar_evaluate_imputed <- predict(MI_model, newdata = data_model_multivar_evaluate)

#data_model_multivar_evaluate_birth_imputed <- data_model_multivar_evaluate_imputed[,c(3,4,5,6,9,10,13,17,24,25,30,31,36,43,44,49,50,61)]

#data_model_multivar_test_imputed <- predict(MI_model, newdata = data_model_multivar_test)

#data_model_multivar_test_birth_imputed <- data_model_multivar_test_imputed[,c(3,4,5,6,9,10,13,17,24,25,30,31,36,43,44,49,50,61)]

#data_model_multivar_

PredClass <- predict(model_RF_1, newdata = data_model_test)

confusionMatrix(PredClass, data_model_test$Target_2, mode = "everything", positive = "T")



PredProb <- predict(model_RF_1, newdata = data_model_test, type = "prob")[,1]

PredClass10 <- as.factor(ifelse(PredProb >= 0.10, "T", "F"))
PredClass30 <- as.factor(ifelse(PredProb >= 0.30, "T", "F"))
PredClass40 <- as.factor(ifelse(PredProb >= 0.40, "T", "F"))
PredClass45 <- as.factor(ifelse(PredProb >= 0.45, "T", "F"))
PredClass55 <- as.factor(ifelse(PredProb >= 0.55, "T", "F"))
PredClass60 <- as.factor(ifelse(PredProb >= 0.6, "T", "F"))
PredClass65 <- as.factor(ifelse(PredProb >= 0.65, "T", "F"))

ROC <- roc(data_model_multivar_evaluate_birth_imputed$Target_2, PredProb)
             



ROC <- roc(data_model_multivar_evaluate_birth_imputed$Target_2, PredProb)

plot(coords(ROC, "best"))




hist(PredProb)


data_model_multivar_evaluate_birth_imputed$Target_2_binary <- ifelse(data_model_multivar_evaluate_birth_imputed$Target_2 == "T", 1, 0)

CaliPlot(PredProb, data_model_test$Target_2_binary, nbins = 5)

PresAbData <- data.frame(plotID = c(1:length(PredProb)), Observed = data_model_test$Target_2, Predicted1 = PredProb)
calibration.plot(PresAbData, which.model=1, N.bins = 10, na.rm = TRUE)
