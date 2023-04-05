#######MANIPULATE FEATURES####

data_combi_cat <- data_combi



data_combi_cat <- data_combi_cat[data_combi_cat$Target_Meyer != "U" & !is.na(data_combi_cat$Target_Meyer),]


data_combi_cat$Target_Meyer <- droplevels(data_combi_cat$Target_Meyer, "U")

data_combi_cat$propposavg_cat <- cut(data_combi_cat$propposavg, breaks = 4)
levels(data_combi_cat$propposavg_cat) <- c(levels(data_combi_cat$propposavg_cat), "Missing")
data_combi_cat$propposavg_cat[is.na(data_combi_cat$propposavg_cat)] <- "Missing"

data_combi_cat$meantitreavg_cat <- cut(data_combi_cat$meantitreavg, breaks = 4)
levels(data_combi_cat$meantitreavg_cat) <- c(levels(data_combi_cat$meantitreavg_cat), "Missing")
data_combi_cat$meantitreavg_cat[is.na(data_combi_cat$meantitreavg_cat)] <- "Missing"

data_combi_cat$meantitrenegcowsavg_cat <- cut(data_combi_cat$meantitrenegcowsavg, breaks = 4)
levels(data_combi_cat$meantitrenegcowsavg_cat) <- c(levels(data_combi_cat$meantitrenegcowsavg_cat), "Missing")
data_combi_cat$meantitrenegcowsavg_cat[is.na(data_combi_cat$meantitrenegcowsavg_cat)] <- "Missing"

data_combi_cat$damstatusbirth_cat <- as.factor(data_combi_cat$damstatusbirth)
levels(data_combi_cat$damstatusbirth_cat) <- c(levels(data_combi_cat$damstatusbirth_cat), "Missing", "Other")
data_combi_cat$damstatusbirth_cat[is.na(data_combi_cat$damstatusbirth_cat)] <- "Missing"
data_combi_cat$damstatusbirth_cat[data_combi_cat$damstatusbirth_cat != "1" & data_combi_cat$damstatusbirth_cat != "2" & data_combi_cat$damstatusbirth_cat != "3"] <- "Other"
data_combi_cat$damstatusbirth_cat <- droplevels(data_combi_cat$damstatusbirth_cat, c("4", "5", "6", "8"))


####EXPORT DATA FOR PYTHON####

data_combi_cat_birth <- data_combi_cat[,c(29:33,36:57,461:464,460)]

write.csv(data_combi_cat_birth, "y:/ian/johnesthresholds/johnesproper/data/python/BirthModelData.csv")

#####SPLIT TRAIN TEST#####

set.seed(1981)
trainRowNumbers <- createDataPartition(data_combi_cat$Target_4, p = 0.8, list = FALSE)
data_modelLR_train <- data_combi_cat[trainRowNumbers,]
data_modelLR_test <- data_combi_cat[-trainRowNumbers,]





dim(data_modelLR_train)
dim(data_modelLR_test)


#####TRAIN MODELS#####

model_LR_1 <- glm(Target_Meyer ~ propposavg_cat + 
                    meantitreavg_cat +
                    meantitrenegcowsavg_cat +
                    damstatusbirth_cat +
                    naunts +
                    ngreataunts +
                    nproximalcalves +
                    nvaguelyproximalcalves +
                    nproximaldams +
                    nvaguelyproximaldams +
                    johnestitre1,                                                     
                  data = data_modelLR_train,
                  family = "binomial")


data_modelML_train <- data_modelLR_train[,c(29:33, 36:57, 460:464, 459)]
data_modelML_test <- data_modelLR_test[,c(29:33, 36:57, 460:464, 459)]

data_modelML_train$Target_4Letter <- ifelse(data_modelML_train$Target_4 == 1, "P", "N")
data_modelML_test$Target_4Letter <- ifelse(data_modelML_test$Target_4 == 1, "P", "N")

data_modelML_train <- data_modelML_train[,-c(33)]
data_modelML_test <- data_modelML_test[,-c(33)]

####CREATE CUSTOM METRIC: BRIER SCORE######


brierscore <- function(data, levs, model){
  
rs <- vector()
  
  
  for(i in 1:nrow(data)){
    label <- ifelse(data$obs == "P",1,0)
    rs[i] <- (data$P[i] - label[i])^2
  }
  
  out <- -mean(rs)
  
  names(out) <- "BrierScore"
  
  return(out)
 
}




#####TRAIN MODELS#####

fitControl <- trainControl(method  = "repeatedcv", number = 5, repeats = 5, classProbs = TRUE)



model_RF_1 <- train(Target_4Letter ~ ., data = data_modelML_train, method = "ranger", trControl = fitControl)

model_glmnet_1 <- train(Target_4Letter ~ ., data = data_modelML_train, method = "glmnet", trControl = fitControl)

model_mars_1 <- train(Target_4Letter ~ ., data = data_modelML_train, method = "earth", trControl = fitControl)


#####CALIBRATION CURVE CARET####

numericclasses <- ifelse(data_modelML_train$Target_4Letter == "P", 1, 0)

CaliPlot(model_mars_1$finalModel$predictions[,2], as.factor(numericclasses), nbins = 10)




#####CROSS VALIDATION (FOR BASIC GLM ONLY)####

CVResults <- data.frame()


for (k in 1:5){
  set.seed(45)
  folds <- groupKFold(data_modelLR_train$Farm, k=5)
  for (j in 1:5) {
    predrun <- 0
    names(folds)[j] <- "train"
    names(folds)[j-1] <- paste0("Fold0",j-1)
    trainset <- data_modelLR_train[folds$train,]
    testset <- data_modelLR_train[-folds$train,]
    print(paste("Testing on Fold",j,"Repetition",k))
    model <- glm(Target_4 ~ 
                   propposavg_cat + 
                   meantitreavg_cat +
                   meantitrenegcowsavg_cat +
                   damstatusbirth_cat +
                   naunts +
                   ngreataunts +
                   nproximalcalves +
                   nvaguelyproximalcalves +
                   nproximaldams +
                   nvaguelyproximaldams +
                   johnestitre1,                                                     
                 data = trainset,
                 family = "binomial",
    )
    if(length(trainset$meantitreavg_cat[trainset$meantitreavg_cat == "(21,27.6]"]) != 0){
      testset$pred <- predict(model, newdata=testset, type="response", allow.new.levels = T)
      predrun <- 1}
    if(predrun == 1){foldresults <- data.frame(Predicted = testset$pred, Observed = testset$Target_4)
    foldresults$Fold <- j
    foldresults$Rep <- k
    CVResults <- rbind(CVResults, foldresults)}
    
  }
  
}


CaliPlot(CVResults$Predicted, CVResults$Observed, nbins = 10)


######PREDICT ON TEST SET#####


testpred <- predict(model_mars_1, newdata = data_modelML_test)

numericclasses <- ifelse(data_modelML_test$Target_4Letter == "P", 1, 0)

CaliPlot(testpred, as.factor(numericclasses), nbins = 4)

confusionMatrix(reference = as.factor(data_modelML_test$Target_4Letter), data = testpred, mode='everything', positive='P')

                 



