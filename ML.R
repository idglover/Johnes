####FIRST THREE TESTS####

#####SET PARAMETERS#####

nteststoinclude <- 3

#####CONSTRUCT DATA#####

data_posteriors <- read.csv("y:/ian/johnesthresholds/johnesproper/data/data_posteriors.csv")

ewdata <- data_posteriors

ewdata$testnum <- as.integer(substr(ewdata$covsandtitre, 13, nchar(ewdata$covsandtitre)))

ewdata <- ewdata[ewdata$testnum <= nteststoinclude,]
ewdata <- ewdata[ewdata$parity == 1,]



#ewdata$ageatfirstposstatus[ewdata$ageatfirstposstatus == 0] <- 100000

#ewdata <- ewdata[ewdata$age < ewdata$ageatfirstposstatus,]

ewdata1 <- ewdata[ewdata$testnum == 1,][,c(2:158,173,161:171,182,185 )]
colnames(ewdata1)[159:171] <- c("age1","parity1","dim1","meantitrenegcows1","yield1","butterfat1",
                                "protein1","lactose1","cellcount1","titre1","class1","likelihood1","PosteriorProb1")
ewdata2 <- ewdata[ewdata$testnum == 2,]

ewdata12 <- merge(ewdata1, ewdata2[,c(3,161:171,182,185)], by = "calfeartag", all.x = TRUE)

colnames(ewdata12)[c(172:184)] <- c("age2","parity2","dim2","meantitrenegcows2","yield2","butterfat2",
                                "protein2","lactose2","cellcount2","titre2","class2","likelihood2","PosteriorProb2")

ewdata3 <- ewdata[ewdata$testnum == 3,]

ewdata123 <- merge(ewdata12, ewdata3[,c(3,161:171,182,185)], by = "calfeartag", all.x = TRUE)

colnames(ewdata123)[c(185:197)] <- c("age3","parity3","dim3","meantitrenegcows3","yield3","butterfat3",
                                    "protein3","lactose3","cellcount3","titre3","class3","likelihood3","PosteriorProb3")


ewdata123 <- ewdata123[ewdata123$titre1 < 30 &
                         ewdata123$titre2 < 30 &
                         ewdata123$titre3 < 30,]

ewdata_ml <- ewdata123[ewdata123$Target_QMMS != "U",c(2,157:197)]

ewdata_ml_cc <- ewdata_ml[complete.cases(ewdata_ml) == TRUE,]

#####FEATURE ENGINEERING####

ewdata_ml_cc$meanage <- (ewdata_ml_cc$age1 + ewdata_ml_cc$age2 + ewdata_ml_cc$age3) / 3
ewdata_ml_cc$meandim <- (ewdata_ml_cc$dim1 + ewdata_ml_cc$dim1 + ewdata_ml_cc$dim1) / 3
ewdata_ml_cc$meanmtnc <- (ewdata_ml_cc$meantitrenegcows1 + ewdata_ml_cc$meantitrenegcows2 + ewdata_ml_cc$meantitrenegcows3) / 3
ewdata_ml_cc$meanyield <- (ewdata_ml_cc$yield1 + ewdata_ml_cc$yield2 + ewdata_ml_cc$yield3) / 3
ewdata_ml_cc$meanbutterfat <- (ewdata_ml_cc$butterfat1 + ewdata_ml_cc$butterfat2 + ewdata_ml_cc$butterfat3) / 3
ewdata_ml_cc$meanprotein <- (ewdata_ml_cc$protein1 + ewdata_ml_cc$protein2 + ewdata_ml_cc$protein3) / 3
ewdata_ml_cc$meancellcount <- (ewdata_ml_cc$cellcount1 + ewdata_ml_cc$cellcount2 + ewdata_ml_cc$cellcount3) / 3
ewdata_ml_cc$meantitre <- (ewdata_ml_cc$titre1 + ewdata_ml_cc$titre2 + ewdata_ml_cc$titre3) / 3
ewdata_ml_cc$meanlikelihood <- (ewdata_ml_cc$likelihood1 + ewdata_ml_cc$likelihood2 + ewdata_ml_cc$likelihood3) / 3
ewdata_ml_cc$meanposterior <- (ewdata_ml_cc$PosteriorProb1 + ewdata_ml_cc$PosteriorProb2 + ewdata_ml_cc$PosteriorProb3) / 3
ewdata_ml_cc$Target_QMMS <- as.factor(ewdata_ml_cc$Target_QMMS)
ewdata_ml_cc$Target_QMMS_alpha <- as.factor(ifelse(ewdata_ml_cc$Target_QMMS == "1", "Pos","Neg"))

write(ewdata_ml_cc, "y:/ian/johnesthresholds/johnesproper/data/EarlyWarning/ewdata_firstthree.csv", row.names = FALSE)

#####SPLIT TRAIN TEST########


uniquefarms <- unique(ewdata_ml_cc$Farm)

  
  no_cores <- detectCores() - 1  
  cl <- makePSOCKcluster(no_cores)
  registerDoParallel(cl)
  getDoParWorkers() 

nr <- foreach(i = 1:length(uniquefarms), .combine = "c") %dopar% {
  nrow(ewdata_ml_cc[ewdata_ml_cc$Farm == uniquefarms[i],])
}

stopCluster(cl)

uniquefarms <- data.frame(Farm = uniquefarms, n = nr)

#Drop farms with too few rows if necessary

uniquefarms <- uniquefarms[uniquefarms$n > 0,]

trainfarms <- sample(uniquefarms$Farm, round(0.8*nrow(uniquefarms)), replace = FALSE)


ewdata_ml_cc_train <- ewdata_ml_cc[ewdata_ml_cc$Farm %in% trainfarms,]
ewdata_ml_cc_test <- ewdata_ml_cc[!(ewdata_ml_cc$Farm %in% trainfarms),]


print(paste0("Training data: ", nrow(ewdata_ml_cc_train)," rows."))
print(paste0("Testing data: ", nrow(ewdata_ml_cc_test)," rows."))

print(paste0("Training data prevalence: ", round((nrow(ewdata_ml_cc_train[ewdata_ml_cc_train$Target_QMMS == "1",])/
                                                    nrow(ewdata_ml_cc_train[ewdata_ml_cc_train$Target_QMMS == "0",]))*100,1),
             "%"))






#####CHECK FOR NZV FEATURES#####



nzv <- nearZeroVar(ewdata_ml_cc_train, saveMetrics = T)
nzv[nzv$nzv,]




#####SET TUNING AND RESAMPLING PARAMETERS#####

folds <- 5
cvIndex <- createFolds(factor(ewdata_ml_cc_train$Farm), folds, returnTrain = T)

allstats  <- function(data, lev = NULL, model = NULL){
  a1 <- defaultSummary(data, lev, model)
  b1 <- twoClassSummary(data, lev, model)
  c1 <- prSummary(data, lev, model)
  out <- c(a1, b1, c1)
  out}

fitControl <- trainControl(index = cvIndex,
                           method = 'cv', 
                           number = folds,
                           classProbs = TRUE,
                           summaryFunction = allstats
                           )
  
ewdata_ml_cc_train$Target_QMMS_alpha <- relevel(ewdata_ml_cc_train$Target_QMMS_alpha, "Pos")
ewdata_ml_cc_test$Target_QMMS_alpha <- relevel(ewdata_ml_cc_test$Target_QMMS_alpha, "Pos")

#####TRAIN MODELS#####

no_cores <- detectCores() - 1  
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
getDoParWorkers() 

train.formula <- formula(Target_QMMS_alpha ~ priorprob_12mold + age1 + dim1 + meantitrenegcows1 + yield1 + butterfat1 + protein1 + cellcount1 + titre1 + likelihood1 +
                           age2 + dim2 + meantitrenegcows2 + yield2 + butterfat2 + protein2 + cellcount2 + titre2 + likelihood2 +
                           age3 + dim3 + meantitrenegcows3 + yield3 + butterfat3 + protein3 + cellcount3 + titre3 + likelihood3 + 
                           meanage + meandim + meanmtnc + meanyield + meanbutterfat + meanprotein + meancellcount + meantitre + meanlikelihood)

ew_model_rf <- train(train.formula, data = ewdata_ml_cc_train, trControl = fitControl, method = "ranger", importance = "permutation", metric = "ROC")
ew_model_knn <- train(train.formula, data = ewdata_ml_cc_train, trControl = fitControl, method = "knn", metric = "ROC")
ew_model_reglr <- train(train.formula, data = ewdata_ml_cc_train, trControl = fitControl, method = "regLogistic", importance = T, metric = "ROC")
ew_model_svmlin <- train(train.formula, data = ewdata_ml_cc_train, trControl = fitControl, method = "svmLinear", importance = T, metric = "ROC")
ew_model_svmrad <- train(train.formula, data = ewdata_ml_cc_train, trControl = fitControl, method = "svmRadial", importance = T, metric = "ROC")
ew_model_svmpoly <- train(train.formula, data = ewdata_ml_cc_train, trControl = fitControl, method = "svmPoly", importance = T, metric = "ROC")
ew_model_gbm <- train(train.formula, data = ewdata_ml_cc_train, trControl = fitControl, method = "gbm", metric = "ROC")
ew_model_nnet <- train(train.formula, data = ewdata_ml_cc_train, trControl = fitControl, method = "nnet", importance = T, metric = "ROC")
ew_model_mars <- train(train.formula, data = ewdata_ml_cc_train, trControl = fitControl, method = "earth", metric = "ROC")

stopCluster(cl)

models_compare <- resamples(list(RF = ew_model_rf,
                                 KNN = ew_model_knn, 
                                 RegLR = ew_model_reglr,
                                 SVMLin = ew_model_svmlin,
                                 SVMRad = ew_model_svmrad,
                                 SVMPoly = ew_model_svmpoly,
                                 GBM = ew_model_gbm,
                                 NNET = ew_model_nnet,
                                 MARS = ew_model_mars))
summary(models_compare)

bwplot(models_compare)


#Tune best model


no_cores <- detectCores() - 1  
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
getDoParWorkers() 


rfGrid <- expand.grid(mtry = c(2,5,10,20,30,40), splitrule = c("gini", "extratrees"), min.node.size = c(1,5,10,100))

ew_model_rf_grid <- train(train.formula, data = ewdata_ml_cc_train, trControl = fitControl, method = "ranger", tuneGrid = rfGrid, importance = "permutation", metric = "Sens")





marsGrid <- expand.grid(nprune = c(1,2,5,9,15,20), degree = c(1, 2, 5, 10))

ew_model_mars_grid <- train(train.formula, data = ewdata_ml_cc_train, trControl = fitControl, method = "earth", tuneGrid = marsGrid, metric = "Sens")


gbmGrid <-  expand.grid(interaction.depth = c(1, 2, 3), 
                        n.trees = c(50,100,150,200), 
                        shrinkage = c(0.1, 0.2, 0.5),
                        n.minobsinnode = c(10,20,30))

ew_model_gbm_grid <- train(train.formula, data = ewdata_ml_cc_train, trControl = fitControl, method = "gbm", tuneGrid = gbmGrid, metric = "Sens")



#svmlinGrid <-  expand.grid(C = c(0.1, 0.5, 1, 2))

#ew_model_svmlin_grid <- train(train.formula, data = ewdata_ml_cc_train, trControl = fitControl, method = "svmLinear", tuneGrid = svmlinGrid)

#svmpolyGrid <-  expand.grid(degree = c (1,2,3,4,5), scale = c(0.001, 0.01, 0.1, 0.5, 1), C = c(0.1,0.25,0.5,1,2))

#ew_model_svmpoly_grid <- train(train.formula, data = ewdata_ml_cc_train, trControl = fitControl, method = "svmPoly", tuneGrid = svmpolyGrid)

models_compare <- resamples(list(RFGrid = ew_model_rf_grid,
                                 #SVMLinGrid = ew_model_rf_grid,
                                 #SVMPolyGrid = ew_model_svmpoly_grid,
                                 GBMGrid = ew_model_gbm_grid,
                                 MARSGrid = ew_model_mars_grid))
summary(models_compare)

bwplot(models_compare)



probpreds_gbm <- predict(ew_model_gbm_grid, newdata = ewdata_ml_cc_test, type = "prob")[,1]
ewdata_ml_cc_test$probpreds_gbm <- probpreds_gbm
ewdata_ml_cc_test$classpreds_gbm <- as.factor(ifelse(ewdata_ml_cc_test$probpreds_gbm >= 0.0253428, "Pos", "Neg"))


confusionMatrix(ewdata_ml_cc_test$classpreds_gbm, ewdata_ml_cc_test$Target_QMMS_alpha, positive = "Pos")


cp <- cutpointr(ewdata_ml_cc_test, probpreds_gbm, Target_QMMS_alpha, 
                method = maximize_metric, metric = sum_sens_spec)


plot(cp)

CaliPlot(probpreds_rf, as.numeric(ewdata_ml_cc_test$Target_QMMS_alpha) -1)

####FIRST LACTATION####

#####CONSTRUCT DATA#####

ewdata <- read.csv("y:/ian/johnesthresholds/johnesproper/data/data_posteriors.csv")

ewdata <- ewdata[ewdata$parity == 1 &
                   ewdata$Target_QMMS != "U",]

ewdata$Target_QMMS_alpha <- as.factor(ifelse(ewdata$Target_QMMS == "1", "Pos", "Neg"))

ewdata_ml <- ewdata[,c(2,3,155,161:167, 169:170,173,182,186)]

ewdata_ml_cc <- ewdata_ml[complete.cases(ewdata_ml) == TRUE,]

write.csv(ewdata_ml_cc, "y:/ian/johnesthresholds/johnesproper/data/EarlyWarning/ewdata_parity1.csv", row.names = FALSE)

#####FEATURE ENGINEERING#####

uniquecalves <- unique(ewdata_ml_cc$calfeartag)


no_cores <- detectCores()
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
getDoParWorkers() 

ag <- foreach(i = uniquecalves, .combine = "rbind") %dopar% {
  Farm <- ewdata_ml_cc$Farm[ewdata_ml_cc$calfeartag == i][1]
  calfeartag <- i
  ageatfirstposstatus <- ewdata_ml_cc$ageatfirstposstatus[ewdata_ml_cc$calfeartag == i][1]
  meanage <- mean(ewdata_ml_cc$age[ewdata_ml_cc$calfeartag == i])
  maxage <- max(ewdata_ml_cc$age[ewdata_ml_cc$calfeartag == i])
  minage <- min(ewdata_ml_cc$age[ewdata_ml_cc$calfeartag == i])
  meanmtnc <- mean(ewdata_ml_cc$meantitrenegcows[ewdata_ml_cc$calfeartag == i])
  maxmtnc <- max(ewdata_ml_cc$meantitrenegcows[ewdata_ml_cc$calfeartag == i])
  minmtnc <- min(ewdata_ml_cc$meantitrenegcows[ewdata_ml_cc$calfeartag == i])
  meanyield <- mean(ewdata_ml_cc$yield[ewdata_ml_cc$calfeartag == i])
  maxyield <- max(ewdata_ml_cc$yield[ewdata_ml_cc$calfeartag == i])
  minyield <- min(ewdata_ml_cc$yield[ewdata_ml_cc$calfeartag == i])
  meanbutterfat <- mean(ewdata_ml_cc$butterfat[ewdata_ml_cc$calfeartag == i])
  maxbutterfat <- max(ewdata_ml_cc$butterfat[ewdata_ml_cc$calfeartag == i])
  minbutterfat <- min(ewdata_ml_cc$butterfat[ewdata_ml_cc$calfeartag == i])
  meanprotein <- mean(ewdata_ml_cc$protein[ewdata_ml_cc$calfeartag == i])
  maxprotein <- max(ewdata_ml_cc$protein[ewdata_ml_cc$calfeartag == i])
  minprotein <- min(ewdata_ml_cc$protein[ewdata_ml_cc$calfeartag == i])
  meancellcount <- mean(ewdata_ml_cc$cellcount[ewdata_ml_cc$calfeartag == i])
  maxcellcount <- max(ewdata_ml_cc$cellcount[ewdata_ml_cc$calfeartag == i])
  mincellcount <- min(ewdata_ml_cc$cellcount[ewdata_ml_cc$calfeartag == i])
  meantitre <- mean(ewdata_ml_cc$titre[ewdata_ml_cc$calfeartag == i])
  maxtitre <- max(ewdata_ml_cc$titre[ewdata_ml_cc$calfeartag == i])
  mintitre <- min(ewdata_ml_cc$titre[ewdata_ml_cc$calfeartag == i])
  meanlikelihood <- mean(ewdata_ml_cc$likelihood[ewdata_ml_cc$calfeartag == i])
  maxlikelihood <- max(ewdata_ml_cc$likelihood[ewdata_ml_cc$calfeartag == i])
  minlikelihood <- min(ewdata_ml_cc$likelihood[ewdata_ml_cc$calfeartag == i])
  prior <- ewdata_ml_cc$priorprob_12mold[ewdata_ml_cc$calfeartag == i][1]
  ntests <- nrow(ewdata_ml_cc[ewdata_ml_cc$calfeartag == i,])
  lastposterior <- ewdata$PosteriorProb[ewdata$calfeartag == i][length(ewdata$PosteriorProb[ewdata$calfeartag == i])]
  Target_QMMS <- as.factor(ewdata_ml_cc$Target_QMMS[ewdata_ml_cc$calfeartag == i][1])
  
  c(Farm,
    calfeartag,
    ageatfirstposstatus,
    meanage,
    maxage,
    minage,
    meanmtnc,
    maxmtnc,
    minmtnc,
    meanyield,
    maxyield,
    minyield,
    meanbutterfat,
    maxbutterfat,
    minbutterfat,
    meanprotein,
    maxprotein,
    minprotein,
    meancellcount,
    maxcellcount,
    mincellcount,
    meantitre,
    maxtitre,
    mintitre,
    meanlikelihood,
    maxlikelihood,
    minlikelihood,
    prior,
    ntests,
    lastposterior,
    Target_QMMS)
    
    
}


stopCluster(cl)

ag <- as.data.frame(ag[,c(1:31)])

colnames(ag) <- c("Farm",
                  "calfeartag",
                  "ageatfirstposstatus",
                  "meanage",
                  "maxage",
                  "minage",
                  "meanmtnc",
                  "maxmtnc",
                  "minmtnc",
                  "meanyield",
                  "maxyield",
                  "minyield",
                  "meanbutterfat",
                  "maxbutterfat",
                  "minbutterfat",
                  "meanprotein",
                  "maxprotein",
                  "minprotein",
                  "meancellcount",
                  "maxcellcount",
                  "mincellcount",
                  "meantitre",
                  "maxtitre",
                  "mintitre",
                  "meanlikelihood",
                  "maxlikelihood",
                  "minlikelihood",
                  "prior",
                  "ntests",
                  "lastposterior",
                  "Target_QMMS")

ag$Target_QMMS_alpha <- ifelse(ag$Target_QMMS == "2", "Pos", "Neg")






#####SPLIT TRAIN/TEST#####


uniquefarms <- unique(ag$Farm)


no_cores <- detectCores() 
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
getDoParWorkers() 

nr <- foreach(i = 1:length(uniquefarms), .combine = "c") %dopar% {
  nrow(ag[ag$Farm == uniquefarms[i],])
}

ag[,c(3:30)] <- lapply(ag[,c(3:30)], as.numeric)
ag[,c(31,32)] <- lapply(ag[,c(31,32)], as.factor)


stopCluster(cl)

uniquefarms <- data.frame(Farm = uniquefarms, n = nr)

#Drop farms with too few rows if necessary

uniquefarms <- uniquefarms[uniquefarms$n > 0,]

trainfarms <- sample(uniquefarms$Farm, round(0.8*nrow(uniquefarms)), replace = FALSE)


ag_train <- ag[ag$Farm %in% trainfarms,]
ag_test <- ag[!(ag$Farm %in% trainfarms),]


print(paste0("Training data: ", nrow(ag_train)," rows."))
print(paste0("Testing data: ", nrow(ag_test)," rows."))

print(paste0("Training data prevalence: ", round(nrow(ag_train[ag_train$Target_QMMS_alpha == "Pos",])/
                                                    (nrow(ag_train[ag_train$Target_QMMS_alpha == "Neg",]) +
                                                       nrow(ag_train[ag_train$Target_QMMS_alpha == "Pos",]))*100,1),
             "%"))

print(paste0("Testing data prevalence: ", round(nrow(ag_test[ag_test$Target_QMMS_alpha == "Pos",])/
                                                   (nrow(ag_test[ag_test$Target_QMMS_alpha == "Neg",]) +
                                                      nrow(ag_test[ag_test$Target_QMMS_alpha == "Pos",]))*100,1),
             "%"))


print(paste0("Training data % Pos animals turning pos in 1st lactation: ", 100 * round(length(unique(ag_train$calfeartag[ag_train$ageatfirstposstatus <= ag_train$maxage &
                ag_train$ageatfirstposstatus != 0])) /
  length(unique(ag_train$calfeartag[ag_train$Target_QMMS_alpha == "Pos"])),3),"%"))



#####CHECK FOR NZV FEATURES#####



nzv <- nearZeroVar(ag_train, saveMetrics = T)
nzv[nzv$nzv,]




#####SET TUNING AND RESAMPLING PARAMETERS#####

folds <- 5
cvIndex <- createFolds(factor(ag_train$Farm), folds, returnTrain = T)

allstats  <- function(data, lev = NULL, model = NULL){
  a1 <- defaultSummary(data, lev, model)
  b1 <- twoClassSummary(data, lev, model)
  c1 <- prSummary(data, lev, model)
  out <- c(a1, b1, c1)
  out}

brierscore <- function(data, levs, model){
  
  rs <- vector()
  
  
  for(i in 1:nrow(data)){
    label <- ifelse(data$obs == "Pos",1,0)
    rs[i] <- (data$P[i] - label[i])^2
  }
  
  out <- -mean(rs)
  
  names(out) <- "BrierScore"
  
  return(out)
  
}

fitControl <- trainControl(index = cvIndex,
                           method = 'cv', 
                           number = folds,
                           classProbs = TRUE,
                           summaryFunction = brierscore
)

ag_train$Target_QMMS_alpha <- as.factor(relevel(as.factor(ag_train$Target_QMMS_alpha), "Pos"))
ag_test$Target_QMMS_alpha <- as.factor(relevel(as.factor(ag_test$Target_QMMS_alpha), "Pos"))




train.formula <- formula(Target_QMMS_alpha ~ 
                         meanage+
                         maxage+
                         minage+
                         meanmtnc+
                         maxmtnc+
                         minmtnc+
                         meanyield+
                         maxyield+
                         minyield+
                         meanbutterfat+
                         maxbutterfat+
                         minbutterfat+
                         meanprotein+
                         maxprotein+
                         minprotein+
                         meancellcount+
                         maxcellcount+
                         mincellcount+
                         meantitre+
                         maxtitre+
                         mintitre+
                         prior+
                         ntests)

#Modified formula

train.formula <- formula(Target_QMMS_alpha ~ 
                           meantitre +
                         mintitre +
                         ntests +
                         maxbutterfat +
                         prior +
                         maxyield +
                         minprotein +
                         maxtitre +
                         meanprotein +
                         mincellcount
                         
                         
                           )

  
  
  
no_cores <- detectCores()  
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
getDoParWorkers() 


ew_model_rf <- train(train.formula, data = ag_train, trControl = fitControl, method = "ranger", importance = "permutation", metric = "BrierScore")
ew_model_knn <- train(train.formula, data = ag_train, trControl = fitControl, method = "knn", metric = "BrierScore")
ew_model_reglr <- train(train.formula, data = ag_train, trControl = fitControl, method = "regLogistic", importance = T, metric = "BrierScore")
ew_model_svmlin <- train(train.formula, data = ag_train, trControl = fitControl, method = "svmLinear", importance = T, metric = "BrierScore")
ew_model_svmrad <- train(train.formula, data = ag_train, trControl = fitControl, method = "svmRadial", importance = T, metric = "BrierScore")
ew_model_svmpoly <- train(train.formula, data = ag_train, trControl = fitControl, method = "svmPoly", importance = T, metric = "BrierScore")
ew_model_gbm <- train(train.formula, data = ag_train, trControl = fitControl, method = "gbm", metric = "BrierScore")
ew_model_nnet <- train(train.formula, data = ag_train, trControl = fitControl, method = "nnet", importance = T, metric = "BrierScore")
ew_model_mars <- train(train.formula, data = ag_train, trControl = fitControl, method = "earth", metric = "BrierScore")

stopCluster(cl)

models_compare <- resamples(list(RF = ew_model_rf,
                                 KNN = ew_model_knn, 
                                 RegLR = ew_model_reglr,
                                 SVMLin = ew_model_svmlin,
                                 SVMRad = ew_model_svmrad,
                                 SVMPoly = ew_model_svmpoly,
                                 GBM = ew_model_gbm,
                                 NNET = ew_model_nnet,
                                 MARS = ew_model_mars))
summary(models_compare)

bwplot(models_compare)


probpreds_gbm <- predict(ew_model_gbm, newdata = ag_test, type = "prob")[,1]
ag_test$probpreds_gbm <- probpreds_gbm
ag_test$classpreds_gbm <- as.factor(ifelse(ag_test$probpreds_gbm >= 0.232656      , "Pos", "Neg"))

probpreds_rf <- predict(ew_model_rf, newdata = ag_test, type = "prob")[,1]
ag_test$probpreds_rf <- probpreds_rf
ag_test$classpreds_rf <- as.factor(ifelse(ag_test$probpreds_rf >= 0.286       , "Pos", "Neg"))

probpreds_mars <- predict(ew_model_mars, newdata = ag_test, type = "prob")[,1]
ag_test$probpreds_mars <- probpreds_mars
ag_test$classpreds_mars <- as.factor(ifelse(ag_test$probpreds_mars >= 0.226818     , "Pos", "Neg"))

probpreds_nnet <- predict(ew_model_nnet, newdata = ag_test, type = "prob")[,1]
ag_test$probpreds_nnet <- probpreds_nnet
ag_test$classpreds_nnet <- as.factor(ifelse(ag_test$probpreds_nnet >= 0.5     , "Pos", "Neg"))

probpreds_svmpoly <- predict(ew_model_svmpoly, newdata = ag_test, type = "prob")[,1]
ag_test$probpreds_svmpoly <- probpreds_svmpoly
ag_test$classpreds_svmpoly <- as.factor(ifelse(ag_test$probpreds_svmpoly >= 0.198156      , "Pos", "Neg"))

probpreds_svmlin <- predict(ew_model_svmlin, newdata = ag_test, type = "prob")[,1]
ag_test$probpreds_svmlin <- probpreds_svmlin
ag_test$classpreds_svmlin <- as.factor(ifelse(ag_test$probpreds_svmlin >= 0.265068       , "Pos", "Neg"))


probpreds_reglr <- predict(ew_model_reglr, newdata = ag_test, type = "prob")[,1]
ag_test$probpreds_reglr <- probpreds_reglr
ag_test$classpreds_reglr <- as.factor(ifelse(ag_test$probpreds_reglr >= 0.257614      , "Pos", "Neg"))



confusionMatrix(ag_test$classpreds_gbm, ag_test$Target_QMMS_alpha, positive = "Pos")
confusionMatrix(ag_test$classpreds_rf, ag_test$Target_QMMS_alpha, positive = "Pos")
confusionMatrix(ag_test$classpreds_mars, ag_test$Target_QMMS_alpha, positive = "Pos")
confusionMatrix(ag_test$classpreds_nnet, ag_test$Target_QMMS_alpha, positive = "Pos")
confusionMatrix(ag_test$classpreds_reglr, ag_test$Target_QMMS_alpha, positive = "Pos")
confusionMatrix(ag_test$classpreds_svmpoly, ag_test$Target_QMMS_alpha, positive = "Pos")
confusionMatrix(ag_test$classpreds_svmlin, ag_test$Target_QMMS_alpha, positive = "Pos")

cp <- cutpointr(ag_test, probpreds_svmlin, Target_QMMS_alpha, 
                method = maximize_metric, metric = sum_sens_spec)


plot(cp)

CaliPlot(ag_test$probpreds_gbm, as.numeric(ag_test$Target_QMMS) -1, nbins = 10, ptitle = "GBM BrierScore")
CaliPlot(ag_test$probpreds_rf, as.numeric(ag_test$Target_QMMS) -1, nbins = 10, ptitle = "RF BrierScore, Top Ten Features")
CaliPlot(ag_test$probpreds_mars, as.numeric(ag_test$Target_QMMS) -1, nbins = 10, ptitle = "MARS BrierScore")
CaliPlot(ag_test$probpreds_nnet, as.numeric(ag_test$Target_QMMS) -1, nbins = 10, ptitle = "NNET Brier Score All Features")
CaliPlot(ag_test$probpreds_svmpoly, as.numeric(ag_test$Target_QMMS) -1, nbins = 10, ptitle = "SVMPoly Brier Score")
CaliPlot(ag_test$probpreds_svmlin, as.numeric(ag_test$Target_QMMS) -1, nbins = 10, ptitle = "SVMLinear Brier Score")
CaliPlot(ag_test$probpreds_reglr, as.numeric(ag_test$Target_QMMS) -1, nbins = 10, ptitle = "RegLR Brier Score")
#Tune best model


no_cores <- detectCores()
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
getDoParWorkers() 


rfGrid <- expand.grid(mtry = seq(1,7,2), splitrule = c("gini", "extratrees"), min.node.size = c(10,50,100, 150, 200))

ew_model_rf_grid <- train(train.formula, data = ag_train, trControl = fitControl, method = "ranger", tuneGrid = rfGrid, importance = "permutation", metric = "BrierScore")





#marsGrid <- expand.grid(nprune = c(1,2,5,9,15,20), degree = c(1, 2, 5, 10))

#ew_model_mars_grid <- train(train.formula, data = ewdata_ml_cc_train, trControl = fitControl, method = "earth", tuneGrid = marsGrid, metric = "Sens")


gbmGrid <-  expand.grid(interaction.depth = c(2, 3, 4, 5, 7), 
                        n.trees = c(20,50,100,150,200,250), 
                        shrinkage = c(0.1, 0.2, 0.5),
                        n.minobsinnode = c(10,20,30))

ew_model_gbm_grid <- train(train.formula, data = ag_train, trControl = fitControl, method = "gbm", tuneGrid = gbmGrid, metric = "BrierScore")



#svmlinGrid <-  expand.grid(C = c(0.1, 0.5, 1, 2))

#ew_model_svmlin_grid <- train(train.formula, data = ewdata_ml_cc_train, trControl = fitControl, method = "svmLinear", tuneGrid = svmlinGrid)

#svmpolyGrid <-  expand.grid(degree = c (1,2,3,4,5), scale = c(0.001, 0.01, 0.1, 0.5, 1), C = c(0.1,0.25,0.5,1,2))

#ew_model_svmpoly_grid <- train(train.formula, data = ewdata_ml_cc_train, trControl = fitControl, method = "svmPoly", tuneGrid = svmpolyGrid)

models_compare <- resamples(list(RFGrid = ew_model_rf_grid,
                                 #SVMLinGrid = ew_model_rf_grid,
                                 #SVMPolyGrid = ew_model_svmpoly_grid,
                                 GBMGrid = ew_model_gbm_grid
                                 #MARSGrid = ew_model_mars_grid
                                 ))
summary(models_compare)

bwplot(models_compare)



probpreds_gbm_tuned <- predict(ew_model_gbm_grid, newdata = ag_test, type = "prob")[,1]
ag_test$probpreds_gbm_tuned <- probpreds_gbm_tuned
ag_test$classpreds_gbm_tuned <- as.factor(ifelse(ag_test$probpreds_gbm_tuned >= 0.293118   , "Pos", "Neg"))

probpreds_rf_tuned <- predict(ew_model_rf_grid, newdata = ag_test, type = "prob")[,1]
ag_test$probpreds_rf_tuned <- probpreds_rf_tuned
ag_test$classpreds_rf_tuned <- as.factor(ifelse(ag_test$probpreds_rf_tuned >= 0.5    , "Pos", "Neg"))


confusionMatrix(ag_test$classpreds_gbm_tuned, ag_test$Target_QMMS_alpha, positive = "Pos")
confusionMatrix(ag_test$classpreds_rf_tuned, ag_test$Target_QMMS_alpha, positive = "Pos")

cp <- cutpointr(ag_test, probpreds_gbm_tuned, Target_QMMS_alpha, 
                method = maximize_metric, metric = sum_sens_spec)

cp <- cutpointr(ag_test, probpreds_rf_tuned, Target_QMMS_alpha, 
                method = maximize_metric, metric = sum_sens_spec)

plot(cp)

CaliPlot(ag_test$probpreds_gbm_tuned, as.numeric(ag_test$Target_QMMS) -1, ptitle = "GBM BrierScore Tuned")
CaliPlot(ag_test$probpreds_rf_tuned, as.numeric(ag_test$Target_QMMS) -1, ptitle = "RF BrierScore Tuned, Top Ten Features")


#####PICKLE CHOSEN MODEL#####

saveRDS(ew_model_rf, "y:/ian/johnesthresholds/johnesproper/data/PickledModels/ew_model_rf_topten.rds")

saveRDS(ew_model_gbm, "Y:/Ian/JohnesThresholds/JohnesProper/Data/PickledModels/ew_model_gbm.rds")

#####Examine High Probability Predictions#####

gbmhighprobanimals <- ag_test[ag_test$probpreds_gbm > 0.5,]

length(unique(gbmhighprobanimals$calfeartag))

group.colours <- c("L" = "green", "M" = "orange", "H" = "red")

for (cow in gbmhighprobanimals$calfeartag){
  print(ggplot(ewdata[ewdata$calfeartag == cow,], aes(x = age)) +
          geom_point(aes(y = titre, color = class)) +
          geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
          geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
          #geom_text(aes(y = -4, label = ifelse(POFloorApplied == 1,"POF","")), size = 2, angle = -90 ) +
          geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
          geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
          geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
          scale_color_manual(values = group.colours) +
          geom_line(aes(y = PosteriorProb*100)) +
          geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
          geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
          scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
          labs(title = cow, subtitle = paste0("ML Probability End Lac 1: ", round(ag_test$probpreds_gbm[ag_test$calfeartag == cow],1))))
  ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/EarlyWarning/HighLac1Prob",cow,".png"))
    
}


#####Plot random selection of animals#####

for(cow in sample(unique(ag_test$calfeartag), 100, replace = FALSE)){
  print(ggplot(ewdata[ewdata$calfeartag == cow,], aes(x = age)) +
          geom_point(aes(y = titre, color = class)) +
          geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
          geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
          #geom_text(aes(y = -4, label = ifelse(POFloorApplied == 1,"POF","")), size = 2, angle = -90 ) +
          geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
          geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
          geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
          scale_color_manual(values = group.colours) +
          geom_line(aes(y = PosteriorProb*100)) +
          geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
          geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
          scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
          labs(title = cow, subtitle = paste0("RF Probability End Lac 1: ", round(ag_test$probpreds_rf[ag_test$calfeartag == cow],2))))
  ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/EarlyWarning/RandomLac1Prob",cow,".png"))
  
}


#####Scatter Plot of ML prediction vs Bayesian Posterior#####


ggplot() +
  geom_point(data = ag_test, aes(x = lastposterior, y = probpreds_rf)) +
  geom_line(aes(x = c(0,1), y = c(0,1)), linetype = "dashed") +
  labs(x = "Last Posterior", y = "ML Probability")
  


ag_test$deviance <- ag_test$probpreds_rf - ag_test$lastposterior

ggplot(ag_test, aes(x = as.factor(ntests), y = deviance)) +
  geom_boxplot() +
   labs(x = "No Tests", y = "Difference Between ML Prob and Bayes Posterior")


corrplot(cor(ag_test[,c(3:28, 34)]), type = "upper", order = "hclust", col=brewer.pal(n = 8, name = "RdYlBu"))


####UP TO SECOND LACTATION####

#####CONSTRUCT DATA#####

ewdata <- read.csv("y:/ian/johnesthresholds/johnesproper/data/data_posteriors.csv")

ewdata <- ewdata[(ewdata$parity == 1 |
                    ewdata$parity == 2) &
                   ewdata$Target_QMMS != "U",]

ewdata$Target_QMMS_alpha <- as.factor(ifelse(ewdata$Target_QMMS == "1", "Pos", "Neg"))

ewdata_ml <- ewdata[,c(2,3,161:167, 169:170,173,182,186)]

ewdata_ml_cc <- ewdata_ml[complete.cases(ewdata_ml) == TRUE,]

write.csv(ewdata_ml_cc, "y:/ian/johnesthresholds/johnesproper/data/EarlyWarning/ewdata_parity1and2.csv", row.names = FALSE)

#####FEATURE ENGINEERING#####

uniquecalves <- unique(ewdata_ml_cc$calfeartag)


no_cores <- detectCores()
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
getDoParWorkers() 

ag <- foreach(i = uniquecalves, .combine = "rbind") %dopar% {
  Farm <- ewdata_ml_cc$Farm[ewdata_ml_cc$calfeartag == i][1]
  calfeartag <- i
  meanage <- mean(ewdata_ml_cc$age[ewdata_ml_cc$calfeartag == i])
  maxage <- max(ewdata_ml_cc$age[ewdata_ml_cc$calfeartag == i])
  minage <- min(ewdata_ml_cc$age[ewdata_ml_cc$calfeartag == i])
  meanmtnc <- mean(ewdata_ml_cc$meantitrenegcows[ewdata_ml_cc$calfeartag == i])
  maxmtnc <- max(ewdata_ml_cc$meantitrenegcows[ewdata_ml_cc$calfeartag == i])
  minmtnc <- min(ewdata_ml_cc$meantitrenegcows[ewdata_ml_cc$calfeartag == i])
  meanyield <- mean(ewdata_ml_cc$yield[ewdata_ml_cc$calfeartag == i])
  maxyield <- max(ewdata_ml_cc$yield[ewdata_ml_cc$calfeartag == i])
  minyield <- min(ewdata_ml_cc$yield[ewdata_ml_cc$calfeartag == i])
  meanbutterfat <- mean(ewdata_ml_cc$butterfat[ewdata_ml_cc$calfeartag == i])
  maxbutterfat <- max(ewdata_ml_cc$butterfat[ewdata_ml_cc$calfeartag == i])
  minbutterfat <- min(ewdata_ml_cc$butterfat[ewdata_ml_cc$calfeartag == i])
  meanprotein <- mean(ewdata_ml_cc$protein[ewdata_ml_cc$calfeartag == i])
  maxprotein <- max(ewdata_ml_cc$protein[ewdata_ml_cc$calfeartag == i])
  minprotein <- min(ewdata_ml_cc$protein[ewdata_ml_cc$calfeartag == i])
  meancellcount <- mean(ewdata_ml_cc$cellcount[ewdata_ml_cc$calfeartag == i])
  maxcellcount <- max(ewdata_ml_cc$cellcount[ewdata_ml_cc$calfeartag == i])
  mincellcount <- min(ewdata_ml_cc$cellcount[ewdata_ml_cc$calfeartag == i])
  meantitre <- mean(ewdata_ml_cc$titre[ewdata_ml_cc$calfeartag == i])
  maxtitre <- max(ewdata_ml_cc$titre[ewdata_ml_cc$calfeartag == i])
  mintitre <- min(ewdata_ml_cc$titre[ewdata_ml_cc$calfeartag == i])
  meanlikelihood <- mean(ewdata_ml_cc$likelihood[ewdata_ml_cc$calfeartag == i])
  maxlikelihood <- max(ewdata_ml_cc$likelihood[ewdata_ml_cc$calfeartag == i])
  minlikelihood <- min(ewdata_ml_cc$likelihood[ewdata_ml_cc$calfeartag == i])
  prior <- ewdata_ml_cc$priorprob_12mold[ewdata_ml_cc$calfeartag == i][1]
  ntests <- nrow(ewdata_ml_cc[ewdata_ml_cc$calfeartag == i,])
  lastposterior <- ewdata$PosteriorProb[ewdata$calfeartag == i][length(ewdata$PosteriorProb[ewdata$calfeartag == i])]
  Target_QMMS <- as.factor(ewdata_ml_cc$Target_QMMS[ewdata_ml_cc$calfeartag == i][1])
  
  c(Farm,
    calfeartag,
    meanage,
    maxage,
    minage,
    meanmtnc,
    maxmtnc,
    minmtnc,
    meanyield,
    maxyield,
    minyield,
    meanbutterfat,
    maxbutterfat,
    minbutterfat,
    meanprotein,
    maxprotein,
    minprotein,
    meancellcount,
    maxcellcount,
    mincellcount,
    meantitre,
    maxtitre,
    mintitre,
    meanlikelihood,
    maxlikelihood,
    minlikelihood,
    prior,
    ntests,
    lastposterior,
    Target_QMMS)
  
  
}


stopCluster(cl)

ag <- as.data.frame(ag[,c(1:30)])

colnames(ag) <- c("Farm",
                  "calfeartag",
                  "meanage",
                  "maxage",
                  "minage",
                  "meanmtnc",
                  "maxmtnc",
                  "minmtnc",
                  "meanyield",
                  "maxyield",
                  "minyield",
                  "meanbutterfat",
                  "maxbutterfat",
                  "minbutterfat",
                  "meanprotein",
                  "maxprotein",
                  "minprotein",
                  "meancellcount",
                  "maxcellcount",
                  "mincellcount",
                  "meantitre",
                  "maxtitre",
                  "mintitre",
                  "meanlikelihood",
                  "maxlikelihood",
                  "minlikelihood",
                  "prior",
                  "ntests",
                  "lastposterior",
                  "Target_QMMS")

ag$Target_QMMS_alpha <- ifelse(ag$Target_QMMS == "2", "Pos", "Neg")



#####SPLIT TRAIN/TEST#####


uniquefarms <- unique(ag$Farm)


no_cores <- detectCores() 
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
getDoParWorkers() 

nr <- foreach(i = 1:length(uniquefarms), .combine = "c") %dopar% {
  nrow(ag[ag$Farm == uniquefarms[i],])
}

ag[,c(3:29)] <- lapply(ag[,c(3:29)], as.numeric)
ag[,c(30,31)] <- lapply(ag[,c(30,31)], as.factor)


stopCluster(cl)

uniquefarms <- data.frame(Farm = uniquefarms, n = nr)

#Drop farms with too few rows if necessary

uniquefarms <- uniquefarms[uniquefarms$n > 0,]

trainfarms <- sample(uniquefarms$Farm, round(0.8*nrow(uniquefarms)), replace = FALSE)


ag_train <- ag[ag$Farm %in% trainfarms,]
ag_test <- ag[!(ag$Farm %in% trainfarms),]


print(paste0("Training data: ", nrow(ag_train)," rows."))
print(paste0("Testing data: ", nrow(ag_test)," rows."))

print(paste0("Training data prevalence: ", round(nrow(ag_train[ag_train$Target_QMMS_alpha == "Pos",])/
                                                   (nrow(ag_train[ag_train$Target_QMMS_alpha == "Neg",]) +
                                                      nrow(ag_train[ag_train$Target_QMMS_alpha == "Pos",]))*100,1),
             "%"))

print(paste0("Testing data prevalence: ", round(nrow(ag_test[ag_test$Target_QMMS_alpha == "Pos",])/
                                                  (nrow(ag_test[ag_test$Target_QMMS_alpha == "Neg",]) +
                                                     nrow(ag_test[ag_test$Target_QMMS_alpha == "Pos",]))*100,1),
             "%"))

#####CHECK FOR NZV FEATURES#####



nzv <- nearZeroVar(ag_train, saveMetrics = T)
nzv[nzv$nzv,]

#####SET TUNING AND RESAMPLING PARAMETERS#####

folds <- 5
cvIndex <- createFolds(factor(ag_train$Farm), folds, returnTrain = T)

allstats  <- function(data, lev = NULL, model = NULL){
  a1 <- defaultSummary(data, lev, model)
  b1 <- twoClassSummary(data, lev, model)
  c1 <- prSummary(data, lev, model)
  out <- c(a1, b1, c1)
  out}

brierscore <- function(data, levs, model){
  
  rs <- vector()
  
  
  for(i in 1:nrow(data)){
    label <- ifelse(data$obs == "Pos",1,0)
    rs[i] <- (data$P[i] - label[i])^2
  }
  
  out <- -mean(rs)
  
  names(out) <- "BrierScore"
  
  return(out)
  
}

fitControl <- trainControl(index = cvIndex,
                           method = 'cv', 
                           number = folds,
                           classProbs = TRUE,
                           summaryFunction = brierscore
)

ag_train$Target_QMMS_alpha <- as.factor(relevel(as.factor(ag_train$Target_QMMS_alpha), "Pos"))
ag_test$Target_QMMS_alpha <- as.factor(relevel(as.factor(ag_test$Target_QMMS_alpha), "Pos"))



#####TRAIN MODELS#####


train.formula <- formula(Target_QMMS_alpha ~ 
                           meanage+
                           maxage+
                           minage+
                           meanmtnc+
                           maxmtnc+
                           minmtnc+
                           meanyield+
                           maxyield+
                           minyield+
                           meanbutterfat+
                           maxbutterfat+
                           minbutterfat+
                           meanprotein+
                           maxprotein+
                           minprotein+
                           meancellcount+
                           maxcellcount+
                           mincellcount+
                           meantitre+
                           maxtitre+
                           mintitre+
                           prior+
                           ntests)

#Modified formula

train.formula <- formula(Target_QMMS_alpha ~ 
                           meantitre +  
                           maxtitre +       
                           mintitre +       
                           minmtnc +        
                           ntests +         
                           minage +         
                           meanyield +      
                           maxyield +        
                           meanmtnc +        
                           meanage              
)




no_cores <- detectCores()  
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
getDoParWorkers() 


ew_model_rf <- train(train.formula, data = ag_train, trControl = fitControl, method = "ranger", importance = "permutation", metric = "BrierScore")
ew_model_knn <- train(train.formula, data = ag_train, trControl = fitControl, method = "knn", metric = "BrierScore")
ew_model_reglr <- train(train.formula, data = ag_train, trControl = fitControl, method = "regLogistic", importance = T, metric = "BrierScore")
ew_model_svmlin <- train(train.formula, data = ag_train, trControl = fitControl, method = "svmLinear", importance = T, metric = "BrierScore")
ew_model_svmrad <- train(train.formula, data = ag_train, trControl = fitControl, method = "svmRadial", importance = T, metric = "BrierScore")
ew_model_svmpoly <- train(train.formula, data = ag_train, trControl = fitControl, method = "svmPoly", importance = T, metric = "BrierScore")
ew_model_gbm <- train(train.formula, data = ag_train, trControl = fitControl, method = "gbm", metric = "BrierScore")
ew_model_nnet <- train(train.formula, data = ag_train, trControl = fitControl, method = "nnet", importance = T, metric = "BrierScore")
ew_model_mars <- train(train.formula, data = ag_train, trControl = fitControl, method = "earth", metric = "BrierScore")

stopCluster(cl)

models_compare <- resamples(list(RF = ew_model_rf,
                                 KNN = ew_model_knn, 
                                 RegLR = ew_model_reglr,
                                 SVMLin = ew_model_svmlin,
                                 SVMRad = ew_model_svmrad,
                                 SVMPoly = ew_model_svmpoly,
                                 GBM = ew_model_gbm,
                                 NNET = ew_model_nnet,
                                 MARS = ew_model_mars))
summary(models_compare)

bwplot(models_compare)


probpreds_gbm <- predict(ew_model_gbm, newdata = ag_test, type = "prob")[,1]
ag_test$probpreds_gbm <- probpreds_gbm
ag_test$classpreds_gbm <- as.factor(ifelse(ag_test$probpreds_gbm >= 0.5     , "Pos", "Neg"))

probpreds_reglr <- predict(ew_model_reglr, newdata = ag_test, type = "prob")[,1]
ag_test$probpreds_reglr <- probpreds_reglr
ag_test$classpreds_reglr <- as.factor(ifelse(ag_test$probpreds_reglr >= 0.5     , "Pos", "Neg"))

probpreds_rf <- predict(ew_model_rf, newdata = ag_test, type = "prob")[,1]
ag_test$probpreds_rf <- probpreds_rf
ag_test$classpreds_rf <- as.factor(ifelse(ag_test$probpreds_rf >= 0.5     , "Pos", "Neg"))


CaliPlot(ag_test$probpreds_reglr, as.numeric(ag_test$Target_QMMS) -1, ptitle = "RegLR")
CaliPlot(ag_test$probpreds_gbm, as.numeric(ag_test$Target_QMMS) -1, ptitle = "GBM")
CaliPlot(ag_test$probpreds_rf, as.numeric(ag_test$Target_QMMS) -1, ptitle = "RandomForest")


#Tune best model


no_cores <- detectCores()
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
getDoParWorkers() 

gbmGrid <-  expand.grid(interaction.depth = c(2, 3, 4, 5, 7), 
                        n.trees = c(100,150,200,250), 
                        shrinkage = c(0.1, 0.2, 0.5),
                        n.minobsinnode = c(10,20,30))

ew_model_gbm_grid <- train(train.formula, data = ag_train, trControl = fitControl, method = "gbm", tuneGrid = gbmGrid, metric = "ROC")

rfGrid <- expand.grid(mtry = seq(1,7,2), splitrule = c("gini", "extratrees"), min.node.size = c(10,50,100, 150, 200))

ew_model_rf_grid <- train(train.formula, data = ag_train, trControl = fitControl, method = "ranger", tuneGrid = rfGrid, importance = "permutation", metric = "BrierScore")



probpreds_gbm_tuned <- predict(ew_model_gbm_grid, newdata = ag_test, type = "prob")[,1]
ag_test$probpreds_gbm_tuned <- probpreds_gbm_tuned
ag_test$classpreds_gbm_tuned <- as.factor(ifelse(ag_test$probpreds_gbm_tuned >= 0.277  , "Pos", "Neg"))

probpreds_rf_tuned <- predict(ew_model_rf_grid, newdata = ag_test, type = "prob")[,1]
ag_test$probpreds_rf_tuned <- probpreds_rf_tuned
ag_test$classpreds_rf_tuned <- as.factor(ifelse(ag_test$probpreds_rf_tuned >= 0.256  , "Pos", "Neg"))


confusionMatrix(ag_test$classpreds_rf_tuned, ag_test$Target_QMMS_alpha, positive = "Pos")

cp <- cutpointr(ag_test, probpreds_rf_tuned, Target_QMMS_alpha, 
                method = maximize_metric, metric = sum_sens_spec)

CaliPlot(ag_test$probpreds_rf_tuned, as.numeric(ag_test$Target_QMMS) -1, ptitle = "RF TUNED")

#####Examine High Probability Animals#####

gbmhighprobanimals <- ag_test[ag_test$probpreds_gbm > 0.5,]
rfhighprobanimals <- ag_test[ag_test$probpreds_rf > 0.5,]

length(unique(rfhighprobanimals$calfeartag))

group.colours <- c("L" = "green", "M" = "orange", "H" = "red")

for (cow in rfhighprobanimals$calfeartag[1:40]){
  print(ggplot(ewdata[ewdata$calfeartag == cow,], aes(x = age)) +
          geom_point(aes(y = titre, color = class)) +
          geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
          geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
          #geom_text(aes(y = -4, label = ifelse(POFloorApplied == 1,"POF","")), size = 2, angle = -90 ) +
          geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
          geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
          geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
          scale_color_manual(values = group.colours) +
          geom_line(aes(y = PosteriorProb*100)) +
          geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
          geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
          scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
          labs(title = cow, subtitle = paste0("ML Probability End Lac 2: ", round(ag_test$probpreds_rf[ag_test$calfeartag == cow],1))))
  ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/EarlyWarning/HighLac2Prob",cow,".png"))
  
}

#####Scatter Plot of ML prediction vs Bayesian Posterior#####


ggplot() +
  geom_point(data = ag_test, aes(x = lastposterior, y = probpreds_gbm)) +
  geom_line(aes(x = c(0,1), y = c(0,1)), linetype = "dashed") +
  labs(x = "Last Posterior", y = "ML Probability")



ag_test$deviance <- ag_test$probpreds_gbm - ag_test$lastposterior

ggplot(ag_test, aes(x = as.factor(ntests), y = deviance)) +
  geom_boxplot() +
  labs(x = "No Tests", y = "Difference Between ML Prob and Bayes Posterior")


corrplot(cor(ag_test[,c(3:28, 34)]), type = "upper", order = "hclust", col=brewer.pal(n = 8, name = "RdYlBu"))


####LIFETIME DATA PRIOR TO FIRST POSITIVE TEST####

#####CONSTRUCT DATA#####

ewdata <- read.csv("y:/ian/johnesthresholds/johnesproper/data/data_posteriors.csv")

ewdata <- ewdata[ewdata$Target_QMMS != "U",]

ewdata$Target_QMMS_alpha <- as.factor(ifelse(ewdata$Target_QMMS == "1", "Pos", "Neg"))

ewdata <- ewdata[ewdata$Target_QMMS_alpha == "Neg" |
                   (ewdata$Target_QMMS_alpha == "Pos" &
                      ewdata$age < ewdata$ageatfirstH),] # Limit to data prior to first ever positive titre

ewdata_ml <- ewdata[,c(2,3,161:167, 169:170,173,182,186)] #Cull dataset

no_cores <- detectCores()
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
getDoParWorkers() 

temp <- foreach(i = 1:nrow(ewdata_ml), .combine = "c")%dopar%{
  nrow(ewdata_ml[ewdata_ml$calfeartag == ewdata_ml$calfeartag[i],])
} # Find number of tests for each animal prior to 1st ever positive test

stopCluster(cl)

ewdata_ml$ntprior1stpostitre <- temp

rm(temp)

ewdata_ml <- ewdata_ml[as.numeric(ewdata_ml$ntprior1stpostitre) >= 3,] #Limit data to animals with at least 3 tests prior to first positive titre





#####FEATURE ENGINEERING#####

uniquecalves <- unique(ewdata_ml_cc$calfeartag)


no_cores <- detectCores()
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
getDoParWorkers() 

md <- as.factor(foreach(i = uniquecalves, .combine = "c") %dopar% {
  ifelse(any(is.na(ewdata_ml[ewdata_ml$calfeartag == i,])) == TRUE, "1", "0")
})

ucdf <- data.frame(calfeartag = uniquecalves, md = md)

ucdf <- ucdf[ucdf$md == "0",]

ewdata_ml_cc <- ewdata_ml[ewdata_ml$calfeartag %in% ucdf$calfeartag,]

uniquecalves <- ucdf$calfeartag

no_cores <- detectCores()
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
getDoParWorkers() 

ag <- foreach(i = uniquecalves, .combine = "rbind") %dopar% {
  Farm <- ewdata_ml_cc$Farm[ewdata_ml_cc$calfeartag == i][1]
  calfeartag <- i
  
  tmpdat <- ewdata_ml_cc[ewdata_ml_cc$calfeartag == i,]
  
  meanage <- mean(tmpdat$age)
  maxage <- max(tmpdat$age)
  minage <- min(tmpdat$age)
  meanmtnc <- mean(tmpdat$meantitrenegcows)
  maxmtnc <- max(tmpdat$meantitrenegcows)
  minmtnc <- min(tmpdat$meantitrenegcows)
  meanyield <- mean(tmpdat$yield)
  maxyield <- max(tmpdat$yield)
  minyield <- min(tmpdat$yield)
  meanbutterfat <- mean(tmpdat$butterfat)
  maxbutterfat <- max(tmpdat$butterfat)
  minbutterfat <- min(tmpdat$butterfat)
  meanprotein <- mean(tmpdat$protein)
  maxprotein <- max(tmpdat$protein)
  minprotein <- min(tmpdat$protein)
  meancellcount <- mean(tmpdat$cellcount)
  maxcellcount <- max(tmpdat$cellcount)
  mincellcount <- min(tmpdat$cellcount)
  meantitre <- mean(tmpdat$titre)
  maxtitre <- max(tmpdat$titre)
  mintitre <- min(tmpdat$titre)
  meanlikelihood <- mean(tmpdat$likelihood)
  maxlikelihood <- max(tmpdat$likelihood)
  minlikelihood <- min(tmpdat$likelihood)
  ntests <- nrow(tmpdat
                 )
  cnt <- nrow(tmpdat)
  st <- cnt-2
  st <- ifelse(st < 1, 1, st)
  
  tmpdat <- tmpdat[c(st:cnt),]
  
  meanagel3 <- mean(tmpdat$age)
  maxagel3 <- max(tmpdat$age)
  minagel3 <- min(tmpdat$age)
  meanmtncl3 <- mean(tmpdat$meantitrenegcows)
  maxmtncl3 <- max(tmpdat$meantitrenegcows)
  minmtncl3 <- min(tmpdat$meantitrenegcows)
  meanyieldl3 <- mean(tmpdat$yield)
  maxyieldl3 <- max(tmpdat$yield)
  minyieldl3 <- min(tmpdat$yield)
  meanbutterfatl3 <- mean(tmpdat$butterfat)
  maxbutterfatl3 <- max(tmpdat$butterfat)
  minbutterfatl3 <- min(tmpdat$butterfat)
  meanproteinl3 <- mean(tmpdat$protein)
  maxproteinl3 <- max(tmpdat$protein)
  minproteinl3 <- min(tmpdat$protein)
  meancellcountl3 <- mean(tmpdat$cellcount)
  maxcellcountl3 <- max(tmpdat$cellcount)
  mincellcountl3 <- min(tmpdat$cellcount)
  meantitrel3 <- mean(tmpdat$titre)
  maxtitrel3 <- max(tmpdat$titre)
  mintitrel3 <- min(tmpdat$titre)
  meanlikelihoodl3 <- mean(tmpdat$likelihood)
  maxlikelihoodl3 <- max(tmpdat$likelihood)
  minlikelihoodl3 <- min(tmpdat$likelihood)
  prior <- tmpdat$priorprob_12mold[1]
  Target_QMMS <- as.factor(tmpdat$Target_QMMS)[1]
  
  c(Farm,
    calfeartag,
    meanage,
    maxage,
    minage,
    meanmtnc,
    maxmtnc,
    minmtnc,
    meanyield,
    maxyield,
    minyield,
    meanbutterfat,
    maxbutterfat,
    minbutterfat,
    meanprotein,
    maxprotein,
    minprotein,
    meancellcount,
    maxcellcount,
    mincellcount,
    meantitre,
    maxtitre,
    mintitre,
    meanlikelihood,
    maxlikelihood,
    minlikelihood,
    meanagel3,
    maxagel3,
    minagel3,
    meanmtncl3,
    maxmtncl3,
    minmtncl3,
    meanyieldl3,
    maxyieldl3,
    minyieldl3,
    meanbutterfatl3,
    maxbutterfatl3,
    minbutterfatl3,
    meanproteinl3,
    maxproteinl3,
    minproteinl3,
    meancellcountl3,
    maxcellcountl3,
    mincellcountl3,
    meantitrel3,
    maxtitrel3,
    mintitrel3,
    meanlikelihoodl3,
    maxlikelihoodl3,
    minlikelihoodl3,
    ntests,
    prior,
    Target_QMMS
  )
  
  
}


stopCluster(cl)

ag <- as.data.frame(ag)

colnames(ag) <- c("Farm",
                  "calfeartag",
                  "meanage",
                  "maxage",
                  "minage",
                  "meanmtnc",
                  "maxmtnc",
                  "minmtnc",
                  "meanyield",
                  "maxyield",
                  "minyield",
                  "meanbutterfat",
                  "maxbutterfat",
                  "minbutterfat",
                  "meanprotein",
                  "maxprotein",
                  "minprotein",
                  "meancellcount",
                  "maxcellcount",
                  "mincellcount",
                  "meantitre",
                  "maxtitre",
                  "mintitre",
                  "meanlikelihood",
                  "maxlikelihood",
                  "minlikelihood",
                  "meanagel3",
                  "maxagel3",
                  "minagel3",
                  "meanmtncl3",
                  "maxmtncl3",
                  "minmtncl3",
                  "meanyieldl3",
                  "maxyieldl3",
                  "minyieldl3",
                  "meanbutterfatl3",
                  "maxbutterfatl3",
                  "minbutterfatl3",
                  "meanproteinl3",
                  "maxproteinl3",
                  "minproteinl3",
                  "meancellcountl3",
                  "maxcellcountl3",
                  "mincellcountl3",
                  "meantitrel3",
                  "maxtitrel3",
                  "mintitrel3",
                  "meanlikelihoodl3",
                  "maxlikelihoodl3",
                  "minlikelihoodl3",
                  "ntests",
                  "prior",
                  "Target_QMMS")


ag$Target_QMMS_alpha <- as.factor(ifelse(ag$Target_QMMS == "2", "Pos", "Neg"))







#####SPLIT TRAIN/TEST#####


uniquefarms <- unique(ag$Farm)


no_cores <- detectCores() 
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
getDoParWorkers() 

nr <- foreach(i = 1:length(uniquefarms), .combine = "c") %dopar% {
  nrow(ag[ag$Farm == uniquefarms[i],])
}

ag[,c(3:52)] <- lapply(ag[,c(3:52)], as.numeric)
ag[,c(53,54)] <- lapply(ag[,c(53,54)], as.factor)


stopCluster(cl)


uniquefarms <- data.frame(Farm = uniquefarms, n = nr)

#Drop farms with too few rows if necessary

uniquefarms <- uniquefarms[uniquefarms$n > 0,]

trainfarms <- sample(uniquefarms$Farm, round(0.8*nrow(uniquefarms)), replace = FALSE)


ag_train <- ag[ag$Farm %in% trainfarms,]
ag_test <- ag[!(ag$Farm %in% trainfarms),]


print(paste0("Training data: ", nrow(ag_train)," rows."))
print(paste0("Testing data: ", nrow(ag_test)," rows."))

print(paste0("Training data prevalence: ", round(nrow(ag_train[ag_train$Target_QMMS_alpha == "Pos",])/
                                                   (nrow(ag_train[ag_train$Target_QMMS_alpha == "Neg",]) +
                                                      nrow(ag_train[ag_train$Target_QMMS_alpha == "Pos",]))*100,1),
             "%"))

print(paste0("Testing data prevalence: ", round(nrow(ag_test[ag_test$Target_QMMS_alpha == "Pos",])/
                                                  (nrow(ag_test[ag_test$Target_QMMS_alpha == "Neg",]) +
                                                     nrow(ag_test[ag_test$Target_QMMS_alpha == "Pos",]))*100,1),
             "%"))


#####CHECK FOR NZV FEATURES#####



nzv <- nearZeroVar(ag_train, saveMetrics = T)
nzv[nzv$nzv,]


#####SET TUNING AND RESAMPLING PARAMETERS#####

folds <- 5
cvIndex <- createFolds(factor(ag_train$Farm), folds, returnTrain = T)

allstats  <- function(data, lev = NULL, model = NULL){
  a1 <- defaultSummary(data, lev, model)
  b1 <- twoClassSummary(data, lev, model)
  c1 <- prSummary(data, lev, model)
  out <- c(a1, b1, c1)
  out}

brierscore <- function(data, levs, model){
  
  rs <- vector()
  
  
  for(i in 1:nrow(data)){
    label <- ifelse(data$obs == "Pos",1,0)
    rs[i] <- (data$P[i] - label[i])^2
  }
  
  out <- -mean(rs)
  
  names(out) <- "BrierScore"
  
  return(out)
  
}

fitControl <- trainControl(index = cvIndex,
                           method = 'cv', 
                           number = folds,
                           classProbs = TRUE,
                           summaryFunction = brierscore
)



ag_train$Target_QMMS_alpha <- as.factor(relevel(as.factor(ag_train$Target_QMMS_alpha), "Pos"))
ag_test$Target_QMMS_alpha <- as.factor(relevel(as.factor(ag_test$Target_QMMS_alpha), "Pos"))



#####TRAIN MODELS#####


train.formula <- formula(Target_QMMS_alpha ~ 
                         meanmtncl3 +
                         maxmtncl3 +
                         minmtncl3 +
                         meanyieldl3 +
                         maxyieldl3 +
                         minyieldl3 +
                         meanbutterfatl3 +
                         maxbutterfatl3 +
                         minbutterfatl3 +
                         meanproteinl3 +
                         maxproteinl3 +
                         minproteinl3 +
                         meancellcountl3 +
                         maxcellcountl3 +
                         mincellcountl3 +
                         meantitrel3 +
                         maxtitrel3 +
                         mintitrel3 +
                         prior)
#Modified formula


no_cores <- detectCores()  
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
getDoParWorkers() 


ew_model_rf <- train(train.formula, data = ag_train, trControl = fitControl, method = "ranger", importance = "permutation", metric = "BrierScore")
ew_model_knn <- train(train.formula, data = ag_train, trControl = fitControl, method = "knn", metric = "BrierScore")
ew_model_reglr <- train(train.formula, data = ag_train, trControl = fitControl, method = "regLogistic", importance = T, metric = "BrierScore")
ew_model_svmlin <- train(train.formula, data = ag_train, trControl = fitControl, method = "svmLinear", importance = T, metric = "BrierScore")
ew_model_svmrad <- train(train.formula, data = ag_train, trControl = fitControl, method = "svmRadial", importance = T, metric = "BrierScore")
ew_model_svmpoly <- train(train.formula, data = ag_train, trControl = fitControl, method = "svmPoly", importance = T, metric = "BrierScore")
ew_model_gbm <- train(train.formula, data = ag_train, trControl = fitControl, method = "gbm", metric = "BrierScore")
ew_model_nnet <- train(train.formula, data = ag_train, trControl = fitControl, method = "nnet", importance = T, metric = "BrierScore")
ew_model_mars <- train(train.formula, data = ag_train, trControl = fitControl, method = "earth", metric = "BrierScore")


stopCluster(cl)

#####COMPARE MODELS#####




models_compare <- resamples(list(RF = ew_model_rf,
                                 KNN = ew_model_knn, 
                                 RegLR = ew_model_reglr,
                                 SVMLin = ew_model_svmlin,
                                 SVMRad = ew_model_svmrad,
                                 SVMPoly = ew_model_svmpoly,
                                 GBM = ew_model_gbm,
                                 NNET = ew_model_nnet,
                                 MARS = ew_model_mars))
summary(models_compare)

bwplot(models_compare)


#####EVALUATE MODELS#####


mod_baccs <- data.frame(mod = c("ew_model_rf",
                                "ew_model_knn", 
                                "ew_model_reglr",
                                "ew_model_svmlin",
                                "ew_model_svmrad",
                                "ew_model_svmpoly",
                                "ew_model_gbm",
                                "ew_model_nnet",
                                "ew_model_mars"),
                        bacc_std = 0,
                        bacc_mod = 0)

for(i in 1:length(mod_baccs$mod)){

probpreds <- predict(get(mod_baccs$mod[i]), newdata = ag_test, type = "prob")[,1]
ag_test$probpreds <- probpreds
ag_test$classpreds <- as.factor(ifelse(ag_test$probpreds >= 0.5, "Pos", "Neg"))


print(CaliPlot(ag_test$probpreds, as.numeric(ag_test$Target_QMMS) -1, ptitle = mod_baccs$mod[i]))


CM <- confusionMatrix(ag_test$classpreds, ag_test$Target_QMMS_alpha, positive = "Pos")

print(paste0("Standard Cutpoint B.acc ",CM$byClass[11]))

mod_baccs$bacc_std[i] <- CM$byClass[11]


cp <- cutpointr(ag_test, probpreds, Target_QMMS_alpha, 
                method = maximize_metric, metric = sum_sens_spec)

optcut <- cp$optimal_cutpoint

ag_test$classpreds <- as.factor(ifelse(ag_test$probpreds >= optcut, "Pos", "Neg"))
CM <- confusionMatrix(ag_test$classpreds, ag_test$Target_QMMS_alpha, positive = "Pos")
print(paste0(mod_baccs$mod[i]," Modified Cutpoint B.acc ",CM$byClass[11]))

mod_baccs$bacc_mod[i] <- CM$byClass[11]

print(mod_baccs)

}

ggplot(mod_baccs) +
  geom_point(aes(x = as.factor(mod), y = bacc_std), color = "green") +
  geom_point(aes(x = as.factor(mod), y = bacc_mod), color = "red") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Model", y = "Balanced Accuracy", title = "Standard and Optimised Classifier Cutpoints")


SVMlinGrid <-  expand.grid(C = c(0.001,0.01,0.1,1,2,5))
SVMpolyGrid <- expand.grid(degree = c(1:5),
                           C = c(0.1, 0.25, 0.5,0.75,1),
                           scale = c(0.01,0.05,0.1,0.2,0.5))
RFGrid <- expand.grid(min.node.size = c(1,2,5),
                      splitrule = c("gini", "extratrees"),
                      mtry = c(1,2,5,10,15,19))


no_cores <- detectCores()  
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
getDoParWorkers() 


ew_model_rf_tuned <- train(train.formula, data = ag_train, trControl = fitControl, method = "ranger", importance = T, metric = "BrierScore", tuneGrid = RFGrid)

stopCluster(cl)

mod <- "ew_model_rf"

probpreds <- predict(get(mod), newdata = ag_test, type = "prob")[,1]
ag_test$probpreds <- probpreds
ag_test$classpreds <- as.factor(ifelse(ag_test$probpreds >= 0.5, "Pos", "Neg"))


print(CaliPlot(ag_test$probpreds, as.numeric(ag_test$Target_QMMS) -1, ptitle = mod))


CM <- confusionMatrix(ag_test$classpreds, ag_test$Target_QMMS_alpha, positive = "Pos")

print(CM$table)
print(paste0("Standard Cutpoint B.acc ",CM$byClass[11]))


cp <- cutpointr(ag_test, probpreds, Target_QMMS_alpha, 
                method = maximize_metric, metric = sum_sens_spec)

optcut <- cp$optimal_cutpoint

ag_test$classpreds <- as.factor(ifelse(ag_test$probpreds >= optcut, "Pos", "Neg"))
CM <- confusionMatrix(ag_test$classpreds, ag_test$Target_QMMS_alpha, positive = "Pos")
print(CM$table)
print(paste0(" Modified Cutpoint B.acc ",CM$byClass[11]))

#####EXAMINE POSITIVE ANIMALS#####

write.csv(ag_test, "y:/ian/johnesthresholds/johnesproper/data/ag_test.csv", row.names = FALSE)

#Call IndiCowPlotter for individual cows


#####COMPARE WITH BAYESIAN PREDICTIONS#####

ag$pred_rf <- as.numeric(predict(ew_model_rf,
                                   newdata = ag,
                               type = "prob")[,1])

ewdata_lastbeforepostitre <- ewdata[ewdata$ageatfirstH == 0 | 
                                      ewdata$age < ewdata$ageatfirstH,]



no_cores <- detectCores()  
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
getDoParWorkers() 



lastbeforepostitre <- foreach(i = 1:nrow(ewdata_lastbeforepostitre)-1, .combine = "c") %dopar% {
  ifelse(ewdata_lastbeforepostitre$calfeartag[i] != ewdata_lastbeforepostitre$calfeartag[i+1],
         1,0)
}

stopCluster(cl)

lastbeforepostitre[length(lastbeforepostitre)+1] <- 1

ewdata_lastbeforepostitre$lastbeforepostitre <- lastbeforepostitre

ewdata_lastbeforepostitre <- ewdata_lastbeforepostitre[ewdata_lastbeforepostitre$lastbeforepostitre == 1,]




ag <- merge(ag, ewdata_lastbeforepostitre[,c(2,3,185)], by = c("Farm", "calfeartag"), all.x = TRUE)

write.csv(ag, "y:/ian/johnesthresholds/johnesproper/data/ag.csv", row.names = FALSE)

ggplot(ag,
       aes(x = PosteriorProb.x, y = pred_rf)) +
  geom_point() +
  labs(x = "Bayesian Posterior", y = "ML Prediction", title = "Last test before first positive")
         

######Check discrepant Bayesian and ML predictions######

ag$BayesMLdiscrep <- ag$pred_rf - ag$PosteriorProb.x

ggplot(ag,
       aes(x = BayesMLdiscrep)) +
  geom_histogram() +
  labs(x = "MLPred - BayesPosterior")

ag_discreps <- ag[ag$BayesMLdiscrep < -0.75 | ag$BayesMLdiscrep > 0.75,]

testcows <- ag_discreps$calfeartag[1:30]

group.colours <- c("L" = "green", "M" = "orange", "H" = "red")

for (cow in testcows){
  print(ggplot(data_posteriors[data_posteriors$calfeartag == cow,], aes(x = age)) +
          geom_point(aes(y = titre, color = class)) +
          geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
          geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
          #geom_text(aes(y = -4, label = ifelse(POFloorApplied == 1,"POF","")), size = 2, angle = -90 ) +
          geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
          geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
          geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
          scale_color_manual(values = group.colours) +
          geom_line(aes(y = PosteriorProb*100)) +
          geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
          geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
          scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
          labs(title = data_posteriors$calfeartag[data_posteriors$calfeartag == cow], subtitle = paste("\nBirth Probability:", round(data_posteriors$priorprob_birth[data_posteriors$calfeartag == cow],2),"12m Probability:",round(data_posteriors$priorprob_12mold[data_posteriors$calfeartag == cow],2),"Crt Probability:",round(data_posteriors$priorprob_crt[data_posteriors$calfeartag == cow],2))))
  
  ggsave(paste0("Y:/Ian/JohnesThresholds/JohnesProper/Data/EarlyWarning/MLBayesDiscrepPreds/",cow,".png"))
}


######Investigate features that are influencing ML predictions######

ggplot(ag,
       aes(x = meantitre - minmtnc, y = pred_svmpoly)) +
  geom_point()



######Compare feature distributions between Pos and Neg ML predictions######

for (ft in c("meanmtncl3",
             "maxmtncl3",
             "minmtncl3",
             "meanyieldl3",
             "maxyieldl3",
             "minyieldl3",
             "meanbutterfatl3",
             "maxbutterfatl3",
             "minbutterfatl3",
             "meanproteinl3",
             "maxproteinl3",
             "minproteinl3",
             "meancellcountl3",
             "maxcellcountl3",
             "mincellcountl3",
             "meantitrel3",
             "maxtitrel3",
             "mintitrel3",
             "prior")){

    print(ggplot(ag,
       aes(x = as.factor(Target_QMMS_alpha), y = get(ft))) +
    geom_boxplot() +
    labs(x = "Target_QMMS", title = ft))
  
}



####ROLLING ML MODEL####

#####CONSTRUCT DATA#####

ewdata <- read.csv("y:/ian/johnesthresholds/johnesproper/data/data_posteriors.csv")

ewdata <- ewdata[ewdata$Target_QMMS != "U",]

ewdata$Target_QMMS_alpha <- as.factor(ifelse(ewdata$Target_QMMS == "1", "Pos", "Neg"))



ewdata$testnum <- 1

for (i in 2:nrow(ewdata)) {
  ewdata$testnum[i] <- ifelse(ewdata$calfeartag[i] != ewdata$calfeartag[i-1], 1,
         ewdata$testnum[i-1] + 1)
  
}

no_cores <- detectCores()
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
getDoParWorkers()




tmp <- as.data.frame(foreach(i = 1:nrow(ewdata), .combine = "rbind") %dopar% {
  a <- i-2
  a <- ifelse(a <0,0,a)
  minagel3 <- ifelse(ewdata$testnum[i] < 3, 10000, min(ewdata$age[a:i]))
  maxagel3 <- ifelse(ewdata$testnum[i] < 3, 10000, max(ewdata$age[a:i]))
  meanagel3 <- ifelse(ewdata$testnum[i] < 3, 10000, mean(ewdata$age[a:i]))
  minmtncl3 <- ifelse(ewdata$testnum[i] < 3, 10000, min(ewdata$meantitrenegcows[a:i]))
  maxmtncl3 <- ifelse(ewdata$testnum[i] < 3, 10000, max(ewdata$meantitrenegcows[a:i]))
  meanmtncl3 <- ifelse(ewdata$testnum[i] < 3, 10000, mean(ewdata$meantitrenegcows[a:i]))
  minyieldl3 <- ifelse(ewdata$testnum[i] < 3, 10000, min(ewdata$yield[a:i]))
  maxyieldl3 <- ifelse(ewdata$testnum[i] < 3, 10000, max(ewdata$yield[a:i]))
  meanyieldl3 <- ifelse(ewdata$testnum[i] < 3, 10000, mean(ewdata$yield[a:i]))
  minbutterfatl3 <- ifelse(ewdata$testnum[i] < 3, 10000, min(ewdata$butterfat[a:i]))
  maxbutterfatl3 <- ifelse(ewdata$testnum[i] < 3, 10000, max(ewdata$butterfat[a:i]))
  meanbutterfatl3 <- ifelse(ewdata$testnum[i] < 3, 10000, mean(ewdata$butterfat[a:i]))
  minproteinl3 <- ifelse(ewdata$testnum[i] < 3, 10000, min(ewdata$protein[a:i]))
  maxproteinl3 <- ifelse(ewdata$testnum[i] < 3, 10000, max(ewdata$protein[a:i]))
  meanproteinl3 <- ifelse(ewdata$testnum[i] < 3, 10000, mean(ewdata$protein[a:i]))
  mincellcountl3 <- ifelse(ewdata$testnum[i] < 3, 10000, min(ewdata$cellcount[a:i]))
  maxcellcountl3 <- ifelse(ewdata$testnum[i] < 3, 10000, max(ewdata$cellcount[a:i]))
  meancellcountl3 <- ifelse(ewdata$testnum[i] < 3, 10000, mean(ewdata$cellcount[a:i]))
  mintitrel3 <- ifelse(ewdata$testnum[i] < 3, 10000, min(ewdata$titre[a:i]))
  maxtitrel3 <- ifelse(ewdata$testnum[i] < 3, 10000, max(ewdata$titre[a:i]))
  meantitrel3 <- ifelse(ewdata$testnum[i] < 3, 10000, mean(ewdata$titre[a:i]))
  minage <- ifelse(ewdata$testnum[i] < 3, 10000, min(ewdata$age[ewdata$calfeartag == ewdata$calfeartag[i] &
                                                                  ewdata$testnum <= ewdata$testnum[i]]))
  maxage <- ifelse(ewdata$testnum[i] < 3, 10000, max(ewdata$age[ewdata$calfeartag == ewdata$calfeartag[i] &
                                                                  ewdata$testnum <= ewdata$testnum[i]]))
  meanage <- ifelse(ewdata$testnum[i] < 3, 10000, mean(ewdata$age[ewdata$calfeartag == ewdata$calfeartag[i] &
                                                                    ewdata$testnum <= ewdata$testnum[i]]))
  minmtnc <- ifelse(ewdata$testnum[i] < 3, 10000, min(ewdata$meantitrenegcows[ewdata$calfeartag == ewdata$calfeartag[i] &
                                                                                ewdata$testnum <= ewdata$testnum[i]]))
  maxmtnc <- ifelse(ewdata$testnum[i] < 3, 10000, max(ewdata$meantitrenegcows[ewdata$calfeartag == ewdata$calfeartag[i] &
                                                                                ewdata$testnum <= ewdata$testnum[i]]))
  meanmtnc <- ifelse(ewdata$testnum[i] < 3, 10000, mean(ewdata$meantitrenegcows[ewdata$calfeartag == ewdata$calfeartag[i] &
                                                                                  ewdata$testnum <= ewdata$testnum[i]]))
  minyield <- ifelse(ewdata$testnum[i] < 3, 10000, min(ewdata$yield[ewdata$calfeartag == ewdata$calfeartag[i] &
                                                                      ewdata$testnum <= ewdata$testnum[i]]))
  maxyield <- ifelse(ewdata$testnum[i] < 3, 10000, max(ewdata$yield[ewdata$calfeartag == ewdata$calfeartag[i] &
                                                                      ewdata$testnum <= ewdata$testnum[i]]))
  meanyield <- ifelse(ewdata$testnum[i] < 3, 10000, mean(ewdata$yield[ewdata$calfeartag == ewdata$calfeartag[i] &
                                                                        ewdata$testnum <= ewdata$testnum[i]]))
  minbutterfat <- ifelse(ewdata$testnum[i] < 3, 10000, min(ewdata$butterfat[ewdata$calfeartag == ewdata$calfeartag[i] &
                                                                              ewdata$testnum <= ewdata$testnum[i]]))
  maxbutterfat <- ifelse(ewdata$testnum[i] < 3, 10000, max(ewdata$butterfat[ewdata$calfeartag == ewdata$calfeartag[i] &
                                                                              ewdata$testnum <= ewdata$testnum[i]]))
  meanbutterfat <- ifelse(ewdata$testnum[i] < 3, 10000, mean(ewdata$butterfat[ewdata$calfeartag == ewdata$calfeartag[i] &
                                                                                ewdata$testnum <= ewdata$testnum[i]]))
  minprotein <- ifelse(ewdata$testnum[i] < 3, 10000, min(ewdata$protein[ewdata$calfeartag == ewdata$calfeartag[i] &
                                                                          ewdata$testnum <= ewdata$testnum[i]]))
  maxprotein <- ifelse(ewdata$testnum[i] < 3, 10000, max(ewdata$protein[ewdata$calfeartag == ewdata$calfeartag[i] &
                                                                          ewdata$testnum <= ewdata$testnum[i]]))
  meanprotein <- ifelse(ewdata$testnum[i] < 3, 10000, mean(ewdata$protein[ewdata$calfeartag == ewdata$calfeartag[i] &
                                                                            ewdata$testnum <= ewdata$testnum[i]]))
  mincellcount <- ifelse(ewdata$testnum[i] < 3, 10000, min(ewdata$cellcount[ewdata$calfeartag == ewdata$calfeartag[i] &
                                                                              ewdata$testnum <= ewdata$testnum[i]]))
  maxcellcount <- ifelse(ewdata$testnum[i] < 3, 10000, max(ewdata$cellcount[ewdata$calfeartag == ewdata$calfeartag[i] &
                                                                              ewdata$testnum <= ewdata$testnum[i]]))
  meancellcount <- ifelse(ewdata$testnum[i] < 3, 10000, mean(ewdata$cellcount[ewdata$calfeartag == ewdata$calfeartag[i] &
                                                                                ewdata$testnum <= ewdata$testnum[i]]))
  mintitre <- ifelse(ewdata$testnum[i] < 3, 10000, min(ewdata$titre[ewdata$calfeartag == ewdata$calfeartag[i] &
                                                                      ewdata$testnum <= ewdata$testnum[i]]))
  maxtitre <- ifelse(ewdata$testnum[i] < 3, 10000, max(ewdata$titre[ewdata$calfeartag == ewdata$calfeartag[i] &
                                                                      ewdata$testnum <= ewdata$testnum[i]]))
  meantitre <- ifelse(ewdata$testnum[i] < 3, 10000, mean(ewdata$titre[ewdata$calfeartag == ewdata$calfeartag[i] &
                                                                        ewdata$testnum <= ewdata$testnum[i]]))
  
  
  c(minage, 
    maxage,
    meanage,
    minmtnc, 
    maxmtnc,
    meanmtnc,
    minyield, 
    maxyield,
    meanyield,
    minbutterfat, 
    maxbutterfat,
    meanbutterfat,
    minprotein, 
    maxprotein,
    meanprotein,
    mincellcount, 
    maxcellcount,
    meancellcount,
    mintitre, 
    maxtitre,
    meantitre,
    minagel3, 
    maxagel3,
    meanagel3,
    minmtncl3, 
    maxmtncl3,
    meanmtncl3,
    minyieldl3, 
    maxyieldl3,
    meanyieldl3,
    minbutterfatl3, 
    maxbutterfatl3,
    meanbutterfatl3,
    minproteinl3, 
    maxproteinl3,
    meanproteinl3,
    mincellcountl3, 
    maxcellcountl3,
    meancellcountl3,
    mintitrel3, 
    maxtitrel3,
    meantitrel3)
})

stopCluster(cl)

colnames(tmp) <- c("minage", 
                   "maxage",
                   "meanage",
                   "minmtnc", 
                   "maxmtnc",
                   "meanmtnc",
                   "minyield", 
                   "maxyield",
                   "meanyield",
                   "minbutterfat", 
                   "maxbutterfat",
                   "meanbutterfat",
                   "minprotein", 
                   "maxprotein",
                   "meanprotein",
                   "mincellcount", 
                   "maxcellcount",
                   "meancellcount",
                   "mintitre", 
                   "maxtitre",
                   "meantitre",
                   "minagel3", 
                   "maxagel3",
                   "meanagel3",
                   "minmtncl3", 
                   "maxmtncl3",
                   "meanmtncl3",
                   "minyieldl3", 
                   "maxyieldl3",
                   "meanyieldl3",
                   "minbutterfatl3", 
                   "maxbutterfatl3",
                   "meanbutterfatl3",
                   "minproteinl3", 
                   "maxproteinl3",
                   "meanproteinl3",
                   "mincellcountl3", 
                   "maxcellcountl3",
                   "meancellcountl3",
                   "mintitrel3", 
                   "maxtitrel3",
                   "meantitrel3")

ewdata <- cbind(ewdata, tmp)

ewdata_ML <- ewdata[,c('Farm',
                    'calfeartag',
                    'testnum',
                    'age',
                    'meantitrenegcows',
                    'yield',
                    'butterfat',
                    'protein',
                    'cellcount',
                    'titre',
                    'minage', 
                    'maxage',
                    'meanage',
                    'minmtnc', 
                    'maxmtnc',
                    'meanmtnc',
                    'minyield', 
                    'maxyield',
                    'meanyield',
                    'minbutterfat', 
                    'maxbutterfat',
                    'meanbutterfat',
                    'minprotein', 
                    'maxprotein',
                    'meanprotein',
                    'mincellcount', 
                    'maxcellcount',
                    'meancellcount',
                    'mintitre', 
                    'maxtitre',
                    'meantitre',
                    'minagel3', 
                    'maxagel3',
                    'meanagel3',
                    'minmtncl3', 
                    'maxmtncl3',
                    'meanmtncl3',
                    'minyieldl3', 
                    'maxyieldl3',
                    'meanyieldl3',
                    'minbutterfatl3', 
                    'maxbutterfatl3',
                    'meanbutterfatl3',
                    'minproteinl3', 
                    'maxproteinl3',
                    'meanproteinl3',
                    'mincellcountl3', 
                    'maxcellcountl3',
                    'meancellcountl3',
                    'mintitrel3', 
                    'maxtitrel3',
                    'meantitrel3',
                    'Target_QMMS',
                    'Target_QMMS_alpha')]

ewdata_ML <- ewdata_ML[ewdata_ML$testnum >= 3,]

ewdata_ML <- ewdata_ML[complete.cases(ewdata_ML) == TRUE,]

#####VISUALISE DATA#####

for (i in 4:52){
  df = as.data.frame(ewdata_ML[,i])
  colnames(df) <- "x"
  print(ggplot(df,
         aes(x = x)) +
    geom_boxplot() +
    labs(title = colnames(ewdata_ML)[i]))
}

ggplot(ewdata_ML,
       aes(x = as.factor(Target_QMMS_alpha))) +
  geom_bar(stat = "count") +
  labs(x = "Target_QMMS", y = "N Rows")
  


#####SPLIT TRAIN/TEST#####


uniquefarms <- unique(ewdata_ML$Farm)


no_cores <- detectCores() 
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
getDoParWorkers() 

nr <- foreach(i = 1:length(uniquefarms), .combine = "c") %dopar% {
  nrow(ewdata_ML[ewdata_ML$Farm == uniquefarms[i],])
}

stopCluster(cl)


ewdata_ML$testnum <- as.integer(ewdata_ML$testnum)
ewdata_ML[,c(4:52)] <- lapply(ewdata_ML[,c(4:52)], as.numeric)
ewdata_ML[,c(53,54)] <- lapply(ewdata_ML[,c(53,54)], as.factor)



uniquefarms <- data.frame(Farm = uniquefarms, n = nr)

#Drop farms with too few rows if necessary

uniquefarms <- uniquefarms[uniquefarms$n > 0,]

trainfarms <- sample(uniquefarms$Farm, round(0.8*nrow(uniquefarms)), replace = FALSE)


ewdata_ML_train <- ewdata_ML[ewdata_ML$Farm %in% trainfarms,]
ewdata_ML_test <- ewdata_ML[!(ewdata_ML$Farm %in% trainfarms),]


print(paste0("Training data: ", nrow(ewdata_ML_train)," rows."))
print(paste0("Testing data: ", nrow(ewdata_ML_test)," rows."))

print(paste0("Training data prevalence: ", round(nrow(ewdata_ML_train[ewdata_ML_train$Target_QMMS_alpha == "Pos",])/
                                                   (nrow(ewdata_ML_train[ewdata_ML_train$Target_QMMS_alpha == "Neg",]) +
                                                      nrow(ewdata_ML_train[ewdata_ML_train$Target_QMMS_alpha == "Pos",]))*100,1),
             "%"))

print(paste0("Testing data prevalence: ", round(nrow(ewdata_ML_test[ewdata_ML_test$Target_QMMS_alpha == "Pos",])/
                                                  (nrow(ewdata_ML_test[ewdata_ML_test$Target_QMMS_alpha == "Neg",]) +
                                                     nrow(ewdata_ML_test[ewdata_ML_test$Target_QMMS_alpha == "Pos",]))*100,1),
             "%"))


#####CHECK FOR NZV FEATURES#####



nzv <- nearZeroVar(ewdata_ML_train, saveMetrics = T)
nzv[nzv$nzv,]


#####SET TUNING AND RESAMPLING PARAMETERS#####

folds <- 5
cvIndex <- createFolds(factor(ewdata_ML_train$Farm), folds, returnTrain = T)

allstats  <- function(data, lev = NULL, model = NULL){
  a1 <- defaultSummary(data, lev, model)
  b1 <- twoClassSummary(data, lev, model)
  c1 <- prSummary(data, lev, model)
  out <- c(a1, b1, c1)
  out}

brierscore <- function(data, levs, model){
  
  rs <- vector()
  
  
  for(i in 1:nrow(data)){
    label <- ifelse(data$obs == "Pos",1,0)
    rs[i] <- (data$P[i] - label[i])^2
  }
  
  out <- -mean(rs)
  
  names(out) <- "BrierScore"
  
  return(out)
  
}

fitControl <- trainControl(index = cvIndex,
                           method = 'cv', 
                           number = folds,
                           classProbs = TRUE,
                           summaryFunction = brierscore
)



ewdata_ML_train$Target_QMMS_alpha <- as.factor(relevel(as.factor(ewdata_ML_train$Target_QMMS_alpha), "Pos"))
ewdata_ML_test$Target_QMMS_alpha <- as.factor(relevel(as.factor(ewdata_ML_test$Target_QMMS_alpha), "Pos"))

#####TRAIN MODELS#####

train.formula <- formula(Target_QMMS_alpha ~ 
                           age +
                         meantitrenegcows +
                         yield +
                         butterfat +
                         protein +
                         cellcount +
                         titre +
                         minage + 
                         maxage +
                         meanage +
                         minmtnc + 
                         maxmtnc +
                         meanmtnc +
                         minyield + 
                         maxyield +
                         meanyield +
                         minbutterfat + 
                         maxbutterfat +
                         meanbutterfat +
                         minprotein + 
                         maxprotein +
                         meanprotein +
                         mincellcount + 
                         maxcellcount +
                         meancellcount +
                         mintitre + 
                         maxtitre +
                         meantitre +
                         minagel3 + 
                         maxagel3 +
                         meanagel3 +
                         minmtncl3 + 
                         maxmtncl3 +
                         meanmtncl3 +
                         minyieldl3 + 
                         maxyieldl3 +
                         meanyieldl3 +
                         minbutterfatl3 + 
                         maxbutterfatl3 +
                         meanbutterfatl3 +
                         minproteinl3 + 
                         maxproteinl3 +
                         meanproteinl3 +
                         mincellcountl3 + 
                         maxcellcountl3 +
                         meancellcountl3 +
                         mintitrel3 + 
                         maxtitrel3 +
                         meantitrel3)


#Modified formula



no_cores <- detectCores()  
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
getDoParWorkers() 


ew_model_rf <- train(train.formula, data = ewdata_ML_train, trControl = fitControl, method = "ranger", importance = "permutation", metric = "BrierScore")
ew_model_knn <- train(train.formula, data = ewdata_ML_train, trControl = fitControl, method = "knn", metric = "BrierScore")
ew_model_reglr <- train(train.formula, data = ewdata_ML_train, trControl = fitControl, method = "regLogistic", importance = T, metric = "BrierScore")

#ew_model_svmlin <- train(train.formula, data = ewdata_ML_train, trControl = fitControl, method = "svmLinear", importance = T, metric = "BrierScore")
#ew_model_svmrad <- train(train.formula, data = ewdata_ML_train, trControl = fitControl, method = "svmRadial", importance = T, metric = "BrierScore")
#ew_model_svmpoly <- train(train.formula, data = ewdata_ML_train, trControl = fitControl, method = "svmPoly", importance = T, metric = "BrierScore")

ew_model_gbm <- train(train.formula, data = ewdata_ML_train, trControl = fitControl, method = "gbm", metric = "BrierScore")
ew_model_nnet <- train(train.formula, data = ewdata_ML_train, trControl = fitControl, method = "nnet", importance = T, metric = "BrierScore")
ew_model_mars <- train(train.formula, data = ewdata_ML_train, trControl = fitControl, method = "earth", metric = "BrierScore")


stopCluster(cl)

#####COMPARE MODELS#####



models_compare <- resamples(list(RF = ew_model_rf,
                                 KNN = ew_model_knn, 
                                 RegLR = ew_model_reglr,
                                 #SVMLin = ew_model_svmlin,
                                 #SVMRad = ew_model_svmrad,
                                 #SVMPoly = ew_model_svmpoly,
                                 GBM = ew_model_gbm,
                                 NNET = ew_model_nnet,
                                 MARS = ew_model_mars))
summary(models_compare)

bwplot(models_compare)



mod_baccs <- data.frame(mod = c("ew_model_rf",
                                "ew_model_knn", 
                                "ew_model_reglr",
                                "ew_model_gbm",
                                "ew_model_nnet",
                                "ew_model_mars"),
                        bacc_std = 0,
                        bacc_mod = 0)

for(i in 1:length(mod_baccs$mod)){
  
  probpreds <- predict(get(mod_baccs$mod[i]), newdata = ewdata_ML_test, type = "prob")[,1]
  ewdata_ML_test$probpreds <- probpreds
  ewdata_ML_test$classpreds <- as.factor(ifelse(ewdata_ML_test$probpreds >= 0.5, "Pos", "Neg"))
  
  cat(paste0("",mod_baccs$mod[i],"\n----------------"))
  
  print(CaliPlot(ewdata_ML_test$probpreds, as.numeric(ewdata_ML_test$Target_QMMS) -1, ptitle = mod_baccs$mod[i]))
  
  
  CM <- confusionMatrix(ewdata_ML_test$classpreds, ewdata_ML_test$Target_QMMS_alpha, positive = "Pos")
  print(CM$table)
  
  print(paste0("Standard Cutpoint B.acc ",round(CM$byClass[11],2)))
  
  mod_baccs$bacc_std[i] <- CM$byClass[11]
  
  
  cp <- cutpointr(ewdata_ML_test, probpreds, Target_QMMS_alpha, 
                  method = maximize_metric, metric = sum_sens_spec)
  
  optcut <- cp$optimal_cutpoint
  
  ewdata_ML_test$classpreds <- as.factor(ifelse(ewdata_ML_test$probpreds >= optcut, "Pos", "Neg"))
  CM <- confusionMatrix(ewdata_ML_test$classpreds, ewdata_ML_test$Target_QMMS_alpha, positive = "Pos")
  print(CM$table)
  print(paste0("Modified Cutpoint B.acc ",round(CM$byClass[11],2)))
  
  mod_baccs$bacc_mod[i] <- CM$byClass[11]
  
  
  
}

print(mod_baccs)

ggplot(mod_baccs) +
  geom_point(aes(x = as.factor(mod), y = bacc_std), color = "green") +
  geom_point(aes(x = as.factor(mod), y = bacc_mod), color = "red") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Model", y = "Balanced Accuracy", title = "Standard and Optimised Classifier Cutpoints")

saveRDS(ew_model_gbm, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/ew_model_gbm.rds")

#####MARGINAL EFFECTS OF ML MODEL#####

sds <- 1000 #Set nrows simulated data

#Set variable ranges

agerange <- c(24,240)
meantitrenegcowsrange <- c(1,10)
yieldrange <- c(5,60)
butterfatrange <- c(1,6)
proteinrange <- c(1,6)
cellcountrange <- c(10,10000)
titrerange <- c(1,250)

for(i in 4:52){

vartotest = toString(colnames(ewdata_ML)[i])

if(grepl("age", vartotest) == TRUE){varrange <- agerange}
if(grepl("meantitrenegcows", vartotest) == TRUE | grepl("mtnc", vartotest) == TRUE){varrange <- meantitrenegcowsrange}
if(grepl("yield", vartotest) == TRUE){varrange <- yieldrange}
if(grepl("butterfat", vartotest) == TRUE){varrange <- butterfatrange}
if(grepl("protein", vartotest) == TRUE){varrange <- proteinrange}
if(grepl("cellcount", vartotest) == TRUE){varrange <- cellcountrange}
if(grepl("titre", vartotest) == TRUE){varrange <- titrerange}



data_sim <- data.frame(age = rep(mean(ewdata_ML$age),sds),
                       meantitrenegcows = rep(mean(ewdata_ML$meantitrenegcows),sds),
                       yield = rep(mean(ewdata_ML$yield),sds),
                       butterfat = rep(mean(ewdata_ML$butterfat),sds),
                       protein = rep(mean(ewdata_ML$protein),sds),
                       cellcount = rep(mean(ewdata_ML$cellcount),sds),
                       titre = rep(mean(ewdata_ML$titre),sds),
                       minage = rep(mean(ewdata_ML$minage),sds),
                       maxage = rep(mean(ewdata_ML$maxage),sds),
                       meanage = rep(mean(ewdata_ML$meanage),sds),
                       minmtnc = rep(mean(ewdata_ML$minmtnc),sds),
                       maxmtnc = rep(mean(ewdata_ML$maxmtnc),sds),
                       meanmtnc = rep(mean(ewdata_ML$meanmtnc),sds),
                       minyield = rep(mean(ewdata_ML$minyield),sds),
                       maxyield = rep(mean(ewdata_ML$maxyield),sds),
                       meanyield = rep(mean(ewdata_ML$meanyield),sds),
                       minbutterfat = rep(mean(ewdata_ML$minbutterfat),sds),
                       maxbutterfat = rep(mean(ewdata_ML$maxbutterfat),sds),
                       meanbutterfat = rep(mean(ewdata_ML$meanbutterfat),sds),
                       minprotein = rep(mean(ewdata_ML$minprotein),sds),
                       maxprotein = rep(mean(ewdata_ML$maxprotein),sds),
                       meanprotein = rep(mean(ewdata_ML$meanprotein),sds),
                       mincellcount = rep(mean(ewdata_ML$mincellcount),sds),
                       maxcellcount = rep(mean(ewdata_ML$maxcellcount),sds),
                       meancellcount = rep(mean(ewdata_ML$meancellcount),sds),
                       mintitre = rep(mean(ewdata_ML$mintitre),sds),
                       maxtitre = rep(mean(ewdata_ML$maxtitre),sds),
                       meantitre = rep(mean(ewdata_ML$meantitre),sds),
                       minagel3 = rep(mean(ewdata_ML$minagel3),sds),
                       maxagel3 = rep(mean(ewdata_ML$maxagel3),sds),
                       meanagel3 = rep(mean(ewdata_ML$meanagel3),sds),
                       minmtncl3 = rep(mean(ewdata_ML$minmtncl3),sds),
                       maxmtncl3 = rep(mean(ewdata_ML$maxmtncl3),sds),
                       meanmtncl3 = rep(mean(ewdata_ML$meanmtncl3),sds),
                       minyieldl3 = rep(mean(ewdata_ML$minyieldl3),sds),
                       maxyieldl3 = rep(mean(ewdata_ML$maxyieldl3),sds),
                       meanyieldl3 = rep(mean(ewdata_ML$meanyieldl3),sds),
                       minbutterfatl3 = rep(mean(ewdata_ML$minbutterfatl3),sds),
                       maxbutterfatl3 = rep(mean(ewdata_ML$maxbutterfatl3),sds),
                       meanbutterfatl3 = rep(mean(ewdata_ML$meanbutterfatl3),sds),
                       minproteinl3 = rep(mean(ewdata_ML$minproteinl3),sds),
                       maxproteinl3 = rep(mean(ewdata_ML$maxproteinl3),sds),
                       meanproteinl3 = rep(mean(ewdata_ML$meanproteinl3),sds),
                       mincellcountl3 = rep(mean(ewdata_ML$mincellcountl3),sds),
                       maxcellcountl3 = rep(mean(ewdata_ML$maxcellcountl3),sds),
                       meancellcountl3 = rep(mean(ewdata_ML$meancellcountl3),sds),
                       mintitrel3 = rep(mean(ewdata_ML$mintitrel3),sds),
                       maxtitrel3 = rep(mean(ewdata_ML$maxtitrel3),sds),
                       meantitrel3 = rep(mean(ewdata_ML$meantitrel3),sds))


a <- which(colnames(data_sim) == vartotest)

data_sim[,a] <- seq(from =  varrange[1],to = varrange[2], length.out = sds) # Set variable to test

data_sim$pred <- predict(ew_model_gbm,
                         type = "prob",
                         newdata = data_sim)[,1]

print(ggplot(data_sim,
       aes(x = get(vartotest), y = pred)) +
  geom_line() +
  labs(x = vartotest, y = "ML Prediction", title = "Marginal Effect", subtitle = vartotest))

ggsave(paste0("Y:/Ian/JohnesThresholds/JohnesProper/Data/EarlyWarning/MLMarginalEffects/",vartotest,".png"))
}

#####COMPARE WITH BAYESIAN PREDICTIONS#####

ewdata_ML$pred_gbm <- as.numeric(predict(ew_model_gbm,
                                 newdata = ewdata_ML,
                                 type = "prob")[,1])


ewdata <- merge(ewdata, ewdata_ML[,c(1,2,3,55)], by = c("Farm", "calfeartag", "testnum"), all.x = TRUE)


write.csv(ewdata, "y:/ian/johnesthresholds/johnesproper/data/ewdata.csv", row.names = FALSE)

ggplot(ewdata,
       aes(x = PosteriorProb, y = pred_gbm)) +
  geom_point() +
  labs(x = "Bayesian Posterior", y = "ML Prediction", title = "All tests")



######Check discrepant Bayesian and ML predictions######

ewdata$BayesMLdiscrep[!is.na(ewdata$pred_gbm)] <- ewdata$pred_gbm[!is.na(ewdata$pred_gbm)] - 
  ewdata$PosteriorProb[!is.na(ewdata$pred_gbm)]

ggplot(ewdata,
       aes(x = BayesMLdiscrep)) +
  geom_histogram() +
  labs(x = "MLPred - BayesPosterior")

ewdata_discreps <- ewdata[!is.na(ewdata$BayesMLdiscrep) & 
                            (ewdata$BayesMLdiscrep < -0.5 | ewdata$BayesMLdiscrep > 0.5),]

testcows <- unique(ewdata_discreps$calfeartag[1:30])

group.colours <- c("L" = "green", "M" = "orange", "H" = "red")

for (cow in testcows){
  print(ggplot(ewdata[ewdata$calfeartag == cow,], aes(x = age)) +
          geom_point(aes(y = titre, color = class)) +
          geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
          geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
          #geom_text(aes(y = -4, label = ifelse(POFloorApplied == 1,"POF","")), size = 2, angle = -90 ) +
          geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
          geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
          geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
          scale_color_manual(values = group.colours) +
          geom_line(aes(y = PosteriorProb*100)) +
          geom_line(aes(y = pred_gbm*100), color = "darkgray") +
          geom_text(aes(x = age, y = pred_gbm * 100, label = round(pred_gbm, 2), color = "darkgray"), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) +
          geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
          geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
          scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
          labs(title = ewdata$calfeartag[ewdata$calfeartag == cow]))
  
  ggsave(paste0("Y:/Ian/JohnesThresholds/JohnesProper/Data/EarlyWarning/MLBayesDiscrepPreds/",cow,".png"))
}

#####PLOTS OF ALL ANIMALS#####


calfsampleqmms1 <- sample(unique(ewdata$calfeartag[ewdata$Target_QMMS == "1"]), 100, replace = FALSE)

calfsampleqmms0 <- sample(unique(ewdata$calfeartag[ewdata$Target_QMMS == "0"]), 100, replace = FALSE)

calfsamplerandom <- sample(unique(ewdata$calfeartag), 200, replace  = FALSE)

calfsamplepofapplied <- sample(unique(ewdata$calfeartag[ewdata$POFloorApplied == 1 &
                                                          ewdata$Target_QMMS_alpha == "Pos"]), 100, replace = FALSE)

for (i in calfsampleqmms1){
  ggplot(ewdata[ewdata$calfeartag == i,], aes(x = age)) +
    geom_point(aes(y = titre, color = class)) + 
    geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
    geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
    geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
    #geom_text(aes(y = -4, label = ifelse(POFloorApplied == 1,"POF","")), size = 2, angle = -90 ) +
    scale_color_manual(values = group.colours) +
    geom_line(aes(y = PosteriorProb*100)) +
    geom_line(aes(y = pred_gbm*100), color = "darkgray") +
    geom_text(aes(x = age, y = pred_gbm * 100, label = round(pred_gbm, 2), color = "darkgray"), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) +
    geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
    scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
    labs(title = i, subtitle = paste(crtmodel,"\nBirth Probability:", round(ewdata$priorprob_birth[ewdata$calfeartag == i],2),"12m Probability:",round(ewdata$priorprob_12mold[ewdata$calfeartag == i],2),"Crt Probability:",round(ewdata$priorprob_crt[ewdata$calfeartag == i],2),"\n Titre Cutpoints:", toString(cutpoints), "\n", "Age cutpoints:", toString(agebreaks)))
  
  ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/PosteriorProbCharts/QMMSPosCows/",i,".png"))
}


  

  group.colours <- c("L" = "green", "M" = "orange", "H" = "red")
  
  for (i in calfsamplerandom){
    ggplot(ewdata[ewdata$calfeartag == i,], aes(x = age)) +
      geom_point(aes(y = titre, color = class)) + 
      geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
      geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
      geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
      geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
      geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
      #geom_text(aes(y = -4, label = ifelse(POFloorApplied == 1,"POF","")), size = 2, angle = -90 ) +
      scale_color_manual(values = group.colours) +
      geom_line(aes(y = PosteriorProb*100)) +
      geom_line(aes(y = pred_gbm*100), color = "darkgray") +
      geom_text(aes(x = age, y = pred_gbm * 100, label = round(pred_gbm, 2), color = "darkgray"), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) +
      geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
      geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
      scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
      labs(title = i, subtitle = paste(crtmodel,"\nBirth Probability:", round(ewdata$priorprob_birth[ewdata$calfeartag == i],2),"12m Probability:",round(ewdata$priorprob_12mold[ewdata$calfeartag == i],2),"Crt Probability:",round(ewdata$priorprob_crt[ewdata$calfeartag == i],2),"\n Titre Cutpoints:", toString(cutpoints), "\n", "Age cutpoints:", toString(agebreaks)))
    
    ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/PosteriorProbCharts/RandomCows/",i,".png"))
  }

  group.colours <- c("L" = "green", "M" = "orange", "H" = "red")
  
  for (i in calfsampleqmms0){
    ggplot(ewdata[ewdata$calfeartag == i,], aes(x = age)) +
      geom_point(aes(y = titre, color = class)) + 
      geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
      geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) + 
      geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
      geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
      geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
      #(aes(y = -4, label = ifelse(POFloorApplied == 1,"POF","")), size = 2, angle = -90 ) +
      scale_color_manual(values = group.colours) +
      geom_line(aes(y = PosteriorProb*100)) +
      geom_line(aes(y = pred_gbm*100), color = "darkgray") +
      geom_text(aes(x = age, y = pred_gbm * 100, label = round(pred_gbm, 2), color = "darkgray"), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) +
      geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
      geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
      scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
      labs(title = i, subtitle = paste(crtmodel,"\nBirth Probability:", round(ewdata$priorprob_birth[ewdata$calfeartag == i],2),"12m Probability:",round(ewdata$priorprob_12mold[ewdata$calfeartag == i],2),"Crt Probability:",round(ewdata$priorprob_crt[ewdata$calfeartag == i],2),"\n Titre Cutpoints:", toString(cutpoints), "\n", "Age cutpoints:", toString(agebreaks)))
    
    ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/PosteriorProbCharts/QMMSNegCows/",i,".png"))
  }
  
  group.colours <- c("L" = "green", "M" = "orange", "H" = "red")
  
  for (i in calfsamplepofapplied){
    ggplot(ewdata[ewdata$calfeartag == i,], aes(x = age)) +
      geom_point(aes(y = titre, color = class)) + 
      geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
      geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) + 
      geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
      geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
      geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
      #(aes(y = -4, label = ifelse(POFloorApplied == 1,"POF","")), size = 2, angle = -90 ) +
      scale_color_manual(values = group.colours) +
      geom_line(aes(y = PosteriorProb*100)) +
      geom_line(aes(y = pred_gbm*100), color = "darkgray") +
      geom_text(aes(x = age, y = pred_gbm * 100, label = round(pred_gbm, 2), color = "darkgray"), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) +
      geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
      geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
      scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
      labs(title = i, subtitle = paste(crtmodel,"\nBirth Probability:", round(ewdata$priorprob_birth[ewdata$calfeartag == i],2),"12m Probability:",round(ewdata$priorprob_12mold[ewdata$calfeartag == i],2),"Crt Probability:",round(ewdata$priorprob_crt[ewdata$calfeartag == i],2),"\n Titre Cutpoints:", toString(cutpoints), "\n", "Age cutpoints:", toString(agebreaks)))
    
    ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/PosteriorProbCharts/QMMSPofApplied/",i,".png"))
  }
  
  
  
  
  #####CHECKING GROUP POSTERIORS#####
  
  print("Reading data (data_posteriors.csv)")
  data_postchecks <- read.csv("y:/ian/johnesthresholds/johnesproper/data/data_posteriors.csv")
  
  data_postchecks$age_cat <- cut(data_postchecks$age, breaks = 10)
  
  data_postchecks$nH <- str_count(data_postchecks$profile,"H")
  data_postchecks$nM <- str_count(data_postchecks$profile,"M")
  
  
  
  
  
  ######Define groups######
  
  print("Defining different profiles...")
  
  data_postchecks$profilegroup <- as.character("NONE") 
  
  
  
  data_postchecks$profilegroup <- ifelse(data_postchecks$Hanypoint == 0 & data_postchecks$nM == 0 &
                                           data_postchecks$ntests >= 8, 
                                         "AllLowAtLeast8Tests", data_postchecks$profilegroup)
  
  data_postchecks$profilegroup <- ifelse(data_postchecks$Hanypoint == 0 & data_postchecks$nM == 0 &
                                           data_postchecks$ntests < 8, 
                                         "AllLowAtFewerThan8Tests", data_postchecks$profilegroup)
  
  
  data_postchecks$profilegroup <- ifelse(data_postchecks$nH == 0 &
                                           data_postchecks$nM == 1 &
                                           grepl("M", substr(data_postchecks$profile, nchar(data_postchecks$profile), nchar(data_postchecks$profile))) == FALSE,
                                         "AllLowOneMAllLow", data_postchecks$profilegroup)
  
  data_postchecks$profilegroup <- ifelse(data_postchecks$nH == 0 &
                                           data_postchecks$nM == 2 &
                                           grepl("MM", substr(data_postchecks$profile, 1, nchar(data_postchecks$profile))) == TRUE &
                                           grepl("M", substr(data_postchecks$profile, nchar(data_postchecks$profile), nchar(data_postchecks$profile))) == FALSE,
                                         "AllLowTwoMAllLow", data_postchecks$profilegroup) 
  
  data_postchecks$profilegroup <- ifelse(data_postchecks$nH == 0 &
                                           data_postchecks$nM == 1 &
                                           grepl("M", substr(data_postchecks$profile, nchar(data_postchecks$profile), nchar(data_postchecks$profile))) == TRUE,
                                         "AllLowOneMFinal", data_postchecks$profilegroup) 
  
  data_postchecks$profilegroup <- ifelse(data_postchecks$nH == 0 &
                                           data_postchecks$nM == 2 &
                                           grepl("MM", substr(data_postchecks$profile, nchar(data_postchecks$profile)-1, nchar(data_postchecks$profile))) == TRUE,
                                         "AllLowTwoMFinal", data_postchecks$profilegroup) 
  
  data_postchecks$profilegroup <- ifelse(data_postchecks$nH == 0 &
                                           data_postchecks$nM >= 1,
                                         "SomeMNeverH", data_postchecks$profilegroup) 
  
  
  data_postchecks$profilegroup <- ifelse(data_postchecks$nH >= 2 &
                                           data_postchecks$HHanypoint == 0 &
                                           grepl("HMH", substr(data_postchecks$profile, 1, nchar(data_postchecks$profile))) == FALSE &
                                           grepl("HLH", substr(data_postchecks$profile, 1, nchar(data_postchecks$profile))) == FALSE,
                                         "SomeHNeverHHorHMHorHLH", data_postchecks$profilegroup) 
  
  
  
  
  
  
  data_postchecks$profilegroup <- ifelse(data_postchecks$nH == 1 &
                                           data_postchecks$nM == 0 &
                                           grepl("H", substr(data_postchecks$profile, nchar(data_postchecks$profile), nchar(data_postchecks$profile))) == FALSE,
                                         "AllLowOneHAllLow", data_postchecks$profilegroup)
  
  
  data_postchecks$profilegroup <- ifelse(data_postchecks$nH == 1 &
                                           data_postchecks$nM >= 1 &
                                           grepl("H", substr(data_postchecks$profile, nchar(data_postchecks$profile), nchar(data_postchecks$profile))) == FALSE,
                                         "OneHWithSomeMedium", data_postchecks$profilegroup)
  
  data_postchecks$profilegroup <- ifelse(data_postchecks$nH == 1 &
                                           data_postchecks$nM >= 1 &
                                           grepl("H", substr(data_postchecks$profile, nchar(data_postchecks$profile), nchar(data_postchecks$profile))) == TRUE,
                                         "OneHFinalWithSomeMedium", data_postchecks$profilegroup)
  
  data_postchecks$profilegroup <- ifelse(data_postchecks$nH == 2 &
                                           data_postchecks$nM == 0 &
                                           grepl("HH", substr(data_postchecks$profile, 1, nchar(data_postchecks$profile))) == TRUE &
                                           grepl("H", substr(data_postchecks$profile, nchar(data_postchecks$profile), nchar(data_postchecks$profile))) == FALSE,
                                         "AllLowTwoHAllLow", data_postchecks$profilegroup)
  
  
  data_postchecks$profilegroup <- ifelse(data_postchecks$nH == 2 &
                                           data_postchecks$nM == 0 &
                                           grepl("HH", substr(data_postchecks$profile, 1, nchar(data_postchecks$profile))) == TRUE &
                                           grepl("H", substr(data_postchecks$profile, nchar(data_postchecks$profile), nchar(data_postchecks$profile))) == FALSE,
                                         "AllLowTwoHAllLow", data_postchecks$profilegroup) 
  
  
  data_postchecks$profilegroup <- ifelse(data_postchecks$nH == 2 &
                                           data_postchecks$nM >= 1 &
                                           grepl("HH", substr(data_postchecks$profile, 1, nchar(data_postchecks$profile))) == TRUE &
                                           grepl("H", substr(data_postchecks$profile, nchar(data_postchecks$profile), nchar(data_postchecks$profile))) == FALSE,
                                         "TwoHWithSomeMedium", data_postchecks$profilegroup) 
  
  
  
  data_postchecks$profilegroup <- ifelse(data_postchecks$nH == 1 &
                                           data_postchecks$nM == 0 &
                                           grepl("H", substr(data_postchecks$profile, nchar(data_postchecks$profile), nchar(data_postchecks$profile))) == TRUE,
                                         "AllLowOneHFinal", data_postchecks$profilegroup) 
  
  data_postchecks$profilegroup <- ifelse(data_postchecks$nH == 2 &
                                           data_postchecks$nM == 0 &
                                           grepl("HH", substr(data_postchecks$profile, nchar(data_postchecks$profile)-1, nchar(data_postchecks$profile))) == TRUE,
                                         "AllLowTwoHFinal", data_postchecks$profilegroup)
  
  data_postchecks$profilegroup <- ifelse(data_postchecks$nH == 2 &
                                           data_postchecks$nM >= 1 &
                                           grepl("HH", substr(data_postchecks$profile, nchar(data_postchecks$profile)-1, nchar(data_postchecks$profile))) == TRUE,
                                         "TwoHFinalWithSomeMedium", data_postchecks$profilegroup)
  
  
  data_postchecks$profilegroup <- ifelse((grepl("HLH", substr(data_postchecks$profile, 1, nchar(data_postchecks$profile))) == TRUE |
                                            grepl("HMH", substr(data_postchecks$profile, 1, nchar(data_postchecks$profile))) == TRUE) &
                                           grepl("HH", substr(data_postchecks$profile, 1, nchar(data_postchecks$profile))) == FALSE,
                                         "HMHorHLHNeverHH", data_postchecks$profilegroup) 
  
  
  
  data_postchecks$profilegroup <- ifelse(data_postchecks$nM == 0 &
                                           grepl("LHHHH", substr(data_postchecks$profile, nchar(data_postchecks$profile)-4, nchar(data_postchecks$profile))) == TRUE,
                                         "AllLowLast4TestsH", data_postchecks$profilegroup)
  
  data_postchecks$profilegroup <- ifelse(data_postchecks$nM > 0 &
                                           grepl("HHHH", substr(data_postchecks$profile, nchar(data_postchecks$profile)-3, nchar(data_postchecks$profile))) == TRUE,
                                         "SomeMLast4TestsH", data_postchecks$profilegroup)
  
  
  data_postchecks$profilegroup <- factor(data_postchecks$profilegroup, levels = c("AllLowAtLeast8Tests", 
                                                                                  "AllLowAtFewerThan8Tests",
                                                                                  "AllLowOneMAllLow", 
                                                                                  "AllLowOneMFinal",
                                                                                  "AllLowTwoMAllLow", 
                                                                                  "AllLowTwoMFinal", 
                                                                                  "SomeMNeverH",
                                                                                  "AllLowOneHAllLow",
                                                                                  "AllLowOneHFinal",
                                                                                  "OneHWithSomeMedium", 
                                                                                  "OneHFinalWithSomeMedium",
                                                                                  "SomeHNeverHHorHMHorHLH",
                                                                                  "HMHorHLHNeverHH", 
                                                                                  "AllLowTwoHAllLow",
                                                                                  "AllLowTwoHFinal",
                                                                                  "TwoHWithSomeMedium",
                                                                                  "TwoHFinalWithSomeMedium",
                                                                                  "AllLowLast4TestsH",
                                                                                  "SomeMLast4TestsH",
                                                                                  "NONE"))
  
  ######Check Profiles######
  
  head(data_postchecks$profile[which(data_postchecks$profilegroup == "NONE")],100)
  
  
  
  ######Reference Ages######
  
  data_postchecks$refage <- as.numeric(0)
  
  data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "NONE", 0, data_postchecks$refage)
  
  data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "AllLowAtLeast8Tests", 0, data_postchecks$refage)
  
  data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "AllLowFewerThan8Tests", 0, data_postchecks$refage)
  
  data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "AllLowOneMAllLow", data_postchecks$ageatfirstM, data_postchecks$refage)
  
  data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "AllLowTwoMAllLow", data_postchecks$ageatfirstM, data_postchecks$refage)
  
  data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "AllLowOneMFinal", data_postchecks$ageatfirstM, data_postchecks$refage)
  
  data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "AllLowTwoMFinal", data_postchecks$ageatfirstM, data_postchecks$refage)
  
  data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "SomeMNeverH", data_postchecks$ageatfirstM, data_postchecks$refage)
  
  
  data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "AllLowOneHAllLow", data_postchecks$ageatfirstH, data_postchecks$refage)
  
  data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "OneHWithSomeMedium", data_postchecks$ageatfirstH, data_postchecks$refage)
  
  data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "AllLowTwoHAllLow", data_postchecks$ageatfirstH, data_postchecks$refage)
  
  data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "AllLowOneHFinal", data_postchecks$ageatfirstH, data_postchecks$refage)
  
  
  data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "SomeHNeverHHorHMHorHLH", data_postchecks$ageatfirstH, data_postchecks$refage)
  
  data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "OneHFinalWithSomeMedium", data_postchecks$ageatfirstH, data_postchecks$refage)
  
  data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "AllLowTwoHFinal", data_postchecks$ageatfirstH, data_postchecks$refage)
  
  
  data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "TwoHWithSomeMedium", data_postchecks$ageatfirstHH, data_postchecks$refage)
  
  data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "HMHorHLHNeverHH", data_postchecks$ageatfirstH, data_postchecks$refage)
  
  data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "TwoHFinalWithSomeMedium", data_postchecks$ageatfirstHH, data_postchecks$refage)
  
  data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "AllLowLast4TestsH", data_postchecks$ageatfirstH, data_postchecks$refage)
  
  data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "SomeMLast4TestsH", data_postchecks$ageatfirstH, data_postchecks$refage)
  
  data_postchecks$profilegroup <- as.factor(data_postchecks$profilegroup)
  
  data_postchecks <- data_postchecks[which(data_postchecks$profilegroup != "NONE"),]
  
  
  ######Predict ML model######
  
  
  data_postchecks$Target_QMMS_alpha <- as.factor(ifelse(data_postchecks$Target_QMMS == "1", "Pos", "Neg"))
  
  
  
  data_postchecks$testnum <- 1
  

  
  for (i in 2:nrow(data_postchecks)) {
    data_postchecks$testnum[i] <- ifelse(data_postchecks$calfeartag[i] != data_postchecks$calfeartag[i-1], 1,
                                data_postchecks$testnum[i-1] + 1)
    
  }
  
  
  
  no_cores <- detectCores()
  cl <- makePSOCKcluster(no_cores)
  registerDoParallel(cl)
  getDoParWorkers()
  
  
  tmp <- as.data.frame(foreach(i = 1:nrow(data_postchecks), .combine = "rbind") %dopar% {
    a <- i-2
    a <- ifelse(a <0,0,a)
    minagel3 <- ifelse(data_postchecks$testnum[i] < 3, 10000, min(data_postchecks$age[a:i]))
    maxagel3 <- ifelse(data_postchecks$testnum[i] < 3, 10000, max(data_postchecks$age[a:i]))
    meanagel3 <- ifelse(data_postchecks$testnum[i] < 3, 10000, mean(data_postchecks$age[a:i]))
    minmtncl3 <- ifelse(data_postchecks$testnum[i] < 3, 10000, min(data_postchecks$meantitrenegcows[a:i]))
    maxmtncl3 <- ifelse(data_postchecks$testnum[i] < 3, 10000, max(data_postchecks$meantitrenegcows[a:i]))
    meanmtncl3 <- ifelse(data_postchecks$testnum[i] < 3, 10000, mean(data_postchecks$meantitrenegcows[a:i]))
    minyieldl3 <- ifelse(data_postchecks$testnum[i] < 3, 10000, min(data_postchecks$yield[a:i]))
    maxyieldl3 <- ifelse(data_postchecks$testnum[i] < 3, 10000, max(data_postchecks$yield[a:i]))
    meanyieldl3 <- ifelse(data_postchecks$testnum[i] < 3, 10000, mean(data_postchecks$yield[a:i]))
    minbutterfatl3 <- ifelse(data_postchecks$testnum[i] < 3, 10000, min(data_postchecks$butterfat[a:i]))
    maxbutterfatl3 <- ifelse(data_postchecks$testnum[i] < 3, 10000, max(data_postchecks$butterfat[a:i]))
    meanbutterfatl3 <- ifelse(data_postchecks$testnum[i] < 3, 10000, mean(data_postchecks$butterfat[a:i]))
    minproteinl3 <- ifelse(data_postchecks$testnum[i] < 3, 10000, min(data_postchecks$protein[a:i]))
    maxproteinl3 <- ifelse(data_postchecks$testnum[i] < 3, 10000, max(data_postchecks$protein[a:i]))
    meanproteinl3 <- ifelse(data_postchecks$testnum[i] < 3, 10000, mean(data_postchecks$protein[a:i]))
    mincellcountl3 <- ifelse(data_postchecks$testnum[i] < 3, 10000, min(data_postchecks$cellcount[a:i]))
    maxcellcountl3 <- ifelse(data_postchecks$testnum[i] < 3, 10000, max(data_postchecks$cellcount[a:i]))
    meancellcountl3 <- ifelse(data_postchecks$testnum[i] < 3, 10000, mean(data_postchecks$cellcount[a:i]))
    mintitrel3 <- ifelse(data_postchecks$testnum[i] < 3, 10000, min(data_postchecks$titre[a:i]))
    maxtitrel3 <- ifelse(data_postchecks$testnum[i] < 3, 10000, max(data_postchecks$titre[a:i]))
    meantitrel3 <- ifelse(data_postchecks$testnum[i] < 3, 10000, mean(data_postchecks$titre[a:i]))
    minage <- ifelse(data_postchecks$testnum[i] < 3, 10000, min(data_postchecks$age[data_postchecks$calfeartag == data_postchecks$calfeartag[i] &
                                                                    data_postchecks$testnum <= data_postchecks$testnum[i]]))
    maxage <- ifelse(data_postchecks$testnum[i] < 3, 10000, max(data_postchecks$age[data_postchecks$calfeartag == data_postchecks$calfeartag[i] &
                                                                    data_postchecks$testnum <= data_postchecks$testnum[i]]))
    meanage <- ifelse(data_postchecks$testnum[i] < 3, 10000, mean(data_postchecks$age[data_postchecks$calfeartag == data_postchecks$calfeartag[i] &
                                                                      data_postchecks$testnum <= data_postchecks$testnum[i]]))
    minmtnc <- ifelse(data_postchecks$testnum[i] < 3, 10000, min(data_postchecks$meantitrenegcows[data_postchecks$calfeartag == data_postchecks$calfeartag[i] &
                                                                                  data_postchecks$testnum <= data_postchecks$testnum[i]]))
    maxmtnc <- ifelse(data_postchecks$testnum[i] < 3, 10000, max(data_postchecks$meantitrenegcows[data_postchecks$calfeartag == data_postchecks$calfeartag[i] &
                                                                                  data_postchecks$testnum <= data_postchecks$testnum[i]]))
    meanmtnc <- ifelse(data_postchecks$testnum[i] < 3, 10000, mean(data_postchecks$meantitrenegcows[data_postchecks$calfeartag == data_postchecks$calfeartag[i] &
                                                                                    data_postchecks$testnum <= data_postchecks$testnum[i]]))
    minyield <- ifelse(data_postchecks$testnum[i] < 3, 10000, min(data_postchecks$yield[data_postchecks$calfeartag == data_postchecks$calfeartag[i] &
                                                                        data_postchecks$testnum <= data_postchecks$testnum[i]]))
    maxyield <- ifelse(data_postchecks$testnum[i] < 3, 10000, max(data_postchecks$yield[data_postchecks$calfeartag == data_postchecks$calfeartag[i] &
                                                                        data_postchecks$testnum <= data_postchecks$testnum[i]]))
    meanyield <- ifelse(data_postchecks$testnum[i] < 3, 10000, mean(data_postchecks$yield[data_postchecks$calfeartag == data_postchecks$calfeartag[i] &
                                                                          data_postchecks$testnum <= data_postchecks$testnum[i]]))
    minbutterfat <- ifelse(data_postchecks$testnum[i] < 3, 10000, min(data_postchecks$butterfat[data_postchecks$calfeartag == data_postchecks$calfeartag[i] &
                                                                                data_postchecks$testnum <= data_postchecks$testnum[i]]))
    maxbutterfat <- ifelse(data_postchecks$testnum[i] < 3, 10000, max(data_postchecks$butterfat[data_postchecks$calfeartag == data_postchecks$calfeartag[i] &
                                                                                data_postchecks$testnum <= data_postchecks$testnum[i]]))
    meanbutterfat <- ifelse(data_postchecks$testnum[i] < 3, 10000, mean(data_postchecks$butterfat[data_postchecks$calfeartag == data_postchecks$calfeartag[i] &
                                                                                  data_postchecks$testnum <= data_postchecks$testnum[i]]))
    minprotein <- ifelse(data_postchecks$testnum[i] < 3, 10000, min(data_postchecks$protein[data_postchecks$calfeartag == data_postchecks$calfeartag[i] &
                                                                            data_postchecks$testnum <= data_postchecks$testnum[i]]))
    maxprotein <- ifelse(data_postchecks$testnum[i] < 3, 10000, max(data_postchecks$protein[data_postchecks$calfeartag == data_postchecks$calfeartag[i] &
                                                                            data_postchecks$testnum <= data_postchecks$testnum[i]]))
    meanprotein <- ifelse(data_postchecks$testnum[i] < 3, 10000, mean(data_postchecks$protein[data_postchecks$calfeartag == data_postchecks$calfeartag[i] &
                                                                              data_postchecks$testnum <= data_postchecks$testnum[i]]))
    mincellcount <- ifelse(data_postchecks$testnum[i] < 3, 10000, min(data_postchecks$cellcount[data_postchecks$calfeartag == data_postchecks$calfeartag[i] &
                                                                                data_postchecks$testnum <= data_postchecks$testnum[i]]))
    maxcellcount <- ifelse(data_postchecks$testnum[i] < 3, 10000, max(data_postchecks$cellcount[data_postchecks$calfeartag == data_postchecks$calfeartag[i] &
                                                                                data_postchecks$testnum <= data_postchecks$testnum[i]]))
    meancellcount <- ifelse(data_postchecks$testnum[i] < 3, 10000, mean(data_postchecks$cellcount[data_postchecks$calfeartag == data_postchecks$calfeartag[i] &
                                                                                  data_postchecks$testnum <= data_postchecks$testnum[i]]))
    mintitre <- ifelse(data_postchecks$testnum[i] < 3, 10000, min(data_postchecks$titre[data_postchecks$calfeartag == data_postchecks$calfeartag[i] &
                                                                        data_postchecks$testnum <= data_postchecks$testnum[i]]))
    maxtitre <- ifelse(data_postchecks$testnum[i] < 3, 10000, max(data_postchecks$titre[data_postchecks$calfeartag == data_postchecks$calfeartag[i] &
                                                                        data_postchecks$testnum <= data_postchecks$testnum[i]]))
    meantitre <- ifelse(data_postchecks$testnum[i] < 3, 10000, mean(data_postchecks$titre[data_postchecks$calfeartag == data_postchecks$calfeartag[i] &
                                                                          data_postchecks$testnum <= data_postchecks$testnum[i]]))
    
    
    c(minage, 
      maxage,
      meanage,
      minmtnc, 
      maxmtnc,
      meanmtnc,
      minyield, 
      maxyield,
      meanyield,
      minbutterfat, 
      maxbutterfat,
      meanbutterfat,
      minprotein, 
      maxprotein,
      meanprotein,
      mincellcount, 
      maxcellcount,
      meancellcount,
      mintitre, 
      maxtitre,
      meantitre,
      minagel3, 
      maxagel3,
      meanagel3,
      minmtncl3, 
      maxmtncl3,
      meanmtncl3,
      minyieldl3, 
      maxyieldl3,
      meanyieldl3,
      minbutterfatl3, 
      maxbutterfatl3,
      meanbutterfatl3,
      minproteinl3, 
      maxproteinl3,
      meanproteinl3,
      mincellcountl3, 
      maxcellcountl3,
      meancellcountl3,
      mintitrel3, 
      maxtitrel3,
      meantitrel3)
  })
  
  stopCluster(cl)
  
  
  
  
  
  colnames(tmp) <- c("minage", 
                     "maxage",
                     "meanage",
                     "minmtnc", 
                     "maxmtnc",
                     "meanmtnc",
                     "minyield", 
                     "maxyield",
                     "meanyield",
                     "minbutterfat", 
                     "maxbutterfat",
                     "meanbutterfat",
                     "minprotein", 
                     "maxprotein",
                     "meanprotein",
                     "mincellcount", 
                     "maxcellcount",
                     "meancellcount",
                     "mintitre", 
                     "maxtitre",
                     "meantitre",
                     "minagel3", 
                     "maxagel3",
                     "meanagel3",
                     "minmtncl3", 
                     "maxmtncl3",
                     "meanmtncl3",
                     "minyieldl3", 
                     "maxyieldl3",
                     "meanyieldl3",
                     "minbutterfatl3", 
                     "maxbutterfatl3",
                     "meanbutterfatl3",
                     "minproteinl3", 
                     "maxproteinl3",
                     "meanproteinl3",
                     "mincellcountl3", 
                     "maxcellcountl3",
                     "meancellcountl3",
                     "mintitrel3", 
                     "maxtitrel3",
                     "meantitrel3")
  
  data_postchecks <- cbind(data_postchecks, tmp)
  
  
  data_postchecks_test3plus <- data_postchecks[data_postchecks$testnum >= 3, c(1,2,191,160:170,192:233)]
  
  data_postchecks_test3plus <- data_postchecks_test3plus[complete.cases(data_postchecks_test3plus) == TRUE,]
  
  data_postchecks_test3plus$pred_gbm <- as.numeric(predict(ew_model_gbm,
                                               newdata = data_postchecks_test3plus,
                                               type = "prob")[,1])
  
  data_postchecks <- merge(data_postchecks, data_postchecks_test3plus[,c(1,2,4,56)], 
                           by = c("Farm", "calfeartag", "age"), 
                           all.x = TRUE)
  
  
  ######Plots######
  
  ggplot(data_postchecks[which((data_postchecks$age - data_postchecks$refage) == 0 | data_postchecks$refage == 0),], aes(x= profilegroup, y = pred_gbm)) +
    geom_boxplot() +
    theme(axis.text.x=element_text(angle= -90)) +
    labs(title = "ML Probability on day of period of interest", x = "Profile", y = "ML Probability")
  
  
  