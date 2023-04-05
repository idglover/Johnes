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


