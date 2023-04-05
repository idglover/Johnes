CVMods <- c("multinom_model_re_age",
"multinom_model_re_age_inter",
"multinom_model_re_agesq_inter",
"multinom_model_re_agesq_inter_mtncsq_proteinsq_cellcountcat_yieldcat",
"multinom_model_re_agesq_inter_proteinsq_cellcountcat_yieldcat",
"multinom_model_re_agesq_inter_mtncsq_cellcountcat_yieldcat",
"multinom_model_re_agesq_inter_mtncsq_proteinsq_yieldcat",
"multinom_model_re_agesq_inter_mtncsq_proteinsq_cellcountcat",
"multinom_model_re_agesq_inter_mtncsq_proteinsq",
"multinom_model_re_agesq_inter_mtncsq_cellcountcat",
"multinom_model_re_agesq_inter_mtncsq_yieldcat",
"multinom_model_re_agesq_inter_proteinsq_cellcountcat",
"multinom_model_re_agesq_inter_proteinsq_yieldcat",
"multinom_model_re_agesq_inter_cellcountcat_yieldcat",
"multinom_model_re_agesq_inter_mtncsq",
"multinom_model_re_agesq_inter_proteinsq",
"multinom_model_re_agesq_inter_cellcountcat",
"multinom_model_re_agesq_inter_yieldcat")

####READ DATA####

data <- read.csv("Y:/Ian/JohnesThresholds/JohnesProper/Data/data_birthpriors.csv")

print("Cleaning data...")

data$Target_QMMS <- as.factor(data$Target_QMMS)

data$titre <- as.numeric(data$titre)

data <- data[which(data$yield <= 70 &
               data$protein <=7 &
               data$cellcount < 10000 &
               data$butterfat < 10 &
               data$age >= 24 &
               !is.na(data$age)),]




data$age_cat <- cut(as.numeric(data$age), breaks = agebreaks)

data$yield_cat <- cut(data$yield, breaks = c(0,10,20,30,40,50,70))
levels(data$yield_cat) <- c(levels(data$yield_cat), "Missing")
data$yield_cat[which(is.na(data$yield_cat))] <- "Missing"

data$protein_cat <- cut(data$protein, breaks = c(0,3,3.5,4,4.5,7))
levels(data$protein_cat) <- c(levels(data$protein_cat), "Missing")
data$protein_cat[which(is.na(data$protein_cat))] <- "Missing"

data$butterfat_cat <- cut(data$butterfat, breaks = c(0,2,4,6,10))
levels(data$butterfat_cat) <- c(levels(data$butterfat_cat), "Missing")
data$butterfat_cat[which(is.na(data$butterfat_cat))] <- "Missing"

data$cellcount_cat <- cut(data$cellcount, breaks = c(0,50,100,150,200,500,1000,10000))
levels(data$cellcount_cat) <- c(levels(data$cellcount_cat), "Missing")
data$cellcount_cat[which(is.na(data$cellcount_cat))] <- "Missing"




print("Creating titre cutpoints...")

data$cutpoint <- length(cutpoints)


time1 <- Sys.time()

source_python("Y:/Ian/JohnesThresholds/JohnesProper/Data/PythonScripts/PyCreateCutpoints.py")

data <- createcutpoints(data, cutpoints)




time2 <- Sys.time()

print(paste("Time taken assigning cutpoints:", round(difftime(time2, time1, units = "mins"),2), "mins"))

print("Titre cutpoints:")
print(head(cbind(data$titre, data$cutpoint),30))


data$cutpoint <- as.factor(data$cutpoint)



####Create data for multinomial model (NNET Package)

print("Creating data for multinomial likelihood models...")

if(target == "MEYER"){
data_multinom <- data[data$Target_Meyer != "U",]
data_multinom$Target_Meyer <- droplevels(data_multinom$Target_Meyer, "U")
data_multinom$Target_Meyer <- as.factor(data_multinom$Target_Meyer)
}

if(target == "QMMS"){
  data_multinom <- data[data$Target_QMMS != "U",]
  data_multinom$Target_QMMS <- as.factor(data_multinom$Target_QMMS)
  data_multinom$Target_QMMS <- droplevels(data_multinom$Target_QMMS, "U")
  
  
}





data_multinom$age <- as.numeric(data_multinom$age)

data_multinom$yield <- as.numeric(data_multinom$yield)

data_multinom$cellcount <- as.numeric(data_multinom$cellcount)

data_multinom$dim <- as.numeric(data_multinom$dim)

data_multinom$meantitrenegcows <- as.numeric(data_multinom$meantitrenegcows)

data_multinom$butterfat <- as.numeric(data_multinom$butterfat)

data_multinom$protein <- as.numeric(data_multinom$protein)


data_multinom$agesq <- data_multinom$age^2
data_multinom$yieldsq <- data_multinom$yield^2
data_multinom$proteinsq <- data_multinom$protein^2
data_multinom$butterfatsq <- data_multinom$butterfat^2
data_multinom$meantitrenegcowssq <- data_multinom$meantitrenegcows^2

data_multinom$cellcount[which(data_multinom$cellcount == 0)] <- 0.0000001
data_multinom$cellcount[which(data_multinom$cellcount < 0)] <- NA
data_multinom$cellcountlog <- log(data_multinom$cellcount)

data_multinom$yield_cat <- cut(data_multinom$yield, breaks = c(0,10,20,30,40,50,70))
levels(data_multinom$yield_cat) <- c(levels(data_multinom$yield_cat), "Missing")
data_multinom$yield_cat[which(is.na(data_multinom$yield_cat))] <- "Missing"

data_multinom$wt <- 1

if(weightedlikelihood == "Y"){






data_multinom$wt <- ifelse(data_multinom$Target_QMMS == "1", 8,1)

}


uniquefarms <- unique(data_multinom$Farm)

trainfarms <- sample(uniquefarms, 63, replace = FALSE)
testfarms <- uniquefarms[!(uniquefarms %in% trainfarms)]

data_multinom_train <- data_multinom[data_multinom$Farm %in% trainfarms,]
data_multinom_test <- data_multinom[data_multinom$Farm %in% testfarms,]



cat(paste0("Trainset: \n",dim(data_multinom_train)[1],
           " rows (",round(dim(data_multinom_train)[1]/(dim(data_multinom_test)[1] + dim(data_multinom_train)[1])*100,1),"%)"),
    "\nPositive Target:", summary(as.factor(data_multinom_train$Target_QMMS))[[2]], "rows",
    "(", round(summary(as.factor(data_multinom_train$Target_QMMS))[[2]]/nrow(data_multinom_train)*100,1),"%)")

cat(paste0("testset: \n",dim(data_multinom_test)[1],
           " rows (",round(dim(data_multinom_test)[1]/(dim(data_multinom_train)[1] + dim(data_multinom_test)[1])*100,1),"%)"),
    "\nPositive Target:", summary(as.factor(data_multinom_test$Target_QMMS))[[2]], "rows",
    "(", round(summary(as.factor(data_multinom_test$Target_QMMS))[[2]]/nrow(data_multinom_test)*100,1),"%)")



####CROSS VALIDATE TARGET_QMMS MODELS#######

if(target == "QMMS"){
  
  #Likelihood models continuous age
  
  
  print("Cross Validating likelihood models...")
  
  
  time1 <- Sys.time()
  
  nfolds <- 5
  nrepeats <- 5
  
  nmodstorun <- length(CVMods) * nfolds * nrepeats
  nmodsrun <- 0
  
  no_cores <- detectCores() 
  cl <- makePSOCKcluster(no_cores)
  registerDoParallel(cl)
  getDoParWorkers() 
  
  
  
  CVResults <- foreach(modeltorun = CVMods, .combine = "rbind") %do% {
    
    modresults <- foreach(k = 1:nrepeats, .combine = "rbind") %do% {
      
      set.seed(sample(1:10000,1))
      
      folds <- groupKFold(data_multinom_train$Farm, k = nfolds)
      
      represults <- foreach(j = 1:nfolds, .combine = "rbind", .packages = "mclogit") %dopar% {
        
        trainset <- data_multinom_train[folds[[j]],]
        testset <- data_multinom_train[-folds[[j]],]
        
        
        if (modeltorun == "multinom_model_re_age"){
          mod <- mblogit(cutpoint ~ age + Target_QMMS, random = ~1|Farm, data = trainset)
        }
        
        if (modeltorun == "multinom_model_re_age_inter"){
          mod <- mblogit(cutpoint ~ age * Target_QMMS, random = ~1|Farm, data = trainset)
        }
        
        if (modeltorun == "multinom_model_re_agesq_inter"){
          mod <- mblogit(cutpoint ~ age * Target_QMMS + agesq * Target_QMMS, random = ~1|Farm, data = trainset)
        }
        
        if (modeltorun == "multinom_model_re_agesq_inter_mtncsq_proteinsq_cellcountcat_yieldcat"){
          mod <- mblogit(cutpoint ~ age * Target_QMMS + agesq * Target_QMMS + meantitrenegcows + meantitrenegcowssq + protein + proteinsq + cellcount_cat + yield_cat, random = ~1|Farm, data = trainset)
        }
        
        if (modeltorun == "multinom_model_re_agesq_inter_proteinsq_cellcountcat_yieldcat"){
          mod <- mblogit(cutpoint ~ age * Target_QMMS + agesq * Target_QMMS + protein + proteinsq + cellcount_cat + yield_cat, random = ~1|Farm, data = trainset)
        }
        
        if (modeltorun == "multinom_model_re_agesq_inter_mtncsq_cellcountcat_yieldcat"){
          mod <- mblogit(cutpoint ~ age * Target_QMMS + agesq * Target_QMMS + meantitrenegcows + meantitrenegcowssq + cellcount_cat + yield_cat, random = ~1|Farm, data = trainset)
        }
        
        if (modeltorun == "multinom_model_re_agesq_inter_mtncsq_proteinsq_yieldcat"){
          mod <- mblogit(cutpoint ~ age * Target_QMMS + agesq * Target_QMMS + meantitrenegcows + meantitrenegcowssq + protein + proteinsq + yield_cat, random = ~1|Farm, data = trainset)
        }
        
        if (modeltorun == "multinom_model_re_agesq_inter_mtncsq_proteinsq_cellcountcat"){
          mod <- mblogit(cutpoint ~ age * Target_QMMS + agesq * Target_QMMS + meantitrenegcows + meantitrenegcowssq + protein + proteinsq + cellcount_cat, random = ~1|Farm, data = trainset)
        }
        
        if (modeltorun == "multinom_model_re_agesq_inter_mtncsq_proteinsq"){
          mod <- mblogit(cutpoint ~ age * Target_QMMS + agesq * Target_QMMS + meantitrenegcows + meantitrenegcowssq + protein + proteinsq, random = ~1|Farm, data = trainset)
        }
        
        if (modeltorun == "multinom_model_re_agesq_inter_mtncsq_cellcountcat"){
          mod <- mblogit(cutpoint ~ age * Target_QMMS + agesq * Target_QMMS + meantitrenegcows + meantitrenegcowssq + cellcount_cat, random = ~1|Farm, data = trainset)
        }
        
        if (modeltorun == "multinom_model_re_agesq_inter_mtncsq_yieldcat"){
          mod <- mblogit(cutpoint ~ age * Target_QMMS + agesq * Target_QMMS + meantitrenegcows + meantitrenegcowssq + yield_cat, random = ~1|Farm, data = trainset)
        }
        
        if (modeltorun == "multinom_model_re_agesq_inter_proteinsq_cellcountcat"){
          mod <- mblogit(cutpoint ~ age * Target_QMMS + agesq * Target_QMMS + protein + proteinsq + cellcount_cat, random = ~1|Farm, data = trainset)
        }
        
        if (modeltorun == "multinom_model_re_agesq_inter_proteinsq_yieldcat"){
          mod <- mblogit(cutpoint ~ age * Target_QMMS + agesq * Target_QMMS + protein + proteinsq + yield_cat, random = ~1|Farm, data = trainset)
        }
        
        if (modeltorun == "multinom_model_re_agesq_inter_cellcountcat_yieldcat"){
          mod <- mblogit(cutpoint ~ age * Target_QMMS + agesq * Target_QMMS + cellcount_cat + yield_cat, random = ~1|Farm, data = trainset)
        }
        
        if (modeltorun == "multinom_model_re_agesq_inter_mtncsq"){
          mod <- mblogit(cutpoint ~ age * Target_QMMS + agesq * Target_QMMS + meantitrenegcows + meantitrenegcowssq, random = ~1|Farm, data = trainset)
        }
        
        if (modeltorun == "multinom_model_re_agesq_inter_proteinsq"){
          mod <- mblogit(cutpoint ~ age * Target_QMMS + agesq * Target_QMMS + protein + proteinsq, random = ~1|Farm, data = trainset)
        }
        
        if (modeltorun == "multinom_model_re_agesq_inter_cellcountcat"){
          mod <- mblogit(cutpoint ~ age * Target_QMMS + agesq * Target_QMMS + cellcount_cat, random = ~1|Farm, data = trainset)
        }
        
        if (modeltorun == "multinom_model_re_agesq_inter_yieldcat"){
          mod <- mblogit(cutpoint ~ age * Target_QMMS + agesq * Target_QMMS + yield_cat, random = ~1|Farm, data = trainset)
        }
        
        pred <- tryCatch({predict(mod, newdata=testset, type="response", conditional = FALSE)},
                         error=function(cond) {
                           print("Error")
                           return("FAILED")})
        
        obs <- testset$cutpoint
        
        foldresults <- data.frame(modeltorun, k, j, pred, obs)
        
        
        
        return(foldresults)
        
        
        
      }
      
      nmodsrun <- nmodsrun + nfolds
      
      message(paste0("Done ", nmodsrun,"/",nmodstorun," (", round(nmodsrun/nmodstorun*100,1),"%)"))
      
      return(represults)
      
    }
    
    write.csv(modresults, paste0("y:/ian/johnesthresholds/johnesproper/data/likelihoodcrossvalidation/",modeltorun,"CV.csv"), row.names = FALSE)
    return(modresults)
    
    }
  }
  
  
  
  stopCluster(cl)
  
  colnames(CVResults) <- c("Model", 
                           "Rep", 
                           "Fold", 
                           "Pred1", 
                           "Pred2",
                           "Pred3",
                           "Pred4",
                           "Pred5",
                           "Pred6",
                           "Pred7",
                           "Obs")
        
  
  
  time2 <- Sys.time()
  
  print(paste("Time taken for model cross validation:", round(difftime(time2, time1, units = "mins"),1), "mins"))
  
  print("Writing cross validation results (CVResults_TQ.csv)...")
  
  write.csv(CVResults,  paste0("y:/ian/johnesthresholds/johnesproper/data/LikelihoodCrossValidation/CVResults_TQ.csv"), row.names = FALSE)
  
  
  
  print("Generating CaliPlots and extracting calibration errors...")
  
  time1 <- Sys.time()
  
  caliberrortable <- data.frame(Model = character(), Cutpoint = numeric(), ECE = numeric(), MCE = numeric(), MeanECEAllCuts = numeric(), MeanMCEAllCuts = numeric())
  
  for (eta in 1:length(CVMods)){
    
    CPData <- CVResults[CVResults$Model == CVMods[eta],]
    for (zeta in 1:length(cutpoints)){
      CPData$correctcut <- ifelse(CPData$Obs == zeta, 1, 0)
      calibtemp <- cbind(CVMods[eta], zeta, ExtractECE(CPData[,3+zeta], CPData$correctcut, nbins = 10),ExtractMCE(CPData[,3+zeta], CPData$correctcut, nbins = 10), 0, 0)
      colnames(calibtemp) <- c("Model", "Cutpoint", "ECE", "MCE", "MeanECEAllCuts", "MeanMCEAllCuts")
      caliberrortable <- rbind(caliberrortable, calibtemp)
      print(CaliPlot(CPData[,3+zeta], CPData$correctcut, 
               nbins = 10, 
               ptitle = CVMods[eta], 
               psubtitle = paste("Titre Cutpoints:", toString(cutpoints), "\nAge Cutpoints:", toString(agebreaks), "\nCutpoint", zeta)))
      ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/LikelihoodCrossValidation/CVCaliPlots/MNCaliPlot",birthmodel,CVMods[eta],"Titres",toString(cutpoints),"Ages",toString(agebreaks),"Cut",zeta,".png"))
    }
    
    
  }
  
  
  
  
  
  for (eta in 1:length(CVMods)){
    caliberrortable$MeanECEAllCuts[caliberrortable$Model == CVMods[eta]] <- mean(as.numeric(caliberrortable$ECE[caliberrortable$Model == CVMods[eta] & caliberrortable$ECE != 0]))
    caliberrortable$MeanMCEAllCuts[caliberrortable$Model == CVMods[eta]] <- mean(as.numeric(caliberrortable$MCE[caliberrortable$Model == CVMods[eta] & caliberrortable$MCE != 0]))
  }
  
  write.csv(caliberrortable, paste0("y:/ian/johnesthresholds/johnesproper/data/LikelihoodCrossValidation/CVCalibErrors",birthmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),nbins,"bins.csv"))
  
  plotdata <- caliberrortable[caliberrortable$Cutpoint == 1,-c(2:4)]
  
  plotdata$MeanECEAllCuts <- as.numeric(plotdata$MeanECEAllCuts)
  plotdata$MeanMCEAllCuts <- as.numeric(plotdata$MeanMCEAllCuts)
  
  print(ggplot(plotdata, aes(x = Model)) +
          geom_bar(aes(y = MeanECEAllCuts), stat = "identity", fill = "green") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
          labs(title = "Cross Validated Calibration Errors: Mean ECE Across Cutpoints", subtitle = paste("Titre Cutpoints:", toString(cutpoints), "\nAge Cutpoints:", toString(agebreaks))))
  
  
  
  print(ggplot(plotdata, aes(x = Model)) +
          geom_bar(aes(y = MeanMCEAllCuts), stat = "identity", fill = "darkred") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
          labs(title = "Cross Validated Calibration Errors: Mean MCE Across Cutpoints", subtitle = paste("Titre Cutpoints:", toString(cutpoints), "\nAge Cutpoints:", toString(agebreaks))))
  
  
  ggplot(caliberrortable, aes(x = Model, y = as.numeric(ECE))) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(title = "ECE All Models All Cutpoints", subtitle = paste("Titre Cutpoints:", toString(cutpoints), "\nAge Cutpoints:", toString(agebreaks))) +
    geom_hline(yintercept = 0, linetype = "dashed")
  ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/LikelihoodCrossValidation/ModelECE",birthmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),".png"))
  
  ggplot(caliberrortable, aes(x = Model, y = as.numeric(MCE))) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(title = "MCE All Models All Cutpoints", subtitle = paste("Titre Cutpoints:", toString(cutpoints), "\nAge Cutpoints:", toString(agebreaks))) +
    geom_hline(yintercept = 0, linetype = "dashed")
  ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/LikelihoodCrossValidation/ModelMCE",birthmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),".png"))
  
  
  time2 <- Sys.time()
  
  print(paste("Time taken for CaliPlots and calibration errors:", round(difftime(time2, time1, units = "mins"),1),"mins"))
  
  
  
  
  time2 <- Sys.time()
  
  print(paste("Total time taken for cross-validation:", difftime(time2, time0, units = "mins"), "mins"))
  
}

#####Fit chosen likelihood model on trainset and test on testset#####



finallikemodel <- mblogit(cutpoint ~ age * Target_QMMS + agesq * Target_QMMS + yield_cat, random = ~1|Farm, data = data_multinom_train, control = mmclogit.control(maxit = 100))

saveRDS(finallikemodel, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/finallikemodel.rds")

pred <- predict(finallikemodel, newdata=data_multinom_test, type="response", conditional = FALSE)

for (zeta in 1:length(cutpoints)){
  correctcut <- ifelse(data_multinom_test$cutpoint == zeta, 1, 0)
  
  print(CaliPlot(pred[,zeta], correctcut, 
                 nbins = 10, 
                 psubtitle = paste("Titre Cutpoints:", toString(cutpoints), "\nAge Cutpoints:", toString(agebreaks), "\nCutpoint", zeta)))
  ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/LikelihoodCrossValidation/CVCaliPlots/MNCaliPlotTestSetCut",zeta,".png"))
}

rm(multinom_model_re_age)
rm(multinom_model_re_age_inter)
rm(multinom_model_re_agesq_inter)
rm(multinom_model_re_agesq_inter_mtncsq_proteinsq_scclog_yieldsq)
rm(multinom_model_re_agesq_inter_mtncsq_proteinsq_yieldsq)
rm(multinom_model_re_agesq_inter_mtncsq_scclog_yieldsq)
rm(multinom_model_re_agesq_inter_mtncsq_cellcountcat_yieldcat)
rm(multinom_model_re_agesq_inter_mtncsq_scclog_yieldsq)
rm(multinom_model_re_agesq_inter_proteinsq_scclog_yieldsq)
rm(multinom_model_re_agesq_inter_mtncsq_proteinsq_scclog)
rm(multinom_model_re_agesq_inter_mtncsq_proteinsq)
rm(multinom_model_re_agesq_inter_mtncsq_scclog)
rm(multinom_model_re_agesq_inter_mtncsq_yieldsq)
rm(multinom_model_re_agesq_inter_proteinsq_scclog)
rm(multinom_model_re_agesq_inter_proteinsq_yieldsq)
rm(multinom_model_re_agesq_inter_scclog_yieldsq)
rm(multinom_model_re_agesq_inter_mtncsq)
rm(multinom_model_re_agesq_inter_proteinsq)
rm(multinom_model_re_agesq_inter_scclog)
rm(multinom_model_re_agesq_inter_yieldsq)


rm(CVResults)
rm(CVResults_nona)
rm(altCVResults)
rm(caliberrortable)
rm(data)

