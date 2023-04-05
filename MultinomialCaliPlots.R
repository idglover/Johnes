time0 <- Sys.time()

#models <- c("multinom_model_age",
           # "multinom_model_age_inter",
          #  "multinom_model_agesq_inter",
          #  "multinom_model_agecb_inter",
          #  "multinom_model_age_inter_dim",
          #  "multinom_model_age_inter_yield",
          #  "multinom_model_age_inter_yieldsq",
          #  "multinom_model_age_inter_yield_mtnc",
          #  "multinom_model_age_inter_mtncsq",
          #  "multinom_model_age_inter_yield_mtnc_scc",
          #  "multinom_model_age_inter_scclog",
          #  "multinom_model_age_inter_yield_mtnc_bf",
          #  "multinom_model_age_inter_yield_mtnc_protein",
          #  "multinom_model_age_inter_yield_mtnc_scc_protein",
          #  "multinom_model_age_inter_proteinsq",
          #  "multinom_model_age_cat", 
          #  "multinom_model_age_cat_inter", 
          #  "multinom_model_age_cat_inter_dim",
          #  "multinom_model_age_cat_inter_yield",
          #  "multinom_model_age_cat_inter_yield_mtnc",
          #  "multinom_model_age_cat_inter_yield_mtnc_scc",
          #  "multinom_model_age_cat_inter_logscc",
          #  "multinom_model_age_cat_inter_yield_mtnc_bf",
          #  "multinom_model_age_cat_inter_yield_mtnc_protein",
          #  "multinom_model_age_cat_inter_yield_mtnc_scc_protein")

#Smaller model set for assessing continuous variable transformations

#models <- c(

#"multinom_model_age_inter",
#"multinom_model_agesq_inter",
#"multinom_model_agesq_inter_yieldsq",
#"multinom_model_agesq_inter_mtncsq",
#"multinom_model_agesq_inter_scclog",
#"multinom_model_agesq_inter_proteinsq"

#)

#Inclusion of transformed continuous variables

models <- c(
  "multinom_model_agesq_inter",
  "multinom_model_agesq_inter_yieldsq",
  "multinom_model_agesq_inter_mtncsq",
  "multinom_model_agesq_inter_scclog",
  "multinom_model_agesq_inter_proteinsq",
  "multinom_model_agesq_inter_mtncsq_proteinsq_scclog_yieldsq",
  "multinom_model_agesq_inter_mtncsq_scclog_yieldsq",
  "multinom_model_agesq_inter_mtncsq_proteinsq_yieldsq",
  "multinom_model_agesq_inter_proteinsq_scclog_yieldsq",
  "multinom_model_agesq_inter_mtncsq_proteinsq_scclog",
  "multinom_model_agesq_inter_mtncsq_proteinsq",
  "multinom_model_agesq_inter_mtncsq_scclog",
  "multinom_model_agesq_inter_mtncsq_yieldsq",
  "multinom_model_agesq_inter_proteinsq_scclog",
  "multinom_model_agesq_inter_proteinsq_yieldsq",
  "multinom_model_agesq_inter_scclog_yieldsq"
)





print("Reading data....")
data <- read.csv("Y:/Ian/JohnesThresholds/JohnesProper/Data/data_birthpriors.csv")

print("Cleaning data...")

data$Target_Meyer <- as.factor(data$Target_Meyer)

data$titre <- as.numeric(data$titre)

data <- data[data$yield > 0 &
               !is.na(data$yield) &
               data$yield <= 70 &
               data$protein > 0 &
               !is.na(data$protein) &
               data$protein <=7 &
               data$cellcount >0 &
               !is.na(data$cellcount) &
               data$cellcount < 10000 &
               data$butterfat >0 &
               !is.na(data$butterfat) &
               data$butterfat < 10 &
               data$age >= 24 &
               !is.na(data$age),]

data$age_cat <- cut(as.numeric(data$age), breaks = agebreaks)

print("Creating titre cutpoints...")

data$cutpoint <- length(cutpoints)

cl <- makePSOCKcluster(5)
registerDoParallel(cl)
cat("No. Cores:",getDoParWorkers())

time1 <- Sys.time()

for (i in 1:nrow(data)){
  for (j in 1:length(cutpoints)-1){
    if(data$titre[i] >= cutpoints[j] && data$titre[i] < cutpoints[j+1]){data$cutpoint[i] <- j}
  }
}

time2 <- Sys.time()

print(paste("Time taken assigning cutpoints:", round(difftime(time2, time1, units = "mins"),2), "mins"))

stopCluster(cl)

data$cutpoint <- as.factor(data$cutpoint)



####Create data for multinomial model (NNET Package)

print("Creating data for multinomial likelihood models...")

data_multinom <- data[data$Target_Meyer != "U",]

data_multinom$Target_Meyer <- droplevels(data_multinom$Target_Meyer, "U")

data_multinom$Target_Meyer <- as.factor(data_multinom$Target_Meyer)

summary(data_multinom$Target_Meyer)

data_multinom$age <- as.numeric(data_multinom$age)

data_multinom$yield <- as.numeric(data_multinom$yield)

data_multinom$cellcount <- as.numeric(data_multinom$cellcount)

data_multinom$dim <- as.numeric(data_multinom$dim)

data_multinom$meantitrenegcows <- as.numeric(data_multinom$meantitrenegcows)

data_multinom$butterfat <- as.numeric(data_multinom$butterfat)

data_multinom$protein <- as.numeric(data_multinom$protein)

#Likelihood models continuous age


print("Fitting likelihood models...")

time1 <- Sys.time()

CVResults <- data.frame()


  for (eta in 1:length(models)){
  
    modeltorun <- models[eta]

    for (k in 1:5){
      set.seed(sample(1:10000,1))
      folds <- createFolds(data_multinom$Target_Meyer, k = 5)
      for (j in 1:5) {
        names(folds) <- c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")
        names(folds)[j] <- "test"
        trainset <- data_multinom[-folds$test,]
        testset <- data_multinom[folds$test,]
        print("-------------------------------------")
        print(paste0("Model ",eta,"/",length(models)))
        print(modeltorun)
        print(paste("Testing on Fold",j,"Repetition",k))
        print(paste0("Progess: ",(eta-1)*5*5 + (((k-1)*5) + j)   ,"/",length(models) * 5 * 5, " (",round((((eta-1)*5*5) + ((k-1)*5) + j) / (length(models) * 5 * 5) * 100, 1),"%)"))
    
        if (modeltorun == "multinom_model_age"){
          multinom_model_age <- multinom(cutpoint ~ age + Target_Meyer, MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
        
        if (modeltorun == "multinom_model_age_inter"){
          multinom_model_age_inter <- multinom(cutpoint ~ age * Target_Meyer, MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
        
        if (modeltorun == "multinom_model_agesq_inter"){
          multinom_model_agesq_inter <- multinom(cutpoint ~ age * Target_Meyer + I(age^2) * Target_Meyer, MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
        
        if (modeltorun == "multinom_model_agecb_inter"){
          multinom_model_agecb_inter <- multinom(cutpoint ~ age * Target_Meyer + I(age^3) * Target_Meyer, MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
      
        if (modeltorun == "multinom_model_age_inter_dim"){
          multinom_model_age_inter_dim <- multinom(cutpoint ~ age * Target_Meyer + dim, MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
      
        if (modeltorun == "multinom_model_age_inter_yield"){
          multinom_model_age_inter_yield <- multinom(cutpoint ~ age * Target_Meyer + yield, MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
      
        if (modeltorun == "multinom_model_age_inter_yieldsq"){
          multinom_model_age_inter_yieldsq <- multinom(cutpoint ~ age * Target_Meyer + yield + I(yield^2), MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
      
        if (modeltorun == "multinom_model_age_inter_mtnc"){
          multinom_model_age_inter_mtnc <- multinom(cutpoint ~ age * Target_Meyer + meantitrenegcows, MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
        
        if (modeltorun == "multinom_model_age_inter_mtncsq"){
          multinom_model_age_inter_mtncsq <- multinom(cutpoint ~ age * Target_Meyer + meantitrenegcows + I(meantitrenegcows^2), MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
        
        if (modeltorun == "multinom_model_age_inter_yield_mtnc"){
          multinom_model_age_inter_yield_mtnc <- multinom(cutpoint ~ age * Target_Meyer + yield + meantitrenegcows, MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
      
        if (modeltorun == "multinom_model_age_inter_scc"){
          multinom_model_age_inter_scc <- multinom(cutpoint ~ age * Target_Meyer + cellcount, MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
        
        if (modeltorun == "multinom_model_age_inter_scclog"){
          multinom_model_age_inter_scclog <- multinom(cutpoint ~ age * Target_Meyer + cellcount + I(log(cellcount)), MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
        
        
        if (modeltorun == "multinom_model_age_inter_yield_mtnc_scc"){
          multinom_model_age_inter_yield_mtnc_scc <- multinom(cutpoint ~ age * Target_Meyer + yield + meantitrenegcows + cellcount, MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
        
        if (modeltorun == "multinom_model_age_inter_yield_mtnc_bf"){
          multinom_model_age_inter_yield_mtnc_bf <- multinom(cutpoint ~ age * Target_Meyer + yield + meantitrenegcows + butterfat, MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
      
        if (modeltorun == "multinom_model_age_inter_yield_mtnc_protein"){
          multinom_model_age_inter_yield_mtnc_protein <- multinom(cutpoint ~ age * Target_Meyer + yield + meantitrenegcows + protein, MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
      
        if (modeltorun == "multinom_model_age_inter_yield_mtnc_scc_protein"){
          multinom_model_age_inter_yield_mtnc_scc_protein <- multinom(cutpoint ~ age * Target_Meyer + yield + meantitrenegcows + cellcount + protein, MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
      
        if (modeltorun == "multinom_model_age_inter_protein"){
          multinom_model_age_inter_protein <- multinom(cutpoint ~ age * Target_Meyer + protein, MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
        
        if (modeltorun == "multinom_model_age_inter_proteinsq"){
          multinom_model_age_inter_proteinsq <- multinom(cutpoint ~ age * Target_Meyer + protein + I(protein^2), MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
        
        if (modeltorun == "multinom_model_agesq_inter_mtncsq_proteinsq_scclog_yieldsq"){
          multinom_model_agesq_inter_mtncsq_proteinsq_scclog_yieldsq <- multinom(cutpoint ~ age * Target_Meyer + I(age^2) * Target_Meyer + meantitrenegcows + I(meantitrenegcows^2) + protein + I(protein^2) + I(log(cellcount)) + yield + I(yield^2), MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
        
        if (modeltorun == "multinom_model_agesq_inter_mtncsq_proteinsq_yieldsq"){
          multinom_model_agesq_inter_mtncsq_proteinsq_yieldsq <- multinom(cutpoint ~ age * Target_Meyer + I(age^2) * Target_Meyer + meantitrenegcows + I(meantitrenegcows^2) + protein + I(protein^1) + yield + I(yield^2), MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
        
        
        if (modeltorun == "multinom_model_agesq_inter_mtncsq_scclog_yieldsq"){
          multinom_model_agesq_inter_mtncsq_scclog_yieldsq <- multinom(cutpoint ~ age * Target_Meyer + I(age^2) * Target_Meyer + meantitrenegcows + I(meantitrenegcows^2) + I(log(cellcount)) + yield + I(yield^2), MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
        
        if (modeltorun == "multinom_model_agesq_inter_proteinsq_scclog_yieldsq"){
          multinom_model_agesq_inter_proteinsq_scclog_yieldsq <- multinom(cutpoint ~ age * Target_Meyer + I(age^2) * Target_Meyer + protein + I(protein^2) + I(log(cellcount)) + yield + I(yield^2), MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
        
        if (modeltorun == "multinom_model_agesq_inter_mtncsq_proteinsq_scclog"){
          multinom_model_agesq_inter_mtncsq_proteinsq_scclog <- multinom(cutpoint ~ age * Target_Meyer + I(age^2) * Target_Meyer + meantitrenegcows + I(meantitrenegcows^2) + protein + I(protein^2) + I(log(cellcount)), MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
        
        if (modeltorun == "multinom_model_agesq_inter_mtncsq_proteinsq"){
          multinom_model_agesq_inter_mtncsq_proteinsq <- multinom(cutpoint ~ age * Target_Meyer + I(age^2) * Target_Meyer + meantitrenegcows + I(meantitrenegcows^2) + protein + I(protein^2), MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
        
        if (modeltorun == "multinom_model_agesq_inter_mtncsq_scclog"){
          multinom_model_agesq_inter_mtncsq_scclog <- multinom(cutpoint ~ age * Target_Meyer + I(age^2) * Target_Meyer + meantitrenegcows + I(meantitrenegcows^2) + I(log(cellcount)), MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
        
        if (modeltorun == "multinom_model_agesq_inter_mtncsq_yieldsq"){
          multinom_model_agesq_inter_mtncsq_yieldsq <- multinom(cutpoint ~ age * Target_Meyer + I(age^2) * Target_Meyer + meantitrenegcows + I(meantitrenegcows^2) + yield + I(yield^2), MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
        
        if (modeltorun == "multinom_model_agesq_inter_proteinsq_scclog"){
          multinom_model_agesq_inter_proteinsq_scclog <- multinom(cutpoint ~ age * Target_Meyer + I(age^2) * Target_Meyer + protein + I(protein^2) + I(log(cellcount)), MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
        
        if (modeltorun == "multinom_model_agesq_inter_proteinsq_yieldsq"){
          multinom_model_agesq_inter_proteinsq_yieldsq <- multinom(cutpoint ~ age * Target_Meyer + I(age^2) * Target_Meyer + protein + I(protein^2) + yield + I(yield^2), MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
        
        if (modeltorun == "multinom_model_agesq_inter_scclog_yieldsq"){
          multinom_model_agesq_inter_scclog_yieldsq <- multinom(cutpoint ~ age * Target_Meyer + I(age^2) * Target_Meyer + I(log(cellcount)) + yield + I(yield^2), MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
        
        if (modeltorun == "multinom_model_agesq_inter_mtncsq"){
          multinom_model_agesq_inter_mtncsq <- multinom(cutpoint ~ age * Target_Meyer + I(age^2) * Target_Meyer + meantitrenegcows + I(meantitrenegcows^2), MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
        
        if (modeltorun == "multinom_model_agesq_inter_proteinsq"){
          multinom_model_agesq_inter_proteinsq <- multinom(cutpoint ~ age * Target_Meyer + I(age^2) * Target_Meyer + protein + I(protein^2), MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
        
        if (modeltorun == "multinom_model_agesq_inter_scclog"){
          multinom_model_agesq_inter_scclog <- multinom(cutpoint ~ age * Target_Meyer + I(age^2) * Target_Meyer + I(log(cellcount)), MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
        
        if (modeltorun == "multinom_model_agesq_inter_yieldsq"){
          multinom_model_agesq_inter_yieldsq <- multinom(cutpoint ~ age * Target_Meyer + I(age^2) * Target_Meyer + yield + I(yield^2), MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
        
        
        
        
        
        
        #Likelihood models categorised age
      
        if (modeltorun == "multinom_model_age_cat"){
          multinom_model_age_cat <- multinom(cutpoint ~ age_cat + Target_Meyer, MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
      
        if (modeltorun == "multinom_model_age_cat_inter"){
          multinom_model_age_cat_inter <- multinom(cutpoint ~ age_cat*Target_Meyer, MaxNWts =10000000, data = trainset) #NNET package without Caret
        }    
      
        if (modeltorun == "multinom_model_age_cat_inter_dim"){
          multinom_model_age_cat_inter_dim <- multinom(cutpoint ~ age_cat * Target_Meyer + dim, MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
      
        if (modeltorun == "multinom_model_age_cat_inter_yield"){
          multinom_model_age_cat_inter_yield <- multinom(cutpoint ~ age_cat * Target_Meyer + yield, MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
      
        if (modeltorun == "multinom_model_age_cat_inter_yield_mtnc"){
          multinom_model_age_cat_inter_yield_mtnc <- multinom(cutpoint ~ age_cat * Target_Meyer + yield + meantitrenegcows, MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
      
        if (modeltorun == "multinom_model_age_cat_inter_yield_mtnc_scc"){
          multinom_model_age_cat_inter_yield_mtnc_scc <-  multinom(cutpoint ~ age_cat * Target_Meyer + yield + meantitrenegcows + cellcount, MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
      
        if (modeltorun == "multinom_model_age_cat_inter_logscc"){
          multinom_model_age_cat_inter_logscc <- multinom(cutpoint ~ age_cat * Target_Meyer + I(log(cellcount)), MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
        
        if (modeltorun == "multinom_model_age_cat_inter_yield_mtnc_bf"){
          multinom_model_age_cat_inter_yield_mtnc_bf <-  multinom(cutpoint ~ age_cat * Target_Meyer + yield + meantitrenegcows + butterfat, MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
      
        if (modeltorun == "multinom_model_age_cat_inter_yield_mtnc_protein"){
          multinom_model_age_cat_inter_yield_mtnc_protein <-  multinom(cutpoint ~ age_cat * Target_Meyer + yield + meantitrenegcows + protein, MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
      
        if (modeltorun == "multinom_model_age_cat_inter_yield_mtnc_scc_protein"){
          multinom_model_age_cat_inter_yield_mtnc_scc_protein <-  multinom(cutpoint ~ age_cat * Target_Meyer + yield + meantitrenegcows + cellcount + protein, MaxNWts =10000000, data = trainset) #NNET package without Caret
        }
      
       
        
        testset$pred <- predict(get(modeltorun), newdata=testset, type="probs", allow.new.levels = T)
        
        foldresults <- data.frame(Model = modeltorun, Rep = k, Fold = j, Observed = testset$cutpoint)
        foldresults <- cbind(foldresults, testset$pred)
        
        
        
        CVResults <<- rbind(CVResults, foldresults)
  
      } 
 
  }

  }

time2 <- Sys.time()

print(paste("Time taken for model cross validation:", round(difftime(time2, time1, units = "mins"),1), "mins"))

print("Writing cross validation results (CVResults.csv)...")

write.csv(CVResults,  paste0("y:/ian/johnesthresholds/johnesproper/data/LikelihoodCrossValidation/CVResults",birthmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),".csv"))



print("Generating CaliPlots and extracting calibration errors...")

time1 <- Sys.time()

caliberrortable <- data.frame(Model = character(), Cutpoint = numeric(), ECE = numeric(), MCE = numeric(), MeanECEAllCuts = numeric(), MeanMCEAllCuts = numeric())

for (eta in 1:length(models)){

  CPData <- CVResults[CVResults$Model == models[eta],]
  for (zeta in 1:length(cutpoints)){
    CPData$correctcut <- ifelse(CPData$Observed == zeta, 1, 0)
    calibtemp <- cbind(models[eta], zeta, ExtractECE(CPData[,4+zeta], CPData$correctcut, nbins = 10),ExtractMCE(CPData[,4+zeta], CPData$correctcut, nbins = 10), 0, 0)
    colnames(calibtemp) <- c("Model", "Cutpoint", "ECE", "MCE", "MeanECEAllCuts", "MeanMCEAllCuts")
    caliberrortable <- rbind(caliberrortable, calibtemp)
    CaliPlot(CPData[,4+zeta], CPData$correctcut, 
             nbins = 10, 
             ptitle = models[eta], 
             psubtitle = paste("Titre Cutpoints:", toString(cutpoints), "\nAge Cutpoints:", toString(agebreaks), "\nCutpoint", zeta))
    ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/LikelihoodCrossValidation/CVCaliPlots/MNCaliPlot",birthmodel,models[eta],"Titres",toString(cutpoints),"Ages",toString(agebreaks),"Cut",zeta,".png"))
  }
  

}

for (eta in 1:length(models)){
 caliberrortable$MeanECEAllCuts[caliberrortable$Model == models[eta]] <- mean(as.numeric(caliberrortable$ECE[caliberrortable$Model == models[eta] & caliberrortable$ECE != 0]))
 caliberrortable$MeanMCEAllCuts[caliberrortable$Model == models[eta]] <- mean(as.numeric(caliberrortable$MCE[caliberrortable$Model == models[eta] & caliberrortable$MCE != 0]))
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


