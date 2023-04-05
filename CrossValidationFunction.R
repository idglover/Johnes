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
      timeelapsed <- difftime(Sys.time(), time1, units = "mins")
      totalmods <- 5*5*length(models)
      modsdone <- (eta-1)*5*5 + (((k-1)*5) + j) -1
      modsremaining = totalmods - modsdone
      timeremaining <- modsremaining /modsdone * timeelapsed
      print(paste("Estimated time remaining:",round(timeremaining,1),"mins."))
      if (modeltorun == "multinom_model_re_age"){
        multinom_model_re_age <- mblogit(cutpoint ~ age + Target_Meyer, random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_age_inter"){
        multinom_model_re_age_inter <- mblogit(cutpoint ~ age * Target_Meyer, random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_agesq_inter"){
        multinom_model_re_agesq_inter <- mblogit(cutpoint ~ age * Target_Meyer + agesq * Target_Meyer, random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_age_inter_dim"){
        multinom_model_re_age_inter_dim <- mblogit(cutpoint ~ age * Target_Meyer + dim, random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_age_re_inter_yield"){
        multinom_model_re_age_re_inter_yield <- mblogit(cutpoint ~ age * Target_Meyer + yield, random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_age_inter_yieldsq"){
        multinom_model_re_age_inter_yieldsq <- mblogit(cutpoint ~ age * Target_Meyer + yield + yieldsq, random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_age_inter_mtnc"){
        multinom_model_re_age_inter_mtnc <- mblogit(cutpoint ~ age * Target_Meyer + meantitrenegcows, random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_age_inter_mtncsq"){
        multinom_model_re_age_inter_mtncsq <- mblogit(cutpoint ~ age * Target_Meyer + meantitrenegcows + I(meantitrenegcows^2), random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_age_inter_yield_mtnc"){
        multinom_model_re_age_inter_yield_mtnc <- mblogit(cutpoint ~ age * Target_Meyer + yield + meantitrenegcows, random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_age_inter_scc"){
        multinom_model_re_age_inter_scc <- mblogit(cutpoint ~ age * Target_Meyer + cellcount, random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_age_inter_scclog"){
        multinom_model_re_age_inter_scclog <- mblogit(cutpoint ~ age * Target_Meyer + I(log(cellcount)), random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_age_inter_yield_mtnc_scc"){
        multinom_model_re_age_inter_yield_mtnc_scc <- mblogit(cutpoint ~ age * Target_Meyer + yield + meantitrenegcows + cellcount, random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_age_inter_yield_mtnc_bf"){
        multinom_model_re_age_inter_yield_mtnc_bf <- mblogit(cutpoint ~ age * Target_Meyer + yield + meantitrenegcows + butterfat, random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_age_inter_yield_mtnc_protein"){
        multinom_model_re_age_inter_yield_mtnc_protein <- mblogit(cutpoint ~ age * Target_Meyer + yield + meantitrenegcows + protein, random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_age_inter_yield_mtnc_scc_protein"){
        multinom_model_re_age_inter_yield_mtnc_scc_protein <- mblogit(cutpoint ~ age * Target_Meyer + yield + meantitrenegcows + cellcount + protein, random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_age_inter_protein"){
        multinom_model_re_age_inter_protein <- mblogit(cutpoint ~ age * Target_Meyer + protein, random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_age_inter_proteinsq"){
        multinom_model_re_age_inter_proteinsq <- mblogit(cutpoint ~ age * Target_Meyer + protein + proteinsq, random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_agesq_inter_mtncsq_proteinsq_scclog_yieldsq"){
        multinom_model_re_agesq_inter_mtncsq_proteinsq_scclog_yieldsq <- mblogit(cutpoint ~ age * Target_Meyer + agesq * Target_Meyer + meantitrenegcows + I(meantitrenegcows^2) + protein + proteinsq + I(log(cellcount)) + yield + yieldsq, random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_agesq_inter_mtncsq_proteinsq_yieldsq"){
        multinom_model_re_agesq_inter_mtncsq_proteinsq_yieldsq <- mblogit(cutpoint ~ age * Target_Meyer + agesq * Target_Meyer + meantitrenegcows + I(meantitrenegcows^2) + protein + proteinsq + yield + yieldsq, random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_agesq_inter_mtncsq_scclog_yieldsq"){
        multinom_model_re_agesq_inter_mtncsq_scclog_yieldsq <- mblogit(cutpoint ~ age * Target_Meyer + agesq * Target_Meyer + meantitrenegcows + I(meantitrenegcows^2) + I(log(cellcount)) + yield + yieldsq, random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_agesq_inter_proteinsq_scclog_yieldsq"){
        multinom_model_re_agesq_inter_proteinsq_scclog_yieldsq <- mblogit(cutpoint ~ age * Target_Meyer + agesq * Target_Meyer + protein + proteinsq + I(log(cellcount)) + yield + yieldsq, random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_agesq_inter_mtncsq_proteinsq_scclog"){
        multinom_model_re_agesq_inter_mtncsq_proteinsq_scclog <- mblogit(cutpoint ~ age * Target_Meyer + agesq * Target_Meyer + meantitrenegcows + I(meantitrenegcows^2) + protein + proteinsq + I(log(cellcount)), random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_agesq_inter_mtncsq_proteinsq"){
        multinom_model_re_agesq_inter_mtncsq_proteinsq <- mblogit(cutpoint ~ age * Target_Meyer + agesq * Target_Meyer + meantitrenegcows + I(meantitrenegcows^2) + protein + proteinsq, random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_agesq_inter_mtncsq_scclog"){
        multinom_model_re_agesq_inter_mtncsq_scclog <- mblogit(cutpoint ~ age * Target_Meyer + agesq * Target_Meyer + meantitrenegcows + I(meantitrenegcows^2) + I(log(cellcount)), random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_agesq_inter_mtncsq_yieldsq"){
        multinom_model_re_agesq_inter_mtncsq_yieldsq <- mblogit(cutpoint ~ age * Target_Meyer + agesq * Target_Meyer + meantitrenegcows + I(meantitrenegcows^2) + yield + yieldsq, random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_agesq_inter_proteinsq_scclog"){
        multinom_model_re_agesq_inter_proteinsq_scclog <- mblogit(cutpoint ~ age * Target_Meyer + agesq * Target_Meyer + protein + proteinsq + I(log(cellcount)), random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_agesq_inter_proteinsq_yieldsq"){
        multinom_model_re_agesq_inter_proteinsq_yieldsq <- mblogit(cutpoint ~ age * Target_Meyer + agesq * Target_Meyer + protein + proteinsq + yield + yieldsq, random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_agesq_inter_scclog_yieldsq"){
        multinom_model_re_agesq_inter_scclog_yieldsq <- mblogit(cutpoint ~ age * Target_Meyer + agesq * Target_Meyer + I(log(cellcount)) + yield + yieldsq, random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_agesq_inter_mtncsq"){
        multinom_model_re_agesq_inter_mtncsq <- mblogit(cutpoint ~ age * Target_Meyer + agesq * Target_Meyer + meantitrenegcows + I(meantitrenegcows^2), random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_agesq_inter_proteinsq"){
        multinom_model_re_agesq_inter_proteinsq <- mblogit(cutpoint ~ age * Target_Meyer + agesq * Target_Meyer + protein + proteinsq, random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_agesq_inter_scclog"){
        multinom_model_re_agesq_inter_scclog <- mblogit(cutpoint ~ age * Target_Meyer + agesq * Target_Meyer + I(log(cellcount)), random = ~1|Farm, data = trainset)
      }
      
      if(modeltorun == "multinom_model_re_agesq_inter_yieldsq"){
        multinom_model_re_agesq_inter_yieldsq <- mblogit(cutpoint ~ age * Target_Meyer + agesq * Target_Meyer + yield + yieldsq, random = ~1|Farm, data = trainset)
      }
      
      
      
      
      testset$pred <- predict(get(modeltorun), newdata=testset, type="response", allow.new.levels = T)
      
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