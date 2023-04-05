time0 <- Sys.time()

#####Likelihood Model

tvupath <- "c:/users/ian.glover.headoffice/desktop/"

  
  data <- read.csv(paste0(tvupath,"data_TESTFARMS_birthpriors.csv"))
  
  data <- data[data$dim >= 0,]

  data <- data[
    with(data, order(calfeartag, age)),
  ]
  
  print("Cleaning data and assigning new cutpoints...")
  
  data$Target_QMMS <- as.factor(data$Target_QMMS)
  
  data$titre <- as.numeric(data$titre)
  
  data$yield[which(is.na(data$yield))] <- 0
  data$cellcount[which(is.na(data$cellcount))] <- 0
  data$butterfat[which(is.na(data$butterfat))] <- 0
  data$protein[which(is.na(data$protein))] <- 0
  data$age[which(is.na(data$age))] <- 0
  
  data$dim_cat <- cut(data$dim,
                      breaks = c(0,10,20,50,500,5000))

  
  
  summary(data$age)
  
  data$age_cat <- cut(as.numeric(data$age), breaks = agebreaks)
  
  summary(data$titre)
  
  print("Creating titre cutpoints...")
  
  data$cutpoint <- length(cutpoints)
  
  #cl <- makePSOCKcluster(5)
  #registerDoParallel(cl)
  #cat("No. Cores:",getDoParWorkers())
  
  cutpoints
  
  time1 <- Sys.time()
  
  source_python("Y:/Ian/JohnesThresholds/JohnesProper/Data/PythonScripts/PyCreateCutpoints.py")
  
  data <- createcutpoints(data, cutpoints)
  
  print(head(cbind(data$titre, data$cutpoint),30))
  
  
  time2 <- Sys.time()
  
  print(paste("Time taken assigning cutpoints:", round(difftime(time2, time1, units = "mins"),2), "mins"))
  
  print("Titre cutpoints:")
  
  data$cutpoint <- as.factor(data$cutpoint)
  
  summary(data$cutpoint)
  
  print("Writing file with new cutpoints...")
  
  write.csv(data, paste0(tvupath,"data_TESTFARMS_birthpriors_cutpoints.csv"), row.names = FALSE)
  
  
  

data$cutpoint <- as.factor(data$cutpoint)





##Predict likelihood model on testing dataset

print("Predicting likelihood model on testing dataset...")

time1 <- Sys.time()

data_temp_TM <- data

data_temp_TM$age <- as.numeric(data_temp_TM$age)
data_temp_TM$yield <- as.numeric(data_temp_TM$yield)
data_temp_TM$dim <- as.numeric(data_temp_TM$dim)
data_temp_TM$meantitrenegcows <- as.numeric(data_temp_TM$meantitrenegcows)
data_temp_TM$protein <- as.numeric(data_temp_TM$protein)






data_temp_TM$agesq <- data_temp_TM$age^2
data_temp_TM$proteinsq <- data_temp_TM$protein^2
data_temp_TM$yieldsq <- data_temp_TM$yield^2
data_temp_TM$butterfatsq <- data_temp_TM$butterfat^2
data_temp_TM$meantitrenegcowssq <- data_temp_TM$meantitrenegcows^2
data_temp_TM$cellcountlog <- ifelse(data_temp_TM$cellcount == 0, log(0.00000001), log(data_temp_TM$cellcount))

data_temp_TM$Target_QMMS <- as.factor(data_temp_TM$Target_QMMS)





data_temp_TM$Target_QMMS <- "0"

data_temp_TM0 <- data_temp_TM

data_temp_TM$Target_QMMS <- "1"

data_temp_TM1 <- data_temp_TM

data_temp_TM <- rbind(data_temp_TM0, data_temp_TM1)



data_temp_TM$cellcount_cat <- cut(data_temp_TM$cellcount, breaks = c(0,50,100,150,200,500,1000,10000))
levels(data_temp_TM$cellcount_cat) <- c(levels(data_temp_TM$cellcount_cat), "Missing")
data_temp_TM$cellcount_cat[which(is.na(data_temp_TM$cellcount_cat))] <- "Missing"

data_temp_TM$yield_cat <- cut(data_temp_TM$yield, breaks = c(0,10,20,30,40,50,70))
levels(data_temp_TM$yield_cat) <- c(levels(data_temp_TM$yield_cat), "Missing")
data_temp_TM$yield_cat[which(is.na(data_temp_TM$yield_cat))] <- "Missing"


#THIS NEEDS TO BE CHANGED BACK TO WHATEVER THE FINAL LIKELIHOOD MODEL IS. 
#THIS MODEL IS FOR AB DEMO PURPOSES ONLY!

#finallikemodel <- readRDS("y:/ian/johnesthresholds/johnesproper/data/pickledmodels/finallikemodel.rds")

finallikemodel <- readRDS("Y:/Ian/JohnesThresholds/JohnesProper/Data/PickledModels/finallikemodel_age_yield_scc_mtnc_dim.rds")

MNPredTM <- predict(finallikemodel, newdata = data_temp_TM, type = "response", conditional = FALSE)

end1 <- nrow(MNPredTM)/2
start2 <- (nrow(MNPredTM)/2)+1
end2 <- nrow(MNPredTM)

MNPredTM0 <- MNPredTM[c(1:end1),]
MNPredTM1 <- MNPredTM[c(start2:end2),]


head(MNPredTM1,10)

head(MNPredTM0,10)

data_temp_TM <- data

regcores()

data_temp_TM$MNPredProbTM0 <- 0
data_temp_TM$MNPredProbTM1 <- 0

y <- foreach(i = 1:nrow(data_temp_TM), .combine = "rbind") %dopar% {
  colnum <- data_temp_TM$cutpoint[i]
  MNPredProbTM0 <- MNPredTM0[i,colnum]
  MNPredProbTM1 <- MNPredTM1[i,colnum]
  c(MNPredProbTM0, MNPredProbTM1)
}

stopCluster(cl)

data_temp_TM$MNPredProbTM0 <- y[,1]
data_temp_TM$MNPredProbTM1 <- y[,2]

data$MNPredProbTM0 <- data_temp_TM$MNPredProbTM0
data$MNPredProbTM1 <- data_temp_TM$MNPredProbTM1

data$likelihood <- data$MNPredProbTM1/data$MNPredProbTM0


write.csv(data, paste0(tvupath,"data_TESTFARMS_birthpriors_cutpoints.csv"), row.names = FALSE)

rm(data_temp_TM)
rm(data_temp_TM0)
rm(data_temp_TM1)

###Posterior Updating in Python######

print("Updating posteriors...")

data$PosteriorOdds <- as.numeric(0)
data$POFloorApplied <- as.numeric(0)



time1 <- Sys.time()

source_python("Y:/Ian/JohnesThresholds/JohnesProper/Data/PythonScripts/PyPOUpdater.py")
data <- updatePO(data, posterioroddsfloor, priorstage)


#####Convert PosteriorOdds to Posterior Probabilities and save data (data_posteriors.csv)#####

time2 <- Sys.time()

print(paste("Time to update posteriors:", round(difftime(time2, time1, units = "mins"),2), "mins"))

data$PosteriorProb <- as.numeric(data$PosteriorOdds / (1 + data$PosteriorOdds))

print(ggplot(data, aes(x = PosteriorProb)) +
        geom_histogram())

ggplot(data, aes(x = as.factor(cutpoint), y = log(likelihood))) +
  geom_boxplot()

data <- data[!is.na(data$PosteriorProb),]

print("Saving data (data_posteriors.csv)")

write.csv(data, paste0(tvupath,"data_TESTFARMS_posteriors.csv"), row.names=FALSE)

time2 <- Sys.time()

print(paste("Time processing likelihood models:", round(difftime(time2, time0, units = "mins"),2), "mins"))


rm(MNPredTM)
rm(MNPredTM0)
rm(MNPredTM1)


