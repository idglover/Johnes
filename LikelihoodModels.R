time0 <- Sys.time()

####CREATE NEW CUTPOINTS####

if (newcutpoints == "Y"){

#####READ DATA#####
  
  data <- read.csv("Y:/Ian/JohnesThresholds/JohnesProper/Data/data_birthpriors.csv")

#####CREATE NEW CUTPOINTS#####

  print("Cleaning data and assigning new cutpoints...")

  if(target == "MEYER"){

   data$Target_Meyer <- as.factor(data$Target_Meyer)

  }

  if(target == "QMMS"){
  
    data$Target_QMMS <- as.factor(data$Target_QMMS)

  }

#####CLEAN AND FORMAT FEATURES#####

######TITRE######

  data$titre <- as.numeric(data$titre)

######AGE AND DIM######

  data <- data[data$age >= 24 &
               !is.na(data$age) &
                 data$dim >= 0,]

  data$dim_cat <- cut(data$dim,
                    breaks = c(0,10,20,50,500,5000))

  data$age_cat <- cut(as.numeric(data$age), breaks = agebreaks)

#####CREATE CUTPOINTS#####

  print("Creating titre cutpoints...")

  print(paste0("Cutpoints: ", cutpoints))

  data$cutpoint <- length(cutpoints)

  time1 <- Sys.time()
  
  source_python("Y:/Ian/JohnesThresholds/JohnesProper/Data/PythonScripts/PyCreateCutpoints.py")
  
  data <- createcutpoints(data, cutpoints)
  
  time2 <- Sys.time()
  
  print(paste("Time taken assigning cutpoints:", round(difftime(time2, time1, units = "mins"),2), "mins"))
  
  print("Titre cutpoints:")
  print(head(cbind(data$titre, data$cutpoint),30))
  
  data$cutpoint <- as.factor(data$cutpoint)
  
  #####WRITE FILE WITH NEW CUTPOINTS#####
  
  print("Writing file with new cutpoints...")
  
  write.csv(data, "Y:/Ian/JohnesThresholds/JohnesProper/Data/data_birthpriors_cutpoints.csv")
  
}

####READ DATA WITH CUTPOINTS####

print("Reading data...")

if(newcutpoints != "Y"){data <- read.csv("Y:/Ian/JohnesThresholds/JohnesProper/Data/data_birthpriors_cutpoints.csv")}

####PREDICT LIKELIHOOD MODEL####

print("Predicting likelihood model on full dataset...")

time1 <- Sys.time()

#####READ PICKLED LIKELIHOOD MODEL#####

finallikemodel <- readRDS(paste0("y:/ian/johnesthresholds/johnesproper/data/pickledmodels/",likemod,".rds"))

#####CREATE TEMPORARY DATASETS FOR POSITIVE AND NEGATIVE TARGET#####

data_temp_TM <- data

######CLEAN AND FORMAT FEATURES######

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
data_temp_TM$cellcount[which(data_temp_TM$cellcount < 0)] <- NA
data_temp_TM$cellcount[which(data_temp_TM$cellcount == 0)] <- 0.0000001
data_temp_TM$cellcountlog <- log(data_temp_TM$cellcount)

data_temp_TM$cellcount_cat <- cut(data_temp_TM$cellcount, breaks = c(0,50,100,150,200,500,1000,10000))
levels(data_temp_TM$cellcount_cat) <- c(levels(data_temp_TM$cellcount_cat), "Missing")
data_temp_TM$cellcount_cat[which(is.na(data_temp_TM$cellcount_cat))] <- "Missing"

data_temp_TM$yield_cat <- cut(data_temp_TM$yield, breaks = c(0,10,20,30,40,50,70))
levels(data_temp_TM$yield_cat) <- c(levels(data_temp_TM$yield_cat), "Missing")
data_temp_TM$yield_cat[which(is.na(data_temp_TM$yield_cat))] <- "Missing"

if(target == "MEYER"){
  data_temp_TM$Target_Meyer <- "0"
}

if(target == "QMMS"){
  data_temp_TM$Target_QMMS <- "0"
}

data_temp_TM0 <- data_temp_TM

if(target == "MEYER"){
  data_temp_TM$Target_Meyer <- "1"
}

if(target == "QMMS"){
  data_temp_TM$Target_QMMS <- "1"
}

data_temp_TM1 <- data_temp_TM

data_temp_TM <- rbind(data_temp_TM0, data_temp_TM1)

#####PREDICT MODEL#####

MNPredTM <- predict(finallikemodel, newdata = data_temp_TM, type = "response", conditional = FALSE)

#####SPLIT PREDICTIONS INTO POSITIVE AND NEGATIVE CASES#####

end1 <- nrow(MNPredTM)/2
start2 <- (nrow(MNPredTM)/2)+1
end2 <- nrow(MNPredTM)

MNPredTM0 <- MNPredTM[c(1:end1),]
MNPredTM1 <- MNPredTM[c(start2:end2),]

print("Multinomal probabilities for positive cases:")

print(head(MNPredTM1,10))

print("Multinomal probabilities for negative cases:")

print(head(MNPredTM0,10))

#####ASSIGN CORRECT PROBABILITY ACCORDING TO CUTPOINT#####

data_temp_TM <- data

no_cores <- detectCores() 
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
getDoParWorkers()

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

#####CALCULATE BAYES FACTOR#####

data$likelihood <- data$MNPredProbTM1/data$MNPredProbTM0

#####PLOT PROBABILITIES AND BAYES FACTOR FOR TITRES < 5#####

print(ggplot(data[data$cutpoint == 1,], aes(x = MNPredProbTM0)) +
        geom_histogram() +
        labs(title = "Probability for titres < 5 given Target_QMMS Negative"))

print(ggplot(data[data$cutpoint == 1,], aes(x = MNPredProbTM1)) +
        geom_histogram() +
        labs(title = "Probability for titres < 5 given Target_QMMS Positive"))

print(ggplot(data[data$cutpoint == 1,], aes(x = likelihood)) +
  geom_histogram() +
  labs(title = "Bayes Factor for titres < 5"))

time2 <- Sys.time()
print(paste("Time for predicting on full dataset:", round(difftime(time2, time1, units = "mins"),2), "mins"))

#####WRITING DATA#####

write.csv(data, "y:/ian/johnesthresholds/johnesproper/data/data_birthpriors_cutpoints.csv", row.names = FALSE)

####MARGINAL EFFECTS####

#####SIMULATE DATA#####

print("Simulating data for marginal effects...")

if(target == "MEYER"){

  data_sim <- expand.grid(Target_Meyer = c("0","1"), 
                        age = seq(20,240,20),
                        yield = seq(10,70,10),
                        protein = seq(1,7,2),
                        meantitrenegcows = seq(0,20,2),
                        cellcount = seq(0,10000,1000),
                        predLR = seq(0,0.4,0.05))

}


if(target == "QMMS"){
  
  data_sim <- expand.grid(Target_QMMS = c("0","1"), 
                          age = seq(20,220,40),
                          yield = seq(10,70,10),
                          #protein = seq(1,7,2),
                          meantitrenegcows = seq(0,20,2),
                          cellcount = seq(0,10000,1000),
                          #predLR = seq(0,0.4,0.05),
                          dim = seq(1,800,10))
  
}

#####FORMAT FEATURES#####

data_sim$age_cat <- cut(data_sim$age, breaks = agebreaks)

data_sim$age <- as.numeric(data_sim$age)
data_sim$yield <- as.numeric(data_sim$yield)
#data_sim$protein <- as.numeric(data_sim$protein)
data_sim$meantitrenegcows <- as.numeric(data_sim$meantitrenegcows)
data_sim$cellcount <- as.numeric(data_sim$cellcount)

data_sim$agesq <- data_sim$age^2
data_sim$yieldsq <- data_sim$yield^2
#data_sim$proteinsq <- data_sim$protein^2
data_sim$meantitrenegcowssq <- data_sim$meantitrenegcows^2
data_sim$cellcountlog <- log(data_sim$cellcount)

data_sim$cellcount_cat <- cut(data_sim$cellcount, breaks = c(0,50,100,150,200,500,1000,10000))
levels(data_sim$cellcount_cat) <- c(levels(data_sim$cellcount_cat), "Missing")
data_sim$cellcount_cat[which(is.na(data_sim$cellcount_cat))] <- "Missing"

data_sim$yield_cat <- cut(data_sim$yield, breaks = c(0,10,20,30,40,50,70))
levels(data_sim$yield_cat) <- c(levels(data_sim$yield_cat), "Missing")
data_sim$yield_cat[which(is.na(data_sim$yield_cat))] <- "Missing"

data_sim$dim_cat <- cut(data_sim$dim,
                        breaks = c(0,10,20,50,500,5000))

data_sim$Farm <- "Unknown"

baselinevars <- ncol(data_sim)

#####PREDICT LIKELIHOOD MODEL ON SIMULATED DATA#####

MNpreds_sim <- predict(finallikemodel, newdata = data_sim, type = "response", conditional = FALSE)

for (i in 1:ncol(MNpreds_sim)){
  data_sim <- cbind(data_sim, MNpreds_sim[,i])
  colnames(data_sim)[ncol(data_sim)] <- paste0("cutpoint",i,"prob")
}


if(target == "MEYER"){
  
  data_sim_pos <- data_sim[data_sim$Target_Meyer == "1",]

  head(data_sim_pos)

  data_sim_neg <- data_sim[data_sim$Target_Meyer == "0",]
  
  head(data_sim_neg)

}

if(target == "QMMS"){
  
  data_sim_pos <- data_sim[data_sim$Target_QMMS == "1",]
  
  head(data_sim_pos)
  
  data_sim_neg <- data_sim[data_sim$Target_QMMS == "0",]
  
  head(data_sim_neg)
  
}

for (i in 1:length(cutpoints)){
  like <- data_sim_pos[,baselinevars + i] / data_sim_neg[,baselinevars + i]
  data_sim_pos <- cbind(data_sim_pos, like)
  colnames(data_sim_pos)[ncol(data_sim_pos)] <- paste0("likelihood_cut",i)
  
}

#####PLOT MARGINAL EFFECTS#####

print("Producing marginal effects plots...")

p <- ggplot(data_sim_pos[data_sim_pos$yield == 30 & data_sim_pos$meantitrenegcows == 2 & data_sim_pos$cellcount == 1000 & data_sim_pos$dim == 101,], aes(x = age)) +
  labs(x = "Age (m)", y  = "log(Bayes Factor)", title = paste(birthmodel,crtmodel), subtitle = paste("Titre Cutpoints:",toString(cutpoints),"\nAge Cutpoints",toString(agebreaks)))

if(length(cutpoints) >= 1){p <- p + geom_line(aes(y = log(likelihood_cut1))) + 
  geom_text(aes(x = max(age), y = log(max(likelihood_cut1)), label ="Cutpoint 1"), nudge_x = -(max(data_sim_pos$meantitrenegcows)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 2){p <- p + geom_line(aes(y = log(likelihood_cut2))) + 
  geom_text(aes(x = max(age), y = log(max(likelihood_cut2)), label ="Cutpoint 2"), nudge_x = -(max(data_sim_pos$meantitrenegcows)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 3){p <- p + geom_line(aes(y = log(likelihood_cut3))) + 
  geom_text(aes(x = max(age), y = log(max(likelihood_cut3)), label ="Cutpoint 3"), nudge_x = -(max(data_sim_pos$meantitrenegcows)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 4){p <- p + geom_line(aes(y = log(likelihood_cut4))) + 
  geom_text(aes(x = max(age), y = log(max(likelihood_cut4)), label ="Cutpoint 4"), nudge_x = -(max(data_sim_pos$meantitrenegcows)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 5){p <- p + geom_line(aes(y = log(likelihood_cut5))) + 
  geom_text(aes(x = max(age), y = log(max(likelihood_cut5)), label ="Cutpoint 5"), nudge_x = -(max(data_sim_pos$meantitrenegcows)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 6){p <- p + geom_line(aes(y = log(likelihood_cut6))) + 
  geom_text(aes(x = max(age), y = log(max(likelihood_cut6)), label ="Cutpoint 6"), nudge_x = -(max(data_sim_pos$meantitrenegcows)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 7){p <- p + geom_line(aes(y = log(likelihood_cut7))) + 
  geom_text(aes(x = max(age), y = log(max(likelihood_cut7)), label ="Cutpoint 7"), nudge_x = -(max(data_sim_pos$meantitrenegcows)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 8){p <- p + geom_line(aes(y = log(likelihood_cut8))) + 
  geom_text(aes(x = max(age), y = log(max(likelihood_cut8)), label ="Cutpoint 8"), nudge_x = -(max(data_sim_pos$meantitrenegcows)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 9){p <- p + geom_line(aes(y = log(likelihood_cut9))) + 
  geom_text(aes(x = max(age), y = log(max(likelihood_cut9)), label ="Cutpoint 9"), nudge_x = -(max(data_sim_pos$meantitrenegcows)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 10){p <- p + geom_line(aes(y = log(likelihood_cut10))) + 
  geom_text(aes(x = max(age), y = log(max(likelihood_cut10)), label ="Cutpoint 10"), nudge_x = -(max(data_sim_pos$meantitrenegcows)/20), nudge_y = 0, size = 3, color = "black")}

p

ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/MarginalEffects/MELikelihoodAge",birthmodel,crtmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),".png"))

p <- ggplot(data_sim_pos[data_sim_pos$age == 60 & data_sim_pos$meantitrenegcows == 2 & data_sim_pos$cellcount == 1000 & data_sim_pos$dim == 101,], aes(x = yield)) +
  labs(x = "Yield (l)", y  = "log(Bayes Factor)", title = paste(birthmodel,crtmodel), subtitle = paste("Titre Cutpoints:",toString(cutpoints),"\nAge Cutpoints",toString(agebreaks)))


if(length(cutpoints) >= 1){p <- p + geom_line(aes(y = log(likelihood_cut1))) + 
  geom_text(aes(x = max(yield), y = log(max(likelihood_cut1)), label ="Cutpoint 1"), nudge_x = -(max(data_sim_pos$yield)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 2){p <- p + geom_line(aes(y = log(likelihood_cut2))) + 
  geom_text(aes(x = max(yield), y = log(max(likelihood_cut2)), label ="Cutpoint 2"), nudge_x = -(max(data_sim_pos$yield)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 3){p <- p + geom_line(aes(y = log(likelihood_cut3))) + 
  geom_text(aes(x = max(yield), y = log(max(likelihood_cut3)), label ="Cutpoint 3"), nudge_x = -(max(data_sim_pos$yield)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 4){p <- p + geom_line(aes(y = log(likelihood_cut4))) + 
  geom_text(aes(x = max(yield), y = log(max(likelihood_cut4)), label ="Cutpoint 4"), nudge_x = -(max(data_sim_pos$yield)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 5){p <- p + geom_line(aes(y = log(likelihood_cut5))) + 
  geom_text(aes(x = max(yield), y = log(max(likelihood_cut5)), label ="Cutpoint 5"), nudge_x = -(max(data_sim_pos$yield)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 6){p <- p + geom_line(aes(y = log(likelihood_cut6))) + 
  geom_text(aes(x = max(yield), y = log(max(likelihood_cut6)), label ="Cutpoint 6"), nudge_x = -(max(data_sim_pos$yield)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 7){p <- p + geom_line(aes(y = log(likelihood_cut7))) + 
  geom_text(aes(x = max(yield), y = log(max(likelihood_cut7)), label ="Cutpoint 7"), nudge_x = -(max(data_sim_pos$yield)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 8){p <- p + geom_line(aes(y = log(likelihood_cut8))) + 
  geom_text(aes(x = max(yield), y = log(max(likelihood_cut8)), label ="Cutpoint 8"), nudge_x = -(max(data_sim_pos$yield)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 9){p <- p + geom_line(aes(y = log(likelihood_cut9))) + 
  geom_text(aes(x = max(yield), y = log(max(likelihood_cut9)), label ="Cutpoint 9"), nudge_x = -(max(data_sim_pos$yield)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 10){p <- p + geom_line(aes(y = log(likelihood_cut10))) + 
  geom_text(aes(x = max(yield), y = log(max(likelihood_cut10)), label ="Cutpoint 10"), nudge_x = -(max(data_sim_pos$yield)/20), nudge_y = 0, size = 3, color = "black")}
p

ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/MarginalEffects/MELikelihoodYield",birthmodel,crtmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),".png"))

p <- ggplot(data_sim_pos[data_sim_pos$age == 60 & data_sim_pos$yield == 30 & data_sim_pos$cellcount == 1000 & data_sim_pos$dim == 101,], aes(x = meantitrenegcows)) +
  labs(x = "Mean Titre Negative Cows", y  = "log(Bayes Factor)", title = paste(birthmodel,crtmodel), subtitle = paste("Titre Cutpoints:",toString(cutpoints),"\nAge Cutpoints",toString(agebreaks)))


if(length(cutpoints) >= 1){p <- p + geom_line(aes(y = log(likelihood_cut1))) + 
  geom_text(aes(x = max(meantitrenegcows), y = log(max(likelihood_cut1)), label ="Cutpoint 1"), nudge_x = -(max(data_sim_pos$meantitrenegcows)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 2){p <- p + geom_line(aes(y = log(likelihood_cut2))) + 
  geom_text(aes(x = max(meantitrenegcows), y = log(max(likelihood_cut2)), label ="Cutpoint 2"), nudge_x = -(max(data_sim_pos$meantitrenegcows)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 3){p <- p + geom_line(aes(y = log(likelihood_cut3))) + 
  geom_text(aes(x = max(meantitrenegcows), y = log(max(likelihood_cut3)), label ="Cutpoint 3"), nudge_x = -(max(data_sim_pos$meantitrenegcows)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 4){p <- p + geom_line(aes(y = log(likelihood_cut4))) + 
  geom_text(aes(x = max(meantitrenegcows), y = log(max(likelihood_cut4)), label ="Cutpoint 4"), nudge_x = -(max(data_sim_pos$meantitrenegcows)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 5){p <- p + geom_line(aes(y = log(likelihood_cut5))) + 
  geom_text(aes(x = max(meantitrenegcows), y = log(max(likelihood_cut5)), label ="Cutpoint 5"), nudge_x = -(max(data_sim_pos$meantitrenegcows)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 6){p <- p + geom_line(aes(y = log(likelihood_cut6))) + 
  geom_text(aes(x = max(meantitrenegcows), y = log(max(likelihood_cut6)), label ="Cutpoint 6"), nudge_x = -(max(data_sim_pos$meantitrenegcows)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 7){p <- p + geom_line(aes(y = log(likelihood_cut7))) + 
  geom_text(aes(x = max(meantitrenegcows), y = log(max(likelihood_cut7)), label ="Cutpoint 7"), nudge_x = -(max(data_sim_pos$meantitrenegcows)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 8){p <- p + geom_line(aes(y = log(likelihood_cut8))) + 
  geom_text(aes(x = max(meantitrenegcows), y = log(max(likelihood_cut8)), label ="Cutpoint 8"), nudge_x = -(max(data_sim_pos$meantitrenegcows)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 9){p <- p + geom_line(aes(y = log(likelihood_cut9))) + 
  geom_text(aes(x = max(meantitrenegcows), y = log(max(likelihood_cut9)), label ="Cutpoint 9"), nudge_x = -(max(data_sim_pos$meantitrenegcows)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 10){p <- p + geom_line(aes(y = log(likelihood_cut10))) + 
  geom_text(aes(x = max(meantitrenegcows), y = log(max(likelihood_cut10)), label ="Cutpoint 10"), nudge_x = -(max(data_sim_pos$meantitrenegcows)/20), nudge_y = 0, size = 3, color = "black")}
p

ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/MarginalEffects/MELikelihoodMTNC",birthmodel,crtmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),".png"))

p <- ggplot(data_sim_pos[data_sim_pos$age == 60 & data_sim_pos$yield == 30 & data_sim_pos$meantitrenegcows == 2 & data_sim_pos$cellcount == 1000 & data_sim_pos$dim == 101,], aes(x = protein)) +
  labs(x = "Protein (%)", y  = "log(Bayes Factor)", title = paste(birthmodel,crtmodel), subtitle = paste("Titre Cutpoints:",toString(cutpoints),"\nAge Cutpoints",toString(agebreaks)))


if(length(cutpoints) >= 1){p <- p + geom_line(aes(y = log(likelihood_cut1))) + 
  geom_text(aes(x = max(protein), y = log(max(likelihood_cut1)), label ="Cutpoint 1"), nudge_x = -(max(data_sim_pos$protein)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 2){p <- p + geom_line(aes(y = log(likelihood_cut2))) + 
  geom_text(aes(x = max(protein), y = log(max(likelihood_cut2)), label ="Cutpoint 2"), nudge_x = -(max(data_sim_pos$protein)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 3){p <- p + geom_line(aes(y = log(likelihood_cut3))) + 
  geom_text(aes(x = max(protein), y = log(max(likelihood_cut3)), label ="Cutpoint 3"), nudge_x = -(max(data_sim_pos$protein)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 4){p <- p + geom_line(aes(y = log(likelihood_cut4))) + 
  geom_text(aes(x = max(protein), y = log(max(likelihood_cut4)), label ="Cutpoint 4"), nudge_x = -(max(data_sim_pos$protein)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 5){p <- p + geom_line(aes(y = log(likelihood_cut5))) + 
  geom_text(aes(x = max(protein), y = log(max(likelihood_cut5)), label ="Cutpoint 5"), nudge_x = -(max(data_sim_pos$protein)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 6){p <- p + geom_line(aes(y = log(likelihood_cut6))) + 
  geom_text(aes(x = max(protein), y = log(max(likelihood_cut6)), label ="Cutpoint 6"), nudge_x = -(max(data_sim_pos$protein)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 7){p <- p + geom_line(aes(y = log(likelihood_cut7))) + 
  geom_text(aes(x = max(protein), y = log(max(likelihood_cut7)), label ="Cutpoint 7"), nudge_x = -(max(data_sim_pos$protein)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 8){p <- p + geom_line(aes(y = log(likelihood_cut8))) + 
  geom_text(aes(x = max(protein), y = log(max(likelihood_cut8)), label ="Cutpoint 8"), nudge_x = -(max(data_sim_pos$protein)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 9){p <- p + geom_line(aes(y = log(likelihood_cut9))) + 
  geom_text(aes(x = max(protein), y = log(max(likelihood_cut9)), label ="Cutpoint 9"), nudge_x = -(max(data_sim_pos$protein)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 10){p <- p + geom_line(aes(y = log(likelihood_cut10))) + 
  geom_text(aes(x = max(protein), y = log(max(likelihood_cut10)), label ="Cutpoint 10"), nudge_x = -(max(data_sim_pos$protein)/20), nudge_y = 0, size = 3, color = "black")}
p

ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/MarginalEffects/MELikelihoodProtein",birthmodel,crtmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),".png"))

p <- ggplot(data_sim_pos[data_sim_pos$age == 60 & data_sim_pos$yield == 30 & data_sim_pos$meantitrenegcows == 2 & data_sim_pos$dim == 101,], aes(x = cellcount)) +
  labs(x = "SCC (x1000 cells/ml)", y  = "log(Bayes Factor)", title = paste(birthmodel,crtmodel), subtitle = paste("Titre Cutpoints:",toString(cutpoints),"\nAge Cutpoints",toString(agebreaks)))


if(length(cutpoints) >= 1){p <- p + geom_line(aes(y = log(likelihood_cut1))) + 
  geom_text(aes(x = max(cellcount), y = log(max(likelihood_cut1)), label ="Cutpoint 1"), nudge_x = -(max(data_sim_pos$cellcount)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 2){p <- p + geom_line(aes(y = log(likelihood_cut2))) + 
  geom_text(aes(x = max(cellcount), y = log(max(likelihood_cut2)), label ="Cutpoint 2"), nudge_x = -(max(data_sim_pos$cellcount)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 3){p <- p + geom_line(aes(y = log(likelihood_cut3))) + 
  geom_text(aes(x = max(cellcount), y = log(max(likelihood_cut3)), label ="Cutpoint 3"), nudge_x = -(max(data_sim_pos$cellcount)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 4){p <- p + geom_line(aes(y = log(likelihood_cut4))) + 
  geom_text(aes(x = max(cellcount), y = log(max(likelihood_cut4)), label ="Cutpoint 4"), nudge_x = -(max(data_sim_pos$cellcount)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 5){p <- p + geom_line(aes(y = log(likelihood_cut5))) + 
  geom_text(aes(x = max(cellcount), y = log(max(likelihood_cut5)), label ="Cutpoint 5"), nudge_x = -(max(data_sim_pos$cellcount)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 6){p <- p + geom_line(aes(y = log(likelihood_cut6))) + 
  geom_text(aes(x = max(cellcount), y = log(max(likelihood_cut6)), label ="Cutpoint 6"), nudge_x = -(max(data_sim_pos$cellcount)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 7){p <- p + geom_line(aes(y = log(likelihood_cut7))) + 
  geom_text(aes(x = max(cellcount), y = log(max(likelihood_cut7)), label ="Cutpoint 7"), nudge_x = -(max(data_sim_pos$cellcount)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 8){p <- p + geom_line(aes(y = log(likelihood_cut8))) + 
  geom_text(aes(x = max(cellcount), y = log(max(likelihood_cut8)), label ="Cutpoint 8"), nudge_x = -(max(data_sim_pos$cellcount)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 9){p <- p + geom_line(aes(y = log(likelihood_cut9))) + 
  geom_text(aes(x = max(cellcount), y = log(max(likelihood_cut9)), label ="Cutpoint 9"), nudge_x = -(max(data_sim_pos$cellcount)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 10){p <- p + geom_line(aes(y = log(likelihood_cut10))) + 
  geom_text(aes(x = max(cellcount), y = log(max(likelihood_cut10)), label ="Cutpoint 10"), nudge_x = -(max(data_sim_pos$cellcount)/20), nudge_y = 0, size = 3, color = "black")}
p

ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/MarginalEffects/MELikelihoodSCC",birthmodel,crtmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),".png"))

p <- ggplot(data_sim_pos[data_sim_pos$age == 60 & data_sim_pos$yield == 30 & data_sim_pos$meantitrenegcows == 2 & data_sim_pos$cellcount == 1000 & data_sim_pos$dim == 101,], aes(x = predLR)) +
  labs(x = "Probability at Birth", y  = "log(Bayes Factor)", title = paste(birthmodel,crtmodel), subtitle = paste("Titre Cutpoints:",toString(cutpoints),"\nAge Cutpoints",toString(agebreaks)))


if(length(cutpoints) >= 1){p <- p + geom_line(aes(y = log(likelihood_cut1))) + 
  geom_text(aes(x = max(predLR), y = log(max(likelihood_cut1)), label ="Cutpoint 1"), nudge_x = -(max(data_sim_pos$predLR)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 2){p <- p + geom_line(aes(y = log(likelihood_cut2))) + 
  geom_text(aes(x = max(predLR), y = log(max(likelihood_cut2)), label ="Cutpoint 2"), nudge_x = -(max(data_sim_pos$predLR)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 3){p <- p + geom_line(aes(y = log(likelihood_cut3))) + 
  geom_text(aes(x = max(predLR), y = log(max(likelihood_cut3)), label ="Cutpoint 3"), nudge_x = -(max(data_sim_pos$predLR)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 4){p <- p + geom_line(aes(y = log(likelihood_cut4))) + 
  geom_text(aes(x = max(predLR), y = log(max(likelihood_cut4)), label ="Cutpoint 4"), nudge_x = -(max(data_sim_pos$predLR)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 5){p <- p + geom_line(aes(y = log(likelihood_cut5))) + 
  geom_text(aes(x = max(predLR), y = log(max(likelihood_cut5)), label ="Cutpoint 5"), nudge_x = -(max(data_sim_pos$predLR)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 6){p <- p + geom_line(aes(y = log(likelihood_cut6))) + 
  geom_text(aes(x = max(predLR), y = log(max(likelihood_cut6)), label ="Cutpoint 6"), nudge_x = -(max(data_sim_pos$predLR)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 7){p <- p + geom_line(aes(y = log(likelihood_cut7))) + 
  geom_text(aes(x = max(predLR), y = log(max(likelihood_cut7)), label ="Cutpoint 7"), nudge_x = -(max(data_sim_pos$predLR)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 8){p <- p + geom_line(aes(y = log(likelihood_cut8))) + 
  geom_text(aes(x = max(predLR), y = log(max(likelihood_cut8)), label ="Cutpoint 8"), nudge_x = -(max(data_sim_pos$predLR)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 9){p <- p + geom_line(aes(y = log(likelihood_cut9))) + 
  geom_text(aes(x = max(predLR), y = log(max(likelihood_cut9)), label ="Cutpoint 9"), nudge_x = -(max(data_sim_pos$predLR)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 10){p <- p + geom_line(aes(y = log(likelihood_cut10))) + 
  geom_text(aes(x = max(predLR), y = log(max(likelihood_cut10)), label ="Cutpoint 10"), nudge_x = -(max(data_sim_pos$predLR)/20), nudge_y = 0, size = 3, color = "black")}
p

ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/MarginalEffects/MELikelihoodpredLR",birthmodel,crtmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),".png"))

p <- ggplot(data_sim_pos[data_sim_pos$age == 60 & data_sim_pos$yield == 30 & data_sim_pos$meantitrenegcows == 2 & data_sim_pos$cellcount == 1000,], aes(x = dim)) +
  labs(x = "DIM", y  = "log(Bayes Factor)", title = paste(birthmodel,crtmodel), subtitle = paste("Titre Cutpoints:",toString(cutpoints),"\nAge Cutpoints",toString(agebreaks)))

if(length(cutpoints) >= 1){p <- p + geom_line(aes(y = log(likelihood_cut1))) + 
  geom_text(aes(x = max(dim), y = log(max(likelihood_cut1)), label ="Cutpoint 1"), nudge_x = -(max(data_sim_pos$dim)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 2){p <- p + geom_line(aes(y = log(likelihood_cut2))) + 
  geom_text(aes(x = max(dim), y = log(max(likelihood_cut2)), label ="Cutpoint 2"), nudge_x = -(max(data_sim_pos$dim)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 3){p <- p + geom_line(aes(y = log(likelihood_cut3))) + 
  geom_text(aes(x = max(dim), y = log(max(likelihood_cut3)), label ="Cutpoint 3"), nudge_x = -(max(data_sim_pos$dim)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 4){p <- p + geom_line(aes(y = log(likelihood_cut4))) + 
  geom_text(aes(x = max(dim), y = log(max(likelihood_cut4)), label ="Cutpoint 4"), nudge_x = -(max(data_sim_pos$dim)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 5){p <- p + geom_line(aes(y = log(likelihood_cut5))) + 
  geom_text(aes(x = max(dim), y = log(max(likelihood_cut5)), label ="Cutpoint 5"), nudge_x = -(max(data_sim_pos$dim)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 6){p <- p + geom_line(aes(y = log(likelihood_cut6))) + 
  geom_text(aes(x = max(dim), y = log(max(likelihood_cut6)), label ="Cutpoint 6"), nudge_x = -(max(data_sim_pos$dim)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 7){p <- p + geom_line(aes(y = log(likelihood_cut7))) + 
  geom_text(aes(x = max(dim), y = log(max(likelihood_cut7)), label ="Cutpoint 7"), nudge_x = -(max(data_sim_pos$dim)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 8){p <- p + geom_line(aes(y = log(likelihood_cut8))) + 
  geom_text(aes(x = max(dim), y = log(max(likelihood_cut8)), label ="Cutpoint 8"), nudge_x = -(max(data_sim_pos$dim)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 9){p <- p + geom_line(aes(y = log(likelihood_cut9))) + 
  geom_text(aes(x = max(dim), y = log(max(likelihood_cut9)), label ="Cutpoint 9"), nudge_x = -(max(data_sim_pos$dim)/20), nudge_y = 0, size = 3, color = "black")}

if(length(cutpoints) >= 10){p <- p + geom_line(aes(y = log(likelihood_cut10))) + 
  geom_text(aes(x = max(dim), y = log(max(likelihood_cut10)), label ="Cutpoint 10"), nudge_x = -(max(data_sim_pos$dim)/20), nudge_y = 0, size = 3, color = "black")}
p

ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/MarginalEffects/MELikelihoodDIM",birthmodel,crtmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),".png"))

####UPDATE POSTERIORS####

print("Updating posteriors...")

data$PosteriorOdds <- as.numeric(0)
data$POFloorApplied <- as.numeric(0)

time1 <- Sys.time()

if(POUpdateMode == "floor"){source_python("Y:/Ian/JohnesThresholds/JohnesProper/Data/PythonScripts/PyPOUpdater.py")
  data <- updatePO(data, posterioroddsfloor, priorstage)}

if(POUpdateMode == "lastxtests"){source_python("y:/ian/johnesthresholds/johnesproper/data/PythonScripts/PyPOUpdaterLastxtests.py")
  data <- updatePO(data, lastxtests,priorstage)}

if(POUpdateMode == "birthfraction"){source_python("y:/ian/johnesthresholds/johnesproper/data/PythonScripts/PyPOUpdaterBirthFraction.py")
  data <- updatePO(data, birthoddsfraction,priorstage)}

if(POUpdateMode == "ignoreuntil"){source_python("y:/ian/johnesthresholds/johnesproper/data/PythonScripts/PyPOUpdaterIgnoreUntil.py")
  data <- updatePO(data, priorstage, ignorethreshold)}

if(POUpdateMode == "highestinparity"){
  
  parities_summary <- unique(data[,c('calfeartag', 'parity')])
  
  write.csv(parities_summary, "y:/ian/johnesthresholds/johnesproper/data/parities_summary.csv", row.names = FALSE)
  
  no_cores <- detectCores() - 1  
  cl <- makePSOCKcluster(no_cores)
  registerDoParallel(cl)
  getDoParWorkers() 
  
  timez <- Sys.time()
  
  parityinfo <- foreach(i = 1:nrow(parities_summary), .verbose=TRUE, .combine = "rbind") %dopar% {
    print(i)
    ntests <- nrow(data[data$calfeartag == parities_summary$calfeartag[i] &
                data$parity == parities_summary$parity[i],])
    nless5 <- nrow(data[data$calfeartag == parities_summary$calfeartag[i] &
                data$parity == parities_summary$parity[i] &
                data$titre <5,])
    n520 <- nrow(data[data$calfeartag == parities_summary$calfeartag[i] &
                data$parity == parities_summary$parity[i] &
                data$titre >=5 & data$titre < 20,])
    n2030 <- nrow(data[data$calfeartag == parities_summary$calfeartag[i] &
                data$parity == parities_summary$parity[i] &
                data$titre >=20 & data$titre < 30,])
    ngreater30 <- nrow(data[data$calfeartag == parities_summary$calfeartag[i] &
                data$parity == parities_summary$parity[i] &
                data$titre >=30,])
    maxtitre <- max(data$titre[data$calfeartag == parities_summary$calfeartag[i] &
                     data$parity == parities_summary$parity[i]])
    
    c(ntests, nless5, n520, n2030, ngreater30, maxtitre)
    
  }
  
  stopCluster(cl)
  
  timey <- Sys.time()
  
  print(paste("Time taken:", round(difftime(timey, timez, units = "mins"),2), "mins"))

  colnames(parityinfo) <- c("ntests", "nless5", "n520", "n2030", "ngreater30" ,"maxtitre")
  
  parities_summary <- cbind(parities_summary, parityinfo)

  parities_summary$onlyless5 <- ifelse(parities_summary$ntests == parities_summary$nless5, 1, 0)
  
  data <- merge(data, parities_summary[,c(1,2,8,9)], by = c("calfeartag", "parity"))
 
  data <- data[data$onlyless5 == 0 | data$titre == data$maxtitre,]
  
  data <- data[,c(3,4,1,5:162,2,163:182)]
  
  source_python("Y:/Ian/JohnesThresholds/JohnesProper/Data/PythonScripts/PyPOUpdater.py")
  data <- updatePO(data, posterioroddsfloor, priorstage)
   
}


####CONVERT POSTERIOR ODDS TO POSTERIOR PROBABILITIES####

time2 <- Sys.time()

print(paste("Time to update posteriors:", round(difftime(time2, time1, units = "mins"),2), "mins"))

data$PosteriorProb <- as.numeric(data$PosteriorOdds / (1 + data$PosteriorOdds))

print(ggplot(data, aes(x = PosteriorProb)) +
  geom_histogram())

print(ggplot(data, aes(x = as.factor(cutpoint), y = log(likelihood))) +
         geom_boxplot())

data <- data[!is.na(data$PosteriorProb),]

####SAVING DATA####

print("Saving data (data_posteriors.csv)")

write.csv(data, "y:/ian/johnesthresholds/johnesproper/data/data_posteriors.csv", row.names=FALSE)

time2 <- Sys.time()

print(paste("Time processing likelihood models:", round(difftime(time2, time0, units = "mins"),2), "mins"))

rm(like)
rm(p)
rm(data_multinom)
rm(MNpreds_sim)
rm(data_sim_pos)
rm(data_sim)
rm(data_sim_neg)

rm(data_temp_TM)
rm(data_temp_TM0)
rm(data_temp_TM1)

rm(MNPredTM)
rm(MNPredTM0)
rm(MNPredTM1)
rm(data)

