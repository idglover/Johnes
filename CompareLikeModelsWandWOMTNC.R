data <- read.csv("y:/ian/johnesthresholds/johnesproper/data/data_birthpriors_cutpoints.csv")

data$Target_QMMS <- as.factor(data$Target_QMMS)

data$titre <- as.numeric(data$titre)

data <- data[
  data$age >= 24 &
    !is.na(data$age),]

oldmod <- readRDS("y:/ian/johnesthresholds/johnesproper/data/pickledmodels/finallikemodel.rds")

newmod <- readRDS("y:/ian/johnesthresholds/johnesproper/data/pickledmodels/finallikemodel_age_yield_scc_mtnc.rds")

farms <- c("319_UoN",
           "1751_Perrett",
           "329_Crooklands",
           "230_Stockton",
           "311_Gleave")

collated <- data.frame()

for (FARM in farms) {
  data_farm <- data[data$Farm == FARM,]
  
  data_temp_TM <- data_farm
  
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
  
  MNPredTM <- predict(oldmod, newdata = data_temp_TM, type = "response", conditional = FALSE)

  end1 <- nrow(MNPredTM)/2
  start2 <- (nrow(MNPredTM)/2)+1
  end2 <- nrow(MNPredTM)
  
  MNPredTM0 <- MNPredTM[c(1:end1),]
  MNPredTM1 <- MNPredTM[c(start2:end2),]
  
  head(MNPredTM1,10)
  
  head(MNPredTM0,10)
  
  data_temp_TM <- data_farm
  
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
  
  data_farm$MNPredProbTM0 <- data_temp_TM$MNPredProbTM0
  data_farm$MNPredProbTM1 <- data_temp_TM$MNPredProbTM1
  
  data_farm$likelihood <- data_farm$MNPredProbTM1/data_farm$MNPredProbTM0
  oldlike <- data_farm$likelihood
  
  data_farm$PosteriorOdds <- as.numeric(0)
  data_farm$POFloorApplied <- as.numeric(0)
  
  posterioroddsfloor = 0.00157
  priorstage = "crt"
  
  source_python("Y:/Ian/JohnesThresholds/JohnesProper/Data/PythonScripts/PyPOUpdater.py")
  data_farm <- updatePO(data_farm, posterioroddsfloor, priorstage)

  oldPostOdds <- data_farm$PosteriorOdds
  
  
  
  data_temp_TM <- data_farm
  
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
  
  MNPredTM <- predict(newmod, newdata = data_temp_TM, type = "response", conditional = FALSE)
  
  end1 <- nrow(MNPredTM)/2
  start2 <- (nrow(MNPredTM)/2)+1
  end2 <- nrow(MNPredTM)
  
  MNPredTM0 <- MNPredTM[c(1:end1),]
  MNPredTM1 <- MNPredTM[c(start2:end2),]
  
  head(MNPredTM1,10)
  
  head(MNPredTM0,10)
  
  data_temp_TM <- data_farm
  
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
  
  data_farm$MNPredProbTM0 <- data_temp_TM$MNPredProbTM0
  data_farm$MNPredProbTM1 <- data_temp_TM$MNPredProbTM1
  
  data_farm$likelihood <- data_farm$MNPredProbTM1/data_farm$MNPredProbTM0
  newlike <- data_farm$likelihood
  
  data_farm$PosteriorOdds <- as.numeric(0)
  data_farm$POFloorApplied <- as.numeric(0)
  
  posterioroddsfloor = 0.00157
  priorstage = "crt"
  
  source_python("Y:/Ian/JohnesThresholds/JohnesProper/Data/PythonScripts/PyPOUpdater.py")
  data_farm <- updatePO(data_farm, posterioroddsfloor, priorstage)
  
  newPostOdds <- data_farm$PosteriorOdds
  
  data_farm$oldPostProb <- oldPostOdds/(1+oldPostOdds)
  data_farm$newPostProb <- newPostOdds/(1+newPostOdds)
  data_farm$oldlike <- oldlike
  data_farm$newlike <- newlike
  
  collated <- rbind(collated, data_farm)
  
}


for (FARM in farms){
  
  if(FARM == "319_UoN"){
    prefix = "UK141797A"}
  
  if(FARM == "1751_Perrett"){
    prefix = "UK354648A"}
  
  if(FARM == "329_Crooklands"){
    prefix = "UK100603A"}
  
  if(FARM == "230_Stockton"){
    prefix = "UK304238A"}
  
  if(FARM == "311_Gleave"){
    prefix = "UK161022A"}
    
  
  con <- dbConnect(RSQLite::SQLite(), paste("C:/Users/Ian.glover.HEADOFFICE/AppData/Local/QMMS/TotalVet_beta/data/",prefix,".tvu", sep = ""))
  
  latestevent <- date(dbGetQuery(con, '
                          select max(ev.date)

    FROM event ev')[[1]])
  
  cowsalive <- dbGetQuery(con, '
                        
    SELECT 

    a.earTag,
    ca.start,
    ca.end

    FROM animal a

    INNER JOIN cow_alive ca on a.id = ca.animalId


    WHERE ca.cOnFarm = 1 AND a.sex = "C"')
  
  cowsalive$start <- date(cowsalive$start)
  cowsalive$end <- date(cowsalive$end)
  
  cowsalive <- unique(cowsalive$earTag[which((cowsalive$end - latestevent) < 200 & (cowsalive$end - latestevent) > -21)])
  
  
  
  cowsaliveindata <- unique(collated$calfeartag[which(collated$calfeartag %in% cowsalive)])
  
  crtjohnesprobdata <- collated[which(collated$age == collated$ageatlasttest & collated$calfeartag %in% cowsaliveindata),]
  crtjohnesprobdata <- crtjohnesprobdata[which(!duplicated(crtjohnesprobdata$calfeartag)),]
  
  crtjohnesprobtable <- data.frame(Eartag = crtjohnesprobdata$calfeartag, 
                                   OldBayesProb = round(crtjohnesprobdata$oldPostProb,2),
                                   NewBayesProb = round(crtjohnesprobdata$newPostProb, 2))
  
  crtjohnesprobtable <- crtjohnesprobtable[order(-crtjohnesprobtable$OldBayesProb),]
  
  linenos <- dbGetQuery(con, '
                      SELECT
                      
                      a.earTag,
                      a.mgtCode
                      
                      FROM animal a')
  
  crtjohnesprobtable <- merge(crtjohnesprobtable, linenos, by.x = "Eartag", by.y = "earTag", all.x = TRUE)
  
  crtjohnesprobtable <- crtjohnesprobtable[,c(4,1,2,3)]
  
  write.csv(crtjohnesprobtable, paste0("y:/ian/johnesthresholds/johnesproper/data/LikelihoodWithOrWithoutMTNC/",FARM,"_LatestProbs.csv"), row.names = FALSE)
  
  dbDisconnect(con)
  
  for (cow in cowsaliveindata){
    ggplot(collated[collated$calfeartag == cow,], aes(x = age)) +
      geom_point(aes(y = titre, color = class)) +
      geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
      geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
      geom_text(aes(x = age, y = oldPostProb * 100, label =round(oldPostProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
      geom_text(aes(x = age, y = newPostProb * 100, label =round(newPostProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3, color = "red") +
      geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
      geom_text(aes(x = age, y = -5, label =round(oldlike, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
      geom_text(aes(x = age, y = -3, label =round(newlike, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "red") + 
      geom_text(aes(x = age, y = -2, label = ifelse(POFloorApplied == 1, "*", ""))) +
      scale_color_manual(values = group.colours) +
      geom_line(aes(y = oldPostProb*100)) +
      geom_line(aes(y = newPostProb*100), color  = "red") +
      geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
      geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
      scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
      labs(title = paste0(FARM,"   ",cow), subtitle = paste0("Birth Probability:", round(collated$priorprob_crt[data$calfeartag == cow],2)))
    ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/likelihoodwithorwithoutmtnc/cowplots/",FARM,"_",cow,".png"))
  }
}
