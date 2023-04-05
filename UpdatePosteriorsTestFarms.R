print("Updating posteriors...")
print(paste0("Posterior Odds Floor: ",posterioroddsfloor))

data <- read.csv("y:/ian/johnesthresholds/johnesproper/data/data_TESTFARMS_birthpriors_cutpoints.csv")

data$PosteriorOdds <- as.numeric(0)
data$POFloorApplied <- as.numeric(0)



time1 <- Sys.time()

#write(data, "y:/ian/johnesthresholds/johnesproper/data/data_posteriors.csv", row.names = FALSE)






source_python("Y:/Ian/JohnesThresholds/JohnesProper/Data/PythonScripts/PyPOUpdater.py")


data <- updatePO(data, posterioroddsfloor)


######Old R Loop for posterior updating######

#data$PosteriorOdds[1] <- data$birthpriorLR[1] * data$likelihood[1]

#data_dt <- as.data.table(data)

#remaining <- length(data_dt$PosteriorOdds[data_dt$PosteriorOdds == 0])

#if(posterioroddsfloor == 0){
#repeat{
#  if(birthmodel == "MARS"){data_dt[,PosteriorOdds := ifelse(calfeartag == shift(calfeartag, 1L), shift(PosteriorOdds) * likelihood, birthpriorMARS * likelihood)]}
#  if(birthmodel == "LR"){data_dt[,PosteriorOdds := ifelse(calfeartag == shift(calfeartag, 1L), shift(PosteriorOdds) * likelihood, birthpriorLR * likelihood)]}
#  if(length(data_dt$PosteriorOdds[data_dt$PosteriorOdds == 0]) == remaining){break}
#  remaining <- length(data_dt$PosteriorOdds[data_dt$PosteriorOdds == 0])
#}      
#}


#  repeat{
#    
#    if (birthmodel == "LR"){
#    
#    data_dt[,PosteriorOdds := ifelse(calfeartag == shift(calfeartag, 1L) & (shift(PosteriorOdds, 1L) * likelihood) < posterioroddsfloor,
#            posterioroddsfloor,
#            ifelse(calfeartag != shift(calfeartag, 1L), birthpriorLR * likelihood,
#            shift(PosteriorOdds, 1L) * likelihood))]
#    
#    }
#    
#    if(length(data_dt$PosteriorOdds[data_dt$PosteriorOdds == 0]) == remaining){break}
#    remaining <- length(data_dt$PosteriorOdds[data_dt$PosteriorOdds == 0])
#
#  }


#data <- as.data.frame(data_dt)


#####Convert PosteriorOdds to Posterior Probabilities and save data (data_posteriors.csv)#####
time2 <- Sys.time()

print(paste("Time to update posteriors:", round(difftime(time2, time1, units = "mins"),2), "mins"))

data$PosteriorProb <- as.numeric(data$PosteriorOdds / (1 + data$PosteriorOdds))

print(ggplot(data, aes(x = PosteriorProb)) +
        geom_histogram())

ggplot(data, aes(x = as.factor(cutpoint), y = log(likelihood))) +
  geom_boxplot()

data <- data[!is.na(data$PosteriorProb),]

print("Saving data (data_TESTFARMS_posteriors.csv)")

write.csv(data, "y:/ian/johnesthresholds/johnesproper/data/data_TESTFARMS_posteriors.csv", row.names=FALSE)
