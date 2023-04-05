if(POUpdateMode == "floor"){
print(paste0("Updating Posteriors. Posterior Odds Floor: ",posterioroddsfloor))}

if(POUpdateMode == "lastxtests"){
  print(paste0("Updating Posteriors. Using last ",lastxtests," tests."))}

if(POUpdateMode == "birthfraction"){
  print(paste0("Updating Posteriors. Posterior Odds Floor: ",birthoddsfraction," * birth prior odds."))}

if(POUpdateMode == "ignoreuntil"){
  print(paste0("Updating Posteriors. Ignore Threshold: ", ignorethreshold))
}

####READ DATA WITH CUTPOINTS####

data <- read.csv("y:/ian/johnesthresholds/johnesproper/data/data_birthpriors_cutpoints.csv")

#####UPDATE POSTERIORS#####

data$PosteriorOdds <- as.numeric(0)
data$POFloorApplied <- as.numeric(0)

time1 <- Sys.time()

if(POUpdateMode == "floor"){source_python("Y:/Ian/JohnesThresholds/JohnesProper/Data/PythonScripts/PyPOUpdater.py")
  data <- updatePO(data, posterioroddsfloor, priorstage)}

if(POUpdateMode == "lastxtests"){source_python("y:/ian/johnesthresholds/johnesproper/data/PythonScripts/PyPOUpdaterLastxtests.py")
  data <- updatePO(data, lastxtests, priorstage)}

if(POUpdateMode == "birthfraction"){source_python("y:/ian/johnesthresholds/johnesproper/data/PythonScripts/PyPOUpdaterBirthFraction.py")
  data <- updatePO(data, birthoddsfraction, priorstage)}

if(POUpdateMode == "ignoreuntil"){source_python("y:/ian/johnesthresholds/johnesproper/data/PythonScripts/PyPOUpdaterIgnoreUntil.py")
  data <- updatePO(data, priorstage, ignorethreshold)}

####CONVERT POSTERIOR ODDS TO POSTERIOR PROBABILITIES####

time2 <- Sys.time()

print(paste("Time to update posteriors:", round(difftime(time2, time1, units = "mins"),2), "mins"))

data$PosteriorProb <- as.numeric(data$PosteriorOdds / (1 + data$PosteriorOdds))

print(ggplot(data, aes(x = PosteriorProb)) +
        geom_histogram())

ggplot(data, aes(x = as.factor(cutpoint), y = log(likelihood))) +
  geom_boxplot()

data <- data[!is.na(data$PosteriorProb),]

####WRITE DATA####

print("Saving data (data_posteriors.csv)")

write.csv(data, "y:/ian/johnesthresholds/johnesproper/data/data_posteriors.csv", row.names=FALSE)

rm(data)

