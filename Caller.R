####SET PARAMETERS####

#####GLOBAL#####

target <- "altdef1" #Or "MEYER" OR "QMMS_Strictneg" OR "altdef1"

cutpoints <- c(0,5,10,20,30,60,100) # Set titre cutpoints
agebreaks <- c(23,30,35,40,45,50,75,300) # Set age cutpoints

#####PRIOR MODELS#####

birthmodel <- "GLMM" #Or "LR"

priormod_birth <- "finalpriormodel_birth" #Choose pickled model
priormod_12mold <- "finalpriormodel_12mold" #Choose pickled model
priormod_crt <- "finalpriormodel_crt" #Choose pickled model

#####FITTING LIKELIHOOD MODELS#####

weightedlikelihood = "N" # Y or N. Applies weights to multinomial regression, based on Target prevalence

downsampling <- "N" # Y or N. Random downsampling of majority class.

#####PREDICTING LIKELIHOOD MODELS#####

newcutpoints <- "Y" #"Y" if using new cutpoints when predicting likelihood model

likemod <- "finallikemodel" #Choose pickled likelihood model

crtmodel <- "multinom_model_re_agesq_inter_yieldcat_mtnc_scc_dim" # Set likelihood model name

priorstage <- "crt" #birth, 12mold or crt

#####POSTERIOR UPDATING#####

POUpdateMode <- "floor" # OR  lastxtests OR birthfraction OR floor OR ignoreuntil OR highestinparity

posterioroddsfloor <- 0.00157 #Set minimum posterior odds allowed
lastxtests <- 15 #Set number of previous tests with which to update posterior
birthoddsfraction <- 0.2
ignorethreshold <- 5 #Set titre above which posteriors begin to be updated

#####PLOTTING OPTIONS#####

producegenplots <- "Y" #Produce reams of Positive/Negative/Unknown Cow Plots

producePPDataplots <- "N" #Produce CaliPlots of user-defined poorly-predicted data points

plotoutliers = "Y" #Plot animals with outlying posteriors for profilegroup in PosteriorChecking.R

#####LINEAR MODEL FOR BAYES FACTOR PREDICTIONS#####

bflinmod <- "FREQUENTIST" #Or BAYESIAN

#####TEST DATA#####

addpurchasedcows = "N" #Add purchased cows to test farms (assigning median birth prior across all herds)


####CALL INDIVIDUAL COMPONENTS####

source("Y:/Ian/R/JohnesProper/TVURaider.R") #Gather data from TVUs

py_run_file("Y:/Ian/JohnesThresholds/JohnesProper/Data/PythonScripts/MergeJohnesProperFarms.py") #Combine SQL data for all farms

source("Y:/Ian/R/JohnesProper/BirthModelSelection.R") #Cross validate candidate birth models

source("Y:/Ian/R/JohnesProper/BirthModels.R") #Create birth models

source("Y:/Ian/R/JohnesProper/LikelihoodRECrossValidation.R") #Cross validate random-effects likelihood models

source("Y:/Ian/R/JohnesProper/LikelihoodModels.R") #Fit likelihood model, produce marginal effects plots, update posteriors

source("Y:/Ian/R/JohnesProper/UpdatePosteriors.R") #Update posteriors (not re-fitting likelihood model)

source("Y:/Ian/R/JohnesProper/Validation.R") #CaliPlots for different age groups/cutpoints, and plot user-defined poorly-predicted data.

source("Y:/Ian/R/JohnesProper/Plots.R") #Final plots for test cows and (optional) groups of cows (Pos/Neg/Unknown/Random)

source("Y:/Ian/R/JohnesProper/PosteriorChecking.R") #Plot posterior densities for different profiles





source("Y:/Ian/R/JohnesProper/TVURaiderTestFarms.R") #Gather data from TEST FARM TVUs

py_run_file("Y:/Ian/JohnesThresholds/JohnesProper/Data/PythonScripts/MergeJohnesProperTESTFarms.py") #Combine SQL data for test farms

source("Y:/Ian/R/JohnesProper/BirthModelsTestFarms.R") #Predict birth model on test farms

source("Y:/Ian/R/JohnesProper/LikelihoodTestFarms.R") #Predict likelihood model on test farms and update posteriors

source("Y:/Ian/R/JohnesProper/PlotsTestFarms.R") #Plot cows on test farms


####SET PARAMETERS FOR LOOPING DIFFERENT MODELS####

cutpointslist <- list(c(0,5,10,20,30,60,100),
                      c(0,10,20,30,60,100),
                      c(0,20,30,60,100),
                      c(0,5,10,20,30,60,80,100,150,200))

agebreakslist <- list(c(23,30,35,40,45,50,75,300),
                      c(23,35,40,45,50,75,300),
                      c(23,40,50,75,300),
                      c(23,30,35,40,45,50,75,100,150,300))
    

####CROSS VALIDATION DIFFERENT MODELS AND DIFFERENT CUTPOINT AND AGE DEFINITIONS#####


timee <- Sys.time()

for (alpha in 1:length(cutpointslist)){
  for (beta in 1:length(agebreakslist)){
    print(paste0("***********CUTPOINTS DEFINITION: ",alpha,"***********"))
    print(paste0("***********AGES DEFINITION: ",beta,"***********"))
    cutpoints <- cutpointslist[[alpha]]
    agebreaks <- agebreakslist[[beta]]
    source("Y:/Ian/R/JohnesProper/MultinomialCaliPlots.R")
  }
}


timef <- Sys.time()

print(paste0("Time taken for all cross validation: ", round(difftime(timef, timee, units = "mins"),1)," mins"))


                  

#####LOOP LIKELIHOOD MODEL FITTING AND POSTERIOR PREDICTIONS######


timea <- Sys.time()
for (alpha in 1:length(cutpointslist)){
  for (beta in 1:length(agebreakslist)){
    timeb <- Sys.time()
    it <- ((alpha-1)*length(cutpointslist)) + beta
    totit <- length(cutpointslist) * length(agebreakslist)
    print(paste0("Iteration ", it, "/",totit," (",round(((it-1)/totit*100),1),"%)"))
    cutpoints <- cutpointslist[[alpha]]
    agebreaks <- agebreakslist[[beta]]
    source("Y:/Ian/R/JohnesProper/LikelihoodModels.R") #
    source("Y:/Ian/R/JohnesProper/Validation.R")
    source("Y:/Ian/R/JohnesProper/Plots.R")
    timec <- Sys.time()
    print(paste("Iteration",it,"time:", round(difftime(timec, timeb, units = "mins"),2),"mins"))
    print("--------------------------------------------")
  }
}
timed <- Sys.time()
print(paste("Time taken for all iterations:", round(difftime(timed, timea, units = "mins"),1),"mins"))






