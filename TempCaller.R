for (i in c(0.001,0.005,0.05)){
  posterioroddsfloor <- i
  source("Y:/Ian/R/JohnesProper/LikelihoodModels.R") #Fit likelihood model, produce marginal effects plots, update posteriors
  source("Y:/Ian/R/JohnesProper/PosteriorChecking.R") #Plot posterior densities for different profiles
}
