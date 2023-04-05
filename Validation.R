time0 <- Sys.time()

#Validating Posteriors

print("Reading data...")

data <- read.csv("y:/ian/johnesthresholds/johnesproper/data/data_posteriors.csv")

print(ggplot(data, aes(x = age, y = PosteriorProb)) +
  geom_point() +
  labs(x = "Age", y = "Posterior Probability", title = "Posterior Probabilities"))

preds <- data$PosteriorProb[data$Target_QMMS != "U" & data$calfeartag != "NA"]
preds <- preds[!is.na(preds)]
obs <- as.factor(data$Target_QMMS[data$Target_QMMS != "U" & data$calfeartag != "NA"])
obs <- obs[!is.na(obs)]
obs <- droplevels(obs, "U")

print(CaliPlot(preds, obs, nbins = 20, ptitle = crtmodel, psubtitle = paste(crtmodel, "\n",birthmodel,"\nTitres", toString(cutpoints),"\nAges",toString(agebreaks))))
ggsave(paste("Y:/Ian/JohnesThresholds/JohnesProper/Data/CaliPlotsOverAgeTestNum/Caliplot",birthmodel,crtmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),"AllAges.png"))


data$testnum <- as.numeric(substr(data$covsandtitre,13,15))

data$Target_QMMS <- as.factor(data$Target_QMMS)

print("CaliPlots for different age categories...")

for (i in levels(as.factor(data$age_cat))){
  preds <- data$PosteriorProb[data$Target_QMMS != "U" & data$age_cat == i & data$calfeartag != "NA"]
  preds <- preds[!is.na(preds)]
  obs <- data$Target_QMMS[data$age_cat == i & data$Target_QMMS != "U" & data$calfeartag != "NA"]
  obs <- obs[!is.na(obs)]
  obs <- droplevels(obs, "U")
  print(CaliPlot(preds, obs,
                 
                 nbins = 10,           
                 ptitle = paste("Predictions From", i, "Age Bracket"), psubtitle = paste(crtmodel, "\n",birthmodel,"\nTitres", toString(cutpoints),"\nAges",toString(agebreaks))))
  ggsave(paste("Y:/Ian/JohnesThresholds/JohnesProper/Data/CaliPlotsOverAgeTestNum/Caliplot",birthmodel,crtmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks), i,".png"))}



levels(as.factor(data$age_cat))[6]

if(producePPDataplots == "Y"){

print("Plots of poorly predicted data (user specified)...")

PPData <- data[data$age_cat == levels(as.factor(data$age_cat))[6] & data$PosteriorProb > 0.5 & data$PosteriorProb < 0.8 & data$Target_QMMS == "0",]

dim(PPData)

group.colours <- c("L" = "green", "M" = "orange", "H" = "red")

for (cow in unique(PPData$calfeartag)){
  ggplot(data[data$calfeartag == cow,], aes(x = age)) +
    geom_point(aes(y = titre, color = class)) +
    geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
    geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
    scale_color_manual(values = group.colours) +
    geom_line(aes(y = PosteriorProb*100)) +
    geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
    scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
    labs(title = data$calfeartag[data$calfeartag == cow], subtitle = paste(crtmodel,"\n",birthmodel, "Birth Probability:", data$birthpriorMARS[data$calfeartag == cow], "\n Titre Cutpoints:", toString(cutpoints), "\n", "Age cutpoints:", agebreaks))
  ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/PosteriorProbCharts/PoorPreds/UnbalancedData/",cow,".png"))
}



set.seed(9560)
data_cali_balanced <- downSample(x = data,
                                 y = data$Target_Meyer)


summary(data_cali_balanced$Target_Meyer)

for (i in levels(as.factor(data_cali_balanced$age_cat))){
  print(CaliPlot(data_cali_balanced$PosteriorProb[data_cali_balanced$Target_Meyer != "U" & data_cali_balanced$age_cat == i & !is.na(data_cali_balanced$PosteriorProb) & data_cali_balanced$calfeartag != "NA"], data_cali_balanced$Target_Meyer[data_cali_balanced$age_cat == i & data_cali_balanced$Target_Meyer != "U" & !is.na(data_cali_balanced$PosteriorProb) & data_cali_balanced$calfeartag != "NA"],
                 nbins = 5,           
                 ptitle = paste("Predictions From", i, "Age Bracket")))}

unique(data_cali_balanced$age_cat)[3]

PPData <- data_cali_balanced[data_cali_balanced$PosteriorProb > 0.15 & data_cali_balanced$PosteriorProb < 0.3 & data_cali_balanced$age_cat == unique(data_cali_balanced$age_cat)[1] & data_cali_balanced$Target_Strict1 == "1" & data_cali_balanced$calfeartag != "NA",]

dim(PPData)

unique(PPData$calfeartag)

group.colours <- c("L" = "green", "M" = "orange", "H" = "red")

for (cow in unique(PPData$calfeartag)){
  ggplot(data[data$calfeartag == cow,], aes(x = age)) +
    geom_point(aes(y = titre, color = class)) +
    geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
    geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
    scale_color_manual(values = group.colours) +
    geom_line(aes(y = PosteriorProb*100)) +
    geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
    scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
    labs(title = data$calfeartag[data$calfeartag == cow], subtitle = paste(crtmodel,"\n",birthmodel, "Birth Probability:", data$birthpriorMARS[data$calfeartag == cow], "\n Titre Cutpoints:", toString(cutpoints), "\n", "Age cutpoints:", agebreaks))
  ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/PosteriorProbCharts/PoorPreds/BalancedData/",cow,".png"))
}

}


print(ggplot(data[data$Target_Meyer == "0",], aes(x = PosteriorProb)) +
  geom_histogram() +
  labs(title = "Meyer Negative Cows", x = "Posterior Probability"))


print(ggplot(data[data$Target_Meyer == "1",], aes(x = PosteriorProb)) +
  geom_histogram() +
  labs(title = "Meyer Positive Cows", x = "Posterior Probability"))


time2 <- Sys.time()

print(paste("Time processing validation:", round(difftime(time2, time0, units = "mins"),2), "mins"))

