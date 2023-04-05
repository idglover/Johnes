####PLOTS OF POSTERIOR PROBABILITY OVER TIME####

data_brms$testnum <- as.integer(substr(data_brms$covsandtitre, 13, 15))


ggplot(data_brms,
       aes(x = testnum,
           y = PosteriorProb)) +
         geom_point() +
         geom_smooth() +
         facet_wrap(~ Target_altdef1)

ggplot(data_brms,
       aes(x = age,
           y = PosteriorProb)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ Target_altdef1)

ggplot(data_brms[data_brms$Target_altdef1 != "U",],
       aes(x = age,
           y = PosteriorProb,
           color = Target_altdef1)) +
  geom_point(alpha = 0.3) +
  geom_smooth(alpha = 0.3) +
  scale_colour_manual(values = c("darkgreen", "darkred"))

ggplot(data_brms[data_brms$Target_altdef1 != "U",],
       aes(x = testnum,
           y = PosteriorProb,
           color = Target_altdef1)) +
  geom_point(alpha = 0.3) +
  geom_smooth(alpha = 0.3) +
  scale_colour_manual(values = c("darkgreen", "darkred"))

agect <- seq(18,102, by = 6)



data_brms$age_cat <- cut(data_brms$age, breaks = agect)



ggplot(data_brms[data_brms$Target_altdef1 != "U" &
                   !is.na(data_brms$age_cat),],
       aes(x = age_cat,
           y = PosteriorProb, 
           color = Target_altdef1)) +
  geom_boxplot() +
  facet_wrap(~Target_altdef1)+
  scale_color_manual(values = c("green", "red")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot(data_brms[data_brms$Target_altdef1 != "U" &
                   !is.na(data_brms$age_cat),],
       aes(x = age_cat,
           y = PosteriorProb, 
           color = Target_altdef1)) +
  geom_violin() +
  facet_wrap(~Target_altdef1)+
  scale_color_manual(values = c("green", "red")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


####TITRE DISTRIBUTION x MONTHS PRIOR TO CONFIRMED POSITIVE STATUS####

#####GET AGE POSITIVE STATUS CONFIRMED#####

poscowsonly <- data_brms[data_brms$ageatfirstposstatus > 0,]

regcores(-1)

confposstatus <- foreach(c = unique(poscowsonly$calfeartag), .combine = "rbind") %dopar% {
  
  tempdata <- poscowsonly[poscowsonly$calfeartag == c,]
  
  tempdata <- tempdata[with(tempdata, order(age)),]
  
  tn = gregexpr("HH", tempdata$profile[1])[[1]][1]
  agehh <- ifelse(tn > 0, tempdata$age[tn + 1], 10000)
  tn = gregexpr("HMH", tempdata$profile[1])[[1]][1]
  agehmh <- ifelse(tn > 0, tempdata$age[tn + 2], 10000)
  tn = gregexpr("HLH", tempdata$profile[1])[[1]][1]
  agehlh <- ifelse(tn > 0, tempdata$age[tn + 2], 10000)
  
  confage <- min(c(agehh, agehmh, agehlh))
  
  print(c)
  
  c(tempdata$Farm[1], tempdata$calfeartag[1], confage)
  
}

stopCluster(cl)

colnames(confposstatus) <- c('Farm', 'calfeartag', 'ageposstatusconf')

data_brms <- merge(data_brms,
                   confposstatus,
                   by = c('Farm',
                          'calfeartag'),
                   all.x = TRUE)

data_brms <- data_brms[with(data_brms, order(Farm, calfeartag, age)),]

data_brms$ageposstatusconf <- as.numeric(data_brms$ageposstatusconf)



data_brms$monthsuntilposstatusconf <- data_brms$ageposstatusconf - data_brms$age

agect <- c(-100,0,6,12,18,24,36,48,72,96,200)

data_brms$monthsuntilposstatusconf_cat <- cut(data_brms$monthsuntilposstatusconf, breaks = agect)

ggplot(data_brms[!is.na(data_brms$monthsuntilposstatusconf_cat),],
       aes(x = monthsuntilposstatusconf_cat,
           y = PosteriorProb)) +
  geom_boxplot() +
  labs(title = "PosteriorProb relative to age at which positive status is confirmed")

ggplot(data_brms[!is.na(data_brms$monthsuntilposstatusconf_cat),],
       aes(x = monthsuntilposstatusconf_cat,
           y = titre)) +
  geom_hline(yintercept = 30, linetype = "dashed") + 
  geom_boxplot() +
  labs(title = "Titres relative to age at which positive status is confirmed")

ggplot(data_brms[!is.na(data_brms$monthsuntilposstatusconf_cat),],
       aes(x = monthsuntilposstatusconf_cat,
           y = likelihood)) +
  geom_boxplot() +
  labs(title = "Bayes Factors relative to age at which positive status is confirmed")


ggplot(data_brms[!is.na(data_brms$monthsuntilposstatusconf_cat) &
                   data_brms$titre < 30,],
       aes(x = monthsuntilposstatusconf_cat,
           y = PosteriorProb)) +
  geom_boxplot() +
  labs(title = "PosteriorProb relative to age at which positive status is confirmed",
       subtitle = "Only titres < 30")

ggplot(data_brms[!is.na(data_brms$monthsuntilposstatusconf_cat) &
                   data_brms$titre < 30,],
       aes(x = monthsuntilposstatusconf_cat,
           y = titre)) +
  geom_hline(yintercept = 30, linetype = "dashed") + 
  geom_boxplot() +
  labs(title = "Titres relative to age at which positive status is confirmed",
       subtitle = "Only titres < 30")

ggplot(data_brms[!is.na(data_brms$monthsuntilposstatusconf_cat) &
                          data_brms$titre < 30,],
       aes(x = monthsuntilposstatusconf_cat,
           y = likelihood)) +
  geom_boxplot() +
  labs(title = "Bayes Factors relative to age at which positive status is confirmed",
       subtitle = "Only titres < 30")


####CALIBRATION####

#####ALL TIMEPOINTS#####

CaliPlot(data_brms$PosteriorProb[data_brms$Target_altdef1 != "U"],
         data_brms$Target_altdef1[data_brms$Target_altdef1 != "U"],
         ptitle = "All Data")

ggsave('y:/ian/johnesthresholds/johnesproper/data/BRMSModelResults/CaliPlot.png')



#####FINAL POSTERIORS#####

tempdata <- data_brms[data_brms$testnum == data_brms$ntests &
                        data_brms$Target_altdef1 != "U",]

CaliPlot(tempdata$PosteriorProb,
         tempdata$Target_altdef1,
         ptitle = "Final Posteriors")

ggsave('y:/ian/johnesthresholds/johnesproper/data/brmsmodelresults/CaliPlot_Finals.png')

#####BY TEST NUMBER#####

for (tn in 1:15){
  print(CaliPlot(data_brms$PosteriorProb[data_brms$Target_altdef1 != "U" &
                                           data_brms$testnum == tn],
                 data_brms$Target_altdef1[data_brms$Target_altdef1 != "U" &
                                            data_brms$testnum == tn],
                 ptitle = "All Data", psubtitle = paste0("Test number: ",tn)))
  ggsave(paste0('Y:/Ian/JohnesThresholds/JohnesProper/Data/BRMSModelResults/CaliPlot_testnum_',tn,'.png'))
}

#####BY AGE#####

agebk <- c(24,36,48,60,72,84,96,108)

for(a in 1:(length(agebk)-1)){
  tempdata <- data_brms[data_brms$Target_altdef1 != "U" &
                          data_brms$age >= agebk[a] &
                          data_brms$age < agebk[a+1],]
  print(CaliPlot(tempdata$PosteriorProb,
                 tempdata$Target_altdef1,
                 ptitle = "All Data",
                 psubtitle = paste0(agebk[a]," <= Age < ",agebk[a+1])))
  ggsave(paste0('Y:/Ian/JohnesThresholds/JohnesProper/Data/BRMSModelResults/CaliPlot_age_',agebk[a],'to',agebk[a+1],'.png'))
}

#####FINAL POSTERIORS BY AGE#####

agebk <- c(24,36,48,60,72,84,96,108)

for(a in 1:(length(agebk)-1)){
  tempdata <- data_brms[data_brms$Target_altdef1 != "U" &
                          data_brms$age >= agebk[a] &
                          data_brms$age < agebk[a+1] &
                          data_brms$testnum == data_brms$ntests,]
  print(CaliPlot(tempdata$PosteriorProb,
                 tempdata$Target_altdef1,
                 ptitle = "All Data, balanced target",
                 psubtitle = paste0(agebk[a]," <= Age < ",agebk[a+1])))
  ggsave(paste0('Y:/Ian/JohnesThresholds/JohnesProper/Data/BRMSModelResults/CaliPlot_balanced_finals_age_',agebk[a],'to',agebk[a+1],'.png'))
}



####TRY BALANCING DATA FOR CALIBRATION####

length(unique(data_brms$calfeartag[data_brms$Target_altdef1 == "1"])) #1390 Positive Cows

negcowsample <- sample(unique(data_brms$calfeartag[data_brms$Target_altdef1 == "0"]),1390)

calidata_balanced <- rbind(data_brms[data_brms$Target_altdef1 == "1",],
                           data_brms[data_brms$calfeartag %in% negcowsample,])


length(unique(calidata_balanced$calfeartag[calidata_balanced$Target_altdef1 == "0"]))
length(unique(calidata_balanced$calfeartag[calidata_balanced$Target_altdef1 == "1"]))

#####ALL TIMEPOINTS#####

CaliPlot(calidata_balanced$PosteriorProb[calidata_balanced$Target_altdef1 != "U"],
         calidata_balanced$Target_altdef1[calidata_balanced$Target_altdef1 != "U"],
         ptitle = "All Data", psubtitle = "Balanced Target")

ggsave('y:/ian/johnesthresholds/johnesproper/data/brmsmodelresults/CaliPlot_balanced.png')

#####FINAL POSTERIORS#####

tempdata <- calidata_balanced[calidata_balanced$testnum == calidata_balanced$ntests &
                                calidata_balanced$Target_altdef1 != "U",]

CaliPlot(tempdata$PosteriorProb,
         tempdata$Target_altdef1,
         ptitle = "Final Posteriors", psubtitle = "Balanced Target")

ggsave('y:/ian/johnesthresholds/johnesproper/data/brmsmodelresults/CaliPlot_balanced_finals.png')


#####BY AGE#####

agebk <- c(24,36,48,60,72,84,96,108)

for(a in 1:(length(agebk)-1)){
  tempdata <- calidata_balanced[calidata_balanced$age >= agebk[a] &
                                  calidata_balanced$age < agebk[a+1],]
  print(CaliPlot(tempdata$PosteriorProb,
                 tempdata$Target_altdef1,
                 ptitle = "All Data, balanced target",
                 psubtitle = paste0(agebk[a]," <= Age < ",agebk[a+1])))
  ggsave(paste0('Y:/Ian/JohnesThresholds/JohnesProper/Data/BRMSModelResults/CaliPlot_balanced_age_',agebk[a],'to',agebk[a+1],'.png'))
}



#####FINAL POSTERIORS BY AGE#####



agebk <- c(24,36,48,60,72,84,96,108)

for(a in 1:(length(agebk)-1)){
  tempdata <- calidata_balanced[calidata_balanced$age >= agebk[a] &
                                  calidata_balanced$age < agebk[a+1] &
                                  calidata_balanced$testnum == calidata_balanced$ntests,]
  print(CaliPlot(tempdata$PosteriorProb,
                 tempdata$Target_altdef1,
                 ptitle = "All Data, balanced target",
                 psubtitle = paste0(agebk[a]," <= Age < ",agebk[a+1])))
  ggsave(paste0('Y:/Ian/JohnesThresholds/JohnesProper/Data/BRMSModelResults/CaliPlot_balanced_finals_age_',agebk[a],'to',agebk[a+1],'.png'))
}


####PLOT POOR PREDICTIONS####


ppdata <- calidata_balanced[calidata_balanced$age >= 84 &
                      calidata_balanced$age < 96 &
  calidata_balanced$PosteriorProb >= 0.2 &
    calidata_balanced$PosteriorProb <= 0.5 &
    calidata_balanced$Target_altdef1 == "1",]

ppsample <- sample(unique(ppdata$calfeartag), 20)

for (cow in ppsample){
  print(ggplot(data_brms[data_brms$calfeartag == cow,], aes(x = age)) +
          geom_point(aes(y = titre, color = class)) +
          geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
          geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
          geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
          geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_y = 5, check_overlap = T, size = 3) + 
          geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, check_overlap = T, size = 3, color = "black") + 
          geom_text(aes(x = age, y = -10, label = cellcount), size = 3) +
          geom_text(aes(x = age, y = -15, label = dim), size = 3) +
          geom_text(aes(x = age, y = -2, label = ifelse(POFloorApplied == 1, "*", "")), color = "black") +
          geom_text(aes(x = min(age) - ((max(age) - min(age))/10), y = -5, label = "BF"), size = 3) +
          geom_text(aes(x = min(age) - ((max(age) - min(age))/10), y = -10, label = "SCC"), size = 3) +
          geom_text(aes(x = min(age) - ((max(age) - min(age))/10), y = -15, label = "DIM"), size = 3) +
          scale_color_manual(values = group.colours) +
          geom_line(aes(y = PosteriorProb*100)) +
          geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
          geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
          scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
          labs(title = data_brms$calfeartag[data_brms$calfeartag == cow], subtitle = paste0("Birth Probability:", round(data_brms$priorprob_crt[data_brms$calfeartag == cow],2))))
} 

