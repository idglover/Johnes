###SELECT ONLY ANIMALS WITH COMPLETE YIELD DATA###

df_complete <- data.frame(cow = unique(data_brms$calfeartag))
df_complete$complete <- 1
df_complete$Target_QMMS <- "0"

regcores()

cc <- foreach(r = 1:nrow(df_complete), .combine = "rbind") %dopar% {
  tmpdata <- data_brms[data_brms$calfeartag == df_complete$cow[r],c('age','yield','titre','cellcount', 'Target_QMMS')]
  tmp <- grepl("NA", tmpdata)
  tmp2 <- ifelse(min(tmpdata$yield[!is.na(tmpdata$yield)]) == 0 |
                   min(tmpdata$titre[!is.na(tmpdata$titre)]) == 0 |
                   min(tmpdata$cellcount[!is.na(tmpdata$cellcount)]) == 0,
                 TRUE, FALSE)
  complete <- ifelse(!is.na(match(TRUE, tmp)) | tmp2 == TRUE, 0, 1)
  target <- tmpdata$Target_QMMS[nrow(tmpdata)]
  
  c(complete, target)
  
}

df_complete$complete <- cc[,1]
df_complete$Target_QMMS <- as.factor(cc[,2])

stopCluster(cl)

calfsampleqmms1 <- unique(df_complete$cow[df_complete$complete == "1" &
                                                   df_complete$Target_QMMS == "1"]), 100, replace = FALSE)

calfsampleqmms0 <- sample(unique(df_complete$cow[df_complete$complete == "1" &
                                                   df_complete$Target_QMMS == "0"]), 100, replace = FALSE)

calfsampleqmmsU <- sample(unique(df_complete$cow[df_complete$complete == "1" &
                                                   df_complete$Target_QMMS == "U"]), 100, replace = FALSE)

calfsamplerandom <- sample(unique(df_complete$cow[df_complete$complete == 1]) , 200, replace  = FALSE)

###GENERATE PLOTS###

for (i in calfsampleqmms1){
  ggplot(data[data$calfeartag == i,], aes(x = age)) +
    geom_point(aes(y = titre, color = class)) + 
    geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
    geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
    geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
    #geom_text(aes(y = -4, label = ifelse(POFloorApplied == 1,"POF","")), size = 2, angle = -90 ) +
    scale_color_manual(values = group.colours) +
    geom_line(aes(y = PosteriorProb*100)) +
    geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
    scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
    labs(title = i, subtitle = paste(crtmodel,"\nBirth Probability:", round(data$priorprob_birth[data$calfeartag == i],2),"12m Probability:",round(data$priorprob_12mold[data$calfeartag == i],2),"Crt Probability:",round(data$priorprob_crt[data$calfeartag == i],2),"\n Titre Cutpoints:", toString(cutpoints), "\n", "Age cutpoints:", toString(agebreaks)))
  
  ggsave(paste("Y:/Ian/JohnesThresholds/JohnesProper/Data/PosteriorProbCharts/QMMSPosCows/",i,".png"))
  
  ggplot(data_brms[data_brms$calfeartag == i,], aes(x = age)) +
    geom_point(aes(y = titre, color = class)) + 
    geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
    geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
    geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
    #geom_text(aes(y = -4, label = ifelse(POFloorApplied == 1,"POF","")), size = 2, angle = -90 ) +
    scale_color_manual(values = group.colours) +
    geom_line(aes(y = PosteriorProb*100)) +
    geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
    scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
    labs(title = i, subtitle = paste("Bayes Likelihood Model\nBirth Probability:", round(data_brms$priorprob_birth[data_brms$calfeartag == i],2),"12m Probability:",round(data_brms$priorprob_12mold[data_brms$calfeartag == i],2),"Crt Probability:",round(data_brms$priorprob_crt[data_brms$calfeartag == i],2)))
  
  ggsave(paste("Y:/Ian/JohnesThresholds/JohnesProper/Data/PosteriorProbCharts/Bayesian/Target_QMMS_Pos/",i,".png"))
  
}

for (i in calfsampleqmmsU){
  ggplot(data[data$calfeartag == i,], aes(x = age)) +
    geom_point(aes(y = titre, color = class)) + 
    geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
    geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
    geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
    #geom_text(aes(y = -4, label = ifelse(POFloorApplied == 1,"POF","")), size = 2, angle = -90 ) +
    scale_color_manual(values = group.colours) +
    geom_line(aes(y = PosteriorProb*100)) +
    geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
    scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
    labs(title = i, subtitle = paste(crtmodel,"\nBirth Probability:", round(data$priorprob_birth[data$calfeartag == i],2),"12m Probability:",round(data$priorprob_12mold[data$calfeartag == i],2),"Crt Probability:",round(data$priorprob_crt[data$calfeartag == i],2),"\n Titre Cutpoints:", toString(cutpoints), "\n", "Age cutpoints:", toString(agebreaks)))
  
  ggsave(paste("Y:/Ian/JohnesThresholds/JohnesProper/Data/PosteriorProbCharts/QMMSUnknownCows/",i,".png"))

  ggplot(data_brms[data_brms$calfeartag == i,], aes(x = age)) +
    geom_point(aes(y = titre, color = class)) + 
    geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
    geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
    geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
    #geom_text(aes(y = -4, label = ifelse(POFloorApplied == 1,"POF","")), size = 2, angle = -90 ) +
    scale_color_manual(values = group.colours) +
    geom_line(aes(y = PosteriorProb*100)) +
    geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
    scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
    labs(title = i, subtitle = paste("Bayes Likelihood Model\nBirth Probability:", round(data_brms$priorprob_birth[data_brms$calfeartag == i],2),"12m Probability:",round(data_brms$priorprob_12mold[data_brms$calfeartag == i],2),"Crt Probability:",round(data_brms$priorprob_crt[data_brms$calfeartag == i],2)))
  
  ggsave(paste("Y:/Ian/JohnesThresholds/JohnesProper/data/PosteriorProbCharts/Bayesian/Target_QMMS_Unknown/",i,".png"))
  
  }

for (i in calfsampleqmms0){
  ggplot(data[data$calfeartag == i,], aes(x = age)) +
    geom_point(aes(y = titre, color = class)) + 
    geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
    geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
    geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
    #geom_text(aes(y = -4, label = ifelse(POFloorApplied == 1,"POF","")), size = 2, angle = -90 ) +
    scale_color_manual(values = group.colours) +
    geom_line(aes(y = PosteriorProb*100)) +
    geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
    scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
    labs(title = i, subtitle = paste(crtmodel,"\nBirth Probability:", round(data$priorprob_birth[data$calfeartag == i],2),"12m Probability:",round(data$priorprob_12mold[data$calfeartag == i],2),"Crt Probability:",round(data$priorprob_crt[data$calfeartag == i],2),"\n Titre Cutpoints:", toString(cutpoints), "\n", "Age cutpoints:", toString(agebreaks)))
  
  ggsave(paste("Y:/Ian/JohnesThresholds/JohnesProper/Data/PosteriorProbCharts/QMMSNegCows/",i,".png"))

  ggplot(data_brms[data_brms$calfeartag == i,], aes(x = age)) +
    geom_point(aes(y = titre, color = class)) + 
    geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
    geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
    geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
    #geom_text(aes(y = -4, label = ifelse(POFloorApplied == 1,"POF","")), size = 2, angle = -90 ) +
    scale_color_manual(values = group.colours) +
    geom_line(aes(y = PosteriorProb*100)) +
    geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
    scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
    labs(title = i, subtitle = paste("Bayes Likelihood Model\nBirth Probability:", round(data_brms$priorprob_birth[data_brms$calfeartag == i],2),"12m Probability:",round(data_brms$priorprob_12mold[data_brms$calfeartag == i],2),"Crt Probability:",round(data_brms$priorprob_crt[data_brms$calfeartag == i],2)))
  
  ggsave(paste("Y:/Ian/JohnesThresholds/JohnesProper/data/PosteriorProbCharts/Bayesian/Target_QMMS_Neg/",i,".png"))
  
}

testcows <- c("UK723449401226",
              "UK706252503632",
              "UK382226501365",
              "UK382226501015",
              "UK382226401014",
              "UK382226101137",
              "UK360336601224",
              "UK344962601339",
              "UK344962601332",
              "UK344962401792",
              "UK344962201363",
              "UK344962201335",
              "UK344962201237",
              "UK323079201963",
              "UK323001301913",
              "UK283905705912",
              "UK283905705800",
              "UK283905602467",
              "UK283905602271",
              "UK283905503789",
              "UK283905203317",
              "UK207270701322",
              "UK200932103046",
              "UK141520703389")

for (i in testcows){
  
  ggplot(data[data$calfeartag == i,], aes(x = age)) +
          geom_point(aes(y = titre, color = class)) +
          geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
          geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
          #geom_text(aes(y = -4, label = ifelse(POFloorApplied == 1,"POF","")), size = 2, angle = -90 ) +
          geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
          geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
          geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
          scale_color_manual(values = group.colours) +
          geom_line(aes(y = PosteriorProb*100)) +
          geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
          geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
          scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
          labs(title = data$calfeartag[data$calfeartag == i], subtitle = paste(crtmodel,"\nBirth Probability:", round(data$priorprob_birth[data$calfeartag == i],2),"12m Probability:",round(data$priorprob_12mold[data$calfeartag == i],2),"Crt Probability:",round(data$priorprob_crt[data$calfeartag == i],2),"\n Titre Cutpoints:", toString(cutpoints), "\n", "Age cutpoints:", toString(agebreaks)))
  ggsave(paste0("Y:/Ian/JohnesThresholds/JohnesProper/Data/PosteriorProbCharts/TestCows/Target_QMMS/POF0/",i,".png"))
  
  ggplot(data_brms[data_brms$calfeartag == i,], aes(x = age)) +
    geom_point(aes(y = titre, color = class)) + 
    geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
    geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
    geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
    #geom_text(aes(y = -4, label = ifelse(POFloorApplied == 1,"POF","")), size = 2, angle = -90 ) +
    scale_color_manual(values = group.colours) +
    geom_line(aes(y = PosteriorProb*100)) +
    geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
    scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
    labs(title = i, subtitle = paste("Bayes Likelihood Model\nBirth Probability:", round(data_brms$priorprob_birth[data_brms$calfeartag == i],2),"12m Probability:",round(data_brms$priorprob_12mold[data_brms$calfeartag == i],2),"Crt Probability:",round(data_brms$priorprob_crt[data_brms$calfeartag == i],2)))
  
  ggsave(paste("Y:/Ian/JohnesThresholds/JohnesProper/data/PosteriorProbCharts/Bayesian/TestCows/",i,".png"))
}


