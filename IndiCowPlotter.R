data <- read.csv("y:/ian/johnesthresholds/johnesproper/data/data_posteriors.csv")

data$titre <- as.numeric(data$titre)
data$PosteriorProb <- as.numeric(data$PosteriorProb)

data$Target_QMMS <- as.factor(data$Target_QMMS)

testcows <- c("UK207658600662")



group.colours <- c("L" = "green", "M" = "orange", "H" = "red")

for (cow in testcows){
  print(ggplot(data[data$calfeartag == cow,], aes(x = age)) +
          geom_point(aes(y = titre, color = class)) +
          geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
          geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
          #geom_text(aes(y = -4, label = ifelse(POFloorApplied == 1,"POF","")), size = 2, angle = -90 ) +
          geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
          geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
          geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
          geom_text(aes(x = age, y = -2, label = ifelse(POFloorApplied == 1,"*",""))) + 
          scale_color_manual(values = group.colours) +
          geom_line(aes(y = PosteriorProb*100)) +
          geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
          geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
          scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
          labs(title = data$calfeartag[data$calfeartag == cow], subtitle = paste("\nBirth Probability:", round(data$priorprob_birth[data$calfeartag == cow],2),"12m Probability:",round(data$priorprob_12mold[data$calfeartag == cow],2),"Crt Probability:",round(data$priorprob_crt[data$calfeartag == cow],2))))
}




