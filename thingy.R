infinitycows <- c('UK103311601604',
                  'UK103311401791',
                  'UK103311201824',
                  'UK103311301839',
                  'UK103311501932',
                  'UK103311401644',
                  'UK103311101746')

thingy <- data_brms_testing[data_brms_testing$calfeartag %in% infinitycows,]


summary(thingy$likelihood)

thingy$likelihood[thingy$likelihood == 100000000] <- 250

thingy <- RparallelUpdatePO(thingy, priorstage, posterioroddsfloor)

thingy$PosteriorProb <- thingy$PosteriorOdds /
  (1 + thingy$PosteriorOdds)

print(ggplot(thingy,
             aes(x = PosteriorProb)) +
        geom_histogram())

thingy$titre <- as.numeric(thingy$titre)
thingy$PosteriorProb <- as.numeric(thingy$PosteriorProb)

thingy$PosteriorProb[thingy$PosteriorOdds == Inf] <- 1

View(thingy[,c('calfeartag', 'date', 'likelihood', 'PosteriorOdds', 'PosteriorProb'),])

for (cow in infinitycows){
  print(ggplot(thingy[thingy$calfeartag == cow,], aes(x = age)) +
    geom_point(aes(y = titre, color = class)) +
    geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
    geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
    geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    #geom_text(aes(x = age, y = PosteriorProb_brms * 100, label =round(PosteriorProb_brms, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3, color = "blue") + 
    geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = -4, label =round(likelihood, 1)), nudge_x = 0, check_overlap = T, size = 3, color = "black") + 
    #geom_text(aes(x = age, y = -6, label =round(likelihood_brms, 1)), nudge_x = 0, check_overlap = T, size = 3, color = "blue") + 
    geom_text(aes(x = age, y = -2, label = ifelse(POFloorApplied == 1, "*", "")), color = "black") +
    #geom_text(aes(x = age, y = -3, label = ifelse(POFloorApplied_brms == 1, "*", "")), color = "blue") +
    geom_text(aes(x = age, y = -8, label = cellcount), color = "black", size = 3) +
    geom_text(aes(x = age, y = -10, label = dim), color = "black", size = 3) +
    geom_text(aes(x = min(age) - (min(age)/10), y = -4, label = "BF"), color = "black" ,size = 3) +
    #geom_text(aes(x = min(age) - (min(age)/10), y = -6, label = "Bayes BF"), color = "blue" ,size = 3) +
    geom_text(aes(x = min(age) - (min(age)/10), y = -8, label = "SCC"), color = "black", size = 3) +
    geom_text(aes(x = min(age) - (min(age)/10), y = -10, label = "DIM"), color = "black", size = 3) +
    scale_color_manual(values = group.colours) +
    geom_line(aes(y = PosteriorProb*100)) +
    #geom_line(aes(y = PosteriorProb_brms * 100), color = "blue") +
    geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
    scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
    labs(title = thingy$calfeartag[thingy$calfeartag == cow], subtitle = paste0("Birth Probability:", round(thingy$priorprob_crt[thingy$calfeartag == cow],2))))
  
}
