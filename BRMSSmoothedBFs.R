####GET DATA####

data_brms <- read.csv('y:/ian/johnesthresholds/johnesproper/data/brmsmodelresults/excludingbaddata/BRMSAgeYieldSCCMTNCDIMIndSimulated1MBF/data_brms.csv')

####ROLLING AVERAGE BAYES FACTOR####

smoother = 2 #Smooth over x number of tests

data_brms$testnum <- as.integer(substr(data_brms$covsandtitre, 13, 15))

sr <- splitacrosscores(data_brms, -1)

regcores(-1)

pp <- foreach(core = 1:length(sr), .combine = "c", .packages = "foreach") %dopar% {
  
  a <- ifelse(core == 1,
              1,
              sr[core-1] + 1)
  
  b <- sr[core]
  
  tempdata <- data_brms[a:b,]

  p <- foreach(r = 1:nrow(tempdata), .combine = "c") %do% {
  
    smoothbf <- ifelse(tempdata$testnum[r] < smoother, NA,
                                   mean(tempdata$likelihood[(r - smoother + 1):r]))
   
  
    tryCatch({write.table(r, paste0("C:/Users/Ian.glover.HEADOFFICE/Documents/PythonTools/ForEachRowCounter/rowsdonecore",core,".txt"), append = FALSE, sep = " ", dec = ".",
                                                      row.names = FALSE, col.names = FALSE)}, 
                                         error = function(cond){})  
    
    smoothbf
  }
  
  p

}

stopCluster(cl)

data_brms$likelihood <- pp

posterioroddsfloor = 0
priorstage = "crt"

data_brms$likelihood[is.na(data_brms$likelihood)] <- 1

data_brms <- RparallelUpdatePO(data_brms, priorstage, posterioroddsfloor)

data_brms$PosteriorProb <- data_brms$PosteriorOdds / (1 + data_brms$PosteriorOdds)


ggplot(data_brms,
       aes(x = PosteriorProb)) +
  geom_histogram()

write.csv(data_brms, 'y:/ian/johnesthresholds/johnesproper/data/brmsmodelresults/excludingbaddata/BRMSAgeYieldSCCMTNCDIMIndSimulated1MBFSmoothedBF/data_brms.csv', row.names = FALSE)

for (type in c('Pos', 'Neg', 'Unknown')){

  files <- list.files(paste0('y:/ian/johnesthresholds/johnesproper/data/brmsmodelresults/excludingbaddata/brmsageyieldsccmtncDIMIndsimulated1mbf/posteriorprobcharts/Target_altdef1_',type,'/'))
  
  sample <- substr(files, 1, nchar(files)-5)
  sample <- sample[2:length(sample)]
  
  for (cow in sample){
    ggplot(data_brms[data_brms$calfeartag == cow,], aes(x = age)) +
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
      labs(title = data_brms$calfeartag[data_brms$calfeartag == cow], subtitle = paste0("Birth Probability:", round(data_brms$priorprob_crt[data_brms$calfeartag == cow],2)))
    
    ggsave(paste0('y:/ian/johnesthresholds/johnesproper/data/brmsmodelresults/excludingbaddata/brmsageyieldsccmtncdimindsimulated1mbfsmoothedbf/PosteriorProbCharts/',type,'/',cow,'.png'))
  }

}


#####ROLLING AVERAGE BAYES FACTOR CALIBRATION PLOTS#####

CaliPlot(data_brms$PosteriorProb[data_brms$Target_altdef1 != "U"],
         data_brms$Target_altdef1[data_brms$Target_altdef1 != "U"],
         ptitle = "Smoothed Bayes Factors",
         psubtitle = "All Data")


####AVERAGE OF LAST FOUR BAYES FACTORS####

data_brms <- read.csv('y:/ian/johnesthresholds/johnesproper/data/brmsmodelresults/excludingbaddata/BRMSAgeYieldSCCMTNCDIMIndSimulated1MBF/data_brms.csv')

data_brms$avglast4bf <- NA

regcores()

data_brms$avglast4bf[4:nrow(data_brms)] <- foreach(r = 4:nrow(data_brms), .combine = "c") %dopar% {
  ifelse(data_brms$calfeartag[r] == data_brms$calfeartag[r-1] & 
           data_brms$calfeartag[r] == data_brms$calfeartag[r-2] &
           data_brms$calfeartag[r] == data_brms$calfeartag[r-3],
         mean(c(data_brms$likelihood[r],
                data_brms$likelihood[r-1],
                data_brms$likelihood[r-2],
                data_brms$likelihood[r-3])),
         NA)
}

stopCluster(cl)

ggplot(data_brms[data_brms$Target_altdef1 != "U",],
       aes(x = Target_altdef1,
           y = avglast4bf)) +
  geom_boxplot()


ggplot(data_brms[data_brms$Target_altdef1 != "U",],
       aes(x = avglast4bf,
           y = as.numeric(Target_altdef1))) +
  geom_point() +
  geom_smooth()
