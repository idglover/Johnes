data_brms <- read.csv('Y:/Ian/JohnesThresholds/JohnesProper/Data/BRMSModelResults/ExcludingBadData/BRMSAgeYieldSCCMTNCDIMIndSimulated1MBF/data_brms.csv')

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


data_brms$monthstoconfpos <- as.numeric(data_brms$ageposstatusconf) - data_brms$age

cpts <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)

data_brms_pre_conf_pos <- data_brms[(data_brms$ageatlasttest - data_brms$age) >= 48 &
                                      (is.na(data_brms$ageposstatusconf) |
                                         data_brms$age < as.numeric(data_brms$ageposstatusconf)),]
  
  
 

futurepossummary <- as.data.frame(foreach (c = 1:(length(cpts)-1), .combine = "rbind") %do% {

  c(paste0(cpts[c]," <= Prob < ",cpts[c+1]), 
    summary(data_brms_pre_conf_pos$monthstoconfpos[data_brms_pre_conf_pos$PosteriorProb >= cpts[c] &
                                                         data_brms_pre_conf_pos$PosteriorProb < cpts[c+1]]),
    nrow(data_brms_pre_conf_pos[data_brms_pre_conf_pos$PosteriorProb >= cpts[c] &
                                  data_brms_pre_conf_pos$PosteriorProb < cpts[c+1],]))

})

colnames(futurepossummary) <- c('PosteriorProb', 'Min', 'Q1', 'Median', 'Mean', 'Q3', 'Max', 'NeverPos','nTests')

futurepossummary$PctNeverPos <- (as.numeric(futurepossummary$NeverPos) / as.numeric(futurepossummary$nTests)) * 100

futurepossummary$NeverPos <- as.numeric(futurepossummary$NeverPos)

ggplot(futurepossummary,
       aes(x = PosteriorProb,
           y = PctNeverPos)) +
  geom_point() +
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  labs(title = "All Tests with at least 4 years of subsequent testing")
