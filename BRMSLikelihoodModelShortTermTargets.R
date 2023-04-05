####READ DATA####

data_brms <- read.csv('Y:/Ian/JohnesThresholds/JohnesProper/Data/BRMSModelResults/ExcludingBadData/BRMSAgeYieldSCCMTNCDIMIndSimulated1MBF/data_brms.csv')

data_brms$testnum <- as.integer(substr(data_brms$covsandtitre, 13, 15))

####GET AGE WHEN POSITIVE STATUS IS CONFIRMED####

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


####CREATE SHORT TERM TARGETS####

#####GET CURRENT STATUS#####

data_brms$crtstatus <- "U"

data_brms <- data_brms[with(data_brms, order(Farm, calfeartag, age)),]


regcores()

data_brms$crtstatus[3:nrow(data_brms)] <- foreach(r = 3:nrow(data_brms), .combine = "c") %dopar% {
  ifelse(!is.na(data_brms$ageposstatusconf[r]) & 
           data_brms$ageposstatusconf[r] <= data_brms$age[r], "1",
         ifelse(data_brms$calfeartag[r] == data_brms$calfeartag[r-1] &
                  data_brms$calfeartag[r] == data_brms$calfeartag[r-2] &
                  data_brms$class[r-2] == "L" &
                  data_brms$class[r-1] == "L" &
                  data_brms$class[r] == "L",
                "0", "U"))
}

stopCluster(cl)

#####CREATE SHORT TERM TARGETS#####

regcores()

futurestatus <- foreach(c = unique(data_brms$calfeartag), .combine = "rbind", .packages ="foreach") %dopar% {
  tempdata <- data_brms[data_brms$calfeartag == c,]
  tempdata <- tempdata[order(tempdata$age),]
  pp <- foreach(r = 1:nrow(tempdata), .combine = "rbind") %do% {
    
    tmp <- tempdata[tempdata$age <= (tempdata$age[r] + 3),]
    tmp <- tmp[nrow(tmp),]
    status3m <- tmp$crtstatus
    
    tmp <- tempdata[tempdata$age <= (tempdata$age[r] + 6),]
    tmp <- tmp[nrow(tmp),]
    status6m <- tmp$crtstatus
    
    tmp <- tempdata[tempdata$age <= (tempdata$age[r] + 12),]
    tmp <- tmp[nrow(tmp),]
    status12m <- tmp$crtstatus
    
    tmp <- tempdata[tempdata$age <= (tempdata$age[r] + 24),]
    tmp <- tmp[nrow(tmp),]
    status24m <- tmp$crtstatus
    
    tmp <- tempdata[tempdata$age <= (tempdata$age[r] + 48),]
    tmp <- tmp[nrow(tmp),]
    status48m <- tmp$crtstatus
    
    c(tempdata$Farm[r], tempdata$calfeartag[r], tempdata$age[r], status3m, status6m, status12m, status24m, status48m)
    
  }
  
  pp
  
}

stopCluster(cl)

futurestatus <- as.data.frame(futurestatus)

colnames(futurestatus) <- c('Farm',
                            'calfeartag',
                            'age',
                            'status3m',
                            'status6m',
                            'status12m',
                            'status24m',
                            'status48m')

futurestatus$age <- as.numeric(futurestatus$age)

data_brms <- merge(data_brms,
                   futurestatus,
                   by = c('Farm', 'calfeartag', 'age'),
                   all.x = TRUE)

data_brms <- data_brms[with(data_brms, order(Farm, calfeartag, age)),]


####LIMIT TO COWS HAVING A FAIR CHANCE TO BE CONFIRMED POSITIVE####

data_brms <- data_brms[with(data_brms, order(Farm, calfeartag,age)),]

data_brms$monthstonexttest <- NA

regcores()

data_brms$monthstonexttest[1:(nrow(data_brms)-1)] <- foreach(r = 1:(nrow(data_brms)-1), .combine = "c") %dopar% {
  ifelse(data_brms$Farm[r] == data_brms$Farm[r+1] &
           data_brms$calfeartag[r] == data_brms$calfeartag[r+1],
         data_brms$age[r+1] - data_brms$age[r],
         NA)    
}




stopCluster(cl)

data_brms <- data_brms[with(data_brms, order(Farm, calfeartag,age)),]

regcores()

p <- foreach(c = unique(data_brms$calfeartag), .combine = "rbind", .packages = "foreach") %dopar% {
  
  tempdata = data_brms[data_brms$calfeartag == c,]
  if(nrow(tempdata) >= 3){
    tempdata$age2testshence <- NA
    tempdata$age2testshence[1:(nrow(tempdata)-2)] <- foreach(r = 1:(nrow(tempdata)-2), .combine = "c") %do% {
      tempdata$age[r+2]
    }
  }
  if(nrow(tempdata) < 3){
    tempdata$age2testshence <- NA
  }
  tempdata[,c('Farm', 'calfeartag', 'age', 'age2testshence')]
  
}

stopCluster(cl)

data_brms <- merge(data_brms,
                p,
                by = c("Farm", "calfeartag", "age"),
                all.x = TRUE)

data_brms <- data_brms[with(data_brms, order(Farm, calfeartag, age)),]


write.csv(data_brms, 'y:/ian/johnesthresholds/johnesproper/data/data_brms.csv', row.names = FALSE)




data_brms_3mfairchance <- data_brms[!is.na(data_brms$age2testshence) & 
                                      data_brms$age2testshence - data_brms$age <= 3 &
                                      data_brms$ageatlasttest - data_brms$age >=3,]

data_brms_6mfairchance <- data_brms[!is.na(data_brms$age2testshence) & 
                                      data_brms$age2testshence - data_brms$age <= 6 &
                                      data_brms$ageatlasttest - data_brms$age >=6,]

data_brms_12mfairchance <- data_brms[!is.na(data_brms$age2testshence) & 
                                      data_brms$age2testshence - data_brms$age <= 12 &
                                       data_brms$ageatlasttest - data_brms$age >=12,]

data_brms_24mfairchance <- data_brms[!is.na(data_brms$age2testshence) & 
                                      data_brms$age2testshence - data_brms$age <= 24 &
                                       data_brms$ageatlasttest - data_brms$age >=24,]

data_brms_48mfairchance <- data_brms[!is.na(data_brms$age2testshence) & 
                                      data_brms$age2testshence - data_brms$age <= 48 &
                                       data_brms$ageatlasttest - data_brms$age >=48,]


write.csv(data_brms_3mfairchance, 'Y:/Ian/JohnesThresholds/JohnesProper/Data/BRMSModelResults/ExcludingBadData/BRMSAgeYieldSCCMTNCDIMIndSimulated1MBF/FutureCalibration/data_brms_3mfairchance.csv', row.names = FALSE)
write.csv(data_brms_6mfairchance, 'Y:/Ian/JohnesThresholds/JohnesProper/Data/BRMSModelResults/ExcludingBadData/BRMSAgeYieldSCCMTNCDIMIndSimulated1MBF/FutureCalibration/data_brms_6mfairchance.csv', row.names = FALSE)
write.csv(data_brms_12mfairchance, 'Y:/Ian/JohnesThresholds/JohnesProper/Data/BRMSModelResults/ExcludingBadData/BRMSAgeYieldSCCMTNCDIMIndSimulated1MBF/FutureCalibration/data_brms_12mfairchance.csv', row.names = FALSE)
write.csv(data_brms_24mfairchance, 'Y:/Ian/JohnesThresholds/JohnesProper/Data/BRMSModelResults/ExcludingBadData/BRMSAgeYieldSCCMTNCDIMIndSimulated1MBF/FutureCalibration/data_brms_24mfairchance.csv', row.names = FALSE)
write.csv(data_brms_48mfairchance, 'Y:/Ian/JohnesThresholds/JohnesProper/Data/BRMSModelResults/ExcludingBadData/BRMSAgeYieldSCCMTNCDIMIndSimulated1MBF/FutureCalibration/data_brms_48mfairchance.csv', row.names = FALSE)


####FAIRCHANCE SHORT TERM CALIPLOTS####

CaliPlot(data_brms_3mfairchance$PosteriorProb[data_brms_3mfairchance$status3m != "U"],
         data_brms_3mfairchance$status3m[data_brms_3mfairchance$status3m != "U"],
         ptitle = "Animals given a fair chance of going positive",
         psubtitle = "Within 3m")

CaliPlot(data_brms_6mfairchance$PosteriorProb[data_brms_6mfairchance$status6m != "U"],
         data_brms_6mfairchance$status6m[data_brms_6mfairchance$status6m != "U"],
         ptitle = "Animals given a fair chance of going positive",
         psubtitle = "Within 6m")

CaliPlot(data_brms_12mfairchance$PosteriorProb[data_brms_12mfairchance$status12m != "U"],
         data_brms_12mfairchance$status12m[data_brms_12mfairchance$status12m != "U"],
         ptitle = "Animals given a fair chance of going positive",
         psubtitle = "Within 12m")

CaliPlot(data_brms_24mfairchance$PosteriorProb[data_brms_24mfairchance$status24m != "U"],
         data_brms_24mfairchance$status24m[data_brms_24mfairchance$status24m != "U"],
         ptitle = "Animals given a fair chance of going positive",
         psubtitle = "Within 24m")

CaliPlot(data_brms_48mfairchance$PosteriorProb[data_brms_48mfairchance$status48m != "U"],
         data_brms_48mfairchance$status48m[data_brms_48mfairchance$status48m != "U"],
         ptitle = "Animals given a fair chance of going positive",
         psubtitle = "Within 48m")


####PICK OUT POOR PREDICTIONS####

ppdata <- data_brms_48mfairchance[data_brms_48mfairchance$PosteriorProb > 0.6 &
                                    data_brms_48mfairchance$PosteriorProb < 0.7 &
                                    data_brms_48mfairchance$status48m == "0",]

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


####CURRENT CALIBRATION####

CaliPlot(data_brms$PosteriorProb[data_brms$crtstatus != "U"],
         data_brms$crtstatus[data_brms$crtstatus != "U"],
         ptitle = "Calibration with current status")

####TRAINING AND TESTING DATASETS WITH NEW SHORTTERM TARGETS####

for (f in c(3,6,12,24,48)){
  assign('tempdata', get(paste0('data_brms_',f,'mfairchance')))
  colnum <- match(paste0('status',f,'m'), colnames(tempdata))
  assign(paste0('data_brms_',f,'mfairchance_modelling'), tempdata[tempdata[,colnum] != "U",])
}

####SHORT TERM MODELS####

#####EXAMINE TITRE DISTRIBUTIONS#####

ggplot() +
  geom_density(data = data_brms_48mfairchance_modelling[data_brms_48mfairchance_modelling$status48m == "0",],
               aes(x = titre),
               color = "red") +
  geom_density(data = data_brms_48mfairchance_modelling[data_brms_48mfairchance_modelling$status48m == "1",],
               aes(x = titre),
               color = "green") +
  labs(title = "Titres of animals confirmed positive/negative within 48 months")


#####FIT MODELS#####

######WITHIN 12m######

brms_mod_short_12m_lognorm_neg_yield_scc_mtnc_dimind <- brm(titre ~ yield + I(log(cellcount)) + meantitrenegcows + dim_dummy_05 + dim_dummy_510,
                                                            data = data_brms_12mfairchance_modelling[data_brms_12mfairchance_modelling$status12m == "0",],
                                                            family = lognormal(link = "identity"),
                                                            warmup = 500,
                                                            iter = 1000,
                                                            chains = 2,
                                                            cores = 6,
                                                            threads = threading(3),
                                                            backend = "cmdstanr")

######WITHIN 24m######

#######NEGATIVE COWS#######

brms_mod_short_24m_lognorm_neg_yield_scc_mtnc_dimind <- brm(titre ~ yield + I(log(cellcount)) + meantitrenegcows + dim_dummy_05 + dim_dummy_510,
                                                            data = data_brms_24mfairchance_modelling[data_brms_24mfairchance_modelling$status24m == "0",],
                                                            family = lognormal(link = "identity"),
                                                            warmup = 500,
                                                            iter = 1500,
                                                            chains = 2,
                                                            cores = 6,
                                                            threads = threading(3),
                                                            backend = "cmdstanr")

######WITHIN 48m######

#######NEGATIVE COWS#######

brms_mod_short_48m_lognorm_neg_yield_scc_mtnc_dimind <- brm(titre ~ yield + I(log(cellcount)) + meantitrenegcows + dim_dummy_05 + dim_dummy_510,
                                                  data = data_brms_48mfairchance_modelling[data_brms_48mfairchance_modelling$status48m == "0",],
                                                  family = lognormal(link = "identity"),
                                                  warmup = 500,
                                                  iter = 1500,
                                                  chains = 2,
                                                  cores = 6,
                                                  threads = threading(3),
                                                  backend = "cmdstanr")

brms_mod_short_48m_lognorm_neg_yield_scc_dimind <- brm(titre ~ yield + I(log(cellcount)) + dim_dummy_05 + dim_dummy_510,
                                                            data = data_brms_48mfairchance_modelling[data_brms_48mfairchance_modelling$status48m == "0",],
                                                            family = lognormal(link = "identity"),
                                                            warmup = 500,
                                                            iter = 1500,
                                                            chains = 2,
                                                            cores = 6,
                                                            threads = threading(3),
                                                            backend = "cmdstanr")


brms_mod_short_48m_gamma_neg_yield_scc_mtnc_dimind <- brm(titre ~ yield + I(log(cellcount)) + meantitrenegcows + dim_dummy_05 + dim_dummy_510,
                                                            data = data_brms_48mfairchance_modelling[data_brms_48mfairchance_modelling$status48m == "0",],
                                                            family = Gamma(link = "log"),
                                                            warmup = 500,
                                                            iter = 1500,
                                                            chains = 2,
                                                            cores = 6,
                                                            threads = threading(3),
                                                            backend = "cmdstanr")



brms_mod_short_48m_weibull_neg_yield_scc_mtnc_dimind <- brm(titre ~ yield + I(log(cellcount)) + meantitrenegcows + dim_dummy_05 + dim_dummy_510,
                                                          data = data_brms_48mfairchance_modelling[data_brms_48mfairchance_modelling$status48m == "0",],
                                                          family = weibull(link = "log"),
                                                          warmup = 500,
                                                          iter = 1500,
                                                          chains = 2,
                                                          cores = 6,
                                                          threads = threading(3),
                                                          backend = "cmdstanr")

brms_mod_short_48m_expon_neg_yield_scc_mtnc_dimind <- brm(titre ~ yield + I(log(cellcount)) + meantitrenegcows + dim_dummy_05 + dim_dummy_510,
                                                            data = data_brms_48mfairchance_modelling[data_brms_48mfairchance_modelling$status48m == "0",],
                                                            family = exponential(link = "log"),
                                                            warmup = 500,
                                                            iter = 1500,
                                                            chains = 2,
                                                            cores = 6,
                                                            threads = threading(3),
                                                            backend = "cmdstanr")

brms_mod_short_48m_frechet_neg_yield_scc_mtnc_dimind <- brm(titre ~ yield + I(log(cellcount)) + meantitrenegcows + dim_dummy_05 + dim_dummy_510,
                                                          data = data_brms_48mfairchance_modelling[data_brms_48mfairchance_modelling$status48m == "0",],
                                                          family = frechet(link = "log"),
                                                          warmup = 500,
                                                          iter = 1500,
                                                          chains = 2,
                                                          cores = 6,
                                                          threads = threading(3),
                                                          backend = "cmdstanr")

brms_mod_short_48m_skewnorm_neg_yield_scc_mtnc_dimind <- brm(titre ~ yield + I(log(cellcount)) + meantitrenegcows + dim_dummy_05 + dim_dummy_510,
                                                            data = data_brms_48mfairchance_modelling[data_brms_48mfairchance_modelling$status48m == "0",],
                                                            family = skew_normal(link = "identity"),
                                                            warmup = 500,
                                                            iter = 1500,
                                                            chains = 2,
                                                            cores = 6,
                                                            threads = threading(3),
                                                            backend = "cmdstanr")



#######POSITIVE COWS#######

######WITHIN 12m######


prior1 <- prior("normal(2,2)", nlpar = "b1") +
  prior("normal(1,2)", nlpar = "b2") +
  prior("normal(0, 0.5)", nlpar = "b3") +
  prior("normal(-1, 0.5", nlpar = "b4") +
  prior("normal(0.1, 0.3", nlpar = "b5") +
  prior("normal(0.1, 0.3", nlpar = "b6") +
  prior("normal(0.5,0.2", nlpar = "b7") +
  prior("normal(0.5,0.2", nlpar = "b8")

brms_mod_short_12m_lognorm_pos_age_yield_scc_mtnc_dimind <- brm(formula = bf(titre ~ b1 - (b2 * (exp(1)^(-b3*age))) + b4 * yield + b5 * log(cellcount) + b6 * meantitrenegcows + b7 * dim_dummy_05 + b8 * dim_dummy_510,
                                                                             b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8 ~ 1, 
                                                                             nl = TRUE),
                                                                prior = prior1,
                                                                data = data_brms_12mfairchance_modelling[data_brms_12mfairchance_modelling$status12m == "1",],
                                                                family = lognormal(link = "identity"),
                                                                warmup = 1000,
                                                                iter = 10000,
                                                                #control = list(adapt_delta = 0.95),
                                                                init = "random",
                                                                chains = 2,
                                                                cores = 4,
                                                                threads = threading(2),
                                                                backend = "cmdstanr")

######WITHIN 48m######

prior1 <- prior("normal(2,2)", nlpar = "b1") +
  prior("normal(1,2)", nlpar = "b2") +
  prior("normal(0, 0.5)", nlpar = "b3") +
  prior("normal(-1, 0.5", nlpar = "b4") +
  prior("normal(0.1, 0.3", nlpar = "b5") +
  prior("normal(0.1, 0.3", nlpar = "b6") +
  prior("normal(0.5,0.2", nlpar = "b7") +
  prior("normal(0.5,0.2", nlpar = "b8")


brms_mod_short_48m_lognorm_pos_age_yield_scc_mtnc_dimind <- brm(formula = bf(titre ~ b1 - (b2 * (exp(1)^(-b3*age))) + b4 * yield + b5 * log(cellcount) + b6 * meantitrenegcows + b7 * dim_dummy_05 + b8 * dim_dummy_510,
                                                              b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8 ~ 1, 
                                                              nl = TRUE),
                                                 prior = prior1,
                                                 data = data_brms_48mfairchance_modelling[data_brms_48mfairchance_modelling$status48m == "1",],
                                                 family = lognormal(link = "identity"),
                                                 warmup = 1000,
                                                 iter = 10000,
                                                 #control = list(adapt_delta = 0.95),
                                                 init = "random",
                                                 chains = 2,
                                                 cores = 6,
                                                 threads = threading(3),
                                                 backend = "cmdstanr")



######WITHIN 12m######

brms_mod_short_12m_lognorm_pos_age_yield_scc_mtnc_dimind <- brm(formula = bf(titre ~ b1 - (b2 * (exp(1)^(-b3*age))) + b4 * yield + b5 * log(cellcount) + b6 * meantitrenegcows + b7 * dim_dummy_05 + b8 * dim_dummy_510,
                                                                             b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8 ~ 1, 
                                                                             nl = TRUE),
                                                                prior = prior1,
                                                                data = data_brms_12mfairchance_modelling[data_brms_12mfairchance_modelling$status12m == "1",],
                                                                family = lognormal(link = "identity"),
                                                                warmup = 1000,
                                                                iter = 5000,
                                                                #control = list(adapt_delta = 0.95),
                                                                init = "random",
                                                                chains = 2,
                                                                cores = 4,
                                                                threads = threading(2),
                                                                backend = "cmdstanr")

####POSTERIOR CHECKS####

p <- pp_check(brms_mod_short_48m_lognorm_pos_age_yield_scc_mtnc_dimind)
p + xlim(0,30) + 
  labs(title = "lognorm",
       subtitle = "positive within 48m")


####SAVE MODEL JSONS####

json1 <- getbrmsmodparams('brms_mod_short_12m_lognorm_neg_yield_scc_mtnc_dimind', json = TRUE)
json2 <- getbrmsmodparams('brms_mod_short_12m_lognorm_pos_age_yield_scc_mtnc_dimind', json = TRUE)

write(json1, 'y:/ian/johnesthresholds/johnesproper/data/pickledmodels/bayesianlikelihood/negativecows/brms_mod_short_12m_lognorm_neg_yield_scc_mtnc_dimind.json')
write(json2, 'y:/ian/johnesthresholds/johnesproper/data/pickledmodels/bayesianlikelihood/positivecows/brms_mod_short_12m_lognorm_pos_age_yield_scc_mtnc_dimind.json')

####SIMULATE BAYES FACTORS ON ENTIRE DATASET####

thingy <- simulate_bayes_factors(data_brms, 
                                 negmod = 'brms_mod_short_24m_lognorm_neg_yield_scc_mtnc_dimind', 
                                 posmod = 'brms_mod_short_24m_lognorm_pos_age_yield_scc_mtnc_dimind',
                                 npd = 1000000)

 data_brms$likelihood <- thingy

data_brms <- data_brms[!is.na(data_brms$priorodds_crt),]

data_brms$PosteriorOdds <- as.numeric(0)
data_brms$POFloorApplied <- as.numeric(0)

posterioroddsfloor <- 0
priorstage <- "crt"

#source_python("Y:/Ian/JohnesThresholds/JohnesProper/Data/PythonScripts/PyPOUpdater.py")
#data_brms <- updatePO(data_brms, posterioroddsfloor, priorstage)


####UPDATE POSTERIORS####

data_brms <- RparallelUpdatePO(data_brms, priorstage, posterioroddsfloor)

data_brms$PosteriorProb <- data_brms$PosteriorOdds /
  (1 + data_brms$PosteriorOdds)

print(ggplot(data_brms,
             aes(x = PosteriorProb)) +
        geom_histogram())

data_brms$titre <- as.numeric(data_brms$titre)
data_brms$PosteriorProb <- as.numeric(data_brms$PosteriorProb)

data_brms$PosteriorProb[data_brms$PosteriorOdds == Inf] <- 1

write.csv(data_brms, 'Y:/Ian/JohnesThresholds/JohnesProper/Data/BRMSModelResults/ShortTermTargets/24m/data_brms.csv', row.names = FALSE)

####PLOT SAMPLE COWS####

samp <- sample(unique(data_brms$calfeartag), 100)


for (cow in samp){
  ggplot(data_brms[data_brms$calfeartag == cow,], aes(x = age)) +
    geom_point(aes(y = titre, color = class)) +
    geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
    geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
    geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, check_overlap = T, size = 3, color = "black") + 
    geom_text(aes(x = age, y = -10, label = cellcount - 0.0001), size = 3) +
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
  
  ggsave(paste("Y:/Ian/JohnesThresholds/JohnesProper/Data/BRMSModelResults/ShortTermTargets/24m/PosteriorProbCharts/",cow,".png"))
}


####MERGE NEW POSTERIORS INTO COWS GIVEN A FAIR CHANCE OF GOING POSITIVE IN THE SHORT TERM####

colnum <- match('PosteriorProb', colnames(data_brms_24mfairchance_modelling))

calidata <- data_brms_24mfairchance_modelling[,-colnum]

calidata <- merge(calidata,
                  data_brms[,c('Farm', 'calfeartag', 'age', 'PosteriorProb')],
                  by = c('Farm', 'calfeartag', 'age'),
                  all.x = TRUE)

CaliPlot(calidata$PosteriorProb,
         calidata$status24m,
         ptitle = "Model predicting positive status within 24 months",
         psubtitle = "Only cows with >= 2 tests within 24 months")


####GET POORLY PREDICTED DATA####

ppdata <- calidata[calidata$PosteriorProb >= 0.9 &
                     calidata$status12m == "0",]

ppsample <- sample(unique(ppdata$calfeartag), 20)


for (cow in ppsample){
  print(ggplot(data_brms[data_brms$calfeartag == cow,], aes(x = age)) +
    geom_point(aes(y = titre, color = class)) +
    geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
    geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
    geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, check_overlap = T, size = 3, color = "black") + 
    geom_text(aes(x = age, y = -10, label = cellcount - 0.0001), size = 3) +
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
