time0 <- Sys.time()

print("Reading data...")

tvupath <- "c:/users/ian.glover.headoffice/desktop/"

farm = "UK376111A"

data <- read.csv(paste0(tvupath,"data_TESTFARMS_birthpriors.csv"))

data$titre <- as.numeric(data$titre)
data$PosteriorProb <- as.numeric(data$PosteriorProb)

data$Target_QMMS <- as.factor(data$Target_QMMS)

str(data$Target_QMMS)




data$testnum <- 1



for (i in 2:nrow(data)) {
  data$testnum[i] <- ifelse(data$calfeartag[i] != data$calfeartag[i-1], 1,
                                       data$testnum[i-1] + 1)
  
}


data_brms <- read.csv(paste0(tvupath,"data_TESTFARMS_birthpriors.csv"))

print(paste0("N Rows: ",nrow(data_brms), " N Cows: ",length(unique(data_brms$calfeartag))))

####CREATE COW GROUPING####

data_brms$farmcow <- paste0(data_brms$Farm, data_brms$calfeartag)

####ENGINEER DATA####

data_brms$dim_cat <- cut(data_brms$dim, breaks = c(-1,5,10,10000))

data_brms$dim_cat <- relevel(data_brms$dim_cat, "(10,1e+04]")

data_brms$dim_dummy_05 <- as.factor(ifelse(data_brms$dim_cat == "(-1,5]","1","0"))
data_brms$dim_dummy_510 <- as.factor(ifelse(data_brms$dim_cat == "(5,10]","1","0"))


####READ MODELS####

brms_mod_weibull_pos_age_yield_scc_mtnc_dimind <- 
  readRDS("y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_weibull_pos_age_yield_scc_mtnc_dimind.rds")

brms_mod_lognorm_neg_yield_scc_mtnc_dimind_altpriors_morepos <-
  readRDS("y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_lognorm_neg_yield_scc_mtnc_dimind_altpriors_morepos.rds")

####GET BAYES FACTORS (TWO MODEL STRATEGY, PROB AT TITRE)####

regcores(-1)

tm <- timerstart()

n_post_draws <- 2000

ivl = 2 #Interval around titre within which to get probabilities

bf_tab <- foreach(r = 1:nrow(data_brms), .combine = "rbind", .packages = "brms") %dopar% {
  
  
  
  post_draws_pos <- posterior_predict(brms_mod_weibull_pos_age_yield_scc_mtnc_dimind, 
                                      newdata = data_brms[r,],
                                      ndraws = n_post_draws)
  
  #n_above_titre_pos <- length(post_draws_pos[post_draws_pos >= dbmte[r,'titre']])
  
  prob_at_titre_pos <- length(post_draws_pos[post_draws_pos >= (data_brms[r, 'titre'] - ivl) &
                                               post_draws_pos < (data_brms[r,'titre'] + ivl)]) / length(post_draws_pos)
  
  
  #prob_at_titre_pos <- prob_at_titre_pos + 0.01
  
  post_draws_neg <- posterior_predict(brms_mod_lognorm_neg_yield_scc_mtnc_dimind_altpriors_morepos,
                                      newdata = data_brms[r,],
                                      ndraws = n_post_draws)
  
  #n_above_titre_neg <- length(post_draws_neg[post_draws_neg >= dbmte[r,'titre']])
  
  prob_at_titre_neg <- prob_at_titre_neg <- length(post_draws_neg[post_draws_neg >= (data_brms[r, 'titre'] - ivl) &
                                                                    post_draws_neg < (data_brms[r,'titre'] + ivl)]) / length(post_draws_neg)
  
  
  #prob_at_titre_neg <- prob_at_titre_neg + 0.01
  
  bayes_fac <- prob_at_titre_pos/prob_at_titre_neg
  
  tmptab <- c(r, 
              data_brms$age[r], 
              data_brms$dim[r],
              data_brms$yield[r], 
              data_brms$titre[r],
              data_brms$cellcount[r],
              data_brms$meantitrenegcows[r],
              prob_at_titre_pos, 
              prob_at_titre_neg, 
              bayes_fac)
  


  tmptab
  
}



bf_tab <- as.data.frame(bf_tab)

colnames(bf_tab) <- c("row",
                      "age",
                      "dim",
                      "yield",
                      "titre",
                      "cellcount",
                      "meantitrenegcows",
                      "prob_at_titre_pos",
                      "prob_at_titre_neg",
                      "bayesfactor")

timerend(tm)

write.csv(bf_tab, paste0(tvupath,"bf_tab.csv"), row.names  = FALSE)


#bf_lm <- lm(bayesfactor ~ age + 
#              yield + 
#              titre + 
#              I(titre^2) +
#              meantitrenegcows +
#              cellcount,
#            data = bf_tab[bf_tab$bayesfactor != Inf &
#                            !is.na(bf_tab$bayesfactor),])



#bf_tab$bayesfactor_pred <- bf_tab$bayesfactor

#bf_tab$bayesfactor_pred[bf_tab$bayesfactor == Inf |
#                          is.na(bf_tab$bayesfactor)] <-
#  predict(bf_lm, newdata = bf_tab[bf_tab$bayesfactor == Inf |
#                                    is.na(bf_tab$bayesfactor),])

#bf_tab$bayesfactor_pred <- bf_tab$bayesfactor

#bf_tab$bayesfactor_pred[bf_tab$bayesfactor == Inf |
#                          is.na(bf_tab$bayesfactor)] <-
#  predict(bf_lm, newdata = bf_tab[bf_tab$bayesfactor == Inf |
#                                    is.na(bf_tab$bayesfactor),])


#print(ggplot(bf_tab[bf_tab$prob_at_titre_pos != 1,],
#             aes(x = titre, y = bayesfactor)) +
#        geom_point() +
#        geom_smooth() +
#        labs(title = "Bayes Factor (Raw)"))

#print(ggplot(bf_tab[bf_tab$prob_at_titre_pos != 1,],
#             aes(x = titre, y = bayesfactor_pred)) +
#        geom_point() +
#        geom_smooth() +
#        labs(title = "Bayes Factor (LM Predicted)"))

#data_brms$likelihood <- bf_tab$bayesfactor_pred

data_brms$likelihood <- bf_tab$bayesfactor

data_brms <- data_brms[!is.na(data_brms$priorodds_crt),]

data_brms$PosteriorOdds <- as.numeric(0)
data_brms$POFloorApplied <- as.numeric(0)

posterioroddsfloor <- 0
priorstage <- "crt"

#source_python("Y:/Ian/JohnesThresholds/JohnesProper/Data/PythonScripts/PyPOUpdater.py")
#data_brms <- updatePO(data_brms, posterioroddsfloor, priorstage)

data_brms <- RparallelUpdatePO(data_brms, priorstage, posterioroddsfloor)

data_brms$PosteriorProb <- data_brms$PosteriorOdds /
  (1 + data_brms$PosteriorOdds)



data_brms$titre <- as.numeric(data_brms$titre)
data_brms$PosteriorProb <- as.numeric(data_brms$PosteriorProb)

data_brms$PosteriorProb[data_brms$PosteriorOdds == Inf] <- 1

print(ggplot(data_brms,
             aes(x = PosteriorProb)) +
        geom_histogram())

data_brms$Target_QMMS <- as.factor(data_brms$Target_QMMS)

write.csv(data_brms, paste0(tvupath,"data_brms.csv"), row.names = FALSE)

#colnames(data_brms)[match("likelihood", colnames(data_brms))] <- "likelihood_brms"
#colnames(data_brms)[match("PosteriorProb", colnames(data_brms))] <- "PosteriorProb_brms"
#colnames(data_brms)[match("POFloorApplied", colnames(data_brms))] <- "POFloorApplied_brms"

#data <- merge(data,
#              data_brms[,c('calfeartag',
#                           'date',
#                           'likelihood_brms',
#                           'PosteriorProb_brms',
#                           'POFloorApplied_brms')],
#              by = c('calfeartag',
#                     'date'),
#              all.x = TRUE)


#FARM = data$Farm[1]


con <- dbConnect(RSQLite::SQLite(), paste0(tvupath,farm,".tvu"), sep = "")

latestevent <- date(dbGetQuery(con, '
                          select max(ev.date)

FROM event ev')[[1]])

cowsalive <- dbGetQuery(con, '
                        
SELECT 

a.earTag,
ca.start,
ca.end

FROM animal a

INNER JOIN cow_alive ca on a.id = ca.animalId


WHERE ca.cOnFarm = 1 AND a.sex = "C"')

cowsalive$start <- date(cowsalive$start)
cowsalive$end <- date(cowsalive$end)

#cowsalive$dsincelatestmr <- cowsalive$start - latestmrdate

cowsalive <- unique(cowsalive$earTag[which((cowsalive$end - latestevent) < 200 & (cowsalive$end - latestevent) > -21)])



cowsaliveindata <- unique(data_brms$calfeartag[which(data_brms$calfeartag %in% cowsalive)])

plotsavepath = "c:/users/ian.glover.headoffice/desktop/testfarmplots/"

group.colours <- c("L" = "green", "M" = "orange", "H" = "red")

for (cow in cowsaliveindata){
  ggplot(data_brms[data_brms$calfeartag == cow,], aes(x = age)) +
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
    labs(title = data_brms$calfeartag[data_brms$calfeartag == cow], subtitle = paste0("Birth Probability:", round(data_brms$priorprob_crt[data_brms$calfeartag == cow],2)))
  ggsave(paste0(plotsavepath,cow,".png"))
}




time2 <- Sys.time()

print(paste("Time processing plots:", round(difftime(time2, time0, units = "mins"),2), "mins"))


crtjohnesprobdata <- data_brms[which(data_brms$age == data_brms$ageatlasttest & data_brms$calfeartag %in% cowsaliveindata),]
crtjohnesprobdata <- crtjohnesprobdata[which(!duplicated(crtjohnesprobdata$calfeartag)),]

crtjohnesprobtable <- data.frame(Eartag = crtjohnesprobdata$calfeartag, 
                                 LatestProbability_Freq = round(crtjohnesprobdata$PosteriorProb,2),
                                 LatestProbability_Bayes = round(crtjohnesprobdata$PosteriorProb_brms,2))

crtjohnesprobtable <- crtjohnesprobtable[order(-crtjohnesprobtable$LatestProbability_Bayes),]

linenos <- dbGetQuery(con, '
                      SELECT
                      
                      a.earTag,
                      a.mgtCode
                      
                      FROM animal a')

crtjohnesprobtable <- merge(crtjohnesprobtable, linenos, by.x = "Eartag", by.y = "earTag", all.x = TRUE)

crtjohnesprobtable <- crtjohnesprobtable[,c(4,1,3,2)]

write.csv(crtjohnesprobtable, paste0(tvupath,"LatestProbs.csv"), row.names = FALSE)

dbDisconnect(con)


rm(crtjohnesprobdata)
rm(crtjohnesprobtable)
rm(cowsalive)
rm(cowsaliveindata)



