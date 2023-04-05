####READ DATA####

path <- "c:/users/ian.glover.headoffice/desktop/"

data_brms <- read.csv(paste0(path,"data_TESTFARMS_birthpriors.csv"))

print(paste0("N Rows: ",nrow(data_brms), " N Cows: ",length(unique(data_brms$calfeartag))))

####CREATE COW GROUPING####

data_brms$farmcow <- paste0(data_brms$Farm, data_brms$calfeartag)

####DIM CAT####

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

regcores()

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
  
  tmptab <- c(r, data_brms$age[r], data_brms$yield[r], data_brms$titre[r], prob_at_titre_pos, prob_at_titre_neg, bayes_fac)
  
  tryCatch({write.table(tmptab, paste0(path,"rowsdone.txt"), append = FALSE, sep = " ", dec = ".",
                        row.names = FALSE, col.names = FALSE)}, 
           error = function(cond){})
  
  tmptab
  
}



bf_tab <- as.data.frame(bf_tab)

colnames(bf_tab) <- c("row",
                      "age",
                      "yield",
                      "titre",
                      "prob_at_titre_pos",
                      "prob_at_titre_neg",
                      "bayesfactor")

timerend(tm)

write.csv(bf_tab, paste0(path,"bf_tab.csv", row.names  = FALSE))

####LINEAR MODEL FOR ESTIMATING BAYES FACTOR WHEN P(TITRE | NEG) == 0####

bf_lm <- lm(bayesfactor ~ age + yield + titre + I(titre^2),
            data = bf_tab[bf_tab$bayesfactor != Inf &
                            !is.na(bf_tab$bayesfactor),])

bf_tab$bayesfactor_pred <- bf_tab$bayesfactor

bf_tab$bayesfactor_pred[bf_tab$bayesfactor == Inf |
                          is.na(bf_tab$bayesfactor)] <-
  predict(bf_lm, newdata = bf_tab[bf_tab$bayesfactor == Inf |
                                    is.na(bf_tab$bayesfactor),])



print(ggplot(bf_tab[bf_tab$prob_at_titre_pos != 1,],
       aes(x = titre, y = bayesfactor)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Bayes Factor (Raw)"))

print(ggplot(bf_tab[bf_tab$prob_at_titre_pos != 1,],
       aes(x = titre, y = bayesfactor_pred)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Bayes Factor (LM Predicted)"))

data_brms$likelihood <- bf_tab$bayesfactor_pred

####UPDATE POSTERIORS####

data_brms <- data_brms[!is.na(data_brms$priorodds_crt),]

data_brms$PosteriorOdds <- as.numeric(0)
data_brms$POFloorApplied <- as.numeric(0)

posterioroddsfloor <- 0.00274
priorstage <- "crt"

source_python("Y:/Ian/JohnesThresholds/JohnesProper/Data/PythonScripts/PyPOUpdater.py")
data_brms <- updatePO(data_brms, posterioroddsfloor, priorstage)

data_brms$PosteriorProb <- data_brms$PosteriorOdds /
  (1 + data_brms$PosteriorOdds)

print(ggplot(data_brms,
       aes(x = PosteriorProb)) +
  geom_histogram())

data_brms$titre <- as.numeric(data_brms$titre)
data_brms$PosteriorProb <- as.numeric(data_brms$PosteriorProb)

data_brms$Target_QMMS <- as.factor(data_brms$Target_QMMS)

write.csv(data_brms, paste0(path,"data_brms.csv"), row.names = FALSE)


