####READ DATA####

data_brms <- read.csv("Y:/Ian/JohnesThresholds/JohnesProper/Data/data_birthpriors.csv")

####CREATE DATA FOR BRMS MODEL####

data_brms$farmcow <- paste0(data_brms$Farm, data_brms$calfeartag)

print("Creating data for likelihood models...")

#####ELIMINATE COWS WITH BAD DATA####

data_brms$goodrows <- ifelse(data_brms$age >= 18 &
                    !is.na(data_brms$yield) &
                    data_brms$yield > 0 &
                      data_brms$protein > 0 &
                      !is.na(data_brms$protein) &
                    !is.na(data_brms$cellcount) &
                      data_brms$cellcount > 0 &
                    !is.na(data_brms$meantitrenegcows) &
                    data_brms$meantitrenegcows != 0 &
                      data_brms$dim >= 0 &
                      data_brms$dim <= 750 &
                      !is.na(data_brms$dim) &
                      !is.na(data_brms$butterfat) &
                      data_brms$butterfat >0,
                  1,0)

regcores(-1)

sr <- splitacrosscores(data_brms, -1)

p <- foreach(core = 1:length(sr), .combine = "rbind", .packages = "foreach") %dopar% {
  a <- ifelse(core == 1, 1, sr[core-1] + 1)
  b <- sr[core]
   
  tempdata <- data_brms[a:b,]
                        


  goodcows <- foreach(c = 1:length(unique(tempdata$calfeartag)), .combine = "rbind") %do% {
    tmp <- tempdata[tempdata$calfeartag == unique(tempdata$calfeartag)[c],]
    goodcow <- ifelse(mean(tmp$goodrows) == 1,
                    1, 0)
    tryCatch({write.table(c, paste0("C:/Users/Ian.glover.HEADOFFICE/Documents/PythonTools/ForEachRowCounter/rowsdonecore",core,".txt"), append = FALSE, sep = " ", dec = ".",
                                                   row.names = FALSE, col.names = FALSE)}, 
                                       error = function(cond){}) 
    c(tmp$Farm[1], tmp$calfeartag[1], goodcow)

  }
  
  goodcows
  
}

stopCluster(cl)


goodcows <- as.data.frame(p)
colnames(goodcows) <- c('Farm', 'calfeartag', 'goodcow')

goodcows <- goodcows[goodcows$goodcow == 1,]

dim(goodcows)

length(unique(data_brms$calfeartag))

data_brms <- data_brms[paste0(data_brms$Farm, data_brms$calfeartag) %in% paste0(goodcows$Farm, goodcows$calfeartag),]

dim(data_brms)

#####ALTERNATIVE NEG COW DEFINITION#####

data_brms$Target_altdef1 <- "U"

data_brms$Target_altdef1 <- as.factor(ifelse(grepl("HH", data_brms$profile) == TRUE |
                                               grepl("HMH", data_brms$profile) == TRUE |
                                               grepl("HLH", data_brms$profile) == TRUE,
                                             "1",
                                             ifelse(data_brms$ntests >= 3 &
                                                      substr(data_brms$profile, nchar(data_brms$profile) - 2, nchar(data_brms$profile)) == "LLL", "0",
                                                    data_brms$Target_altdef1)))





######ADD SMALL AMOUNT TO ZERO TITRES######

data_brms$titre <- data_brms$titre + 0.0001

######ADD SMALL AMOUNT TO ZERO CELLCOUNTS

#data_brms$cellcount <- data_brms$cellcount + 0.0001

######DIM CATEGORIES######



data_brms$dim_cat <- cut(data_brms$dim, breaks = c(-1,5,10,10000))

data_brms$dim_cat <- relevel(data_brms$dim_cat, "(10,1e+04]")



data_brms$dim_dummy_05 <- as.factor(ifelse(data_brms$dim_cat == "(-1,5]","1","0"))
data_brms$dim_dummy_510 <- as.factor(ifelse(data_brms$dim_cat == "(5,10]","1","0"))




#####LIMIT TO COWS WITH KNOWN JOHNE'S STATUS (-> DATA_BRMS_MODELLING)#####

data_brms_modelling <- data_brms[data_brms$Target_altdef1 != "U",]

data_brms_modelling$Target_altdef1 <- as.factor(data_brms_modelling$Target_altdef1)

data_brms_modelling$Target_altdef1 <- droplevels(data_brms_modelling$Target_altdef1, "U")


######CREATE COW GROUPING######



####SPLIT TRAIN/TEST####

set.seed(1981)

uniquefarms <- unique(data_brms_modelling$Farm)

trainfarms <- sample(uniquefarms, 63, replace = FALSE)
testfarms <- uniquefarms[!(uniquefarms %in% trainfarms)]

dbmtr <- data_brms_modelling[data_brms_modelling$Farm %in% trainfarms,]
dbmte <- data_brms_modelling[data_brms_modelling$Farm %in% testfarms,]

cat(paste0("Trainset: \n",dim(dbmtr)[1],
           " rows (",round(dim(dbmtr)[1]/(dim(dbmte)[1] + dim(dbmtr)[1])*100,1),"%)"),
    "\nPositive Target:", summary(as.factor(dbmtr$Target_altdef1))[[2]], "rows",
    "(", round(summary(as.factor(dbmtr$Target_altdef1))[[2]]/nrow(dbmtr)*100,1),"%)")

cat(paste0("testset: \n",dim(dbmte)[1],
           " rows (",round(dim(dbmte)[1]/(dim(dbmtr)[1] + dim(dbmte)[1])*100,1),"%)"),
    "\nPositive Target:", summary(as.factor(dbmte$Target_altdef1))[[2]], "rows",
    "(", round(summary(as.factor(dbmte$Target_altdef1))[[2]]/nrow(dbmte)*100,1),"%)")


####SPLIT TRAINING AND TESTING DATA INTO TARGET_QMMS POS AND NEG SUBSETS####

dbmtr_pos <- dbmtr[dbmtr$Target_altdef1 == "1",]
dbmtr_neg <- dbmtr[dbmtr$Target_altdef1 == "0",]

dbmte_pos <- dbmte[dbmte$Target_altdef1 == "1",]
dbmte_neg <- dbmte[dbmte$Target_altdef1 == "0",]

####SAVE TRAINING AND TESTING DATASETS####

write.csv(dbmtr, "y:/ian/johnesthresholds/johnesproper/data/dbmtr.csv", row.names = FALSE)
write.csv(dbmte, "y:/ian/johnesthresholds/johnesproper/data/dbmte.csv", row.names = FALSE)

####EXAMINE TITRE DISTRIBUTION####

#####ALL TRAINING DATA COMBINED#####

x <- seq(0,250,0.1)

y <- dgamma(x, 4, 1.2)

pdata <- data.frame(x,y)

ggplot() +
  geom_density(data = dbmtr,
               aes(x = titre)) +
  geom_line(data = pdata,
               aes(x = x, y = y), color = "red") +
  labs(title = "All cows", subtitle = "Trainset")

#####TRAINING DATA POSITIVE COWS#####

x <- seq(0,250,0.1)

y <- dgamma(x, 1.1, 0.05)

pdata <- data.frame(x,y)

ggplot() +
  geom_density(data = dbmtr_pos,
               aes(x = titre)) +
  geom_line(data = pdata,
            aes(x = x, y = y), color = "red") +
  labs(title = "Positive cows", subtitle = "Trainset")

#####TRAINING DATA NEGATIVE COWS#####

x <- seq(0,250,0.1)

y <- dgamma(x, 1.2, 0.5)

pdata <- data.frame(x,y)

ggplot() +
  geom_density(data = dbmtr_neg,
               aes(x = titre)) +
  geom_line(data = pdata,
            aes(x = x, y = y), color = "red") +
  labs(title = "Negative cows", subtitle = "Trainset")


####DESCRIBE TRAINING DATA####

#####AGE#####

ggplot(dbmtr_neg,
       aes(x = age)) +
  geom_histogram(bins = 100)

descstats(dbmtr_neg, ftname = "age")

ggplot(dbmtr_neg,
       aes(x = age,
           y = titre)) +
  geom_point() +
  geom_smooth()


ggplot(dbmtr_pos,
       aes(x = age)) +
  geom_histogram(bins = 100)

descstats(dbmtr_pos, ftname = "age")

ggplot(dbmtr_pos,
       aes(x = age,
           y = titre)) +
  geom_point() +
  geom_smooth()

#####YIELD#####

ggplot(dbmtr_neg,
       aes(x = yield)) +
  geom_histogram(bins = 100)

descstats(dbmtr_neg, ftname = "yield")

ggplot(dbmtr_neg,
       aes(x = yield,
           y = titre)) +
  geom_point() +
  geom_smooth()


ggplot(dbmtr_pos,
       aes(x = yield)) +
  geom_histogram(bins = 100)

descstats(dbmtr_pos, ftname = "yield")

ggplot(dbmtr_pos,
       aes(x = yield,
           y = titre)) +
  geom_point() +
  geom_smooth()

#####DIM#####

ggplot(dbmtr_neg,
       aes(x = dim)) +
  geom_histogram(bins = 100)

descstats(dbmtr_neg, ftname = "dim")

ggplot(dbmtr_neg,
       aes(x = dim,
           y = titre)) +
  geom_point() +
  geom_smooth()


ggplot(dbmtr_pos,
       aes(x = dim)) +
  geom_histogram(bins = 100)

descstats(dbmtr_pos, ftname = "dim")

ggplot(dbmtr_pos,
       aes(x = dim,
           y = titre)) +
  geom_point() +
  geom_smooth()



#####PROTEIN#####

ggplot(dbmtr_neg,
       aes(x = protein)) +
  geom_histogram(bins = 100)

descstats(dbmtr_neg, ftname = "protein")

ggplot(dbmtr_neg,
       aes(x = protein,
           y = titre)) +
  geom_point() +
  geom_smooth()

ggplot(dbmtr_neg,
       aes(x = protein,
           y = titre)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~cut(yield, 4) + cut(log(cellcount), 4) )

ggplot(dbmtr_pos,
       aes(x = protein)) +
  geom_histogram(bins = 100)

descstats(dbmtr_pos, ftname = "protein")

ggplot(dbmtr_pos,
       aes(x = protein,
           y = titre)) +
  geom_point() +
  geom_smooth()

ggplot(dbmtr_pos,
       aes(x = protein,
           y = titre)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~cut(yield,4) +
               cut(log(cellcount),4))

#####CELL COUNT#####

ggplot(dbmtr_neg,
       aes(x = log(cellcount))) +
  geom_histogram(bins = 100)

descstats(dbmtr_neg, ftname = "cellcount")

ggplot(dbmtr_neg,
       aes(x = log(cellcount),
           y = titre)) +
  geom_point() +
  geom_smooth()

 ggplot(dbmtr_pos,
       aes(x = log(cellcount))) +
  geom_histogram(bins = 100)

descstats(dbmtr_pos, ftname = "cellcount")

ggplot(dbmtr_pos,
       aes(x = log(cellcount),
           y = titre)) +
  geom_point() +
  geom_smooth()

#####BUTTERFAT#####

ggplot(dbmtr_neg,
       aes(x = butterfat)) +
  geom_histogram(bins = 100)

descstats(dbmtr_neg, ftname = "butterfat")

ggplot(dbmtr_neg,
       aes(x = butterfat,
           y = titre)) +
  geom_point() +
  geom_smooth()


ggplot(dbmtr_pos,
       aes(x = butterfat)) +
  geom_histogram(bins = 100)

descstats(dbmtr_pos, ftname = "butterfat")

ggplot(dbmtr_pos,
       aes(x = butterfat,
           y = titre)) +
  geom_point() +
  geom_smooth()

#####MTNC#####

ggplot(dbmtr_neg,
       aes(x = meantitrenegcows)) +
  geom_histogram(bins = 100)

descstats(dbmtr_neg, ftname = "meantitrenegcows")

ggplot(dbmtr_neg,
       aes(x = meantitrenegcows,
           y = titre)) +
  geom_point() +
  geom_smooth()


ggplot(dbmtr_pos,
       aes(x = meantitrenegcows)) +
  geom_histogram(bins = 100)

descstats(dbmtr_pos, ftname = "meantitrenegcows")

ggplot(dbmtr_pos,
       aes(x = meantitrenegcows,
           y = titre)) +
  geom_point() +
  geom_smooth()


####CROSS VALIDATION WORK OUT SPLINES####



####FIT BRMS MODELS (FIXED EFFECTS ONLY)####

#####GAMMA SEPARATE POS/NEG MODELS#####

######FIT NEGATIVE MODELS######

#######YIELD#######

brms_mod_gamma_neg_yield <- brm(titre ~ yield,
                                data = dbmtr_neg,
                                family = Gamma(link = "log"),
                                warmup = 500,
                                iter = 1500,
                                chains = 2,
                                cores = 4,
                                threads = threading(2),
                                backend = "cmdstanr")
              
saveRDS(brms_mod_gamma_neg_yield, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_gamma_neg_yield.rds")


#prior1 <- prior(normal(4,0.3), class = shape)

#brms_mod_gamma_neg_yield_cmprior <- brm(titre ~ yield,
#                                data = dbmtr_neg,
#                                family = Gamma(link = "log"),
#                                warmup = 500,
#                                iter = 1500,
#                                chains = 2,
#                                cores = 4,
#                                threads = threading(2),
#                                backend = "cmdstanr",
#                                prior = prior1)

#saveRDS(brms_mod_gamma_neg_yield, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_gamma_neg_yield_cmprior.rds")



#######YIELD + SCC#######

brms_mod_gamma_neg_yield_scc <- brm(titre ~ yield + I(log(cellcount)),
                                data = dbmtr_neg,
                                family = Gamma(link = "log"),
                                warmup = 500,
                                iter = 1500,
                                chains = 2,
                                cores = 4,
                                threads = threading(2),
                                backend = "cmdstanr")

saveRDS(brms_mod_gamma_neg_yield_scc, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_gamma_neg_yield_scc.rds")

#######YIELD + SCC + MTNC#######

brms_mod_gamma_neg_yield_scc_mtnc <- brm(titre ~ yield + I(log(cellcount)) + meantitrenegcows,
                                    data = dbmtr_neg,
                                    family = Gamma(link = "log"),
                                    warmup = 500,
                                    iter = 2000,
                                    chains = 2,
                                    cores = 4,
                                    threads = threading(2),
                                    backend = "cmdstanr")

saveRDS(brms_mod_gamma_neg_yield_scc_mtnc, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_gamma_neg_yield_scc_mtnc.rds")

#######YIELD + SCC + MTNC + DIM (Indicator)#######

brms_mod_gamma_neg_yield_scc_mtnc_dimind <- brm(titre ~ yield + I(log(cellcount)) + meantitrenegcows + dim_cat,
                                         data = dbmtr_neg,
                                         family = Gamma(link = "log"),
                                         warmup = 500,
                                         iter = 2000,
                                         chains = 2,
                                         cores = 4,
                                         threads = threading(2),
                                         backend = "cmdstanr")

saveRDS(brms_mod_gamma_neg_yield_scc_mtnc_dimind, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_gamma_neg_yield_scc_mtnc_dimind.rds")


######FIT POSITIVE MODELS######

#######AGE#######

prior1 <- prior("normal(4,2)", nlpar = "b1") +
  prior("normal(20,5)", nlpar = "b2") +
  prior("normal(0.08, 0.02)", nlpar = "b3")

brms_mod_gamma_pos_age <- brm(formula = bf(titre ~ b1 - (b2 * (exp(1)^(-b3*age))),
                                                          b1 + b2 + b3 ~ 1, 
                                                          nl = TRUE),
                                             prior = prior1,
                                             data = dbmtr_pos,
                                             family = Gamma(link = "log"),
                                             warmup = 1000,
                                             iter = 5000,
                                             control = list(adapt_delta = 0.95),
                                             init = "random",
                                             chains = 2,
                                             cores = 4,
                                             threads = threading(2),
                                             backend = "cmdstanr")

saveRDS(brms_mod_gamma_pos_age, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_gamma_pos_age.rds")

#######AGE + YIELD#######

prior1 <- prior("normal(2,2)", nlpar = "b1") +
  prior("normal(1,2)", nlpar = "b2") +
  prior("normal(0, 0.5)", nlpar = "b3") +
  prior("normal(-1, 0.5", nlpar = "b4")

brms_mod_gamma_pos_age_yield <- brm(formula = bf(titre ~ b1 - (b2 * (exp(1)^(-b3*age))) + b4 * yield,
                                           b1 + b2 + b3 + b4 ~ 1, 
                                           nl = TRUE),
                              prior = prior1,
                              data = dbmtr_pos,
                              family = Gamma(link = "log"),
                              warmup = 1000,
                              iter = 10000,
                              control = list(adapt_delta = 0.95),
                              init = "random",
                              chains = 2,
                              cores = 4,
                              threads = threading(2),
                              backend = "cmdstanr")

saveRDS(brms_mod_gamma_pos_age_yield, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_gamma_pos_age_yield.rds")

#######AGE + YIELD + SCC#######

prior1 <- prior("normal(2,2)", nlpar = "b1") +
  prior("normal(1,2)", nlpar = "b2") +
  prior("normal(0, 0.5)", nlpar = "b3") +
  prior("normal(-1, 0.5", nlpar = "b4") +
  prior("normal(0.1, 0.3", nlpar = "b5")

brms_mod_gamma_pos_age_yield_scc <- brm(formula = bf(titre ~ b1 - (b2 * (exp(1)^(-b3*age))) + b4 * yield + b5 * log(cellcount),
                                                 b1 + b2 + b3 + b4 + b5 ~ 1, 
                                                 nl = TRUE),
                                    prior = prior1,
                                    data = dbmtr_pos,
                                    family = Gamma(link = "log"),
                                    warmup = 1000,
                                    iter = 10000,
                                    control = list(adapt_delta = 0.95),
                                    init = "random",
                                    chains = 2,
                                    cores = 4,
                                    threads = threading(2),
                                    backend = "cmdstanr")

saveRDS(brms_mod_gamma_pos_age_yield_scc, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_gamma_pos_age_yield_scc.rds")

#######AGE + YIELD + SCC + MTNC + DIM (INDICATOR)#######

prior1 <- prior("normal(4,2)", nlpar = "b1") +
  prior("normal(20,5)", nlpar = "b2") +
  prior("normal(0.08, 0.02)", nlpar = "b3") +
  prior("normal(0,0.5", nlpar = "b4") +
  prior("normal(0,0.5", nlpar = "b5") +
  prior("normal(0,0.5", nlpar = "b6") +
  prior("normal(0,0.5", nlpar = "b7") +
  prior("normal(0,0.5", nlpar = "b8")


brms_mod_gamma_pos_age_yield_scc_mtnc_dimind <- brm(formula = bf(titre ~ b1 - (b2 * (exp(1)^(-b3*age))) + b4 * yield + b5 * log(cellcount) + b6 * meantitrenegcows + b7 * dim_dummy_05 + b8 * dim_dummy_510,
                                                                   b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8 ~ 1, 
                                                                   nl = TRUE),
                                                      prior = prior1,
                                                      data = dbmtr_pos,
                                                      family = Gamma(link = "log"),
                                                      warmup = 1000,
                                                      iter = 5000,
                                                      #control = list(adapt_delta = 0.95),
                                                      init = "random",
                                                      chains = 2,
                                                      cores = 4,
                                                      threads = threading(2),
                                                      backend = "cmdstanr")

saveRDS(brms_mod_gamma_pos_age_yield_scc_mtnc_dimind, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_gamma_pos_age_yield_scc_mtnc_dimind.rds")


#####WEIBULL SEPARATE POS/NEG MODELS#####

######FIT NEGATIVE MODELS######

#######YIELD#######

brms_mod_weibull_neg_yield <- brm(titre ~ yield,
                                data = dbmtr_neg,
                                family = weibull(link = "log"),
                                warmup = 500,
                                iter = 1500,
                                chains = 2,
                                cores = 4,
                                threads = threading(2),
                                backend = "cmdstanr")

saveRDS(brms_mod_weibull_neg_yield, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_weibull_neg_yield.rds")

#######YIELD + SCC + MTNC + DIM(Indicator)#######

brms_mod_weibull_neg_yield_scc_mtnc_dimind <- brm(titre ~ yield +
                                                    log(cellcount) +
                                                    meantitrenegcows +
                                                    dim_dummy_05 +
                                                    dim_dummy_510,
                                  data = dbmtr_neg,
                                  family = weibull(link = "log"),
                                  warmup = 500,
                                  iter = 5000,
                                  chains = 2,
                                  cores = 4,
                                  threads = threading(2),
                                  backend = "cmdstanr")

saveRDS(brms_mod_weibull_neg_yield_scc_mtnc_dimind, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_weibull_neg_yield_scc_mtnc_dimind.rds")


######FIT POSITIVE MODELS######

#######AGE#######

prior1 <- prior("normal(4,2)", nlpar = "b1") +
  prior("normal(20,5)", nlpar = "b2") +
  prior("normal(0.08, 0.02)", nlpar = "b3")

brms_mod_weibull_pos_age <- brm(formula = bf(titre ~ b1 - (b2 * (exp(1)^(-b3*age))),
                                           b1 + b2 + b3 ~ 1, 
                                           nl = TRUE),
                              prior = prior1,
                              data = dbmtr_pos,
                              family = weibull(link = "log"),
                              warmup = 1000,
                              iter = 5000,
                              #control = list(adapt_delta = 0.95),
                              init = "random",
                              chains = 2,
                              cores = 4,
                              threads = threading(2),
                              backend = "cmdstanr")

saveRDS(brms_mod_weibull_pos_age, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_weibull_pos_age.rds")

#######AGE + YIELD#######

prior1 <- prior("normal(4,2)", nlpar = "b1") +
  prior("normal(20,5)", nlpar = "b2") +
  prior("normal(0.08, 0.02)", nlpar = "b3") +
  prior("normal(0,0.5", nlpar = "b4")

brms_mod_weibull_pos_age_yield <- brm(formula = bf(titre ~ b1 - (b2 * (exp(1)^(-b3*age))) + b4 * yield,
                                             b1 + b2 + b3 + b4 ~ 1, 
                                             nl = TRUE),
                                prior = prior1,
                                data = dbmtr_pos,
                                family = weibull(link = "log"),
                                warmup = 1000,
                                iter = 5000,
                                #control = list(adapt_delta = 0.95),
                                init = "random",
                                chains = 2,
                                cores = 4,
                                threads = threading(2),
                                backend = "cmdstanr")

saveRDS(brms_mod_weibull_pos_age_yield, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_weibull_pos_age_yield.rds")

#######AGE + YIELD + SCC#######

prior1 <- prior("normal(4,2)", nlpar = "b1") +
  prior("normal(20,5)", nlpar = "b2") +
  prior("normal(0.08, 0.02)", nlpar = "b3") +
  prior("normal(0,0.5", nlpar = "b4") +
  prior("normal(0,0.5", nlpar = "b5")

brms_mod_weibull_pos_age_yield_scc <- brm(formula = bf(titre ~ b1 - (b2 * (exp(1)^(-b3*age))) + b4 * yield + b5 * log(cellcount),
                                                   b1 + b2 + b3 + b4 + b5 ~ 1, 
                                                   nl = TRUE),
                                      prior = prior1,
                                      data = dbmtr_pos,
                                      family = weibull(link = "log"),
                                      warmup = 1000,
                                      iter = 5000,
                                      #control = list(adapt_delta = 0.95),
                                      init = "random",
                                      chains = 2,
                                      cores = 4,
                                      threads = threading(2),
                                      backend = "cmdstanr")

saveRDS(brms_mod_weibull_pos_age_yield, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_weibull_pos_age_yield.rds")

#######AGE + YIELD + SCC + MTNC#######

prior1 <- prior("normal(4,2)", nlpar = "b1") +
  prior("normal(20,5)", nlpar = "b2") +
  prior("normal(0.08, 0.02)", nlpar = "b3") +
  prior("normal(0,0.5", nlpar = "b4") +
  prior("normal(0,0.5", nlpar = "b5") +
  prior("normal(0,0.5", nlpar = "b6")

brms_mod_weibull_pos_age_yield_scc_mtnc <- brm(formula = bf(titre ~ b1 - (b2 * (exp(1)^(-b3*age))) + b4 * yield + b5 * log(cellcount) + b6 * meantitrenegcows,
                                                       b1 + b2 + b3 + b4 + b5 + b6 ~ 1, 
                                                       nl = TRUE),
                                          prior = prior1,
                                          data = dbmtr_pos,
                                          family = weibull(link = "log"),
                                          warmup = 1000,
                                          iter = 5000,
                                          #control = list(adapt_delta = 0.95),
                                          init = "random",
                                          chains = 2,
                                          cores = 4,
                                          threads = threading(2),
                                          backend = "cmdstanr")

saveRDS(brms_mod_weibull_pos_age_yield_scc_mtnc, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_weibull_pos_age_yield_scc_mtnc.rds")

#######AGE + YIELD + SCC + MTNC + DIM (INDICATOR)#######

prior1 <- prior("normal(4,2)", nlpar = "b1") +
  prior("normal(20,5)", nlpar = "b2") +
  prior("normal(0.08, 0.02)", nlpar = "b3") +
  prior("normal(0,0.5", nlpar = "b4") +
  prior("normal(0,0.5", nlpar = "b5") +
  prior("normal(0,0.5", nlpar = "b6") +
  prior("normal(0,0.5", nlpar = "b7") +
  prior("normal(0,0.5", nlpar = "b8")
  

brms_mod_weibull_pos_age_yield_scc_mtnc_dimind <- brm(formula = bf(titre ~ b1 - (b2 * (exp(1)^(-b3*age))) + b4 * yield + b5 * log(cellcount) + b6 * meantitrenegcows + b7 * dim_dummy_05 + b8 * dim_dummy_510,
                                                            b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8 ~ 1, 
                                                            nl = TRUE),
                                               prior = prior1,
                                               data = dbmtr_pos,
                                               family = weibull(link = "log"),
                                               warmup = 1000,
                                               iter = 5000,
                                               #control = list(adapt_delta = 0.95),
                                               init = "random",
                                               chains = 2,
                                               cores = 4,
                                               threads = threading(2),
                                               backend = "cmdstanr")

saveRDS(brms_mod_weibull_pos_age_yield_scc_mtnc_dimind, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_weibull_pos_age_yield_scc_mtnc_dimind.rds")

#######PREVIOUS TITRE + YIELD + SCC + MTNC + DIM (INDICATOR)#######

dbmtr_pos$prevtitre <- NA

regcores(-1)


dbmtr_pos$prev_titre[2:nrow(dbmtr_pos)] <- foreach(r = 2:nrow(dbmtr_pos), .combine = "c") %dopar% {
  ifelse(dbmtr_pos$calfeartag[r] != dbmtr_pos$calfeartag[r-1],
         NA,
         dbmtr_pos$titre[r-1])
}

stopCluster(cl)

dbmte_pos$prevtitre <- NA

regcores(-1)


dbmte_pos$prev_titre[2:nrow(dbmte_pos)] <- foreach(r = 2:nrow(dbmte_pos), .combine = "c") %dopar% {
  ifelse(dbmte_pos$calfeartag[r] != dbmte_pos$calfeartag[r-1],
         NA,
         dbmte_pos$titre[r-1])
}

stopCluster(cl)


prior1 <- c(
  prior("normal(0,0.2)", class = "b", coef = "yield"),
  prior("normal(-1,0.2)", class = "b", coef = "logcellcount"),
  prior("normal(-0.5,0.2)", class = "b", coef = "meantitrenegcows"),
  prior("normal(0,0.2)", class = "b", coef = "dim_dummy_051")
)


brms_mod_weibull_pos_prevtitre_yield_scc_mtnc_dimind <- brm(titre ~ 
                                                        prev_titre +
                                                        yield +
                                                        log(cellcount) +
                                                        meantitrenegcows +
                                                        dim_dummy_05 +
                                                        dim_dummy_510,
                                                        prior = prior1,
                                                      data = dbmtr_pos[!is.na(dbmtr_pos$prev_titre),],
                                                      family = weibull(link = "log"),
                                                      warmup = 1000,
                                                      iter = 5000,
                                                      #control = list(adapt_delta = 0.95),
                                                      init = "random",
                                                      chains = 2,
                                                      cores = 4,
                                                      threads = threading(2),
                                                      backend = "cmdstanr")

saveRDS(brms_mod_weibull_pos_prevtitre_yield_scc_mtnc_dimind, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_weibull_pos_prevtitre_yield_scc_mtnc_dimind.rds")


#######AGE + YIELD + SCC + MTNC(BoxCox) + DIM (INDICATOR)#######

prior1 <- prior("normal(4,2)", nlpar = "b1") +
  prior("normal(20,5)", nlpar = "b2") +
  prior("normal(0.08, 0.02)", nlpar = "b3") +
  prior("normal(0,0.5", nlpar = "b4") +
  prior("normal(0,0.5", nlpar = "b5") +
  prior("normal(0,0.5", nlpar = "b6") +
  prior("normal(0,0.5", nlpar = "b7") +
  prior("normal(0,0.5", nlpar = "b8")


brms_mod_weibull_pos_age_yield_scc_mtncboxcox_dimind <- brm(formula = bf(titre ~ b1 - (b2 * (exp(1)^(-b3*age))) + b4 * yield + b5 * log(cellcount) + b6 * meantitrenegcows_boxcox + b7 * dim_dummy_05 + b8 * dim_dummy_510,
                                                                   b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8 ~ 1, 
                                                                   nl = TRUE),
                                                      prior = prior1,
                                                      data = dbmtr_pos,
                                                      family = weibull(link = "log"),
                                                      warmup = 1000,
                                                      iter = 5000,
                                                      #control = list(adapt_delta = 0.95),
                                                      init = "random",
                                                      chains = 2,
                                                      cores = 4,
                                                      threads = threading(2),
                                                      backend = "cmdstanr")

saveRDS(brms_mod_weibull_pos_age_yield_scc_mtncboxcox_dimind, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_weibull_pos_age_yield_scc_mtncboxcox_dimind.rds")

######AGE + YIELD + SCC + MTNC + DIM (INDICATOR) + HERDAVGDIM_CAT######

prior1 <- prior("normal(4,2)", nlpar = "b1") +
  prior("normal(20,5)", nlpar = "b2") +
  prior("normal(0.08, 0.02)", nlpar = "b3") +
  prior("normal(0,0.5", nlpar = "b4") +
  prior("normal(0,0.5", nlpar = "b5") +
  prior("normal(0,0.5", nlpar = "b6") +
  prior("normal(0,0.5", nlpar = "b7") +
  prior("normal(0,0.5", nlpar = "b8") +
  prior("normal(0,0.5", nlpar = "b9") +
  prior("normal(0,0.5", nlpar = "b10")

brms_mod_weibull_pos_age_yield_scc_mtnc_dimind_avgdimcat <- brm(formula = bf(titre ~ b1 - (b2 * (exp(1)^(-b3*age))) + 
                                                                                   b4 * yield + 
                                                                                   b5 * log(cellcount) + 
                                                                                   b6 * meantitrenegcows + 
                                                                                   b7 * dim_dummy_05 + 
                                                                                   b8 * dim_dummy_510 +
                                                                                   b9 * herdavgdim_dummy050 +
                                                                                   b10 * herdavgdim_dummy2001000,
                                                                                 b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8 + b9 + b10 ~ 1, 
                                                                                 nl = TRUE),
                                                                    prior = prior1,
                                                                    data = dbmtr_pos,
                                                                    family = weibull(link = "log"),
                                                                    warmup = 1000,
                                                                    iter = 10000,
                                                                    #control = list(adapt_delta = 0.95),
                                                                    init = "random",
                                                                    chains = 2,
                                                                    cores = 6,
                                                                    threads = threading(3),
                                                                    backend = "cmdstanr")

saveRDS(brms_mod_weibull_pos_age_yield_scc_mtnc_dimind_avgdimcat, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_weibull_pos_age_yield_scc_mtnc_dimind_avgdimcat.rds")

#######AGE + YIELD + SCC + MTNC*AVGDIM_CAT + DIM (INDICATOR)#######

prior1 <- prior("normal(4,2)", nlpar = "b1") +
  prior("normal(20,5)", nlpar = "b2") +
  prior("normal(0.08, 0.02)", nlpar = "b3") +
  prior("normal(0,0.5", nlpar = "b4") +
  prior("normal(0,0.5", nlpar = "b5") +
  prior("normal(0,0.5", nlpar = "b6") +
  prior("normal(0,0.5", nlpar = "b7") +
  prior("normal(0,0.5", nlpar = "b8") +
  prior("normal(0,0.5", nlpar = "b9") +
  prior("normal(0,0.5", nlpar = "b10")


brms_mod_weibull_pos_age_yield_scc_mtnc_int_avgdimcat_dimind <- brm(formula = bf(titre ~ b1 - (b2 * (exp(1)^(-b3*age))) +
                                                                                   b4 * yield +
                                                                                   b5 * log(cellcount) +
                                                                                   b6 * dim_dummy_05 +
                                                                                   b7 * dim_dummy_510 +
                                                                                   b8 * meantitrenegcows +
                                                                                   b9 * meantitrenegcows * herdavgdim_dummy050 +
                                                                                   b10 * meantitrenegcows * herdavgdim_dummy2001000,
                                                                                 b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8 + b9 + b10 ~ 1,
                                                                                 nl = TRUE),
                                                                    prior = prior1,
                                                                    data = dbmtr_pos,
                                                                    family = weibull(link = "log"),
                                                                    warmup = 1000,
                                                                    iter = 5000,
                                                                    #control = list(adapt_delta = 0.95),
                                                                    init = "random",
                                                                    chains = 2,
                                                                    cores = 6,
                                                                    threads = threading(3),
                                                                    backend = "cmdstanr")

saveRDS(brms_mod_weibull_pos_age_yield_scc_mtnc_int_avgdimcat_dimind, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_weibull_pos_age_yield_scc_mtnc_int_avgdimcat_dimind.rds")

#######AGE + YIELD + SCC + DIM (INDICATOR)#######

prior1 <- prior("normal(4,2)", nlpar = "b1") +
  prior("normal(15,5)", nlpar = "b2") +
  prior("normal(0.08, 0.02)", nlpar = "b3") +
  prior("normal(0,0.02", nlpar = "b4") +
  prior("normal(0.05,0.02", nlpar = "b5") +
  prior("normal(0.5,0.2", nlpar = "b6") +
  prior("normal(0.3,0.2", nlpar = "b7")


brms_mod_weibull_pos_age_yield_scc_dimind <- brm(formula = bf(titre ~ b1 - (b2 * (exp(1)^(-b3*age))) + b4 * yield + b5 * log(cellcount) + b6 * dim_dummy_05 + b7 * dim_dummy_510,
                                                                   b1 + b2 + b3 + b4 + b5 + b6 + b7 ~ 1, 
                                                                   nl = TRUE),
                                                      prior = prior1,
                                                      data = dbmtr_pos,
                                                      family = weibull(link = "log"),
                                                      warmup = 1000,
                                                      iter = 15000,
                                                      #control = list(adapt_delta = 0.95),
                                                      init = "random",
                                                      chains = 2,
                                                      cores = 4,
                                                      threads = threading(2),
                                                      backend = "cmdstanr")

saveRDS(brms_mod_weibull_pos_age_yield_scc_dimind, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_weibull_pos_age_yield_scc_dimind.rds")



#####EXPONENTIAL SEPARATE POS/NEG MODELS#####

######FIT NEGATIVE MODELS######

#######YIELD#######

brms_mod_expo_neg_yield <- brm(titre ~ yield,
                                  data = dbmtr_neg,
                                  family = exponential(link = "log"),
                                  warmup = 500,
                                  iter = 1500,
                                  chains = 2,
                                  cores = 4,
                                  threads = threading(2),
                                  backend = "cmdstanr")

saveRDS(brms_mod_expo_neg_yield, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_expo_neg_yield.rds")

#####LOGNORMAL SEPARATE POS/NEG MODELS#####

######FIT NEGATIVE MODELS######

#######YIELD#######

brms_mod_lognorm_neg_yield <- brm(titre ~ yield,
                               data = dbmtr_neg,
                               family = lognormal(link = "identity"),
                               warmup = 500,
                               iter = 1500,
                               chains = 2,
                               cores = 4,
                               threads = threading(2),
                               backend = "cmdstanr")

saveRDS(brms_mod_lognorm_neg_yield, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_lognorm_neg_yield.rds")

#######YIELD + SCC#######

brms_mod_lognorm_neg_yield_scc <- brm(titre ~ yield + I(log(cellcount)),
                                  data = dbmtr_neg,
                                  family = lognormal(link = "identity"),
                                  warmup = 500,
                                  iter = 1500,
                                  chains = 2,
                                  cores = 4,
                                  threads = threading(2),
                                  backend = "cmdstanr")

saveRDS(brms_mod_lognorm_neg_yield_scc, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_lognorm_neg_yield_scc.rds")

#######YIELD + SCC + MTNC#######

brms_mod_lognorm_neg_yield_scc_mtnc <- brm(titre ~ yield + I(log(cellcount)) + meantitrenegcows,
                                      data = dbmtr_neg,
                                      family = lognormal(link = "identity"),
                                      warmup = 500,
                                      iter = 1500,
                                      chains = 2,
                                      cores = 4,
                                      threads = threading(2),
                                      backend = "cmdstanr")

saveRDS(brms_mod_lognorm_neg_yield_scc_mtnc, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_lognorm_neg_yield_scc_mtnc.rds")

#######YIELD + SCC + MTNC + DIM (INDICATOR)#######

brms_mod_lognorm_neg_yield_scc_mtnc_dimind <- brm(titre ~ yield + I(log(cellcount)) + meantitrenegcows + dim_dummy_05 + dim_dummy_510,
                                           data = dbmtr_neg,
                                           family = lognormal(link = "identity"),
                                           warmup = 500,
                                           iter = 1500,
                                           chains = 2,
                                           cores = 6,
                                           threads = threading(3),
                                           backend = "cmdstanr")

saveRDS(brms_mod_lognorm_neg_yield_scc_mtnc_dimind, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_lognorm_neg_yield_scc_mtnc_dimind.rds")

#######YIELD + SCC + MTNC + DIM (INDICATOR) + HERDAVGDIMCAT#######

brms_mod_lognorm_neg_yield_scc_mtnc_dimind_avgdimcat <- brm(titre ~ yield + I(log(cellcount)) + meantitrenegcows + dim_cat + herdavgdim_cat,
                                                  data = dbmtr_neg,
                                                  family = lognormal(link = "identity"),
                                                  warmup = 500,
                                                  iter = 1500,
                                                  chains = 2,
                                                  cores = 4,
                                                  threads = threading(2),
                                                  backend = "cmdstanr")

saveRDS(brms_mod_lognorm_neg_yield_scc_mtnc_dimind_avgdimcat, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_lognorm_neg_yield_scc_mtnc_dimind_avgdimcat.rds")

#######YIELD + SCC + MTNC + DIM (INDICATOR) * HERDAVGDIMCAT#######

brms_mod_lognorm_neg_yield_scc_mtnc_int_avgdimcat_dimind <- brm(titre ~ yield + I(log(cellcount)) + meantitrenegcows * herdavgdim_cat + dim_cat,
                                                            data = dbmtr_neg,
                                                            family = lognormal(link = "identity"),
                                                            warmup = 500,
                                                            iter = 1500,
                                                            chains = 2,
                                                            cores = 4,
                                                            threads = threading(2),
                                                            backend = "cmdstanr")

saveRDS(brms_mod_lognorm_neg_yield_scc_mtnc_int_avgdimcat_dimind, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_lognorm_neg_yield_scc_mtnc_int_avgdimcat_dimind.rds")


#######YIELD + SCC + MTNC * AVGDIM + DIM (INDICATOR)#######

brms_mod_lognorm_neg_yield_scc_mtnc*avgdim_dimind <- brm(titre ~ yield + I(log(cellcount)) + meantitrenegcows*herdavgdim + dim_cat,
                                                  data = dbmtr_neg,
                                                  family = lognormal(link = "identity"),
                                                  warmup = 500,
                                                  iter = 1500,
                                                  chains = 2,
                                                  cores = 4,
                                                  threads = threading(2),
                                                  backend = "cmdstanr")

saveRDS(brms_mod_lognorm_neg_yield_scc_mtnc*avgdim_dimind, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_lognorm_neg_yield_scc_mtnc*avgdim_dimind.rds")



#######YIELD + SCC + MTNC + DIM (INDICATOR) ALTERNATIVE PRIORS#######

brms_mod_lognorm_neg_yield_scc_mtnc_dimind_altpriors <- brm(titre ~ yield + I(log(cellcount)) + meantitrenegcows + dim_cat,
                                                  data = dbmtr_neg,
                                                  family = lognormal(link = "identity"),
                                                  prior = c(prior("normal(0,1000)", class = "Intercept"),
                                                            prior("normal(0,1000)", class = "sigma")),
                                                  warmup = 500,
                                                  iter = 1500,
                                                  chains = 2,
                                                  cores = 4,
                                                  threads = threading(2),
                                                  backend = "cmdstanr")



saveRDS(brms_mod_lognorm_neg_yield_scc_mtnc_dimind_altpriors, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_lognorm_neg_yield_scc_mtnc_dimind_altpriors.rds")

#######YIELD + SCC + DIM (INDICATOR)#######

brms_mod_lognorm_neg_yield_scc_dimind <- brm(titre ~ yield + I(log(cellcount)) + dim_dummy_05 + dim_dummy_510,
                                                            data = dbmtr_neg,
                                                            family = lognormal(link = "identity"),
                                                            warmup = 500,
                                                            iter = 1500,
                                                            chains = 2,
                                                            cores = 6,
                                                            threads = threading(3),
                                                            backend = "cmdstanr")



saveRDS(brms_mod_lognorm_neg_yield_scc_dimind, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_lognorm_neg_yield_scc_dimind.rds")

#######AGE + YIELD + SCC + PROTEIN + BUTTERFAT + DIM#######

brms_mod_lognorm_neg_age_yield_scc_mtnc_protein_butterfat_dim <- brm(titre ~
                                                                       s(age) +
                                                                       yield +
                                                                       log(cellcount) +
                                                                       meantitrenegcows +
                                                                       protein +
                                                                       butterfat +
                                                                       s(dim),
                                                                     data = dbmtr_neg,
                                                                     family = lognormal(link = "identity"),
                                                                     warmup = 500,
                                                                     iter = 1500,
                                                                     chains = 2,
                                                                     cores = 6,
                                                                     threads = threading(3),
                                                                     backend = "cmdstanr")

conditional_effects(brms_mod_lognorm_neg_age_yield_scc_mtnc_protein_butterfat_dim)

######FIT POSITIVE MODELS######

#######AGE + YIELD + SCC + MTNC#######

prior1 <- prior("normal(2,2)", nlpar = "b1") +
  prior("normal(1,2)", nlpar = "b2") +
  prior("normal(0, 0.5)", nlpar = "b3") +
  prior("normal(-1, 0.5", nlpar = "b4") +
  prior("normal(0.1, 0.3", nlpar = "b5") +
  prior("normal(0.1, 0.3", nlpar = "b6")
  

brms_mod_lognormal_pos_age_yield_scc_mtnc <- brm(formula = bf(titre ~ b1 - (b2 * (exp(1)^(-b3*age))) + b4 * yield + b5 * log(cellcount) + b6 * meantitrenegcows,
                                                     b1 + b2 + b3 + b4 + b5 + b6 ~ 1, 
                                                     nl = TRUE),
                                        prior = prior1,
                                        data = dbmtr_pos,
                                        family = lognormal(link = "identity"),
                                        warmup = 1000,
                                        iter = 10000,
                                        control = list(adapt_delta = 0.95),
                                        init = "random",
                                        chains = 2,
                                        cores = 4,
                                        threads = threading(2),
                                        backend = "cmdstanr")

saveRDS(brms_mod_lognormal_pos_age_yield_scc_mtnc, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_lognormal_pos_age_yield_scc_mtnc.rds")

#######AGE + YIELD + SCC + DIM (INDICATOR)#######


prior1 <- prior("normal(2,0.3)", nlpar = "b1") +
  prior("normal(7,0.5)", nlpar = "b2") +
  prior("normal(0,0.2)", nlpar = "b3") +
  prior("normal(0,0.2", nlpar = "b4") +
  prior("normal(0,0.2", nlpar = "b5") +
  prior("normal(0.9,0.2", nlpar = "b6") +
  prior("normal(0.3,0.2", nlpar = "b7")

inits1 <- 
  list(b_b1 = 1,
       b_b2 = 9,
       b_b3 = 0.01,
       b_b4 = 0.05,
       b_b5 = 0.01,
       b_b6 = 1.2,
       b_b7 = 0.1,
       sigma = 1)

inits2 <- 
  list(b_b1 = 3,
       b_b2 = 6,
       b_b3 = 0.1,
       b_b4 = -0.1,
       b_b5 = 0.2,
       b_b6 = 0.5,
       b_b7 = 0.5,
       sigma = 1.5)

initlist <- list(inits1, inits2)

brms_mod_lognorm_pos_age_yield_scc_dimind <- brm(formula = bf(titre ~ b1 - (b2 * (exp(1)^(-b3*age))) + b4 * yield + b5 * log(cellcount) + b6 * dim_dummy_05 + b7 * dim_dummy_510,
                                                                   b1 + b2 + b3 + b4 + b5 + b6 + b7 ~ 1, 
                                                                   nl = TRUE),
                                                      prior = prior1,
                                                      data = dbmtr_pos,
                                                      family = lognormal(link = "identity"),
                                                      warmup = 1000,
                                                      iter = 5000,
                                                      #control = list(adapt_delta = 0.95),
                                                      init = initlist,
                                                      chains = 2,
                                                      cores = 6,
                                                      threads = threading(3),
                                                      backend = "cmdstanr")



saveRDS(brms_mod_lognorm_pos_age_yield_scc_dimind, "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/brms_mod_lognorm_pos_age_yield_scc_dimind.rds")


####FIT BRMS MODELS (WITH RANDOM EFFECTS)####

#####GAMMA SEPARATE POS/NEG MODELS#####

######FIT MODELS######

prior1 <- prior("normal(4,20)", nlpar = "b1") +
  prior("normal(13,2)", nlpar = "b2") +
  prior("normal(0.08,0.02)", nlpar = "b3") +
  prior("normal(-0.1,2)", nlpar = "b4")

c1_inits <- list(b_b1 = 40,
                 b_b2 = 100,
                 b_b3 = 0.04,
                 b_b4 = -0.5)

c2_inits <- list(b_b1 = 40,
                 b_b2 = 100,
                 b_b3 = 0.04,
                 b_b4 = -0.5)

brms_mod_gamma_pos_refarmcow_age_yield <- brm(formula = bf(titre ~ b1 - (b2 * (exp(1)^(-b3*age))) + 
                                                b4 * yield,
                                                b1 ~ 1 + (1 | Farm) + (1 | farmcow),
                                              b2 + b3 + b4 ~ 1, 
                                              nl = TRUE),
                                 prior = prior1,
                                 data = dbmtr_pos,
                                 family = Gamma(link = "log"),
                                 warmup = 1000,
                                 iter = 10000,
                                 control = list(adapt_delta = 0.95),
                                 init = all_inits,
                                 chains = 2,
                                 cores = 4,
                                 threads = threading(2),
                                 backend = "cmdstanr")

c1_inits <- list(b_Intercept = 40,
                 b_yield = -0.05,
                 b_cellcount = 0.0002,
                 b_meantitrenegcows = 0.2,
                 b_shape = 2)

c2_inits <- list(b_Intercept = 40,
                 b_yield = -0.05,
                 b_cellcount = 0.0002,
                 b_meantitrenegcows = 0.2,
                 b_shape = 2)

all_inits <- list(c1_inits, c2_inits)

brms_mod_gamma_neg_refarmcow_yield <- brm(formula = titre ~ yield + (1 | Farm) + (1 | farmcow),
                                         data = dbmtr_neg,
                                         family = Gamma(link = "log"),
                                         warmup = 1000,
                                         iter = 12500,
                                         control = list(adapt_delta = 0.9),
                                         init = all_inits,
                                         chains = 2,
                                         cores = 6,
                                         threads = threading(3),
                                         backend = "cmdstanr")

####COMPARE MODELS####

#####READ PICKLED MODELS#####

modslist <- list.files("y:/ian/johnesthresholds/johnesproper/data/pickledmodels",
           pattern = "brms_mod.+rds")

print(modslist)

for (i in 1:length(modslist)){
  modslist[i] <- substring(modslist[i], 1, nchar(modslist[i]) - 4)
}


for(f in modslist){
  assign(f, readRDS(paste0("y:/ian/johnesthresholds/johnesproper/data/pickledmodels/",f,".rds")))
}

#####COMPARE MODELS (LOO/WAIC)#####

loo(brms_mod_gamma_pos_age_yield, brms_mod_gamma_pos_age_yield_scc)

#####POSTERIOR CHECKS#####

mod_to_check <- "brms_mod_lognorm_neg_age_yield_scc_mtnc_protein_butterfat_dim"

if(grepl("_neg_", mod_to_check) == TRUE){
  assign("dt", dbmte_neg)
  xtop <- 50
}

if(grepl("_pos_", mod_to_check) == TRUE){
  assign("dt", dbmte_pos)
  xtop <- 100
}


pp <- pp_check(get(mod_to_check), type = "dens_overlay")

pp + xlim(0,xtop) +
  labs(title = mod_to_check, subtitle = "Training Data")

pp <- pp_check(get(mod_to_check),
               newdata = get("dt"), type = "dens_overlay")

pp + xlim(0, xtop) +
  labs(title = mod_to_check, subtitle = "Testing Data")


####GET BAYES FACTORS FOR TWO MODEL STRATEGY (PROBABILITY AT TITRE)####

#colnum1 <- match('Farm', colnames(data_brms))
#colnum2 <- match('calfeartag', colnames(data_brms))
#colnum3 <- match('date', colnames(data_brms))

#data_brms <- data_brms[order(data_brms[,colnum1], data_brms[,colnum2], as.POSIXct(data_brms[,colnum3])),]


#negmod <- "brms_mod_lognorm_neg_yield_scc_dimind"
#posmod <- "brms_mod_weibull_pos_age_yield_scc_dimind"

#tm <- timerstart()

#repsperunittitre = 0.5

#n_post_draws <- 2000

#ivl = 3 #Interval around titre within which to get probabilities

#slicedrows <- c(round(nrow(data_brms)/10*1),
#                round(nrow(data_brms)/10*2),
#                round(nrow(data_brms)/10*3),
#                round(nrow(data_brms)/10*4),
#                round(nrow(data_brms)/10*5),
#                round(nrow(data_brms)/10*6),
#                round(nrow(data_brms)/10*7),
#                round(nrow(data_brms)/10*8),
#                round(nrow(data_brms)/10*9),
#                round(nrow(data_brms)/10*10))
#
#bf_tab <- foreach(seg = 1:length(slicedrows), .combine = "rbind") %do% {
#
#  nc <- detectCores()
#  
#  sr <- rep(0,nc-1)
#
#  a <- ifelse(seg == 1, 1, slicedrows[seg - 1] + 1)
#  b <- slicedrows[seg]  
#  
#  data_brms_seg <- data_brms[a:b,]
#  
#  
#  for(i in 1:length(sr)){
#    sr[i] <- round(nrow(data_brms_seg)/length(sr)*i)
#  }
#  
#  print("Data chunks to be distributed across cores:")
#  print(sr)
#  
#  regcores(-1)
#  
#  bf_tab_seg <- foreach(i = 1:length(sr), .packages = c("foreach", "brms"), .combine = "rbind") %dopar%{
#    
#    a <- ifelse(i == 1, 1, sr[i-1] + 1)
#    b <- sr[i]
#    
#    tempdata <- data_brms_seg[a:b,]
#                        
#    pp <- foreach(r = 1:nrow(tempdata), .combine = "rbind", .packages = c("brms")) %do% {
#
#      nreps = round(repsperunittitre * tempdata$titre[r],0)
#      nreps <- ifelse(nreps < 1, 1, nreps)
#      
#      pp2 <- foreach(rep = 1:nreps, .combine = "c") %do% {
#      
#        post_draws_pos <- posterior_predict(brms_mod_weibull_pos_age_yield_scc_mtnc_dimind, 
#                                            newdata = tempdata[r,],
#                                            #re.form = ~ 0,
#                                            ndraws = n_post_draws)
#        
#        #n_above_titre_pos <- length(post_draws_pos[post_draws_pos >= dbmte[r,'titre']])
#        
#        prob_at_titre_pos <- length(post_draws_pos[post_draws_pos >= (tempdata[r, 'titre'] - ivl) &
#                                                     post_draws_pos < (tempdata[r,'titre'] + ivl)]) / length(post_draws_pos)
#        
#        rm(post_draws_pos)
#        
#        #prob_at_titre_pos <- prob_at_titre_pos + 0.01
#        
#        post_draws_neg <- posterior_predict(brms_mod_lognorm_neg_yield_scc_mtnc_dimind,
#                                           newdata = tempdata[r,],
#                                           #re.form = ~ 0,
#                                           ndraws = n_post_draws)
#        
#        #n_above_titre_neg <- length(post_draws_neg[post_draws_neg >= dbmte[r,'titre']])
#        
#        prob_at_titre_neg <- length(post_draws_neg[post_draws_neg >= (tempdata[r, 'titre'] - ivl) &
#                                                                          post_draws_neg < (tempdata[r,'titre'] + ivl)]) / length(post_draws_neg)
#        
#        
#        rm(post_draws_neg)
#        
#        prob_at_titre_neg <- prob_at_titre_neg + 0.01
#        
#        
#        prob_at_titre_pos/prob_at_titre_neg
#        
#        
#      }
#      
#      
#      
#      med <- ifelse(length(pp2[pp2 == Inf]) < length(pp2), median(pp2[pp2 != Inf]),
#                    Inf)
#      
#      tryCatch({write.table(r, paste0("C:/Users/Ian.glover.HEADOFFICE/Documents/PythonTools/ForEachRowCounter/rowsdonecore",i,".txt"), append = FALSE, sep = " ", dec = ".",
#                            row.names = FALSE, col.names = FALSE)}, 
#               error = function(cond){})  
#      
#      c(r, 
#        tempdata$Farm[r],
#        tempdata$calfeartag[r],
#        tempdata$date[r],
#        tempdata$age[r],
#        tempdata$dim[r],
#        tempdata$yield[r], 
#        tempdata$titre[r],
#        tempdata$cellcount[r],
#        tempdata$meantitrenegcows[r],
#        prob_at_titre_pos, 
#        prob_at_titre_neg, 
#        med,
#        nreps)
#    }  
#    pp
#  }
#  
#  stopCluster(cl)
#  
#  print(paste0("Segment ",seg,"/",length(slicedrows)," done"))
#  
#  write.csv(bf_tab_seg, paste0('y:/ian/johnesthresholds/johnesproper/data/bf_tab_seg_',seg,'.csv'), row.names = FALSE)
#  
#  bf_tab_seg
#}
#  
#bf_tab <- as.data.frame(bf_tab)

#colnames(bf_tab) <- c("row",
#                      "farm",
#                      "eartag",
#                      "date",
#                      "age",
#                      "dim",
#                      "yield",
#                      "titre",
#                      "cellcount",
#                      "meantitrenegcows",
#                      "prob_at_titre_pos",
#                      "prob_at_titre_neg",
#                      "bayesfactor",
#                      "nreps")

#bf_tab[,c("age",
#          "dim",
#          "yield",
#          "titre",
#          "cellcount",
#          "meantitrenegcows",
#          "prob_at_titre_pos",
#          "prob_at_titre_neg",
#          "bayesfactor")] <-
#  lapply(bf_tab[,c("age",
#                   "dim",
#                   "yield",
#                   "titre",
#                   "cellcount",
#                   "meantitrenegcows",
#                   "prob_at_titre_pos",
#                   "prob_at_titre_neg",
#                   "bayesfactor")],
#         as.numeric)

#write.csv(bf_tab, "y:/ian/johnesthresholds/johnesproper/data/bf_tab.csv", row.names = FALSE)

#timerend(tm)
#
#####PREDICTING UNKNOWN BAYES FACTORS####
#
#
######FREQUENTIST LINEAR MODEL FOR ESTIMATING BAYES FACTOR WHEN P(TITRE | NEG) == 0#####
#
#if(bflinmod == "FREQUENTIST"){
#
#  bf_lm <- lm(bayesfactor ~ age + 
#               yield + 
#                titre + 
#                #I(titre^2) +
#                meantitrenegcows +
#                cellcount,
#              data = bf_tab[bf_tab$bayesfactor != Inf &
#                              !is.na(bf_tab$bayesfactor),])



#  bf_tab$bayesfactor_pred <- bf_tab$bayesfactor
#  
#  bf_tab$bayesfactor_pred[bf_tab$bayesfactor == Inf |
#                            is.na(bf_tab$bayesfactor)] <-
#    predict(bf_lm, newdata = bf_tab[bf_tab$bayesfactor == Inf |
#                                      is.na(bf_tab$bayesfactor),])
#}

######BAYESIAN LINEAR MODEL FOR ESTIMATING BAYES FACTOR WHEN P(TITRE | NEG) == 0#####
#
#if(bflinmod == "BAYESIAN"){
#
#  bf_lm_bayes <- brm(bayesfactor ~ age + 
#                       yield + 
#                       titre +
#                       meantitrenegcows +
#                       cellcount,
#                     data = bf_tab[bf_tab$bayesfactor != Inf &
#                                     !is.na(bf_tab$bayesfactor),],
#                     warmup = 1000,
#                     iter = 5000,
#                     #control = list(adapt_delta = 0.95),
#                    #init = all_inits,
#                    chains = 2,
#                    cores = 4,
#                    threads = threading(2),
#                    backend = "cmdstanr")
#
#  bf_tab$bayesfactor_pred <- bf_tab$bayesfactor

#  bf_tab$bayesfactor_pred[bf_tab$bayesfactor == Inf |
#                            is.na(bf_tab$bayesfactor)] <-
#    posterior_epred(bf_lm_bayes, newdata = bf_tab[bf_tab$bayesfactor == Inf |
#                                      is.na(bf_tab$bayesfactor),])

#}

#####ML Model for Estimating Bayes Factors#####

#fitControl <- trainControl(
#  method = "repeatedcv",
#  number = 10,
#  repeats = 10)

#train.formula <- formula(likelihood ~ age + 
#                           yield + 
#                           titre +
#                           meantitrenegcows +
#                           dim_cat +
#                           cellcount)
#
#regcores(-1)
#
#bf_mars_mod <- train(train.formula,
#                    data = data_brms[data_brms$likelihood != Inf &
#                                       !is.na(data_brms$likelihood),], 
#                    trControl = fitControl, 
#                    method = "earth")
#
#  
#stopCluster(cl)

#thingy <- predict(bf_mars_mod,
#                  newdata = data_brms[data_brms$likelihood == Inf |
#                                        is.na(data_brms$likelihood),])


#data_brms$likelihood_pred <- data_brms$likelihood

#data_brms$likelihood_pred[data_brms$likelihood_pred == Inf |
#                            is.na(data_brms$likelihood_pred)] <-
#  predict(bf_mars_mod,
#          newdata = data_brms[data_brms$likelihood_pred == Inf |
#                                is.na(data_brms$likelihood_pred),])

#ggplot(data_brms,
#       aes(x = titre,
#           y = likelihood_pred)) +
#  geom_point()

#####PLOT PREDICTED BAYES FACTORS#####
  
#print(ggplot(bf_tab[bf_tab$prob_at_titre_pos != 1,],
#       aes(x = titre, y = bayesfactor)) +
#  geom_point() +
#  geom_smooth() +
#  labs(title = "Bayes Factor (Raw)"))

#print(ggplot(bf_tab[bf_tab$prob_at_titre_pos != 1,],
#       aes(x = titre, y = bayesfactor_pred)) +
#  geom_point() +
#  geom_smooth() +
#  labs(title = "Bayes Factor (LM Predicted)"))

#data_brms$likelihood <- bf_tab$bayesfactor_pred

#data_brms$likelihood <- bf_tab$bayesfactor
#
#data_brms$likelihood[is.na(data_brms$likelihood)] <- 250 #Titres too high for positive distribution
#data_brms$likelihood[data_brms$likelihood == Inf] <- 250 #Titres too high for negative cow distribution


####SIMULATE BAYES FACTORS####

data_brms$likelihood <- simulate_bayes_factors(data_brms, npd = 1000000)

####UPDATE POSTERIORS####

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

print(ggplot(data_brms,
       aes(x = PosteriorProb)) +
  geom_histogram())

data_brms$titre <- as.numeric(data_brms$titre)
data_brms$PosteriorProb <- as.numeric(data_brms$PosteriorProb)

data_brms$PosteriorProb[data_brms$PosteriorOdds == Inf] <- 1

#data_brms$PosteriorProb[data_brms$likelihood == Inf |
#                          is.na(data_brms$likelihood)] <- 1

data_brms$Target_QMMS <- as.factor(data_brms$Target_QMMS)

####WRITE DATA####

write.csv(data_brms, "y:/ian/johnesthresholds/johnesproper/data/data_brms.csv", row.names = FALSE)

####PLOT SAMPLES OF ANIMALS####

calfsampleqmms1 <- sample(unique(data_brms$calfeartag[data_brms$Target_altdef1 == "1"]), 50, replace = FALSE)

calfsampleqmms0 <- sample(unique(data_brms$calfeartag[data_brms$Target_altdef1 == "0"]), 100, replace = FALSE)

calfsampleqmmsU <- sample(unique(data_brms$calfeartag[data_brms$Target_altdef1 == "U"]), 100, replace = FALSE)

calfsamplerandom <- sample(unique(data_brms$calfeartag), 200, replace  = FALSE)


for (cow in calfsampleqmms1){
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
  
  ggsave(paste("Y:/Ian/JohnesThresholds/JohnesProper/Data/PosteriorProbCharts/Bayesian/Target_altdef1_Pos/",cow,".png"))
}

  for (cow in calfsampleqmmsU){
    ggplot(data_brms[data_brms$calfeartag == cow,], aes(x = age)) +
      geom_point(aes(y = titre, color = class)) +
      geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
      geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
      geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
      geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_y = 5, check_overlap = T, size = 3) + 
      geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, check_overlap = T, size = 3, color = "black") + 
      geom_text(aes(x = age, y = -8, label = cellcount - 0.0001), size = 3) +
      geom_text(aes(x = age, y = -11, label = dim), size = 3) +
      geom_text(aes(x = age, y = -2, label = ifelse(POFloorApplied == 1, "*", "")), color = "black") +
      geom_text(aes(x = min(age) - ((max(age) - min(age))/10), y = -5, label = "BF"), size = 3) +
      geom_text(aes(x = min(age) - ((max(age) - min(age))/10), y = -8, label = "SCC"), size = 3) +
      geom_text(aes(x = min(age) - ((max(age) - min(age))/10), y = -11, label = "DIM"), size = 3) +
      scale_color_manual(values = group.colours) +
      geom_line(aes(y = PosteriorProb*100)) +
      geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
      geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
      scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
      labs(title = data_brms$calfeartag[data_brms$calfeartag == cow], subtitle = paste0("Birth Probability:", round(data_brms$priorprob_crt[data_brms$calfeartag == cow],2)))
    ggsave(paste("Y:/Ian/JohnesThresholds/JohnesProper/data/PosteriorProbCharts/Bayesian/Target_altdef1_Unknown/",cow,".png"))
  }

for (cow in calfsampleqmms0){
  ggplot(data_brms[data_brms$calfeartag == cow,], aes(x = age)) +
    geom_point(aes(y = titre, color = class)) +
    geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
    geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
    geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, check_overlap = T, size = 3, color = "black") + 
    geom_text(aes(x = age, y = -8, label = cellcount - 0.0001), size = 3) +
    geom_text(aes(x = age, y = -11, label = dim), size = 3) +
    geom_text(aes(x = age, y = -2, label = ifelse(POFloorApplied == 1, "*", "")), color = "black") +
    geom_text(aes(x = min(age) - ((max(age) - min(age))/10), y = -5, label = "BF"), size = 3) +
    geom_text(aes(x = min(age) - ((max(age) - min(age))/10), y = -8, label = "SCC"), size = 3) +
    geom_text(aes(x = min(age) - ((max(age) - min(age))/10), y = -11, label = "DIM"), size = 3) +
    scale_color_manual(values = group.colours) +
    geom_line(aes(y = PosteriorProb*100)) +
    geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
    scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
    labs(title = data_brms$calfeartag[data_brms$calfeartag == cow], subtitle = paste0("Birth Probability:", round(data_brms$priorprob_crt[data_brms$calfeartag == cow],2)))
  
  ggsave(paste("Y:/Ian/JohnesThresholds/JohnesProper/data/PosteriorProbCharts/Bayesian/Target_altdef1_Neg/",cow,".png"))
}
  

  

testcows <- c("UK342393402800",
              "UK283905202841")


for (i in testcows){
  ggplot(data_brms[data_brms$calfeartag == cow,], aes(x = age)) +
    geom_point(aes(y = titre, color = class)) +
    geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
    geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
    geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, check_overlap = T, size = 3, color = "black") + 
    geom_text(aes(x = age, y = -8, label = cellcount - 0.0001), size = 3) +
    geom_text(aes(x = age, y = -11, label = dim), size = 3) +
    geom_text(aes(x = age, y = -2, label = ifelse(POFloorApplied == 1, "*", "")), color = "black") +
    geom_text(aes(x = min(age) - ((max(age) - min(age))/10), y = -5, label = "BF"), size = 3) +
    geom_text(aes(x = min(age) - ((max(age) - min(age))/10), y = -8, label = "SCC"), size = 3) +
    geom_text(aes(x = min(age) - ((max(age) - min(age))/10), y = -11, label = "DIM"), size = 3) +
    scale_color_manual(values = group.colours) +
    geom_line(aes(y = PosteriorProb*100)) +
    geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
    scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
    labs(title = data_brms$calfeartag[data_brms$calfeartag == cow], subtitle = paste0("Birth Probability:", round(data_brms$priorprob_crt[data_brms$calfeartag == cow],2)))
  
  ggsave(paste("Y:/Ian/JohnesThresholds/JohnesProper/data/PosteriorProbCharts/Bayesian/TestCows/",i,".png"))
}

