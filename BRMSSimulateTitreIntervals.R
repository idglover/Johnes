####SIMULATE DATA####

s <- expand.grid(titre = c(1,3,5,10,15,20,30,40,60),
                        age = c(24,36,48,60,72,84,96,108),
                        yield = c(5,10,15,20,30,40,50,70),
                        meantitrenegcows = c(1,3,6,9),
                        cellcount = c(10,50,100,150,200,500,1000,2000),
                        dim = c(3,7,50))
                 
                 #,
                  #      herdavgdim_cat = c("(-1,50]", "(50,200]", "(200,1e+03]"))

s$dim_cat <- cut(s$dim, breaks = c(-1,5,10,10000))

s$dim_cat <- relevel(s$dim_cat, "(10,1e+04]")

s$dim_dummy_05 <- as.factor(ifelse(s$dim_cat == "(-1,5]","1","0"))
s$dim_dummy_510 <- as.factor(ifelse(s$dim_cat == "(5,10]","1","0"))


####GET BAYES FACTORS####

dt <- s #or d

bf <- simulate_bayes_factors(dt, 
                             negmod = 'brms_mod_lognorm_neg_yield_scc_dimind', 
                             posmod = 'brms_mod_lognorm_pos_age_yield_scc_dimind',
                             npd = 1000000)


####PLOTS####

#####UNIVARIABLE#####

dt$bf <- bf

summary(as.factor(dt$titre))
summary(as.factor(dt$age))
summary(as.factor(dt$yield))
summary(as.factor(dt$cellcount))
summary(as.factor(dt$meantitrenegcows))
summary(as.factor(dt$dim))

ggplot(dt[dt$age == 60 & 
            dt$yield == 30 &
            dt$cellcount == 100 &
            dt$meantitrenegcows == 6 &
            dt$dim == 50,],
       aes(x = titre,
           y = bf)) +
  geom_line()

ggsave('Y:/Ian/JohnesThresholds/JohnesProper/Data/BRMSModelResults/ExcludingBadData/BRMSAgeYieldSCCDIMIndSimulated1MBF/MarginalEffects/titre.png')

ggplot(dt[dt$titre == 5 & 
            dt$yield == 30 &
            dt$cellcount == 100 &
            dt$meantitrenegcows == 6 &
            dt$dim == 50,],
       aes(x = age,
           y = bf)) +
  geom_line()

ggsave('Y:/Ian/JohnesThresholds/JohnesProper/Data/BRMSModelResults/ExcludingBadData/BRMSAgeYieldSCCDIMIndSimulated1MBF/MarginalEffects/age.png')

ggplot(dt[dt$age == 60 & 
            dt$titre == 5 &
            dt$cellcount == 100 &
            dt$meantitrenegcows == 6 &
            dt$dim == 50,],
       aes(x = yield,
           y = bf)) +
  geom_line()

ggsave('Y:/Ian/JohnesThresholds/JohnesProper/Data/BRMSModelResults/ExcludingBadData/BRMSAgeYieldSCCDIMIndSimulated1MBF/MarginalEffects/yield.png')

ggplot(dt[dt$age == 60 & 
            dt$yield == 30 &
            dt$titre == 5 &
            dt$meantitrenegcows == 6 &
            dt$dim == 50,],
       aes(x = cellcount,
           y = bf)) +
  geom_line()

ggsave('Y:/Ian/JohnesThresholds/JohnesProper/Data/BRMSModelResults/ExcludingBadData/BRMSAgeYieldSCCDIMIndSimulated1MBF/MarginalEffects/cellcount.png')

ggplot(dt[dt$age == 60 & 
            dt$yield == 30 &
            dt$cellcount == 100 &
            dt$titre == 5 &
            dt$dim == 50,],
       aes(x = meantitrenegcows,
           y = bf)) +
  geom_line()

ggsave('Y:/Ian/JohnesThresholds/JohnesProper/Data/BRMSModelResults/ExcludingBadData/BRMSAgeYieldSCCDIMIndSimulated1MBF/MarginalEffects/mtnc.png')

ggplot(dt[dt$age == 60 & 
            dt$yield == 30 &
            dt$cellcount == 100 &
            dt$meantitrenegcows == 6 &
            dt$titre == 5,],
       aes(x = dim,
           y = bf)) +
  geom_line()

ggsave('Y:/Ian/JohnesThresholds/JohnesProper/Data/BRMSModelResults/ExcludingBadData/BRMSAgeYieldSCCDIMIndSimulated1MBF/MarginalEffects/dim.png')


#####INTERACTIONS#####

ggplot(dt[dt$yield == 30 &
            dt$cellcount == 100 &
            dt$meantitrenegcows == 6 &
            dt$dim == 50,],
       aes(x = titre,
           y = bf,
           color = as.factor(age))) +
  geom_line()

ggsave('Y:/Ian/JohnesThresholds/JohnesProper/Data/BRMSModelResults/ExcludingBadData/BRMSAgeYieldSCCDIMIndSimulated1MBF/MarginalEffects/titrebyage.png')

ggplot(dt[dt$age == 60 &
            dt$cellcount == 100 &
            dt$meantitrenegcows == 6 &
            dt$dim == 50,],
       aes(x = titre,
           y = bf,
           color = as.factor(yield))) +
  geom_line()

ggsave('Y:/Ian/JohnesThresholds/JohnesProper/Data/BRMSModelResults/ExcludingBadData/BRMSAgeYieldSCCDIMIndSimulated1MBF/MarginalEffects/titrebyyield.png')

ggplot(dt[dt$yield == 30 &
            dt$dim == 50 &
            dt$meantitrenegcows == 6 &
            dt$age == 60,],
       aes(x = titre,
           y = bf,
           color = as.factor(cellcount))) +
  geom_line()

ggsave('Y:/Ian/JohnesThresholds/JohnesProper/Data/BRMSModelResults/ExcludingBadData/BRMSAgeYieldSCCDIMIndSimulated1MBF/MarginalEffects/titrebycellcount.png')

ggplot(dt[dt$yield == 30 &
            dt$cellcount == 100 &
            dt$age == 60 &
            dt$dim == 50,],
       aes(x = titre,
           y = bf,
           color = as.factor(meantitrenegcows))) +
  geom_line()

ggsave('Y:/Ian/JohnesThresholds/JohnesProper/Data/BRMSModelResults/ExcludingBadData/BRMSAgeYieldSCCDIMIndSimulated1MBF/MarginalEffects/titrebymtnc.png')

ggplot(dt[dt$yield == 30 &
            dt$cellcount == 100 &
            dt$age == 60 &
            dt$meantitrenegcows == 6,],
       aes(x = titre,
           y = bf,
           color = as.factor(dim))) +
  geom_line()

ggsave('Y:/Ian/JohnesThresholds/JohnesProper/Data/BRMSModelResults/ExcludingBadData/BRMSAgeYieldSCCDIMIndSimulated1MBF/MarginalEffects/titrebydim.png')



