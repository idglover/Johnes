####DRAW LENGTH OPTIONS####

drawlengths = c(5000,10000,50000,100000,500000,1000000)

####SET TOLERANCE (HOW NEAR TO "TRUE" DO WE NEED TO BE?)####

tol = 10


####GET SAMPLES WITH GOOD DISTRIBUTION OF TITRE####

data_brms$index = 1:nrow(data_brms)

cut = c(0,5,10,20,30,40,60,90,140,200,1000)

rowstosamp <- foreach (i = 1:(length(cut)-1), .combine = "c") %do% {
  tempdata <- data_brms[data_brms$titre >= cut[i] &
                          data_brms$titre < cut[i + 1],]
  samp <- sample(tempdata$index, 50)
  
  
}

dt <- data_brms[rowstosamp,]


####GET BAYES FACTORS @ 1M DRAWS AND THREE REPS OF DIFFERENT DRAW LENGTH OPTIONS####

sr = splitacrosscores(dt, -1)

regcores(-1)

pppp = foreach(core = 1:length(sr), .combine = "rbind", .packages = "foreach") %dopar% {
  
  a = ifelse(core == 1, 1, sr[core - 1] + 1)
  b = sr[core]

  dt2 = dt[a:b,]
  
  ppp = foreach(r = 1:nrow(dt2), .combine = "rbind") %do% {
    
    tryCatch({write.table(r, paste0("C:/Users/Ian.glover.HEADOFFICE/Documents/PythonTools/ForEachRowCounter/rowsdonecore",core,".txt"), append = FALSE, sep = " ", dec = ".",
                          row.names = FALSE, col.names = FALSE)}, 
             error = function(cond){})
  
    tempdata = dt2[r,]
    
    truebf = simulate_bayes_factors(tempdata, npd = 1000000)
    
    pp = foreach(ndraws = drawlengths, .combine = "rbind") %do% {
    
      p = foreach(rep = 1:3, .combine = "c") %do% {
        simulate_bayes_factors(tempdata, npd = ndraws)
      }
      
      c(tempdata$calfeartag,
        tempdata$date,
        tempdata$titre,
        tempdata$age,
        tempdata$yield,
        tempdata$cellcount,
        tempdata$meantitrenegcows,
        tempdata$dim_cat,
        truebf,
        ndraws, p)
    }
    
    pp
  
  }
  
  ppp

}

stopCluster(cl)

colnames(pppp) <- c('calfeartag', 
                    'date', 
                    'titre', 
                    'age', 
                    'yield', 
                    'cellcount', 
                    'meantitrenegcows', 
                    'dim_cat',
                    'truebf',
                    'ndraws',
                    'rep1',
                    'rep2',
                    'rep3')

pppp <- as.data.frame(pppp)

pppp[,c('rep1',
        'rep2',
        'rep3',
        'truebf')] <-
  lapply(pppp[,c('rep1',
                         'rep2',
                         'rep3',
                         'truebf')], as.numeric)

####DECLARE STABILITY OF BF ESTIMATES####

pppp$stable = ifelse(pppp$rep1 >= (pppp$truebf - ((tol/100) * pppp$truebf)) &
                       pppp$rep1 <= (pppp$truebf + ((tol/100) * pppp$truebf)) &
                       pppp$rep2 >= (pppp$truebf - ((tol/100) * pppp$truebf)) &
                       pppp$rep2 <= (pppp$truebf + ((tol/100) * pppp$truebf)) &
                       pppp$rep3 >= (pppp$truebf - ((tol/100) * pppp$truebf)) &
                       pppp$rep3 <= (pppp$truebf + ((tol/100) * pppp$truebf)),
                     1,0)

regcores()

results = foreach(c = 1:nrow(dt), .combine = "rbind") %dopar% {
  
  tempdata = pppp[pppp$calfeartag == dt$calfeartag[c],]
  firststable = match(1, tempdata$stable)
  tempdata$stablendraws = tempdata$ndraws[firststable]
  
  tempdata[1,c('calfeartag',
               'date',
               'titre',
               'age',
               'yield',
               'cellcount',
               'meantitrenegcows',
               'dim_cat',
               'truebf',
               'stablendraws')]
  
}

stopCluster(cl)

####PLOTS####

#NB LOOKS LIKE TITRE IS BY FAR THE MOST INFLUENTIAL ON BAYES FACTOR

results$titre = as.numeric(results$titre)
results$age = as.numeric(results$age)
results$yield  <- as.numeric(results$yield)
results$cellcount <- as.numeric(results$cellcount)
results$meantitrenegcows <- as.numeric(results$meantitrenegcows)
results$stablendraws = as.numeric(results$stablendraws)

ggplot(results,
       aes(x = titre,
           y = stablendraws)) +
  geom_point()

ggplot(results,
       aes(x = age,
           y = stablendraws)) +
  geom_point()

results$titre = as.numeric(results$yield)

ggplot(results,
       aes(x = yield,
           y = stablendraws)) +
  geom_point()

ggplot(results,
       aes(x = cellcount,
           y = stablendraws)) +
  geom_point()

ggplot(results,
       aes(x = meantitrenegcows,
           y = stablendraws)) +
  geom_point()

ggplot(results,
       aes(x = dim_cat,
           y = stablendraws)) +
  geom_bar(stat = "summary", fun = "mean")


####MODEL INFLUENCES ON BAYES FACTOR####

mod <- lm(stablendraws ~
            titre +
            age +
            yield +
            cellcount +
            meantitrenegcows +
            dim_cat,
          data = results)

summary(mod)

plot(ggeffect(mod))

ggplot(results,
       aes(x = pred_n_draws,
           y = stablendraws)) +
  geom_point()

####TEST MODEL####

results$pred_n_draws <- predict(mod, newdata = results)

results$resid = results$pred_n_draws - results$stablendraws

min(results$titre[results$stablendraws == 5000], na.rm = TRUE)
min(results$titre[results$stablendraws == 10000], na.rm = TRUE)
min(results$titre[results$stablendraws == 50000], na.rm = TRUE)
min(results$titre[results$stablendraws == 100000], na.rm = TRUE)
min(results$titre[results$stablendraws == 500000], na.rm = TRUE)
min(results$titre[results$stablendraws == 1000000], na.rm = TRUE)

#Model not great!

#Suggest:

#Titre 0-2.99: 5000 draws
#3 - 7.99: 10000 draws
#8 -11.99: 50000 draws
#12 - 14.99: 100000 draws
#15 - 24.99: 500000 draws
#>= 25: 1000000 draws

####CHECK RATE OF 1M DRAWS vs DRAW LENGTH VARYING WITH TITRE####

tm <- timerstart()

thingytrue <- simulate_bayes_factors(data_brms[1:10000,], npd = 1000000)

timerend(tm) #Approx 600/mln (approx 7 hours for full dataset)

tm <- timerstart()

thingydynamic <- simulate_bayes_factors(data_brms[1:10000,], npd = "dynamic")

timerend(tm) #Approx 1300/min (approx 3 hours for full dataset)


####CHECK THAT BAYES FACTORS AGREE BETWEEN 1M DRAWS AND DYNAMIC DRAW LENGTH####

pdata <- data.frame(truebf = thingytrue,
                    dynamicbf = thingydynamic)

pdata <- pdata[pdata$truebf != Inf &
                 pdata$dynamicbf != Inf,]

ggplot(pdata,
       aes(x = truebf,
           y = dynamicbf)) +
  geom_point() +
  labs(title = "Bayes Factors (True vs Dynamic)",
       x = "1 Million Draws",
       y = "Dynamic Draw Length") +
  xlim(0,30000) +
  ylim(0,30000) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_smooth(method = "lm")

ggplot(pdata,
       aes(x = log(truebf),
           y = log(dynamicbf))) +
  geom_point() +
  labs(title = "Log Bayes Factors (True vs Dynamic)",
       x = "1 Million Draws",
       y = "Dynamic Draw Length") +
  #xlim(0,30000) +
  #ylim(0,30000) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_smooth(method = "lm")

ggplot(pdata,
       aes(x = truebf,
           y = dynamicbf)) +
  geom_point() +
  labs(title = "Bayes Factors (True vs Dynamic)",
       subtitle = "Lower Bayes Factors only",
       x = "1 Million Draws",
       y = "Dynamic Draw Length") +
  xlim(0,200) +
  ylim(0,200) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_smooth(method = "lm")


#Correlation between true and dynamic BFs not great at very high end, but very decent at low end (BF > 500)


#The above works, but could we speed up any more?

dat <- data_brms[data_brms$titre >=25 &
                   data_brms$titre < Inf,]
dat <- dat[1:1000,]

tm <- timerstart()

thingytrue <- simulate_bayes_factors(dat, npd = 5000000)

timerend(tm)

tm <- timerstart()


thingy200000 <- simulate_bayes_factors(dat, npd = 200000)


timerend(tm)

plot(thingytrue, thingy200000)

#Try the following instead:

#Titre <3 : 500
#3-8 2000
#8-12 20000
#12-15 50000
#15-25 200000
#>25 1 Million

#Now try "true" vs dynamic bayes factors again

tm <- timerstart()

thingytrue <- simulate_bayes_factors(data_brms[1:10000,], npd = 1000000) #16.9 Minutes

timerend(tm) #Approx 600/mln (approx 7 hours for full dataset)

tm <- timerstart()

thingydynamic <- simulate_bayes_factors(data_brms[1:10000,], npd = "dynamic") #7.7 Mins

timerend(tm) #Approx 1300/min (approx 3 hours for full dataset) - NO TIME GAIN WITH LOWERING NUMBER OF DRAWS FURTHER.

pdata <- data.frame(truebf = thingytrue,
                    dynamicbf = thingydynamic)

pdata <- pdata[pdata$truebf != Inf &
                 pdata$dynamicbf != Inf,]

ggplot(pdata,
       aes(x = truebf,
           y = dynamicbf)) +
  geom_point() +
  labs(title = "Bayes Factors (True vs Dynamic)",
       x = "1 Million Draws",
       y = "Dynamic Draw Length") +
  xlim(0,30000) +
  ylim(0,30000) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_smooth(method = "lm")

ggplot(pdata,
       aes(x = truebf,
           y = dynamicbf)) +
  geom_point() +
  labs(title = "Bayes Factors (True vs Dynamic)",
       subtitle = "Lower Bayes Factors only",
       x = "1 Million Draws",
       y = "Dynamic Draw Length") +
  xlim(0,200) +
  ylim(0,200) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_smooth(method = "lm")

#STICK WITH ORIGINAL PLAN
