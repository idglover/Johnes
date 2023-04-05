####GET DATA####


data_brms <- read.csv("Y:/Ian/JohnesThresholds/JohnesProper/Data/data_birthpriors.csv")


data_brms$testnum <- substr(data_brms$covsandtitre, 13, 15)

data_cortitre <- data_brms[,c('Farm', 
                              'calfeartag',
                              'date',
                              'parity',
                              'age',
                              'dim',
                              'yield',
                              'cellcount',
                              'protein',
                              'butterfat',
                              'titre'
                              )]


#####ELIMINATE COWS WITH BAD DATA####

data_cortitre$goodrows <- ifelse(data_cortitre$age >= 18 &
                                   !is.na(data_cortitre$age) &
                               !is.na(data_cortitre$yield) &
                               data_cortitre$yield > 0 &
                               !is.na(data_cortitre$cellcount) &
                                        data_cortitre$cellcount > 0 &
                                 !is.na(data_cortitre$protein) &
                                          data_cortitre$protein > 0 &
                                 data_cortitre$protein <= 7.5 &
                                 !is.na(data_cortitre$butterfat) &
                                 data_cortitre$butterfat > 0 &
                               data_cortitre$dim > 0 &
                               !is.na(data_cortitre$dim) &
                                 data_cortitre$dim <= 750 &
                                        !is.na(data_cortitre$titre) &
                                                 data_cortitre$titre > 0,
                             1,0)

regcores(-1)

sr <- splitacrosscores(data_cortitre, -1)

p <- foreach(core = 1:length(sr), .combine = "rbind", .packages = "foreach") %dopar% {
  a <- ifelse(core == 1, 1, sr[core-1] + 1)
  b <- sr[core]
  
  tempdata <- data_cortitre[a:b,]
  
  
  
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

length(unique(data_cortitre$calfeartag))

data_cortitre <- data_cortitre[paste0(data_cortitre$Farm, data_cortitre$calfeartag) %in% paste0(goodcows$Farm, goodcows$calfeartag),]

dim(data_cortitre)

colnum <- match('goodrows', colnames(data_cortitre))

data_cortitre <- data_cortitre[,-colnum]

####DESCRIPTIVE STATS####

descstats(data_cortitre)

ggplot(data_cortitre,
       aes(x = age)) +
  geom_histogram()

ggplot(data_cortitre,
       aes(x = dim)) +
  geom_histogram()


ggplot(data_cortitre,
       aes(x = yield)) +
  geom_histogram()

ggplot(data_cortitre,
       aes(x = cellcount)) +
  geom_histogram()

ggplot(data_cortitre,
       aes(x = protein)) +
  geom_histogram()

ggplot(data_cortitre,
       aes(x = butterfat)) +
  geom_histogram()

ggplot(data_cortitre,
       aes(x = titre)) +
  geom_histogram()


data_cortitre$titre_log <- log(data_cortitre$titre)

hist(data_cortitre$titre_log,100)

data_cortitre$cellcount_log <- log(data_cortitre$cellcount)

hist(data_cortitre$cellcount_log,100)


####SPLIT PRIMI/MULTIPAROUS

data_cortitre_primi <- data_cortitre[data_cortitre$parity == 1,]
data_cortitre_multi <- data_cortitre[data_cortitre$parity > 1,]

####REMOVE OUTLIERS####

#Note that data is not restricted to +/- 2 SDs for titre

#for(v in c('dim',
#           'age',
#           'yield',
#           'cellcount_log',
#           'protein',
#           'butterfat')){


#colnum <- match(v, colnames(data_cortitre_primi))
#sdv <- sd(data_cortitre_primi[,colnum])
#meanv <- mean(data_cortitre_primi[,colnum])
#data_cortitre_primi <- data_cortitre_primi[data_cortitre_primi[,colnum] >= meanv - (2 * sdv) &
#                                             data_cortitre_primi[,colnum] <= meanv + (2 * sdv),]


#colnum <- match(v, colnames(data_cortitre_multi))
#sdv <- sd(data_cortitre_multi[,colnum])
#meanv <- mean(data_cortitre_multi[,colnum])
#data_cortitre_multi <- data_cortitre_multi[data_cortitre_multi[,colnum] >= meanv - (2 * sdv) &
#                                             data_cortitre_multi[,colnum] <= meanv + (2 * sdv),]

#}


####UNIVARIABLE PLOTS####

#####PRIMPAROUS#####

ggplot(data_cortitre_primi,
       aes(x = dim,
           y = titre_log)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Primiparous")

ggplot(data_cortitre_primi,
       aes(x = dim,
           y = yield)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Primiparous")

ggplot(data_cortitre_primi,
       aes(x = yield,
           y = titre)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ cut(dim,10)) +
  labs(title = "Primiparous")

ggplot(data_cortitre_primi,
       aes(x = yield,
           y = titre)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ cut(dim,4) +
               cut(age,4)) +
  labs(title = "Primiparous")

ggplot(data_cortitre_primi,
       aes(x = age,
           y = titre)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ cut(dim,4) +
               cut(yield,4)) +
  labs(title = "Primiparous")

ggplot(data_cortitre_primi,
       aes(x = age,
           y = titre_log)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Primiparous")

ggplot(data_cortitre_primi,
       aes(x = dim,
           y = age)) +
  geom_point() +
  geom_smooth()

cor(data_cortitre_primi$dim,
    data_cortitre_primi$age)

mod <- lmer(titre_log ~
            age +
            dim +
            I(dim^2) +
              yield +
            (1 | Farm) +
            (1 | calfeartag),
          data = data_cortitre_primi)

plot(ggpredict(mod))


#Age, DIM and yield are all correlated in primiparous data.
#Non-linear DIM
#Non-linear yield
#Age not necessary

ggplot(data_cortitre_primi,
       aes(x = yield,
           y = titre_log)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Primiparous")


ggplot(data_cortitre_primi,
       aes(x = cellcount_log,
           y = titre_log)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Primiparous")

ggplot(data_cortitre_primi,
       aes(x = cellcount_log,
           y = titre_log)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Primiparous") +
  facet_wrap(~cut(yield, 10))

ggplot(data_cortitre_primi,
       aes(x = protein,
           y = titre_log)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Primiparous") +
  facet_wrap(~cut(yield, 6))

ggplot(data_cortitre_primi,
       aes(x = butterfat,
           y = titre_log)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Primiparous") +
  facet_wrap(~cut(yield, 6))


#####MULTIPAROUS#####

ggplot(data_cortitre_multi,
       aes(x = dim,
           y = titre_log)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Multiparous")

ggplot(data_cortitre_multi,
       aes(x = age,
           y = titre_log)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Multiparous")


ggplot(data_cortitre_multi,
       aes(x = yield,
           y = titre_log)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Multiparous")

ggplot(data_cortitre_multi,
       aes(x = yield,
           y = titre_log)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Multiparous") +
  facet_wrap(~cut(dim, 10))


ggplot(data_cortitre_multi,
       aes(x = cellcount_log,
           y = titre_log)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Multiparous")

ggplot(data_cortitre_multi,
       aes(x = yield,
           y = titre_log)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Multiparous") +
  facet_wrap(~cut(dim, 6))

ggplot(data_cortitre_multi,
       aes(x = protein,
           y = titre_log)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Multiparous")

  ggplot(data_cortitre_multi,
       aes(x = protein,
           y = titre_log)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Multiparous") +
  facet_wrap(~cut(yield, 6))

ggplot(data_cortitre_multi,
       aes(x = butterfat,
           y = titre_log)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Multiparous")


ggplot(data_cortitre_primi,
       aes(x = age,
           y = dim)) +
  geom_point() +
  geom_smooth()



####CREATE B SPLINE MODELS####

#####DIM#####

######PRIMIPAROUS######

overall <- foreach(d = c(6:25), .combine = "rbind") %do% {

  mod <- lm(titre_log ~ 
              bs(dim, df = d, degree = 3),
            data = data_cortitre_primi)

  pred <- predict(mod,
                  newdata = data_cortitre_primi)
  
  e <- data_cortitre_primi$titre_log - pred
  se = e^2
  mse = mean(se)
  rmse = sqrt(mse)
  
  c(d, summary(mod)$r.squared, rmse)
  
}

colnames(overall) <- c('df', 'rsq', 'RMSE')

regcores()

crossval <- foreach(d = c(6:25), .combine = "rbind", .packages = c("foreach", "splines", "caret")) %dopar% {
  p <- foreach(i = 1:10, .combine = "rbind") %do% {
    set.seed(as.integer(rnorm(1,1000,100)))
    folds <- createFolds(1:nrow(data_cortitre_primi),10)
    pp <- foreach(j = 1:10, .combine = "rbind") %do% {
      trainset <- data_cortitre_primi[-folds[[j]],]
      testset <- data_cortitre_primi[folds[[j]],]
      mod <- lm(titre_log ~ 
                  bs(dim, df = d, degree = 3),
                data = trainset)
      
      pred <- predict(mod,
                      newdata = testset,
                      type = "response")
      
      e <- testset$titre_log - pred
      se = e^2
      mse = mean(se)
      rmse = sqrt(mse)
      
      #x = testset$dim
      #x2 = x^2
      #y = testset$titre_log
      #y2 = y^2
      #xy = x*y
      #n = nrow(testset)
      
      #nsumxy = n * sum(xy)
      #sumx = sum(x)
      #sumy = sum(y)
      #nsumx2 = n * sum(x2)
      #nsumy2 = n * sum(y2)
      
      #numr = nsumxy - (sumx * sumy)
      #denr = sqrt(nsumx2 - (sumx^2)) * sqrt(nsumy2 - (sumy^2))
      
      #rsq = (numr/denr)^2

      r = cor(pred, testset$titre_log)
      rsq = r^2
            
      c(rsq, rmse)
    }
    
    c(mean(pp[,1]), mean(pp[,2]))

  }
  
  c(d, mean(p[,1]), mean(p[,2]))
}

stopCluster(cl)

colnames(crossval) <- c('df', 'cv_rsq', 'cv_RMSE')

rslts <- merge(overall, 
               crossval,
               by = "df",
               all.x = TRUE)

rslts$diffrsq = rslts$rsq - rslts$cv_rsq
rslts$diffRMSE = rslts$RMSE - rslts$cv_RMSE


ggplot(rslts,
       aes(x = df)) +
  geom_line(aes(y = rsq), color = "red") +
  geom_line(aes(y = cv_rsq), color = "darkred")

ggplot(rslts,
       aes(x = df)) +
  geom_line(aes(y = RMSE), color = "blue") +
  geom_line(aes(y = cv_RMSE), color = "darkblue")






mod <- lm(titre_log ~ 
            bs(dim, df = 12, degree = 3),
          data = data_cortitre_primi) 

df <- data.frame(dim = 8:749)

df$pred <- predict(mod,
                   newdata = df)

ggplot(df,
       aes(x = dim,
           y = pred)) +
  geom_point() +
  labs(title = "B-Splines DIM Primiparous")


######MULTIPAROUS######

overall <- foreach(d = c(6:25), .combine = "rbind") %do% {
  
  mod <- lm(titre_log ~ 
              bs(dim, df = d),
            data = data_cortitre_multi)
  
  pred <- predict(mod,
                  newdata = data_cortitre_multi)
  
  e <- data_cortitre_multi$titre_log - pred
  se = e^2
  mse = mean(se)
  rmse = sqrt(mse)
  
  c(d, summary(mod)$r.squared, rmse)
  
}

colnames(overall) <- c('df', 'rsq', 'RMSE')

regcores()

crossval <- foreach(d = c(6:25), .combine = "rbind", .packages = c("foreach", "splines", "caret")) %dopar% {
  p <- foreach(i = 1:10, .combine = "rbind") %do% {
    set.seed(as.integer(rnorm(1,1000,100)))
    folds <- createFolds(1:nrow(data_cortitre_multi),10)
    pp <- foreach(j = 1:10, .combine = "rbind") %do% {
      trainset <- data_cortitre_multi[-folds[[j]],]
      testset <- data_cortitre_multi[folds[[j]],]
      mod <- lm(titre_log ~ 
                  bs(dim, df = d),
                data = trainset)
      
      pred <- predict(mod,
                      newdata = testset,
                      type = "response")
      
      e <- testset$titre_log - pred
      se = e^2
      mse = mean(se)
      rmse = sqrt(mse)
      
      #x = testset$dim
      #x2 = x^2
      #y = testset$titre_boxcox
      #y2 = y^2
      #xy = x*y
      #n = nrow(testset)
      
      #nsumxy = n * sum(xy)
      #sumx = sum(x)
      #sumy = sum(y)
      #nsumx2 = n * sum(x2)
      #nsumy2 = n * sum(y2)
      
      #numr = nsumxy - (sumx * sumy)
      #denr = sqrt(nsumx2 - (sumx^2)) * sqrt(nsumy2 - (sumy^2))
      
      #rsq = (numr/denr)^2
      
      r = cor(pred, testset$titre_log)
      rsq = r^2
      
      c(rsq, rmse)
    }
    
    c(mean(pp[,1]), mean(pp[,2]))
    
  }
  
  c(d, mean(p[,1]), mean(p[,2]))
}

stopCluster(cl)

colnames(crossval) <- c('df', 'cv_rsq', 'cv_RMSE')

rslts <- merge(overall, 
               crossval,
               by = "df",
               all.x = TRUE)


ggplot(rslts,
       aes(x = df)) +
  geom_line(aes(y = rsq), color = "red") +
  geom_line(aes(y = cv_rsq), color = "darkred")

ggplot(rslts,
       aes(x = df)) +
  geom_line(aes(y = RMSE), color = "blue") +
  geom_line(aes(y = cv_RMSE), color = "darkblue")




mod <- lm(titre_log ~ 
            bs(dim, df = 12),
          data = data_cortitre_multi) 

df <- data.frame(dim = 1:745)

df$pred <- predict(mod,
                   newdata = df)

ggplot(df,
       aes(x = dim,
           y = pred)) + 
  geom_point() +
  labs(title = "B-Splines DIM Multiparous")

#####YIELD#####

######MULTIPAROUS######

overall <- foreach(d = c(3:10), .combine = "rbind") %do% {
  
  mod <- lm(titre_log ~ 
              bs(yield, df = d),
            data = data_cortitre_multi)
  
  pred <- predict(mod,
                  newdata = data_cortitre_multi)
  
  e <- data_cortitre_multi$titre_log - pred
  se = e^2
  mse = mean(se)
  rmse = sqrt(mse)
  
  c(d, summary(mod)$r.squared, rmse)
  
}

colnames(overall) <- c('df', 'rsq', 'RMSE')

regcores()

crossval <- foreach(d = c(3:10), .combine = "rbind", .packages = c("foreach", "splines", "caret")) %dopar% {
  p <- foreach(i = 1:10, .combine = "rbind") %do% {
    set.seed(as.integer(rnorm(1,1000,100)))
    folds <- createFolds(1:nrow(data_cortitre_multi),10)
    pp <- foreach(j = 1:10, .combine = "rbind") %do% {
      trainset <- data_cortitre_multi[-folds[[j]],]
      testset <- data_cortitre_multi[folds[[j]],]
      mod <- lm(titre_log ~ 
                  bs(yield, df = d),
                data = trainset)
      
      pred <- predict(mod,
                      newdata = testset,
                      type = "response")
      
      e <- testset$titre_log - pred
      se = e^2
      mse = mean(se)
      rmse = sqrt(mse)
      
      #x = testset$dim
      #x2 = x^2
      #y = testset$titre_boxcox
      #y2 = y^2
      #xy = x*y
      #n = nrow(testset)
      
      #nsumxy = n * sum(xy)
      #sumx = sum(x)
      #sumy = sum(y)
      #nsumx2 = n * sum(x2)
      #nsumy2 = n * sum(y2)
      
      #numr = nsumxy - (sumx * sumy)
      #denr = sqrt(nsumx2 - (sumx^2)) * sqrt(nsumy2 - (sumy^2))
      
      #rsq = (numr/denr)^2
      
      r = cor(pred, testset$titre_log)
      rsq = r^2
      
      c(rsq, rmse)
    }
    
    c(mean(pp[,1]), mean(pp[,2]))
    
  }
  
  c(d, mean(p[,1]), mean(p[,2]))
}

stopCluster(cl)

colnames(crossval) <- c('df', 'cv_rsq', 'cv_RMSE')

rslts <- merge(overall, 
               crossval,
               by = "df",
               all.x = TRUE)


ggplot(rslts,
       aes(x = df)) +
  geom_line(aes(y = rsq), color = "red") +
  geom_line(aes(y = cv_rsq), color = "darkred")

ggplot(rslts,
       aes(x = df)) +
  geom_line(aes(y = RMSE), color = "blue") +
  geom_line(aes(y = cv_RMSE), color = "darkblue")




mod <- lm(titre_log ~ 
            bs(yield, df = 4),
          data = data_cortitre_multi) 

df <- data.frame(yield = 1:99)

df$pred <- predict(mod,
                   newdata = df)

ggplot(df,
       aes(x = yield,
           y = pred)) + 
  geom_point() +
  labs(title = "B-Splines Yield Multiparous")

#####AGE#####

######MULTIPAROUS######

overall <- foreach(d = c(3:25), .combine = "rbind") %do% {
  
  mod <- lm(titre_log ~ 
              bs(age, df = d),
            data = data_cortitre_multi)
  
  pred <- predict(mod,
                  newdata = data_cortitre_multi)
  
  e <- data_cortitre_multi$titre_log - pred
  se = e^2
  mse = mean(se)
  rmse = sqrt(mse)
  
  c(d, summary(mod)$r.squared, rmse)
  
}

colnames(overall) <- c('df', 'rsq', 'RMSE')

regcores()

crossval <- foreach(d = c(3:25), .combine = "rbind", .packages = c("foreach", "splines", "caret")) %dopar% {
  p <- foreach(i = 1:10, .combine = "rbind") %do% {
    set.seed(as.integer(rnorm(1,1000,100)))
    folds <- createFolds(1:nrow(data_cortitre_multi),10)
    pp <- foreach(j = 1:10, .combine = "rbind") %do% {
      trainset <- data_cortitre_multi[-folds[[j]],]
      testset <- data_cortitre_multi[folds[[j]],]
      mod <- lm(titre_log ~ 
                  bs(age, df = d),
                data = trainset)
      
      pred <- predict(mod,
                      newdata = testset,
                      type = "response")
      
      e <- testset$titre_log - pred
      se = e^2
      mse = mean(se)
      rmse = sqrt(mse)
      
      #x = testset$dim
      #x2 = x^2
      #y = testset$titre_boxcox
      #y2 = y^2
      #xy = x*y
      #n = nrow(testset)
      
      #nsumxy = n * sum(xy)
      #sumx = sum(x)
      #sumy = sum(y)
      #nsumx2 = n * sum(x2)
      #nsumy2 = n * sum(y2)
      
      #numr = nsumxy - (sumx * sumy)
      #denr = sqrt(nsumx2 - (sumx^2)) * sqrt(nsumy2 - (sumy^2))
      
      #rsq = (numr/denr)^2
      
      r = cor(pred, testset$titre_log)
      rsq = r^2
      
      c(rsq, rmse)
    }
    
    c(mean(pp[,1]), mean(pp[,2]))
    
  }
  
  c(d, mean(p[,1]), mean(p[,2]))
}

stopCluster(cl)

colnames(crossval) <- c('df', 'cv_rsq', 'cv_RMSE')

rslts <- merge(overall, 
               crossval,
               by = "df",
               all.x = TRUE)


ggplot(rslts,
       aes(x = df)) +
  geom_line(aes(y = rsq), color = "red") +
  geom_line(aes(y = cv_rsq), color = "darkred")

ggplot(rslts,
       aes(x = df)) +
  geom_line(aes(y = RMSE), color = "blue") +
  geom_line(aes(y = cv_RMSE), color = "darkblue")




mod <- lm(titre_log ~ 
            bs(age, df = 3),
          data = data_cortitre_multi) 

df <- data.frame(age = 22:198)

df$pred <- predict(mod,
                   newdata = df)

ggplot(df,
       aes(x = age,
           y = pred)) + 
  geom_point() +
  labs(title = "B-Splines age Multiparous")




####SPLINES SUMMARY####

#Primiparous DIM DF = 12
#Multiparous DIM DF = 12
#Multiparous Yield DF = 4
#Multiparous Age DF = 3

####UNIVARIABLE MODELS####

#####PRIMIPAROUS#####

######AGE#######

mod <- lmer(titre_log ~
             age +
              (1 | Farm) +
              (1 | calfeartag),
            data = data_cortitre_primi)


######DIM#######

mod <- lmer(titre_log ~
              bs(dim, df = 12, degree = 3) +
              (1 | Farm) +
              (1 | calfeartag),
            data = data_cortitre_primi)

######YIELD######

mod <- lmer(titre_log ~
               yield +
               (1 | Farm) +
               (1 | calfeartag),
             data = data_cortitre_primi)

######CELLCOUNT######

mod <- lmer(titre_log ~
              cellcount_log +
              (1 | Farm) +
              (1 | calfeartag),
            data = data_cortitre_primi)

######PROTEIN######

mod <- lmer(titre_log ~
              protein +
              (1 | Farm) +
              (1 | calfeartag),
            data = data_cortitre_primi)

######BUTTERFAT######

mod <- lmer(titre_log ~
              butterfat +
              (1 | Farm) +
              (1 | calfeartag),
            data = data_cortitre_primi)




#####MULTIPAROUS#####

######AGE######

mod <- lmer(titre_log ~
              bs(age, df = 3, degree = 3) +
              (1 | Farm) +
              (1 | calfeartag),
            data = data_cortitre_multi)

######DIM######

mod <- lmer(titre_log ~
              bs(dim, df = 12, degree = 3) +
              (1 | Farm) +
              (1 | calfeartag),
            data = data_cortitre_multi)

######YIELD######

mod <- lmer(titre_log ~
             bs(yield, df = 4, degree = 3) +
              (1 | Farm) +
              (1 | calfeartag),
            data = data_cortitre_multi)

######CELLCOUNT######

mod <- lmer(titre_log ~
              cellcount_log +
              (1 | Farm) +
              (1 | calfeartag),
            data = data_cortitre_multi)

######PROTEIN######

mod <- lmer(titre_log ~
              protein +
              (1 | Farm) +
              (1 | calfeartag),
            data = data_cortitre_multi)

######BUTTERFAT######

mod <- lmer(titre_log ~
              butterfat +
              (1 | Farm) +
              (1 | calfeartag),
            data = data_cortitre_multi)



####MULTIVARIABLE MODELS####

#####PRIMIPAROUS#####

primi_mod1 <- lmer(titre_log ~
                     bs(dim, df = 12, degree = 3) +
                     yield +
                     age +
                     cellcount_log +
                     protein +
                     butterfat +
                     (1 | Farm) +
                     (1 | calfeartag),
                   data = data_cortitre_primi)

primi_mod2 <- lmer(titre_log ~
       bs(dim, df = 12, degree = 3) +
       yield *
       cellcount_log +
       age +
       protein +
       butterfat +
       (1 | Farm) +
       (1 | calfeartag),
     data = data_cortitre_primi)

plot(ggpredict(primi_mod2, terms = c("cellcount_log", "yield")))

#Interesting yield*cellcount interaction

primi_mod3 <- lmer(titre_log ~
                     bs(dim, df = 12, degree = 3) +
                     yield *
                     protein +
                     cellcount_log +
                     age +
                     butterfat +
                     (1 | Farm) +
                     (1 | calfeartag),
                   data = data_cortitre_primi)

plot(ggpredict(primi_mod3, terms = c("protein", "yield")))

#Yield * protein interaction is negligible.

primi_mod4 <- lmer(titre_log ~
                     bs(dim, df = 12, degree = 3) +
                     yield +
                     protein *
                     cellcount_log +
                     age +
                     butterfat +
                     (1 | Farm) +
                     (1 | calfeartag),
                   data = data_cortitre_primi)

plot(ggpredict(primi_mod4, terms = c("protein", "cellcount_log")))

primi_mod5 <-  lmer(titre_log ~
                      bs(dim, df = 12, degree = 3) +
                      yield *
                      cellcount_log +
                      protein *
                      cellcount_log +
                      age +
                      butterfat +
                      (1 | Farm) +
                      (1 | calfeartag),
                    data = data_cortitre_primi)

plot(ggpredict(primi_mod5, terms = c('yield', 'protein', 'cellcount_log')))

#STICK WITH PRIMI_MOD5: Interactions between yield and cellcount and protein and cellcount


#####MULTIPAROUS#####

multi_mod1 <- lmer(titre_log ~
                     bs(dim, df = 12, degree = 3) +
                     bs(yield, df = 4, degree = 3) +
                     bs(age, df = 3, degree = 3) +
                     cellcount_log +
                     protein +
                     butterfat +
                     (1 | Farm) +
                     (1 | calfeartag),
                   data = data_cortitre_multi)

#Butterfat not significant

multi_mod2 <- lmer(titre_log ~
                     bs(dim, df = 12, degree = 3) +
                     bs(yield, df = 4, degree = 3) +
                     bs(age, df = 3, degree = 3) +
                     cellcount_log +
                     protein +
                     (1 | Farm) +
                     (1 | calfeartag),
                   data = data_cortitre_multi)

#Interactions:

multi_mod3 <- lmer(titre_log ~
                     bs(dim, df = 12, degree = 3) +
                     bs(yield, df = 4, degree = 3) +
                     bs(age, df = 3, degree = 3) +
                     cellcount_log *
                     protein +
                     (1 | Farm) +
                     (1 | calfeartag),
                   data = data_cortitre_multi)


plot(ggpredict(multi_mod3, terms = c("cellcount_log", "protein")))

#Keep Multi_mod3: Interesting interaction between cell count and protein

#####CHECK NORMALITY OF RANDOM EFFECTS#####

re <- ranef(primi_mod5)

hist(re$calfeartag$`(Intercept)`)
hist(re$Farm$`(Intercept)`)


re <- ranef(multi_mod3)

hist(re$calfeartag$`(Intercept)`)
hist(re$Farm$`(Intercept)`)

####CONDITIONAL EFFECTS####

#####PRIMIPAROUS MODEL#####


plot(ggpredict(primi_mod5))

plot(ggpredict(primi_mod5, terms = c('protein', 'cellcount_log')))
plot(ggpredict(primi_mod5, terms = c('yield', 'cellcount_log')))



#####MULTIPAROUS MODEL#####

plot(ggpredict(multi_mod3))


plot(ggpredict(multi_mod3, terms = c('protein', 'cellcount_log')))


#####PLOTS#####

ce_primi <- ggpredict(primi_mod5, terms = c("butterfat"))
ce_primi$predicted_exp <- exp(ce_primi$predicted)

ce_multi <- ggpredict(multi_mod3, terms = "butterfat")

ce_multi$predicted_exp <- exp(ce_multi$predicted)

ggplot() +
  geom_point(data = ce_primi,
             aes(x = x,
                 y = predicted_exp)) +
  geom_line(data = ce_primi,
            aes(x = x,
                y = predicted_exp)) +
  geom_point(data = ce_multi,
             aes(x = x,
                 y = predicted_exp), shape = 2) +
  geom_line(data = ce_multi,
            aes(x = x,
                y = predicted_exp)) +
  labs(x = "Butterfat (%)", y = "Predicted Titre") 


ggplot() +
  geom_point(data = ce_primi,
             aes(x = x,
                 y = predicted_exp)) +
  geom_line(data = ce_primi,
            aes(x = x,
                y = predicted_exp)) +
  
  labs(x = "Butterfat (%)", y = "Predicted Titre") 

####GET CORRECTED TITRES ON MODELLING DATA####

#####PRIMIPAROUS#####

data_cortitre_primi$titre_pred <- predict(primi_mod5,
                                          newdata = data_cortitre_primi,
                                          re.form = ~ 0)

data_cortitre_primi$unexp_titre_comp <- data_cortitre_primi$titre -
  data_cortitre_primi$titre_pred

median_primi_cow <- data.frame(age = median(data_cortitre_primi$age),
                               dim = median(data_cortitre_primi$dim),
                               yield = median(data_cortitre_primi$yield),
                               cellcount_log = median(data_cortitre_primi$cellcount_log),
                               protein = median(data_cortitre_primi$protein),
                               butterfat = median(data_cortitre_primi$butterfat))

titre_median_primi <- predict(primi_mod5,
                              newdata = median_primi_cow,
                              re.form = ~ 0)

data_cortitre_primi$titre_corrected <- data_cortitre_primi$unexp_titre_comp + titre_median_primi


ggplot(data_cortitre_primi,
       aes(x = titre,
           y = titre_corrected)) +
  geom_point() +
  labs(title = "Primiparous Corrected Titres")

ggplot(data_cortitre_primi[data_cortitre_primi$titre <= 20,],
       aes(x = titre,
           y = titre_corrected)) +
  geom_point() +
  labs(title = "Primiparous Corrected Titres")

ggplot(data_cortitre_primi,
       aes(x = unexp_titre_comp)) +
  geom_histogram(bins = 100) +
  labs(x = "Residual Corrected Titre",
       title = "Primiparous")


#####MULTIPAROUS#####

data_cortitre_multi$titre_pred <- predict(multi_mod3,
                                          newdata = data_cortitre_multi,
                                          re.form = ~ 0)

data_cortitre_multi$unexp_titre_comp <- data_cortitre_multi$titre -
  data_cortitre_multi$titre_pred

median_multi_cow <- data.frame(age = median(data_cortitre_multi$age), 
                               dim = median(data_cortitre_multi$dim),
                               yield = median(data_cortitre_multi$yield),
                               cellcount_log = median(data_cortitre_multi$cellcount_log),
                               protein = median(data_cortitre_multi$protein),
                               butterfat = median(data_cortitre_multi$butterfat))

titre_median_multi <- predict(multi_mod3,
                              newdata = median_multi_cow,
                              re.form = ~ 0)

data_cortitre_multi$titre_corrected <- data_cortitre_multi$unexp_titre_comp + titre_median_multi


ggplot(data_cortitre_multi,
       aes(x = titre,
           y = titre_corrected)) +
  geom_point() +
  labs(title = "Multiparous Corrected Titres")


ggplot(data_cortitre_multi[data_cortitre_multi$titre <= 20,],
       aes(x = titre,
           y = titre_corrected)) +
  geom_point() +
  labs(title = "Multiparous Corrected Titres")

ggplot(data_cortitre_multi,
       aes(x = unexp_titre_comp)) +
  geom_histogram(bins = 100) +
  labs(x = "Residual Corrected Titre",
       title = "Multiparous")

####MODEL CROSS VALIDATION vs APPARENT PERFORMANCE####

#####PRIMIPAROUS#####

######APPARENT ERRORS######

data_cortitre_primi$pred <- predict(primi_mod5,
                                    newdata = data_cortitre_primi,
                                    re.form = ~ 0)

rsq_primi_app <- (cor(data_cortitre_primi$pred,
               data_cortitre_primi$titre_log))^2

rmse_primi_app <- sqrt(mean((data_cortitre_primi$titre_log -
  data_cortitre_primi$pred)^2))

mae_primi_app <- mean(data_cortitre_primi$titre_log -
  data_cortitre_primi$pred)

######CV ERRORS######

regcores(-1)

cv_primi <- foreach(j = 1:10, .packages = c("lme4", "foreach", "caret", "splines"), .combine = "rbind") %dopar% {

  set.seed(as.integer(rnorm(1,1000,1000)))
  
  folds <- groupKFold(data_cortitre_primi$Farm, k = 10)
  
  p <- foreach(k = 1:length(folds), .combine = "rbind") %do% {
  
    trainset <- data_cortitre_primi[folds[[k]],]
    testset <- data_cortitre_primi[-folds[[k]],]
    
    mod <- lmer(titre_log ~ 
                  bs(dim, df = 12, degree = 3) + 
                  yield *
                  cellcount_log + 
                  protein * cellcount_log + 
                  age + 
                  butterfat + 
                  (1 | Farm) + 
                  (1 | calfeartag),
                data = trainset)
    
    testset$pred <- predict(mod,
                            newdata = testset,
                            re.form = ~ 0)
    
    testset$resid <- testset$titre_log -
      testset$pred
  
    r <- cor(testset$pred,
              testset$titre_log)
    rsq <- r^2
    
    se <- testset$resid^2
    mse <- mean(se)
    rmse <- sqrt(mse)
    
    mae <- mean(testset$resid)
    
    c(j, k, rsq, rmse, mae)
    
  }
  
  p
  
}

stopCluster(cl)

colnames(cv_primi) <- c('rep', 'fold', 'rsq', 'rmse', 'mae')

rsq_primi_cv <- mean(cv_primi[,3])
rmse_primi_cv <- mean(cv_primi[,4])
mae_primi_cv <- mean(cv_primi[,5])

#####MULTIPAROUS#####

######APPARENT ERRORS######

data_cortitre_multi$pred <- predict(multi_mod3,
                                    newdata = data_cortitre_multi,
                                    re.form = ~ 0)

rsq_multi_app <- (cor(data_cortitre_multi$pred,
                      data_cortitre_multi$titre_log))^2

rmse_multi_app <- sqrt(mean((data_cortitre_multi$titre_log -
                               data_cortitre_multi$pred)^2))

mae_multi_app <- mean(data_cortitre_multi$titre_log -
                        data_cortitre_multi$pred)

######CV ERRORS######

regcores(-1)

cv_multi <- foreach(j = 1:10, .packages = c("lme4", "foreach", "caret", "splines"), .combine = "rbind") %dopar% {
  
  set.seed(as.integer(rnorm(1,1000,1000)))
  
  folds <- groupKFold(data_cortitre_multi$Farm, k = 10)
  
  p <- foreach(k = 1:length(folds), .combine = "rbind") %do% {
    
    trainset <- data_cortitre_multi[folds[[k]],]
    testset <- data_cortitre_multi[-folds[[k]],]
    
    mod <- lmer(titre_log ~ bs(dim, df = 12, degree = 3) + 
                  bs(yield, df = 4, degree = 3) + 
                  bs(age, df = 3, degree = 3) + 
                  cellcount_log * 
                  protein + 
                  (1 | Farm) + 
                  (1 | calfeartag),
                data = trainset)
    
    testset$pred <- predict(mod,
                            newdata = testset,
                            re.form = ~ 0)
    
    testset$resid <- testset$titre_log -
      testset$pred
    
    r <- cor(testset$pred,
             testset$titre_log)
    rsq <- r^2
    
    se <- testset$resid^2
    mse <- mean(se)
    rmse <- sqrt(mse)
    
    mae <- mean(testset$resid)
    
    c(j, k, rsq, rmse, mae)
    
  }
  
  p
  
}

stopCluster(cl)

colnames(cv_multi) <- c('rep', 'fold', 'rsq', 'rmse', 'mae')

rsq_multi_cv <- mean(cv_multi[,3])
rmse_multi_cv <- mean(cv_multi[,4])
mae_multi_cv <- mean(cv_multi[,5])

#####COMPARISON#####

modperftab <- matrix(c(rsq_primi_app,
                    rmse_primi_app,
                    mae_primi_app,
                    rsq_primi_cv,
                    rmse_primi_cv,
                    mae_primi_cv,
                    rsq_multi_app,
                    rmse_multi_app,
                    mae_multi_app,
                    rsq_multi_cv,
                    rmse_multi_cv,
                    mae_multi_cv),
                    ncol = 4)

colnames(modperftab) <- c('Primiparous_Apparent',
                          'Primiparous_CV',
                          'Multiparous_Apparent',
                          'Multiparous_CV')

rownames(modperftab) <- c('RSq',
                          'RMSE',
                          'MAE')

modperftab

####PLOT RESIDUALS####

#####PRIMIPAROUS#####

ggplot(data_cortitre_primi,
       aes(x = pred,
           y = titre_log)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Primiparous",
       x = 'Predicted Log Titre',
       y = 'Observed Log Titre')

data_cortitre_primi$resid <- data_cortitre_primi$titre_log -
  data_cortitre_primi$pred

ggplot(data_cortitre_primi,
       aes(x = pred,
           y = resid)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Primiparous",
       x = 'Predicted Log Titre',
       y = 'Residual Log Titre')


#####MULTIPAROUS#####

ggplot(data_cortitre_multi,
       aes(x = pred,
           y = titre_log)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Multiparous",
       x = 'Predicted Log Titre',
       y = 'Observed Log Titre')

data_cortitre_multi$resid <- data_cortitre_multi$titre_log -
  data_cortitre_multi$pred

ggplot(data_cortitre_multi,
       aes(x = pred,
           y = resid)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Multiparous",
       x = 'Predicted Log Titre',
       y = 'Residual Log Titre')

####EXTREMENESS OF CORRECTED TITRES####

#####PRIMIPAROUS MODEL#####

regcores()

data_cortitre_primi$titre_ext <- foreach(r = 1:nrow(data_cortitre_primi), .combine = "c") %dopar% {
  nrow(data_cortitre_primi[data_cortitre_primi$titre_corrected < 
                             data_cortitre_primi$titre_corrected[r],]) /
    nrow(data_cortitre_primi)
} 

stopCluster(cl)

ggplot(data_cortitre_primi,
       aes(x = titre_corrected,
           y = titre_ext)) +
  geom_point() +
  labs(title = "Cumulative Distribution",
       subtitle = "Primiparous Model",
       x = "Corrected Titre",
       y = "Percentile")

#####MULTIPAROUS MODEL#####

regcores()

data_cortitre_multi$titre_ext <- foreach(r = 1:nrow(data_cortitre_multi), .combine = "c") %dopar% {
  nrow(data_cortitre_multi[data_cortitre_multi$titre_corrected < 
                             data_cortitre_multi$titre_corrected[r],]) /
    nrow(data_cortitre_multi)
} 

stopCluster(cl)

ggplot(data_cortitre_multi,
       aes(x = titre_corrected,
           y = titre_ext)) +
  geom_point() +
  labs(title = "Cumulative Distribution",
       subtitle = "Multiparous Model",
       x = "Corrected Titre",
       y = "Percentile")


####PREDICT NEXT CORRECTED TITRE####

data_brms <- read.csv("Y:/Ian/JohnesThresholds/JohnesProper/Data/data_birthpriors.csv")


data_brms$testnum <- substr(data_brms$covsandtitre, 13, 15)



#####ELIMINATE COWS WITH BAD DATA#####

data_brms$goodrows <- ifelse(data_brms$age >= 18 &
                                   !is.na(data_brms$age) &
                                   !is.na(data_brms$yield) &
                                   data_brms$yield > 0 &
                                   !is.na(data_brms$cellcount) &
                                   data_brms$cellcount > 0 &
                                   !is.na(data_brms$protein) &
                                   data_brms$protein > 0 &
                                   data_brms$protein <= 7.5 &
                                   !is.na(data_brms$butterfat) &
                                   data_brms$butterfat > 0 &
                                   data_brms$dim > 0 &
                                   !is.na(data_brms$dim) &
                                   data_brms$dim <= 750 &
                                   !is.na(data_brms$titre) &
                                   data_brms$titre > 0,
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

colnum <- match('goodrows', colnames(data_brms))

data_brms <- data_brms[,-colnum]


#####CORRECT TITRES#####

data_brms_primi <- data_brms[data_brms$parity == 1,]
data_brms_multi <- data_brms[data_brms$parity != 1,]

data_brms_primi$cellcount_log <- log(data_brms_primi$cellcount)
data_brms_multi$cellcount_log <- log(data_brms_multi$cellcount)

data_brms_primi$titre_pred <- predict(primi_mod5,
                                      newdata = data_brms_primi,
                                      re.form = ~ 0)

data_brms_multi$titre_pred <- predict(primi_mod5,
                                      newdata = data_brms_multi,
                                      re.form = ~ 0)


data_brms_primi$unexp_titre <- data_brms_primi$titre - data_brms_primi$titre_pred
data_brms_multi$unexp_titre <- data_brms_multi$titre - data_brms_multi$titre_pred

primimedcow <- data.frame(age = median(data_brms_primi$age),
                 yield = median(data_brms_primi$yield),
                 cellcount_log = median(data_brms_primi$cellcount_log),
                 dim = median(data_brms_primi$dim),
                 protein = median(data_brms_primi$protein),
                 butterfat = median(data_brms_primi$butterfat))

primimedcowtitre = predict(primi_mod5, newdata = primimedcow, re.form = ~0)

multimedcow <- data.frame(age = median(data_brms_multi$age),
                          yield = median(data_brms_multi$yield),
                          cellcount_log = median(data_brms_multi$cellcount_log),
                          dim = median(data_brms_multi$dim),
                          protein = median(data_brms_multi$protein),
                          butterfat = median(data_brms_multi$butterfat))

multimedcowtitre = predict(multi_mod3, newdata = multimedcow, re.form = ~0)

data_brms_primi$titre_cor <- data_brms_primi$unexp_titre + primimedcowtitre
data_brms_multi$titre_cor <- data_brms_multi$unexp_titre + multimedcowtitre

ggplot(data_brms_multi,
       aes(x = titre,
           y = titre_cor)) +
  geom_point()

data_brms <- rbind(data_brms_primi,
                   data_brms_multi)

data_brms <- data_brms[with(data_brms, order(Farm, calfeartag, date)),]

ggplot(data_brms,
       aes(x = titre,
       y = titre_cor)) +
  geom_point()

#####EXAMINE CORRECTED TITRES IN POSITIVE AND NEGATIVE COWS####

poscows <- data_brms[data_brms$Target_QMMS == "1" &
                       data_brms$ageatfirstH != 0 &
                       (data_brms$ageatfirstH - data_brms$age) >= 0.5,]

poscows$target <- "1"

negcows <- data_brms[data_brms$ageatfirstH == 0 &
                       data_brms$ageatfirstM == 0,]
negcows$target <- "0"


ggplot() +
  geom_density(data = negcows,
               aes(x = titre_cor),
               color = "green") +
  geom_density(data = poscows,
               aes(x = titre_cor),
               color = "red")


#####CREATE DATASET#####

data_brms_slct <- data_brms[,c("Farm",
                               "calfeartag",
                               "ageatfirstH",
                               "ageatfirstposstatus",
                               "testnum",
                               "ntests",
                               "date",
                               "age",
                               "parity",
                               "titre_cor")
                               ,]



data_brms_slct$monthssincelast <- NA

regcores()

data_brms_slct$monthssincelast[2:nrow(data_brms_slct)] <- foreach(r = 2:nrow(data_brms_slct), .combine = "c") %dopar% {
  ifelse(data_brms_slct$Farm[r] == data_brms_slct$Farm[r-1] &
           data_brms_slct$calfeartag[r] == data_brms_slct$calfeartag[r-1],
         data_brms_slct$age[r] - data_brms_slct$age[r-1],
         NA)
}

stopCluster(cl)

regcores()

p <- foreach(r = 1:nrow(data_brms_slct), .combine = "rbind", .packages = "foreach") %dopar% {
  
  foreach(m = 1:5) %do% {
    assign(paste0('datem',m),
           ifelse(as.numeric(data_brms_slct$testnum[r]) > m,
                  data_brms_slct$date[r-m],
                  NA))
    
    assign(paste0('agem',m),
           ifelse(as.numeric(data_brms_slct$testnum[r]) > m,
                  data_brms_slct$age[r-m],
                  NA))
    
    assign(paste0('paritym',m),
           ifelse(as.numeric(data_brms_slct$testnum[r]) > m,
                  data_brms_slct$parity[r-m],
                  NA))
    
    assign(paste0('monthssincelastm',m),
           ifelse(as.numeric(data_brms_slct$testnum[r]) > m,
                  data_brms_slct$monthssincelast[r-m],
                  NA))
    
    assign(paste0('titre_corm',m),
           ifelse(as.numeric(data_brms_slct$testnum[r]) > m,
                  data_brms_slct$titre_cor[r-m],
                  NA))
  }
  
  c(data_brms_slct$Farm[r],
    data_brms_slct$calfeartag[r],
    data_brms_slct$testnum[r],
    data_brms_slct$date[r],
    data_brms_slct$age[r],
    data_brms_slct$parity[r],
    data_brms_slct$monthssincelast[r],
    data_brms_slct$titre_cor[r],
    datem1,
    agem1,
    paritym1,
    monthssincelastm1,
    titre_corm1,
    datem2,
    agem2,
    paritym2,
    monthssincelastm2,
    titre_corm2,
    datem3,
    agem3,
    paritym3,
    monthssincelastm3,
    titre_corm3,
    datem4,
    agem4,
    paritym4,
    monthssincelastm4,
    titre_corm4,
    datem5,
    agem5,
    paritym5,
    monthssincelastm5,
    titre_corm5)
}

stopCluster(cl)

data_brms_slct_wide <- as.data.frame(p)

colnames(data_brms_slct_wide) <- c('Farm',
                                   'calfeartag',
                                   'testnum',
                                   'date',
                                   'age',
                                   'parity',
                                   'monthsincelast',
                                   'titre_cor',
                                   'datem1',
                                   'agem1',
                                   'paritym1',
                                   'monthsincelastm1',
                                   'titre_corm1',
                                   'datem2',
                                   'agem2',
                                   'paritym2',
                                   'monthsincelastm2',
                                   'titre_corm2',
                                   'datem3',
                                   'agem3',
                                   'paritym3',
                                   'monthsincelastm3',
                                   'titre_corm3',
                                   'datem4',
                                   'agem4',
                                   'paritym4',
                                   'monthsincelastm4',
                                   'titre_corm4',
                                   'datem5',
                                   'agem5',
                                   'paritym5',
                                   'monthsincelastm5',
                                   'titre_corm5')



data_brms_slct_wide[,c(5:8,10:13,15:18,20:23,25:28,30:33)] <-
  lapply(data_brms_slct_wide[,c(5:8,10:13,15:18,20:23,25:28,30:33)],
         as.numeric)

data_brms_slct_wide$monthsagom1 <- data_brms_slct_wide$monthsincelast
data_brms_slct_wide$monthsagom2 <- data_brms_slct_wide$monthsincelast + data_brms_slct_wide$monthsincelastm1
data_brms_slct_wide$monthsagom3 <- data_brms_slct_wide$monthsincelast + data_brms_slct_wide$monthsincelastm1 + data_brms_slct_wide$monthsincelastm2
data_brms_slct_wide$monthsagom4 <- data_brms_slct_wide$monthsincelast + data_brms_slct_wide$monthsincelastm1 + data_brms_slct_wide$monthsincelastm2 + data_brms_slct_wide$monthsincelastm3
data_brms_slct_wide$monthsagom5 <- data_brms_slct_wide$monthsincelast + data_brms_slct_wide$monthsincelastm1 + data_brms_slct_wide$monthsincelastm2 + data_brms_slct_wide$monthsincelastm3 + data_brms_slct_wide$monthsincelastm4

data_brms_slct_wide <- data_brms_slct_wide[,c("Farm",
                                              "calfeartag",
                                              "testnum",
                                              "date",
                                              "age",
                                              "parity",
                                              "monthsincelast",
                                              "titre_cor",
                                              "datem1",
                                              "monthsagom1",
                                              "agem1",
                                              "paritym1",
                                              "monthsincelastm1",
                                              "titre_corm1",
                                              "datem2",
                                              "monthsagom2",
                                              "agem2",
                                              "paritym2",
                                              "monthsincelastm2",
                                              "titre_corm2",
                                              "datem3",
                                              "monthsagom3",
                                              "agem3",
                                              "paritym3",
                                              "monthsincelastm3",
                                              "titre_corm3",
                                              "datem4",
                                              "monthsagom4",
                                              "agem4",
                                              "paritym4",
                                              "monthsincelastm4",
                                              "titre_corm4",
                                              "datem5",
                                              "monthsagom5",
                                              "agem5",
                                              "paritym5",
                                              "monthsincelastm5",
                                              "titre_corm5"
)]

colnum <- match('monthsincelastm5', colnames(data_brms_slct_wide))

data_brms_slct_wide_complete <- data_brms_slct_wide[complete.cases(data_brms_slct_wide[,-colnum]) == TRUE,]

#####CHECK FOR DATA PROBLEMS#####

nzv <- nearZeroVar(data_brms_slct_wide_complete, saveMetrics= TRUE)
nzv[nzv$nzv,][1:10,]

M <- cor(na.omit(data_brms_slct_wide_complete[,c(5:8,10:14,16:20,22:26,28:32,34:38)]))

corrplot(M)

tempdata <- data_brms_slct_wide_complete[,c(5:8,10:14,16:20,22:26,28:32,34:38)]

comboInfo <- findLinearCombos(na.omit(tempdata))
comboInfo

rm(tempdata)

#Some linear combos in data: e.g. combination of age this test, age last test and months since last test!

#####A BIT OF FEATURE ENGINEERING#####

colnum1 <- match('titre_corm1', colnames(data_brms_slct_wide_complete))
colnum2 <- match('titre_corm2', colnames(data_brms_slct_wide_complete))
colnum3 <- match('titre_corm3', colnames(data_brms_slct_wide_complete))
colnum4 <- match('titre_corm4', colnames(data_brms_slct_wide_complete))
colnum5 <- match('titre_corm5', colnames(data_brms_slct_wide_complete))

data_brms_slct_wide_complete$meantitre_corm <- rowMeans(data_brms_slct_wide_complete[,c(colnum1,
                                                                                        colnum2,
                                                                                        colnum3,
                                                                                        colnum4,
                                                                                        colnum5)])

data_brms_slct_wide_complete$ratio_latesttwo <- data_brms_slct_wide_complete$titre_corm1 /
  data_brms_slct_wide_complete$titre_corm2

for(r in 1:nrow(data_brms_slct_wide_complete)){
  data_brms_slct_wide_complete$sdtitre_corm[r] <- sd(data_brms_slct_wide_complete[r,c('titre_corm1', 
                                                                                     'titre_corm2', 
                                                                                     'titre_corm3',
                                                                                     'titre_corm4',
                                                                                     'titre_corm5')])
}

#####FIT MODELS#####

######SIMPLE LINEAR REGRESSION######

linmod = lm(titre_cor ~
              age +
              monthsagom1 + 
              titre_corm1 +
              monthsagom1 + 
              titre_corm2 +
              monthsagom1 + 
              titre_corm3 +
              monthsagom1 + 
              titre_corm4 +
              monthsagom1 +
              titre_corm5 +
              ratio_latesttwo +
              mean_titrecorm,
            data = data_brms_slct_wide_complete)

#CROSS VALIDATED PERFORMANCE

regcores(-1)

crossval <- foreach(i = 1:10, .combine = "rbind", .packages = c('foreach', 'caret')) %dopar% {
    set.seed(as.integer(rnorm(1,1000,100)))
    folds <- createFolds(1:nrow(data_brms_slct_wide_complete),10)
    pp <- foreach(j = 1:10, .combine = "rbind") %do% {
      trainset <- data_brms_slct_wide_complete[-folds[[j]],]
      testset <- data_brms_slct_wide_complete[folds[[j]],]
      mod <- lm(titre_cor ~
                  age +
                  monthsagom1 * titre_corm1 +
                  monthsagom1 * titre_corm2 +
                  monthsagom1 * titre_corm3 +
                  monthsagom1 * titre_corm4 +
                  monthsagom1 * titre_corm5,
                data = trainset)
      
      pred <- predict(mod,
                      newdata = testset,
                      type = "response")
      
      e <- testset$titre_cor - pred
      se = e^2
      mse = mean(se)
      rmse = sqrt(mse)
      
      r = cor(pred, testset$titre_cor)
      rsq = r^2
      
      c(rsq, rmse)
  }
    
  c(mean(pp[,1]), mean(pp[,2]))
    
}

stopCluster(cl)

colnames(crossval) <- c('cv_rsq', 'cv_RMSE')

crossval

#APPARENT PERFORMANCE

pred = predict(linmod,
               newdata = data_brms_slct_wide_complete,
               type = 'response')

resid = data_brms_slct_wide_complete$titre_cor - pred

rmse = sqrt(mean(resid^2))



fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10)




model.formula <- formula(titre_cor ~
                           age +
                           monthsagom1 +
                           titre_corm1 +
                           titre_corm2 +
                           titre_corm3 +
                           titre_corm4 +
                           titre_corm5 +
                           monthsagom1 +
                           monthsagom5 +
                           ratio_latesttwo +
                           meantitre_corm +
                           sdtitre_corm)




regcores(-1)

grid_enet <- expand.grid(fraction = c(0.8,1),
                         lambda = c(0, 0.0001, 0.0002, 0.0004, 0.0006))

mod_enet <- train(model.formula, 
                data = data_brms_slct_wide_complete, 
                 method = "enet", 
                 trControl = fitControl,
                #tuneGrid = grid_enet,
                preProc = c('center', 'scale'))

saveRDS(mod_enet, 'Y:/Ian/JohnesThresholds/JohnesProper/Data/PickledModels/MLPredictNextTitre/mod_enet.rds')




mod_mars <- train(model.formula, 
                  data = data_brms_slct_wide_complete, 
                  method = "earth", 
                  trControl = fitControl,
                  preProc = c('center', 'scale'))

mod_svm_lin <- train(model.formula, 
                  data = data_brms_slct_wide_complete, 
                  method = "svmLinear", 
                  trControl = fitControl,
                  preProc = c('center', 'scale'))

saveRDS(mod_svm_lin, 'Y:/Ian/JohnesThresholds/JohnesProper/Data/PickledModels/MLPredictNextTitre/mod_svm_lin.rds')

mod_svm_rad <- train(model.formula, 
                     data = data_brms_slct_wide_complete, 
                     method = "svmLinear", 
                     trControl = fitControl,
                     preProc = c('center', 'scale'))

grid_gbm <- expand.grid(n.trees = c(200,250),
                        interaction.depth = c(4,5),
                        shrinkage = c(0.02, 0.04, 0.05),
                        n.minobsinnode = c(15,20))

mod_gbm <- train(model.formula, 
                 data = data_brms_slct_wide_complete, 
                 method = "gbm", 
                 trControl = fitControl,
                 tuneGrid = grid_gbm,
                 preProc = c('center', 'scale'))

saveRDS(mod_gbm, 'Y:/Ian/JohnesThresholds/JohnesProper/Data/PickledModels/MLPredictNextTitre/mod_gbm.rds')

grid_rf <- expand.grid(min.node.size = c(3,5,7),
                       splitrule = 'extratrees',
                       mtry = c(4,6,8))

mod_rf <- train(model.formula, 
                           data = data_brms_slct_wide_complete, 
                           method = "ranger", 
                           trControl = fitControl,
                           tuneGrid = grid_rf,
                           preProc = c('center', 'scale'),
                           importance = 'impurity')

saveRDS(mod_rf, 'Y:/Ian/JohnesThresholds/JohnesProper/Data/PickledModels/MLPredictNextTitre/mod_rf.rds')

mod_nnet <- train(model.formula, 
                             data = data_brms_slct_wide_complete, 
                             method = "nnet", 
                             trControl = fitControl,
                             preProc = c('center', 'scale'),
                             linout = TRUE)

saveRDS(mod_nnet, 'Y:/Ian/JohnesThresholds/JohnesProper/Data/PickledModels/MLPredictNextTitre/mod_nnet.rds')

stopCluster(cl)

models_compare <- resamples(list(ENET = mod_enet,
                                 GBM = mod_gbm))
summary(models_compare)

bwplot(models_compare)

#####TUNE RANDOM FOREST#####

grid_rf <- expand.grid(mtry = c(2,4,6,8),
                       splitrule = "extratrees",
                       min.node.size = c(3,5,7,9))

regcores(-1)

perf <- foreach(r = 1:nrow(grid_rf), .combine = "rbind", .packages = "caret") %dopar% {
  mod <- train(model.formula,
               method = "ranger",
               data = data_brms_slct_wide_complete,
               tuneGrid = grid_rf[1,],
               trControl = trainControl(method='none'),
               preProc = c('center', 'scale'),
               importance = 'impurity')
  
  pred <- predict(mod,
                  newdata = data_brms_slct_wide_complete)
  
  resid = pred - data_brms_slct_wide_complete$titre_cor
  se = resid^2
  mse = mean(se)
  rmse = sqrt(mse)
  
  r2 = cor(pred,
           data_brms_slct_wide_complete$titre_cor)^2
  
  c(grid_rf[r,1],
    grid_rf[r,2],
    grid_rf[r,3],
    rmse, 
    r2)
  
  
}

stopCluster(cl)

perf <- as.data.frame(perf)

colnames(perf) <- c('mtry',
                    'splitrule',
                    'minnodesize',
                    'rmse',
                    'r2')

ggplot(perf,
       aes(x = mtry)) +
  geom_line(aes(y = rmse, color = as.factor(minnodesize))) +
  #geom_line(aes(y = rmse), color = "blue") +
  labs(title = "Apparent performance",
       x = "mtry",
       y = "RMSE")

plot(mod_rf)

######SIMPLIFIED RANDOM FOREST (NO AGES)######

regcores(-1)

mod_rf_simp1 <- train(titre_cor ~
                        titre_corm1 +
                        titre_corm2 +
                        titre_corm3 +
                        titre_corm4 +
                        titre_corm5,
                      method = "ranger",
                      data = data_brms_slct_wide_complete,
                      trControl = fitControl,
                      preProc = c('center', 'scale'),
                      importance = 'impurity')

stopCluster(cl)

#####PREDICT ON FULL DATASET#####

finmod <- linmod
  
data_brms_slct_wide_complete$pred <- predict(finmod,
                                             newdata = data_brms_slct_wide_complete)
                                             
                                             #,
                                             #newdata = data_brms_slct_wide_complete,
                                             #type = "prob")

data_brms_slct_wide_complete$resid <- data_brms_slct_wide_complete$titre_cor -
  data_brms_slct_wide_complete$pred


######APPARENT PERFORMANCE######

se = (data_brms_slct_wide_complete$resid)^2
mse = mean(se)
rmse = sqrt(mse)

r2 = cor(data_brms_slct_wide_complete$pred,
         data_brms_slct_wide_complete$titre_cor)^2

####CONDITIONAL EFFECTS####

ft = "age"

age_vec <- ifelse(ft == "age", seq(min(data_brms_slct_wide_complete$age), max(data_brms_slct_wide_complete$age), length.out = 500), median(data_brms_slct_wide_complete$age))
                        titre_corm1_vec = ifelse(ft == "titre_corm1", seq(min(data_brms_slct_wide_complete$titre_corm1), max(data_brms_slct_wide_complete$titre_corm1), length.out = 500), median(data_brms_slct_wide_complete$titre_corm1))
                        titre_corm2_vec = ifelse(ft == "titre_corm2", seq(min(data_brms_slct_wide_complete$titre_corm2), max(data_brms_slct_wide_complete$titre_corm2), length.out = 500), median(data_brms_slct_wide_complete$titre_corm2))
                        titre_corm3_vec = ifelse(ft == "titre_corm3", seq(min(data_brms_slct_wide_complete$titre_corm3), max(data_brms_slct_wide_complete$titre_corm3), length.out = 500), median(data_brms_slct_wide_complete$titre_corm3))
                        titre_corm4_vec = ifelse(ft == "titre_corm4", seq(min(data_brms_slct_wide_complete$titre_corm4), max(data_brms_slct_wide_complete$titre_corm4), length.out = 500), median(data_brms_slct_wide_complete$titre_corm4))
                        titre_corm5_vec = ifelse(ft == "titre_corm5", seq(min(data_brms_slct_wide_complete$titre_corm5), max(data_brms_slct_wide_complete$titre_corm5), length.out = 500), median(data_brms_slct_wide_complete$titre_corm5))
                        monthsagom1_vec = ifelse(ft == "monthsagom1", seq(min(data_brms_slct_wide_complete$monthsagom1), max(data_brms_slct_wide_complete$monthsagom1), length.out = 500), median(data_brms_slct_wide_complete$monthsagom1))
                        monthsagom2_vec = ifelse(ft == "monthsagom2", seq(min(data_brms_slct_wide_complete$monthsagom2), max(data_brms_slct_wide_complete$monthsagom2), length.out = 500), median(data_brms_slct_wide_complete$monthsagom2))
                        monthsagom3_vec = ifelse(ft == "monthsagom3", seq(min(data_brms_slct_wide_complete$monthsagom3), max(data_brms_slct_wide_complete$monthsagom3), length.out = 500), median(data_brms_slct_wide_complete$monthsagom3))
                        monthsagom4_vec = ifelse(ft == "monthsagom4", seq(min(data_brms_slct_wide_complete$monthsagom4), max(data_brms_slct_wide_complete$monthsagom4), length.out = 500), median(data_brms_slct_wide_complete$monthsagom4))
                        monthsagom5_vec = ifelse(ft == "monthsagom5", seq(min(data_brms_slct_wide_complete$monthsagom5), max(data_brms_slct_wide_complete$monthsagom5), length.out = 500), median(data_brms_slct_wide_complete$monthsagom5))

data_sim <- data.frame(age = age_vec,
                       titre_corm1 = titre_corm1_vec,
                       titre_corm2 = titre_corm2_vec,
                       titre_corm3 = titre_corm3_vec,
                       titre_corm4 = titre_corm4_vec,
                       titre_corm5 = titre_corm5_vec,
                       monthsagom1 = monthsagom1_vec,
                       monthsagom2 = monthsagom2_vec,
                       monthsagom3 = monthsagom3_vec,
                       monthsagom4 = monthsagom4_vec,
                       monthsagom5 = monthsagom5_vec)                        
                        
data_sim$pred <- predict(mod_rf,
                         newdata = data_sim)


ggplot(data_sim,
       aes(x = get(ft),
           y = pred),
       geom_point) +
  labs(x = ft, y = "Predicted Titre")

####PLOT RESIDUALS####

ggplot(data_brms_slct_wide_complete,
       aes(x = titre_cor,
           y = pred)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Predicted",
       x = "Actual")

ggplot(data_brms_slct_wide_complete,
       aes(x = titre_cor,
           y = resid)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Corrected Titre",
       y = "Residual")



ggplot(data_brms_slct_wide_complete,
       aes(x = pred,
           y = resid)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Predicted",
       y = "Residual") +
  facet_wrap(~as.factor(parity))

ggplot(data_brms_slct_wide_complete,
       aes(x = resid)) +
  geom_histogram(bins = 100) +
  labs(x = "Predicted",
       y = "Residual") +
  facet_wrap(~as.factor(parity))

ggplot(data_brms_slct_wide_complete,
       aes(x = as.factor(parity),
           y = resid)) +
  geom_boxplot() +
  labs(x = "Parity",
       y = "Residual") 

ggplot(data_brms_slct_wide_complete,
       aes(x = as.factor(parity),
           y = resid)) +
  geom_violin() +
  labs(x = "Parity",
       y = "Residual") 

ggplot(data_brms_slct_wide_complete,
       aes(x = cut(age,10),
           y = resid)) +
  geom_violin() +
  labs(x = "Age",
       y = "Residual") 

ggplot(data_brms_slct_wide_complete,
       aes(x = age,
           y = resid,
           group = paste0(Farm, calfeartag))) +
  geom_line(alpha = 0.3)



samp <- sample(unique(data_brms_slct_wide_complete$calfeartag[data_brms_slct_wide_complete$testnum >= 11]), 200)


for(cow in samp){
  
  ggplot(data_brms_slct_wide_complete[data_brms_slct_wide_complete$calfeartag == cow,],
               aes(x = age)) +
          geom_point(aes(y = titre_cor), color = "black", size = 2) +
          #geom_line(aes(y = titre_cor), color = "black") +
          geom_point(aes(y = pred), color = "blue", size = 2) +
          #geom_line(aes(y = pred), color = "blue") +
          geom_point(aes(y = resid), color = "darkred", size = 3, shape = 2) +
          geom_text(aes(y = resid, label = round(resid,1), color = ifelse(resid >= 0, "darkred", "green")), hjust=-0.5, vjust=0) +
          geom_line(aes(y = resid), color = "darkred") +
          geom_hline(yintercept = 0, linetype = "dashed") +
          labs(title = cow,
               x = "Age (m)",
               y = "Titre") +
          theme(legend.position = "none")
  
  ggsave(paste0('y:/ian/johnesthresholds/johnesproper/data/residualclustering/cowplots/residuals/',cow,'.png'))
  
}

data_brms_slct_wide_complete$resid_cumul <- data_brms_slct_wide_complete$resid

for (r in 2:nrow(data_brms_slct_wide_complete)){

  data_brms_slct_wide_complete$resid_cumul[r] <- 
    ifelse(data_brms_slct_wide_complete$calfeartag[r-1] == data_brms_slct_wide_complete$calfeartag[r] &
             data_brms_slct_wide_complete$Farm[r-1] == data_brms_slct_wide_complete$Farm[r],
           data_brms_slct_wide_complete$resid_cumul[r-1] + data_brms_slct_wide_complete$resid[r],
                                        data_brms_slct_wide_complete$resid[r])

}

for(cow in samp){
  
  ggplot(data_brms_slct_wide_complete[data_brms_slct_wide_complete$calfeartag == cow,],
         aes(x = age)) +
    geom_point(aes(y = titre_cor), color = "black", size = 2) +
    #geom_line(aes(y = titre_cor), color = "black") +
    geom_point(aes(y = pred), color = "blue", size = 2) +
    #geom_line(aes(y = pred), color = "blue") +
    geom_point(aes(y = resid_cumul), color = "darkred", size = 3, shape = 2) +
    geom_text(aes(y = resid_cumul, label = round(resid_cumul,1), color = ifelse(resid_cumul >= 0, "darkred", "green")), hjust=-0.5, vjust=0) +
    geom_line(aes(y = resid_cumul), color = "darkred") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = cow,
         x = "Age (m)",
         y = "Titre") +
    theme(legend.position = "none")
  
  ggsave(paste0('y:/ian/johnesthresholds/johnesproper/data/residualclustering/cowplots/cumulativeresiduals/',cow,'.png'))
  
}

####CLUSTERING####


#####GET RESIDS INTO WIDE DATA#####

data_clust <- data_brms_slct_wide_complete[,c('Farm',
                                              'calfeartag',
                                              'testnum',
                                              'resid')]

data_clust_wide <- pivot_wider(data_clust,
                               id_cols = c(Farm, calfeartag),
                               names_from = testnum,
                               values_from = resid
                               )
#Limit to cows with at least six residuals


data_clust_wide <- data_clust_wide[!is.na(data_clust_wide$'11'),]

#Convert to list for DTW clustering

regcores(-1)

data_clust_list <- foreach(r = 1:nrow(data_clust_wide)) %dopar% {
  tempdata = unlist(data_clust_wide[r,3:31])
  tempdata <- tempdata[!is.na(tempdata)]
  tempdata
}

stopCluster(cl)

regcores(-1)

clust <- tsclust(data_clust_list, type="partitional", k=2L:10L, distance="dtw", centroid="pam")

stopCluster(cl)

regcores(-1)

cvis <- foreach(k = 1:length(clust), .combine = "rbind") %do% {

  c(k+1,cvi(clust[[k]], type = "internal"))
  
}

cvis <- as.data.frame(cvis)

colnames(cvis)[1] <- "k"

ggplot(cvis,
       aes(x = k)) +
  geom_line(aes(y = Sil)) +
  labs(title = "Sil: Maximise")

ggplot(cvis,
       aes(x = k)) +
  geom_line(aes(y = D)) +
  labs(title = "D: Maximise")

ggplot(cvis,
       aes(x = k)) +
  geom_line(aes(y = CH)) +
  labs(title = "CH: Maximise")
ggplot(cvis,
       aes(x = k)) +
  geom_line(aes(y = SF)) +
  labs(title = "SF: Maximise")

ggplot(cvis,
       aes(x = k)) +
  geom_line(aes(y = COP)) +
  labs(title = "COP: Minimise")

ggplot(cvis,
       aes(x = k)) +
  geom_line(aes(y = DB)) +
  labs(title = "DB: Minimise")
ggplot(cvis,
       aes(x = k)) +
  geom_line(aes(y = DBstar)) +
  labs(title = "DBstar: Minimise")

regcores(-1)

clust2k <- tsclust(data_clust_list, type="partitional", k=2, distance="dtw", centroid="pam")

clust3k <- tsclust(data_clust_list, type="partitional", k=3, distance="dtw", centroid="pam")

plot(clust3k, type = "sc")
