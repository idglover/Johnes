sig <- c(0,5,10,20,30,60,100,1000)

hightitredata <- data_brms[1,]

for (i in 2:length(sig)){
  temp <- data_brms[data_brms$titre >= sig[i-1] &
                      data_brms$titre < sig[i],]
  temp <- temp[sample(1:nrow(temp), 20),]
  
  hightitredata <- rbind(hightitredata, temp)
}

sr <- splitacrosscores(hightitredata, -1)

n_post_draws = 2000

nreps = 20

pctolerance = 20

regcores(-1)

p <- foreach(i = 1:length(sr), .packages = c("brms", "foreach"), .combine = "rbind") %dopar% {

  a <- ifelse(i == 1, 1, sr[i-1] + 1)
  b <- sr[i]
  tmp <- hightitredata[a:b,]
  
  pp <- foreach(r = 1:nrow(tmp), .combine = "rbind", .packages = c("brms", "foreach")) %do% {
    
    chunk <- tmp[r,]
  
    bf_vec <- NULL
    
    conv = FALSE
    
    tms = 1
    
    while(conv == FALSE) {
    
      post_draws_pos <- posterior_predict(brms_mod_weibull_pos_age_yield_scc_mtnc_dimind, 
                                          newdata = chunk,
                                          ndraws = n_post_draws)
      
      prob_at_titre_pos <- length(post_draws_pos[post_draws_pos >= (chunk[, 'titre'] - ivl) &
                                                   post_draws_pos < (chunk[,'titre'] + ivl)]) / length(post_draws_pos)
      
      post_draws_neg <- posterior_predict(brms_mod_lognorm_neg_yield_scc_mtnc_dimind,
                                          newdata = chunk,
                                          ndraws = n_post_draws)
      
      prob_at_titre_neg <- prob_at_titre_neg <- length(post_draws_neg[post_draws_neg >= (chunk[, 'titre'] - ivl) &
                                                                        post_draws_neg < (chunk[,'titre'] + ivl)]) / length(post_draws_neg)
      
      bayes_fac <- prob_at_titre_pos/prob_at_titre_neg
      
      tryCatch({write.table(r, paste0("C:/Users/Ian.glover.HEADOFFICE/Documents/PythonTools/ForEachRowCounter/rowsdonecore",i,".txt"), append = FALSE, sep = " ", dec = ".",
                            row.names = FALSE, col.names = FALSE)}, 
               error = function(cond){}) 
      
      if(bayes_fac != Inf & !is.na(bayes_fac) & bayes_fac != "NaN"){
             
            bf_vec <- rbind(bf_vec, c(chunk$calfeartag, chunk$testnum, chunk$titre, tms, bayes_fac))
            
      }
      
      tms = tms + 1
      if(!is.null(bf_vec)){
        if(nrow(bf_vec) >= nreps){conv = TRUE}
      }
      if(is.null(bf_vec) & tms == 100){conv = TRUE}
      
      if(!is.null(bf_vec)){
        if(nrow(bf_vec) < nreps & tms == 100){conv = TRUE}
      }
    }
      
    if(!is.null(bf_vec)){rtn = bf_vec}
    if(is.null(bf_vec)){rtn = c(chunk$calfeartag, chunk$testnum, chunk$titre, "FAILED")}
    
    rtn
  }
  
  pp
    
    
}

stopCluster(cl)




p <- as.data.frame(p)




colnames(p) <- c('eartag', 'testnum', 'titre', 'rep', 'bf')


p_nofails <- p[p$bf != "FAILED",]

uniquecows <- unique(p_nofails[,c('eartag', 'testnum')])



p_nofails$cum_med_bf <- foreach(c = 1:nrow(uniquecows), .combine = "c") %do% {
  
  tmp <- p_nofails[p_nofails$eartag == uniquecows$eartag[c] &
                     p_nofails$testnum == uniquecows$testnum[c],]
  
  pp <- foreach(r = 1:nrow(tmp), .combine = "c") %do% {
    median(as.numeric(tmp$bf[1:r]))
    
  }
  
  pp
  
}

p_nofails$pctchange <- 0

p_nofails$pctchange[2:nrow(p_nofails)] <- foreach(c = 2:nrow(p_nofails), .combine = "c") %do% {
  
  ifelse(p_nofails$eartag[c] != p_nofails$eartag[c-1] |
           p_nofails$testnum[c] != p_nofails$testnum[c-1],
         0,((p_nofails$cum_med_bf[c]/p_nofails$cum_med_bf[c-1]) - 1) * 100)
  
  
}

uniquecows$firstrepstable <- foreach(c = 1:nrow(uniquecows), .combine = "c") %do% {

  tmp <- p_nofails[p_nofails$eartag == uniquecows$eartag[c] &
                     p_nofails$testnum == uniquecows$testnum[c],]
  tmp$stable <- 0
  
  if(nrow(tmp) != 1){
  
    for(r in 2:nrow(tmp)){
      tmp$stable[r] <- ifelse(tmp$eartag[r] != tmp$eartag[r-1] |
                             tmp$testnum[r] != tmp$testnum[r-1],
                           0,
                           ifelse((tmp$cum_med_bf[r]/tmp$cum_med_bf[nrow(tmp)]*100)-1 <= (100 + pctolerance) &
                                    (tmp$cum_med_bf[r]/tmp$cum_med_bf[nrow(tmp)]*100)-1 >= (100-pctolerance),1,0))
    }
    
  st <- match(1, tmp$stable)
  
  }
  
  if(nrow(tmp) == 1){st = NA}
  
  st
  
}



uniquecows$titre <- foreach(r = 1:nrow(uniquecows)) %do% {
  p_nofails$titre[p_nofails$eartag == uniquecows$eartag[r] &
                    p_nofails$testnum == uniquecows$testnum[r]][1]
}

uniquecows$titre <- merge(uniquecows, p_nofails[,c('eartag', 'testnum', 'titre')],
                          by = c('eartag', 'testnum'),
                          all.x = TRUE)


ggplot(uniquecows,
       aes(x = firstrepstable)) +
  geom_histogram() +
  labs(title = paste0("N Reps for Stability at ",pctolerance,"%"))


ggplot(uniquecows,
       aes(x = as.numeric(titre),
           y = firstrepstable)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = paste0("Stability by titre at ",pctolerance,"%"))

uniquecows$convfact <- ceiling(0.5*as.numeric(uniquecows$titre))
  
ggplot(uniquecows) +
  geom_point(aes(x = as.numeric(titre),
                 y = firstrepstable)) +
  geom_line(aes(x = as.numeric(titre),
                y = convfact)) +
  labs(title = paste0("Stability by titre at ",pctolerance,"%"))

mod <- lm(firstrepstable ~
            as.numeric(titre),
          data = p)

df  <- data.frame(titre = 1:50)

df$predreps <- predict(mod, df)
