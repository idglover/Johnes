timestable <- data.frame(npd = c(50,100,500,1000,2000,5000,10000), time = 0)

for(npd in timestable$npd){
  
  print(paste0("n Posterior Draws: ",npd))
  
  n_post_draws <- npd
  
  tma <- Sys.time()
  
  no_cores <- detectCores()
  cl <- makePSOCKcluster(no_cores-1)
  registerDoParallel(cl)
  getDoParWorkers() 
  
  bf_tab <- foreach(r = 1:50, .combine = "rbind", .packages = "brms") %dopar% {

    post_draws_pos <- posterior_predict(brms_mod_gamma_pos_age_yield_scc_mtnc, 
                                    newdata = data_brms[r,],
                                    re.form = ~ 0,
                                    ndraws = n_post_draws)
    
    post_draws_neg <- posterior_predict(brms_mod_gamma_neg_yield_scc_mtnc,
                                        newdata = data_brms[r,],
                                        re.form = ~ 0,
                                        ndraws = n_post_draws)
    
    prob_at_titre_pos <- length(post_draws_pos[post_draws_pos >= (data_brms[r, 'titre'] - ivl) &
                                                 post_draws_pos < (data_brms[r,'titre'] + ivl)]) / length(post_draws_pos)
    
    prob_at_titre_neg <- prob_at_titre_neg <- length(post_draws_neg[post_draws_neg >= (data_brms[r, 'titre'] - ivl) &
                                                                      post_draws_neg < (data_brms[r,'titre'] + ivl)]) / length(post_draws_neg)
    
    bayes_fac <- prob_at_titre_pos/prob_at_titre_neg

    c(r, data_brms$age[r], data_brms$yield[r], data_brms$titre[r], prob_at_titre_pos, prob_at_titre_neg, bayes_fac)
    
  }
  
  stopCluster(cl)
  
  assign(paste0("bf_tab",npd), bf_tab)
  
  tmb <- Sys.time()
  
  timestable$time[timestable$npd == n_post_draws] <- difftime(tmb, tma, units = "mins")
}

acctab <- cbind(bf_tab50[,7],
                bf_tab100[,7],
                bf_tab500[,7],
                bf_tab1000[,7],
                bf_tab2000[,7],
                bf_tab5000[,7],
                bf_tab10000[,7])

acctabtime <- data.frame(acctab[,2] - acctab[,1],
                         acctab[,3] - acctab[,2],
                         acctab[,4] - acctab[,3],
                         acctab[,5] - acctab[,4],
                         acctab[,6] - acctab[,5],
                         acctab[,7] - acctab[,6])

timestable$change <- 0

timestable$change[2] <- median(acctabtime[acctabtime[,1] != Inf & acctabtime[,1] != -Inf,1], na.rm = TRUE)
timestable$change[3] <- median(acctabtime[acctabtime[,2] != Inf & acctabtime[,2] != -Inf,2], na.rm = TRUE)
timestable$change[4] <- median(acctabtime[acctabtime[,3] != Inf & acctabtime[,3] != -Inf,3], na.rm = TRUE)
timestable$change[5] <- median(acctabtime[acctabtime[,4] != Inf & acctabtime[,4] != -Inf,4], na.rm = TRUE)
timestable$change[6] <- mean(acctabtime[acctabtime[,5] != Inf & acctabtime[,5] != -Inf,5], na.rm = TRUE)
timestable$change[7] <- mean(acctabtime[acctabtime[,6] != Inf & acctabtime[,6] != -Inf,6], na.rm = TRUE)

