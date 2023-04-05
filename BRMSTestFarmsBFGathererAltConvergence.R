pp <- foreach(i = 1:length(sr), .packages = c("foreach", "brms"), .combine = "c") %dopar% {
  
  a <- ifelse(i == 1, 1, sr[i-1] + 1)
  b <- sr[i]
  tempdata <- data_brms_testing[a:b,]
  
  pp2 <- foreach(r = 1:nrow(tempdata), .combine = "c", .packages = "brms") %do% {
    
    tryCatch({write.table(r, paste0("C:/Users/Ian.glover.HEADOFFICE/Documents/PythonTools/ForEachRowCounter/rowsdonecore",i,".txt"), append = FALSE, sep = " ", dec = ".",
                          row.names = FALSE, col.names = FALSE)}, 
             error = function(cond){})  
    
    bf_vec <- NULL
    
    conv = FALSE
    
    while(conv == FALSE) {
      
      post_draws_pos <- posterior_predict(brms_mod_weibull_pos_age_yield_scc_mtnc_dimind, 
                                          newdata = tempdata[r,],
                                          ndraws = n_post_draws)
      
      prob_at_titre_pos <- length(post_draws_pos[post_draws_pos >= (tempdata[r, 'titre'] - ivl) &
                                                   post_draws_pos < (tempdata[r,'titre'] + ivl)]) / length(post_draws_pos)
      
      post_draws_neg <- posterior_predict(brms_mod_lognorm_neg_yield_scc_mtnc_dimind,
                                          newdata = tempdata[r,],
                                          ndraws = n_post_draws)
      
      prob_at_titre_neg <- prob_at_titre_neg <- length(post_draws_neg[post_draws_neg >= (tempdata[r, 'titre'] - ivl) &
                                                                        post_draws_neg < (tempdata[r,'titre'] + ivl)]) / length(post_draws_neg)
      
      bayes_fac <- prob_at_titre_pos/prob_at_titre_neg
      
      bayes_fac 
      
      bf_vec <- c(bf_vec, bayes_fac)
      
      if(length(bf_vec[bf_vec != Inf]) >= sqrt(tempdata$titre[r])){
        conv = TRUE
      }
    }
    
    med <- median(bf_vec[bf_vec != Inf])
    
    med
  }
  
  pp2
  
}