balancedpost <- function(dt, id, tn, npd, ivl, nreps){
  
  data = dt
  
  n_post_draws = npd
  
  ivl = ivl
  
  cow <- id
  
  testnum <- tn
  
  tempdata <- data[data$calfeartag == cow,]
  
  tempdata <- tempdata[testnum,]
  
  
  regcores(-1)
  
  pp <- foreach(i = 1:nreps, .combine = "c", .packages = "brms") %dopar% {
    
    brms_mod_weibull_pos_age_yield_scc_mtnc_dimind <- readRDS('y:/ian/johnesthresholds/johnesproper/data/pickledmodels/bayesianlikelihood/positivecows/brms_mod_weibull_pos_age_yield_scc_mtnc_dimind.rds')
    
    brms_mod_lognorm_neg_yield_scc_mtnc_dimind <- readRDS('y:/ian/johnesthresholds/johnesproper/data/pickledmodels/bayesianlikelihood/negativecows/brms_mod_lognorm_neg_yield_scc_mtnc_dimind.rds')
    
    post_draws_pos <- posterior_predict(brms_mod_weibull_pos_age_yield_scc_mtnc_dimind, 
                                        newdata = tempdata,
                                        re.form = ~ 0,
                                        ndraws = n_post_draws)
    
    
    
    
    prob_at_titre_pos <- length(post_draws_pos[post_draws_pos >= (tempdata$titre - ivl) &
                                                 post_draws_pos < (tempdata$titre + ivl)]) / length(post_draws_pos)
    
    
    
    
    post_draws_neg <- posterior_predict(brms_mod_lognorm_neg_yield_scc_mtnc_dimind,
                                        newdata = tempdata,
                                        re.form = ~ 0,
                                        ndraws = n_post_draws)
    
    #n_above_titre_neg <- length(post_draws_neg[post_draws_neg >= dbmte[r,'titre']])
    
    prob_at_titre_neg <- length(post_draws_neg[post_draws_neg >= (tempdata$titre - ivl) &
                                                 post_draws_neg < (tempdata$titre + ivl)]) / length(post_draws_neg)
    
    prob_at_titre_pos/prob_at_titre_neg
  }
  
  stopCluster(cl)
  
  print(paste0("N reps: ",nreps))
  print(pp)
  print(paste0("Median: ",median(pp)))
  ggplot(as.data.frame(pp),
         aes(x = pp)) +
    geom_histogram() +
    geom_vline(xintercept = median(pp)) +
    labs(title = paste0(id,"\nTest Number: ",tn,"\nTitre: ",round(tempdata$titre,1), "\nn Reps: ",nreps),
         subtitle = paste0("Median BF: ", round(median(pp),1)),
         x = "Bayes Factor")
}

balancedpost(data_brms, "UK342393103301", 3, 2000, 3, 20)
