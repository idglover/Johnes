data_brms <- read.csv("y:/ian/johnesthresholds/johnesproper/data/data_brms.csv")

jplotbf_sim <- function(dt, id, tn, npd, ivl){
  
  data = dt
  
  n_post_draws = npd
  
  ivl = ivl
  
  cow <- id
  
  testnum <- tn
  
  tempdata <- data[data$calfeartag == cow,]
  
  tempdata <- tempdata[testnum,]
  
  
  simulate_bayes_factors(tempdata)
  
  
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
  
  bayes_fac <- prob_at_titre_pos/prob_at_titre_neg
  
  pdata <- data.frame(pos = post_draws_pos, neg = post_draws_neg)
  
  pdata <- gather(pdata, key = "status", value = "predtitre")
  
  print(ggplot(pdata, 
         aes(x = predtitre,
             group = status,
             fill = status)) + 
    geom_density(alpha = 0.5) +
    geom_vline(xintercept = tempdata$titre) +
    geom_vline(xintercept = tempdata$titre - ivl, linetype = "dashed") +
    geom_vline(xintercept = tempdata$titre + ivl, linetype = "dashed") +
    labs(title = paste0("Posterior prediction of models for pos and neg cows\n",
                        tempdata$calfeartag,
                        "\nTest number ",testnum),
         subtitle = paste0("Posterior interval around titre: ",ivl,
                           "\nProb Positive: ", prob_at_titre_pos,
                           "\nProb Negative: ", prob_at_titre_neg,
                           "\nBayes Factor: ",round(bayes_fac,2),
                           "\nNumber of posterior draws: ", n_post_draws,
                           "\nAge: ",tempdata$age,
                           "\nDIM: ", tempdata$dim,
                           "\nYield: ", tempdata$yield,
                           "\nSCC: ",tempdata$cellcount,
                           "\nMTNC: ",tempdata$meantitrenegcows),
         x = "Titre") +
    xlim(0, tempdata$titre + 30) +
    scale_fill_manual( values = c("green","red")))
}


  

jplotbf(data_brms, "UK342393102013", 4, 2000, 3)
        

