mod <- glmer(Target_QMMS ~ age + agesq + titre + protein + yield + (1|Farm), family = "binomial", data = data_multinom_train)




rocdata <- data.frame(preds = predict(mod, newdata = data_multinom_test, type = "response", re.form=~0),
                      obs = data_multinom_test$Target_QMMS)

ncuts = 100

rocdata$predcat <- cut(rocdata$preds, ncuts)
rocdata$predcatlevel <- as.integer(cut(rocdata$preds, ncuts, labels = FALSE))


sesptable <- data.frame(predcat = unique(rocdata[,c('predcat', 'predcatlevel')]))
colnames(sesptable) <- c("predcat", "predcatlevel")
sesptable <- sesptable[order(sesptable$predcatlevel),]


no_cores <- detectCores()  
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
getDoParWorkers() 


rocstats <- as.data.frame(foreach(i = 1:nrow(sesptable), .combine = "rbind") %dopar% {
  tp <- nrow(rocdata[rocdata$predcatlevel >= sesptable$predcatlevel[i] &
                       rocdata$obs == "1",])
  fp <- nrow(rocdata[rocdata$predcatlevel >= sesptable$predcatlevel[i] &
                       rocdata$obs == "0",])
  tn <- nrow(rocdata[rocdata$predcatlevel < sesptable$predcatlevel[i] &
                       rocdata$obs == "0",])
  fn <- nrow(rocdata[rocdata$predcatlevel < sesptable$predcatlevel[i] &
                       rocdata$obs == "1",])
  c(tp,fp,tn,fn)
  
  
})

stopCluster(cl)

colnames(rocstats) <- c("tp","fp","tn","fn")

sesptable <- cbind(sesptable, rocstats)

sesptable$n <- sesptable$tp +
  sesptable$fp +
  sesptable$tn +
  sesptable$fn


sesptable$Se <- sesptable$tp/(sesptable$tp+sesptable$fn)
sesptable$Sp <- sesptable$tn/(sesptable$tn+sesptable$fp)



#ROC curve:

ggplot(sesptable, aes(x = 1-Sp, y = Se)) +
  geom_line() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "ROC Curve")

no_cores <- detectCores()  
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
getDoParWorkers() 

                        
grad <- foreach (i = 2:nrow(sesptable), .combine = "c") %dopar% {
  horizshift <- (1-sesptable$Sp[i - 1]) - (1-sesptable$Sp[i + 1])
  vertshift <- sesptable$Se[i - 1] - sesptable$Se[i + 1]
  vertshift/horizshift
  
}

stopCluster(cl)

sesptable$gradient <- 0
sesptable$gradient[2:nrow(sesptable)] <- grad


ggplot(sesptable, aes(x = predcatlevel, y = log(gradient))) +
  geom_point() +
  geom_smooth() +
  labs(x = "Prediction Category", y = "Log Bayes Factor")










rocpreds <- prediction(rocdata$preds, rocdata$obs)

rocplotdata <- data.frame(cutpoint = unlist(slot(rocpreds, "cutoffs")),
                          n = length(unlist(slot(rocpreds, "cutoffs"))),
                          tp = unlist(slot(rocpreds, "tp")),
                          fp = unlist(slot(rocpreds, "fp")),
                          tn = unlist(slot(rocpreds, "tn")),
                          fn = unlist(slot(rocpreds, "fn"))
                          )

rocplotdata$Se <- rocplotdata$tp / (rocplotdata$tp + rocplotdata$fn)
rocplotdata$Sp <- rocplotdata$tn / (rocplotdata$tn + rocplotdata$fp)

#rocplotdata <- rocplotdata[rocplotdata$cutpoint != "Inf",]


ggplot(rocplotdata, aes(x = 1-Sp, y = Se)) +
  geom_line() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")






liketable <- data.frame(portion = c(1:20))

for(i in 1:nrow(liketable)){

liketable$hrcut[i] <- rocplotdata$cutpoint[round(nrow(rocplotdata)/20 * (i -1),0) + 1]
liketable$hr1mSp[i] <- 1 - rocplotdata$Sp[rocplotdata$cutpoint == liketable$hrcut[i]]
liketable$hrse[i] <- rocplotdata$Se[rocplotdata$cutpoint == liketable$hrcut[i]]
liketable$lrcut[i] <- rocplotdata$cutpoint[round(nrow(rocplotdata)/20 * i,0)]
liketable$lr1mSp[i] <- 1 - rocplotdata$Sp[rocplotdata$cutpoint == liketable$lrcut[i]]
liketable$lrse[i] <- rocplotdata$Se[rocplotdata$cutpoint == liketable$lrcut[i]]
}

liketable$gradient <-  (liketable$lrse - liketable$hrse) / (liketable$lr1mSp - liketable$hr1mSp)
