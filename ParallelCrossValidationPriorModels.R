CVMods <- c("priorglmm_birth_1",
            "priorglmm_birth_2",
            "priorglmm_birth_3",
            "priorglmm_birth_4",
            "priorglmm_birth_5",
            "priorglmm_birth_6",
            "priorglmm_birth_7",
            "priorglmm_birth_8",
            "priorglmm_birth_9",
            "priorglmm_birth_10",
            "priorglmm_birth_11",
            "priorglmm_birth_12",
            "priorglmm_birth_13",
            "priorglmm_birth_14",
            "priorglmm_birth_15",
            "priorglmm_birth_16",
            "priorglmm_birth_17",
            "priorglmm_birth_18",
            "priorglmm_birth_19",
            "priorglmm_birth_20",
            "priorglmm_birth_21",
            "priorglmm_12mold_1",
            "priorglmm_12mold_2",
            "priorglmm_12mold_3",
            "priorglmm_12mold_4",
            "priorglmm_12mold_5",
            "priorglmm_12mold_6",
            "priorglmm_12mold_7",
            "priorglmm_12mold_8",
            "priorglmm_12mold_9",
            "priorglmm_12mold_10",
            "priorglmm_12mold_11",
            "priorglmm_12mold_12",
            "priorglmm_12mold_13",
            "priorglmm_12mold_14",
            "priorglmm_12mold_15",
            "priorglmm_crt_1",
            "priorglmm_crt_2",
            "priorglmm_crt_3",
            "priorglmm_crt_4",
            "priorglmm_crt_5",
            "priorglmm_crt_6",
            "priorglmm_crt_7",
            "priorglmm_crt_8",
            "priorglmm_crt_9",
            "priorglmm_crt_10",
            "priorglmm_crt_11",
            "priorglmm_crt_12",
            "priorglmm_crt_13",
            "priorglmm_crt_14",
            "priorglmm_crt_15",
            "priorglmm_crt_16",
            "priorglmm_crt_17",
            "priorglmm_crt_18")

CVMods <- c("priorglmm_birth_1",
            "priorglmm_birth_2")


nfolds <- 5
nrepeats <- 5

nmodstorun <- length(CVMods) * nfolds * nrepeats
nmodsrun <- 0

no_cores <- detectCores() - 1  
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
getDoParWorkers() 



CVResults <- foreach(modeltorun = CVMods, .combine = "rbind") %do% {
  
  modresults <- foreach(k = 1:nrepeats, .combine = "rbind") %do% {
    
    set.seed(sample(1:10000,1))

    folds <- groupKFold(data_modelling_train$Farm, k = nfolds)

    represults <- foreach(j = 1:nfolds, .combine = "rbind", .packages = "lme4") %dopar% {
      
      trainset <- data_modelling_train[folds[[j]],]
      testset <- data_modelling_train[-folds[[j]],]
  
      if(modeltorun == "priorglmm_birth_1"){
    
        mod <- glmer(Target_QMMS_strictneg ~ damstatusbirth_cat +
                       (1|Farm),
                     family = "binomial",
                     data = trainset,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl=list(maxfun=100000)))
    
      }
      
      if(modeltorun == "priorglmm_birth_2"){
        mod <- glmer(Target_QMMS_strictneg ~ damstatusbirth_cat +
                granddamstatusbirth_cat +
                (1|Farm),
              family = "binomial",
              data = trainset,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl=list(maxfun=100000)))
      }
      
      if(modeltorun == "priorglmm_birth_3"){
        mod <- glmer(Target_QMMS_strictneg ~ damstatusbirth_cat +
                       granddamstatusbirth_cat +
                       prophorizontalrelsstatus2birth_cat +
                       
                       (1|Farm),
                     family = "binomial",
                     data = trainset,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl=list(maxfun=100000)))
      }
      
      if(modeltorun == "priorglmm_birth_4"){
        mod <- glmer(Target_QMMS_strictneg ~ damstatusbirth_cat +
                       granddamstatusbirth_cat +
                       prophorizontalrelsstatus2birth_cat +
                       prophorizontalrelsstatus3birth_cat +
                       
                       (1|Farm),
                     family = "binomial",
                     data = trainset,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl=list(maxfun=100000)))
      }
      
      if(modeltorun == "priorglmm_birth_5"){
        mod <- glmer(Target_QMMS_strictneg ~ damstatusbirth_cat +
                       granddamstatusbirth_cat +
                       prophorizontalrelsstatus2birth_recat +
                       prophorizontalrelsstatus3birth_recat +
                       
                       (1|Farm),
                     family = "binomial",
                     data = trainset,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl=list(maxfun=100000)))
        
      }
      
      if(modeltorun == "priorglmm_birth_6"){
        mod <- glmer(Target_QMMS_strictneg ~ damstatusbirth_cat * damntestsbirth +
                       granddamstatusbirth_cat *grandamdamntestsbirth +
                       prophorizontalrelsstatus2birth_recat +
                       prophorizontalrelsstatus3birth_recat +
                       
                       (1|Farm),
                     family = "binomial",
                     data = trainset,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl=list(maxfun=100000)))
      }
      
      if(modeltorun == "priorglmm_birth_7"){
        mod <- glmer(Target_QMMS_strictneg ~ damstatusbirth_cat * damntestsbirth +
                       granddamstatusbirth_cat *grandamdamntestsbirth +
                       prophorizontalrelsstatus2birth_recat +
                       prophorizontalrelsstatus3birth_recat +
                       propproximaldamsstatus2birth +
                       propproximaldamsstatus3birth +
                       (1|Farm),
                     family = "binomial",
                     data = trainset,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl=list(maxfun=100000)))
      }
      
      if(modeltorun == "priorglmm_birth_8"){
        mod <- glmer(Target_QMMS_strictneg ~ damstatusbirth_cat * damntestsbirth +
                       granddamstatusbirth_cat *grandamdamntestsbirth +
                       prophorizontalrelsstatus2birth_recat +
                       prophorizontalrelsstatus3birth_recat +
                       propvaguelyproximaldamsstatus2birth +
                       propvaguelyproximaldamsstatus3birth +
                       (1|Farm),
                     family = "binomial",
                     data = trainset,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl=list(maxfun=100000)))
      }
      
      if(modeltorun == "priorglmm_birth_9"){
        mod <- glmer(Target_QMMS_strictneg ~ damstatusbirth_cat * damntestsbirth +
                       granddamstatusbirth_cat *grandamdamntestsbirth +
                       prophorizontalrelsstatus2birth_recat +
                       prophorizontalrelsstatus3birth_recat +
                       propvaguelyproximaldamsstatus2birth +
                       propvaguelyproximaldamsstatus3birth +
                       propposavg_cat +
                       (1|Farm),
                     family = "binomial",
                     data = trainset,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl=list(maxfun=100000)))
      }
      
      if(modeltorun == "priorglmm_birth_10"){
        mod <- glmer(Target_QMMS_strictneg ~ damstatusbirth_cat * damntestsbirth +
                       granddamstatusbirth_cat *grandamdamntestsbirth +
                       prophorizontalrelsstatus2birth_recat +
                       prophorizontalrelsstatus3birth_recat +
                       propvaguelyproximaldamsstatus2birth +
                       propvaguelyproximaldamsstatus3birth +
                       meantitreavg_cat +
                       (1|Farm),
                     family = "binomial",
                     data = trainset,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl=list(maxfun=100000)))
      }
      
      if(modeltorun == "priorglmm_birth_11"){
        mod <- glmer(Target_QMMS_strictneg ~ damstatusbirth_cat * damntestsbirth +
                       granddamstatusbirth_cat *grandamdamntestsbirth +
                       prophorizontalrelsstatus2birth_recat +
                       prophorizontalrelsstatus3birth_recat +
                       propvaguelyproximaldamsstatus2birth +
                       propvaguelyproximaldamsstatus3birth +
                       meantitrenegcowsavg_cat +
                       (1|Farm),
                     family = "binomial",
                     data = trainset,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl=list(maxfun=100000)))
        
      }
      
      if(modeltorun == "priorglmm_birth_12"){
        mod <- glmer(Target_QMMS_strictneg ~ damstatusbirth_cat * damntestsbirth +
                       granddamstatusbirth_cat *grandamdamntestsbirth +
                       prophorizontalrelsstatus2birth_recat +
                       prophorizontalrelsstatus3birth_recat +
                       propvaguelyproximaldamsstatus2birth +
                       propvaguelyproximaldamsstatus3birth +
                       meantitrenegcowsavg_recat +
                       (1|Farm),
                     family = "binomial",
                     data = trainset,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl=list(maxfun=100000)))
      }
      
      if(modeltorun == "priorglmm_birth_13"){
        mod <- glmer(Target_QMMS_strictneg ~ damstatusbirth_cat * damntestsbirth +
                       granddamstatusbirth_cat *grandamdamntestsbirth +
                       propsiblingsstatus2birth +
                       propvaguelyproximaldamsstatus2birth +
                       propvaguelyproximaldamsstatus3birth +
                       meantitrenegcowsavg_recat +
                       (1|Farm),
                     family = "binomial",
                     data = trainset,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl=list(maxfun=100000)))
      }
      
      if(modeltorun == "priorglmm_birth_14"){
        mod <- glmer(Target_QMMS_strictneg ~ damstatusbirth_cat * damntestsbirth +
                       granddamstatusbirth_cat *grandamdamntestsbirth +
                       propsiblingsstatus2birth_cat +
                       propvaguelyproximaldamsstatus2birth +
                       propvaguelyproximaldamsstatus3birth +
                       meantitrenegcowsavg_recat +
                       (1|Farm),
                     family = "binomial",
                     data = trainset,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl=list(maxfun=100000)))
      }
      
      if(modeltorun == "priorglmm_birth_15"){
        mod <- glmer(Target_QMMS_strictneg ~ damstatusbirth_cat * damntestsbirth +
                       granddamstatusbirth_cat *grandamdamntestsbirth +
                       propsiblingsstatus2birth +
                       propsiblingsstatus3birth +
                       propvaguelyproximaldamsstatus2birth +
                       propvaguelyproximaldamsstatus3birth +
                       meantitrenegcowsavg_recat +
                       (1|Farm),
                     family = "binomial",
                     data = trainset,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl=list(maxfun=100000)))
        
      }
      
      if(modeltorun == "priorglmm_birth_16"){
        mod <- glmer(Target_QMMS_strictneg ~ damstatusbirth_cat * damntestsbirth +
                       granddamstatusbirth_cat *grandamdamntestsbirth +
                       propsiblingsstatus2birth +
                       propsiblingsstatus3birth +
                       propauntsstatus2birth +
                       propvaguelyproximaldamsstatus2birth +
                       propvaguelyproximaldamsstatus3birth +
                       meantitrenegcowsavg_recat +
                       (1|Farm),
                     family = "binomial",
                     data = trainset,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl=list(maxfun=100000)))
      }
      
      if(modeltorun == "priorglmm_birth_17"){
        mod <- glmer(Target_QMMS_strictneg ~ damstatusbirth_cat * damntestsbirth +
                       granddamstatusbirth_cat *grandamdamntestsbirth +
                       propsiblingsstatus2birth +
                       propsiblingsstatus3birth +
                       propauntsstatus3birth +
                       propvaguelyproximaldamsstatus2birth +
                       propvaguelyproximaldamsstatus3birth +
                       meantitrenegcowsavg_recat +
                       (1|Farm),
                     family = "binomial",
                     data = trainset,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl=list(maxfun=100000)))
        
      }
      
      if(modeltorun == "priorglmm_birth_18"){
        mod <- glmer(Target_QMMS_strictneg ~ damstatusbirth_cat * damntestsbirth +
                       propsiblingsstatus2birth +
                       propsiblingsstatus3birth +
                       propvaguelyproximaldamsstatus2birth +
                       propvaguelyproximaldamsstatus3birth +
                       meantitrenegcowsavg_recat +
                       (1|Farm),
                     family = "binomial",
                     data = trainset,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl=list(maxfun=100000)))
        
      }
      
      if(modeltorun == "priorglmm_birth_19"){
        mod <- glmer(Target_QMMS_strictneg ~ damstatusbirth_cat * damntestsbirth +
                       prophorizontalrelsstatus2birth +
                       prophorizontalrelsstatus3birth +
                       propvaguelyproximaldamsstatus2birth +
                       propvaguelyproximaldamsstatus3birth +
                       meantitrenegcowsavg_recat +
                       (1|Farm),
                     family = "binomial",
                     data = trainset,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl=list(maxfun=100000)))
      }
      
      if(modeltorun == "priorglmm_birth_20"){
        mod <- glmer(Target_QMMS_strictneg ~ damstatusbirth_cat * damntestsbirth +
                       prophorizontalrelsstatus2birth +
                       propvaguelyproximaldamsstatus2birth +
                       propvaguelyproximaldamsstatus3birth +
                       meantitrenegcowsavg_recat +
                       (1|Farm),
                     family = "binomial",
                     data = trainset,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl=list(maxfun=100000)))
      }
      
      if(modeltorun == "priorglmm_birth_21"){
        mod <- glmer(Target_QMMS_strictneg ~ damstatusbirth_cat * damntestsbirth +
                       prophorizontalrelsstatus2birth_cat +
                       propvaguelyproximaldamsstatus2birth +
                       propvaguelyproximaldamsstatus3birth +
                       meantitrenegcowsavg_recat +
                       (1|Farm),
                     family = "binomial",
                     data = trainset,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl=list(maxfun=100000)))
        
      }
      
      if(modeltorun == "priorglmm_12mold_1"){
      
      mod <- glmer(Target_QMMS_strictneg ~ damstatus12mold_cat +
                                    (1|Farm),
                                  family ="binomial",
                                  data = trainset,
                                  control = glmerControl(optimizer = "bobyqa",
                                                         optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_12mold_2"){
      
      
      mod <- glmer(Target_QMMS_strictneg ~ damstatus12mold_cat * damntests12mold +
                                    (1|Farm),
                                  family ="binomial",
                                  data = trainset,
                                  control = glmerControl(optimizer = "bobyqa",
                                                         optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_12mold_3"){
      
      mod <- glmer(Target_QMMS_strictneg ~ damstatus12mold_cat * damntests12mold +
                                    granddamstatus12mold_cat +
                                    (1|Farm),
                                  family ="binomial",
                                  data = trainset,
                                  control = glmerControl(optimizer = "bobyqa",
                                                         optCtrl = list(maxfun = 100000)))
      
      }
      
      if(modeltorun == "priorglmm_12mold_4"){
      
      mod <- glmer(Target_QMMS_strictneg ~ damstatus12mold_cat * damntests12mold +
                                    granddamstatus12mold_cat * grandamdamntests12mold +
                                    (1|Farm),
                                  family ="binomial",
                                  data = trainset,
                                  control = glmerControl(optimizer = "bobyqa",
                                                         optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_12mold_5"){
        
      mod <- glmer(Target_QMMS_strictneg ~ damstatus12mold_cat * damntests12mold +
                                    granddamstatus12mold_cat * grandamdamntests12mold +
                                    prophorizontalrelsstatus212mold_cat +
                                    prophorizontalrelsstatus312mold_cat +
                                    (1|Farm),
                                  family ="binomial",
                                  data = trainset,
                                  control = glmerControl(optimizer = "bobyqa",
                                                         optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_12mold_6"){
      
      
      mod <- glmer(Target_QMMS_strictneg ~ damstatus12mold_cat * damntests12mold +
                                    granddamstatus12mold_cat * grandamdamntests12mold +
                                    prophorizontalrelsstatus212mold_cat +
                                    (1|Farm),
                                  family ="binomial",
                                  data = trainset,
                                  control = glmerControl(optimizer = "bobyqa",
                                                         optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_12mold_7"){
      
      mod <- glmer(Target_QMMS_strictneg ~ damstatus12mold_cat * damntests12mold +
                                    granddamstatus12mold_cat * grandamdamntests12mold +
                                    propsiblingsstatus212mold +
                                    (1|Farm),
                                  family ="binomial",
                                  data = trainset,
                                  control = glmerControl(optimizer = "bobyqa",
                                                         optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_12mold_8"){
      
      mod <- glmer(Target_QMMS_strictneg ~ damstatus12mold_cat * damntests12mold +
                                    granddamstatus12mold_cat * grandamdamntests12mold +
                                    propsiblingsstatus212mold +
                                    propsiblingsstatus312mold +
                                    (1|Farm),
                                  family ="binomial",
                                  data = trainset,
                                  control = glmerControl(optimizer = "bobyqa",
                                                         optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_12mold_9"){
      
      mod <- glmer(Target_QMMS_strictneg ~ damstatus12mold_cat * damntests12mold +
                                    granddamstatus12mold_cat * grandamdamntests12mold +
                                    propsiblingsstatus212mold +
                                    propauntsstatus212mold +
                                    (1|Farm),
                                  family ="binomial",
                                  data = trainset,
                                  control = glmerControl(optimizer = "bobyqa",
                                                         optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_12mold_10"){
      
      mod <- glmer(Target_QMMS_strictneg ~ damstatus12mold_cat * damntests12mold +
                                     granddamstatus12mold_cat * grandamdamntests12mold +
                                     propsiblingsstatus212mold +
                                     propvaguelyproximaldamsstatus212mold +
                                     (1|Farm),
                                   family ="binomial",
                                   data = trainset,
                                   control = glmerControl(optimizer = "bobyqa",
                                                          optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_12mold_11"){
      
      
      mod <- glmer(Target_QMMS_strictneg ~ damstatus12mold_cat * damntests12mold +
                                     granddamstatus12mold_cat * grandamdamntests12mold +
                                     propsiblingsstatus212mold +
                                     propproximaldamsstatus212mold +
                                     (1|Farm),
                                   family ="binomial",
                                   data = trainset,
                                   control = glmerControl(optimizer = "bobyqa",
                                                          optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_12mold_12"){
      
      
      mod <- glmer(Target_QMMS_strictneg ~ damstatus12mold_cat * damntests12mold +
                                     granddamstatus12mold_cat * grandamdamntests12mold +
                                     propsiblingsstatus212mold +
                                     propvaguelyproximaldamsstatus212mold +
                                     propvaguelyproximaldamsstatus312mold +
                                     (1|Farm),
                                   family ="binomial",
                                   data = trainset,
                                   control = glmerControl(optimizer = "bobyqa",
                                                          optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_12mold_13"){
      
      
      mod <- glmer(Target_QMMS_strictneg ~ damstatus12mold_cat * damntests12mold +
                                     granddamstatus12mold_cat * grandamdamntests12mold +
                                     propsiblingsstatus212mold +
                                     propvaguelyproximaldamsstatus212mold +
                                     propvaguelyproximaldamsstatus312mold +
                                     meantitrenegcowsavg_recat +
                                     (1|Farm),
                                   family ="binomial",
                                   data = trainset,
                                   control = glmerControl(optimizer = "bobyqa",
                                                          optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_12mold_14"){
      
      mod <- glmer(Target_QMMS_strictneg ~ damstatus12mold_cat * damntests12mold +
                                     granddamstatus12mold_cat * grandamdamntests12mold +
                                     prophorizontalrelsstatus212mold +
                                     propvaguelyproximaldamsstatus212mold +
                                     propvaguelyproximaldamsstatus312mold +
                                     meantitrenegcowsavg_recat +
                                     (1|Farm),
                                   family ="binomial",
                                   data = trainset,
                                   control = glmerControl(optimizer = "bobyqa",
                                                          optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_12mold_15"){
      
      mod <- glmer(Target_QMMS_strictneg ~ damstatus12mold_cat * damntests12mold +
                                     granddamstatus12mold_cat * grandamdamntests12mold +
                                     prophorizontalrelsstatus212mold +
                                     prophorizontalrelsstatus312mold +
                                     propvaguelyproximaldamsstatus212mold +
                                     propvaguelyproximaldamsstatus312mold +
                                     meantitrenegcowsavg_recat +
                                     (1|Farm),
                                   family ="binomial",
                                   data = trainset,
                                   control = glmerControl(optimizer = "bobyqa",
                                                          optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_crt_1"){
      
      mod <- glmer(Target_QMMS_strictneg ~ damcrtstatus_cat * damntestscrt +
                                 (1|Farm),
                               family ="binomial",
                               data = trainset,
                               control = glmerControl(optimizer = "bobyqa",
                                                      optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_crt_2"){
      
      mod <- glmer(Target_QMMS_strictneg ~ damcrtstatus_cat * damntestscrt +
                                 granddamcrtstatus_cat * grandamdamntestscrt +
                                 (1|Farm),
                               family ="binomial",
                               data = trainset,
                               control = glmerControl(optimizer = "bobyqa",
                                                      optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_crt_3"){
      
      mod <- glmer(Target_QMMS_strictneg ~ damcrtstatus_cat * damntestscrt +
                                 granddamcrtstatus_cat * grandamdamntestscrt +
                                 prophorizontalrelsstatus2crt_cat +
                                 (1|Farm),
                               family ="binomial",
                               data = trainset,
                               control = glmerControl(optimizer = "bobyqa",
                                                      optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_crt_3"){
      
      mod <- glmer(Target_QMMS_strictneg ~ damcrtstatus_cat * damntestscrt +
                                 granddamcrtstatus_cat * grandamdamntestscrt +
                                 prophorizontalrelsstatus2crt_cat +
                                 (1|Farm),
                               family ="binomial",
                               data = trainset,
                               control = glmerControl(optimizer = "bobyqa",
                                                      optCtrl = list(maxfun = 100000)))
      
      }
      
      if(modeltorun == "priorglmm_crt_4"){
      
      mod <- glmer(Target_QMMS_strictneg ~ damcrtstatus_cat * damntestscrt +
                                 granddamcrtstatus_cat * grandamdamntestscrt +
                                 prophorizontalrelsstatus2crt_recat +
                                 (1|Farm),
                               family ="binomial",
                               data = trainset,
                               control = glmerControl(optimizer = "bobyqa",
                                                      optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_crt_5"){
      
      mod <- glmer(Target_QMMS_strictneg ~ damcrtstatus_cat * damntestscrt +
                                 granddamcrtstatus_cat * grandamdamntestscrt +
                                 prophorizontalrelsstatus2crt +
                                 (1|Farm),
                               family ="binomial",
                               data = trainset,
                               control = glmerControl(optimizer = "bobyqa",
                                                      optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_crt_6"){
      
      mod <- glmer(Target_QMMS_strictneg ~ damcrtstatus_cat * damntestscrt +
                                 granddamcrtstatus_cat * grandamdamntestscrt +
                                 prophorizontalrelsstatus2crt_recat +
                                 prophorizontalrelsstatus3crt_cat +
                                 (1|Farm),
                               family ="binomial",
                               data = trainset,
                               control = glmerControl(optimizer = "bobyqa",
                                                      optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_crt_7"){
      
      mod <- glmer(Target_QMMS_strictneg ~ damcrtstatus_cat * damntestscrt +
                                 granddamcrtstatus_cat * grandamdamntestscrt +
                                 propsiblingsstatus2crt +
                                 (1|Farm),
                               family ="binomial",
                               data = trainset,
                               control = glmerControl(optimizer = "bobyqa",
                                                      optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_crt_8"){
      
      mod <- glmer(Target_QMMS_strictneg ~ damcrtstatus_cat * damntestscrt +
                                 granddamcrtstatus_cat * grandamdamntestscrt +
                                 propsiblingsstatus2crt_cat +
                                 (1|Farm),
                               family ="binomial",
                               data = trainset,
                               control = glmerControl(optimizer = "bobyqa",
                                                      optCtrl = list(maxfun = 100000)))
      
      }
      
      if(modeltorun == "priorglmm_crt_9"){
      
      mod <- glmer(Target_QMMS_strictneg ~ damcrtstatus_cat * damntestscrt +
                                 granddamcrtstatus_cat * grandamdamntestscrt +
                                 prophorizontalrelsstatus2crt_recat +
                                 prophorizontalrelsstatus3crt_recat +
                                 (1|Farm),
                               family ="binomial",
                               data = trainset,
                               control = glmerControl(optimizer = "bobyqa",
                                                      optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_crt_10"){
      
      mod <- glmer(Target_QMMS_strictneg ~ damcrtstatus_cat * damntestscrt +
                                  granddamcrtstatus_cat * grandamdamntestscrt +
                                  prophorizontalrelsstatus2crt_recat +
                                  prophorizontalrelsstatus3crt_recat +
                                  propproximaldamsstatus2crt +
                                  (1|Farm),
                                family ="binomial",
                                data = trainset,
                                control = glmerControl(optimizer = "bobyqa",
                                                       optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_crt_11"){
      
      mod <- glmer(Target_QMMS_strictneg ~ damcrtstatus_cat * damntestscrt +
                                  granddamcrtstatus_cat * grandamdamntestscrt +
                                  prophorizontalrelsstatus2crt_recat +
                                  prophorizontalrelsstatus3crt_recat +
                                  propvaguelyproximaldamsstatus2crt +
                                  (1|Farm),
                                family ="binomial",
                                data = trainset,
                                control = glmerControl(optimizer = "bobyqa",
                                                       optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_crt_12"){
      
      mod <- glmer(Target_QMMS_strictneg ~ damcrtstatus_cat * damntestscrt +
                                  granddamcrtstatus_cat * grandamdamntestscrt +
                                  prophorizontalrelsstatus2crt_recat +
                                  prophorizontalrelsstatus3crt_recat +
                                  propvaguelyproximaldamsstatus2crt +
                                  propvaguelyproximaldamsstatus3crt +
                                  (1|Farm),
                                family ="binomial",
                                data = trainset,
                                control = glmerControl(optimizer = "bobyqa",
                                                       optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_crt_13"){
      
      mod <- glmer(Target_QMMS_strictneg ~ damcrtstatus_cat * damntestscrt +
                                  granddamcrtstatus_cat * grandamdamntestscrt +
                                  prophorizontalrelsstatus2crt_recat +
                                  prophorizontalrelsstatus3crt_recat +
                                  propvaguelyproximaldamsstatus2crt +
                                  propvaguelyproximaldamsstatus3crt +
                                  propproximalcalvesstatus2crt +
                                  (1|Farm),
                                family ="binomial",
                                data = trainset,
                                control = glmerControl(optimizer = "bobyqa",
                                                       optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_crt_14"){
      
      mod <- glmer(Target_QMMS_strictneg ~ damcrtstatus_cat * damntestscrt +
                                  granddamcrtstatus_cat * grandamdamntestscrt +
                                  prophorizontalrelsstatus2crt_recat +
                                  prophorizontalrelsstatus3crt_recat +
                                  propvaguelyproximaldamsstatus2crt +
                                  propvaguelyproximaldamsstatus3crt +
                                  propvaguelyproximalcalvesstatus2crt +
                                  (1|Farm),
                                family ="binomial",
                                data = trainset,
                                control = glmerControl(optimizer = "bobyqa",
                                                       optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_crt_15"){
      
      mod <- glmer(Target_QMMS_strictneg ~ damcrtstatus_cat * damntestscrt +
                                  granddamcrtstatus_cat * grandamdamntestscrt +
                                  prophorizontalrelsstatus2crt_recat +
                                  prophorizontalrelsstatus3crt_recat +
                                  propvaguelyproximaldamsstatus2crt +
                                  propvaguelyproximaldamsstatus3crt +
                                  propvaguelyproximalcalvesstatus2crt +
                                  propproximalcalvesstatus3crt +
                                  (1|Farm),
                                family ="binomial",
                                data = trainset,
                                control = glmerControl(optimizer = "bobyqa",
                                                       optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_crt_16"){
        
      mod <- glmer(Target_QMMS_strictneg ~ damcrtstatus_cat * damntestscrt +
                                  granddamcrtstatus_cat * grandamdamntestscrt +
                                  prophorizontalrelsstatus2crt_recat +
                                  prophorizontalrelsstatus3crt_recat +
                                  propvaguelyproximaldamsstatus2crt +
                                  propvaguelyproximaldamsstatus3crt +
                                  propvaguelyproximalcalvesstatus2crt +
                                  propvaguelyproximalcalvesstatus3crt +
                                  (1|Farm),
                                family ="binomial",
                                data = trainset,
                                control = glmerControl(optimizer = "bobyqa",
                                                       optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_crt_17"){
      
      mod <- glmer(Target_QMMS_strictneg ~ damcrtstatus_cat * damntestscrt +
                                  granddamcrtstatus_cat * grandamdamntestscrt +
                                  prophorizontalrelsstatus2crt_recat +
                                  prophorizontalrelsstatus3crt_recat +
                                  propvaguelyproximaldamsstatus2crt +
                                  propvaguelyproximaldamsstatus3crt +
                                  propvaguelyproximalcalvesstatus2crt +
                                  propvaguelyproximalcalvesstatus3crt +
                                  meantitrenegcowsavg_recat +
                                  (1|Farm),
                                family ="binomial",
                                data = trainset,
                                control = glmerControl(optimizer = "bobyqa",
                                                       optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_crt_18"){
      
      
      mod <- glmer(Target_QMMS_strictneg ~ damcrtstatus_cat * damntestscrt +
                                  prophorizontalrelsstatus2crt_recat +
                                  prophorizontalrelsstatus3crt_recat +
                                  propvaguelyproximaldamsstatus2crt +
                                  propvaguelyproximaldamsstatus3crt +
                                  propvaguelyproximalcalvesstatus2crt +
                                  propvaguelyproximalcalvesstatus3crt +
                                  meantitrenegcowsavg_recat +
                                  (1|Farm),
                                family ="binomial",
                                data = trainset,
                                control = glmerControl(optimizer = "bobyqa",
                                                       optCtrl = list(maxfun = 100000)))
      }
      
      
      

      pred <- tryCatch({predict(mod, newdata=testset, type="response", re.form=~0, allow.new.levels = T)},
                    error=function(cond) {
                    print("Error")
                    return("FAILED")})
      
      obs <- testset$Target_QMMS_strictneg
  
      foldresults <- data.frame(modeltorun, k, j, pred, obs)
      
      
  
      return(foldresults)
  
  
  
    }

  nmodsrun <- nmodsrun + nfolds
    
  message(paste0("Done ", nmodsrun,"/",nmodstorun," (", round(nmodsrun/nmodstorun*100,1),"%)"))
  
  return(represults)
  
  }
  
  return(modresults)
  
}



stopCluster(cl)

colnames(CVResults) <- c("Model", "Rep", "Fold", "Pred", "Obs")


write.csv(CVResults, "y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/priorglmmCVResults.csv", row.names = FALSE)

CVResults$Pred <- as.numeric(CVResults$Pred)
CVResults$Obs <- as.factor(CVResults$Obs)


for (eta in 1:length(CVMods)){
  CPData <- CVResults[which(CVResults$Model == CVMods[eta]),]
  print(CaliPlot(CPData$Pred, CPData$Obs, 
                 nbins = 10, 
                 ptitle = CVMods[eta],
                 psubtitle = "5:5 Cross Validation")) 
  ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/",CVMods[eta],".png"))
}









caliberrortable <- data.frame(Model = character(), ECE = numeric(), MCE = numeric())

for (eta in 1:length(CVMods)){
  
  CPData <- CVResults[which(CVResults$Model == CVMods[eta] & !is.na(CVResults$Pred)),]
  
  
  calibtemp <- cbind(CVMods[eta], ExtractECE(CPData$Pred, CPData$Obs), ExtractMCE(CPData$Pred, CPData$Obs))
  colnames(calibtemp) <- c("Model", "ECE", "MCE")
  caliberrortable <- rbind(caliberrortable, calibtemp)
  
  
  
  
}



plotdata <- caliberrortable

plotdata$ECE <- as.numeric(plotdata$ECE)
plotdata$MCE <- as.numeric(plotdata$MCE)

print(ggplot(plotdata, aes(x = Model)) +
        geom_bar(aes(y = ECE), stat = "identity", fill = "green") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        labs(title = "Cross Validated Calibration Errors"))

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/CVModelsECE.png")

print(ggplot(plotdata, aes(x = Model)) +
        geom_bar(aes(y = MCE), stat = "identity", fill = "darkred") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        labs(title = "Cross Validated Calibration Errors"))

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/CVModelsMCE.png")



