priormods <- c("priorglmm_birth_1",
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




no_cores <- detectCores() 
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
getDoParWorkers()

fittedmods <- foreach(modeltorun = priormods, .packages = "lme4") %dopar% {

if(modeltorun == "priorglmm_birth_1"){
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatusbirth_cat +
                               (1|Farm),
                             family = "binomial",
                             data = data_modelling_train,
                             control = glmerControl(optimizer = "bobyqa",
                                                    optCtrl=list(maxfun=100000)))
}
  
if(modeltorun == "priorglmm_birth_2"){
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatusbirth_cat +
          granddamstatusbirth_cat +
          (1|Farm),
        family = "binomial",
        data = data_modelling_train,
        control = glmerControl(optimizer = "bobyqa",
                               optCtrl=list(maxfun=100000)))
  
}

if(modeltorun == "priorglmm_birth_3"){
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatusbirth_cat +
                               granddamstatusbirth_cat +
                               prophorizontalrelsstatus2birth_cat +
                               
                               (1|Farm),
                             family = "binomial",
                             data = data_modelling_train,
                             control = glmerControl(optimizer = "bobyqa",
                                                    optCtrl=list(maxfun=100000)))
}
  
if(modeltorun == "priorglmm_birth_4"){
  
  
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatusbirth_cat +
                               granddamstatusbirth_cat +
                               prophorizontalrelsstatus2birth_cat +
                               prophorizontalrelsstatus3birth_cat +
                               
                               (1|Farm),
                             family = "binomial",
                             data = data_modelling_train,
                             control = glmerControl(optimizer = "bobyqa",
                                                    optCtrl=list(maxfun=100000)))
}

if(modeltorun == "priorglmm_birth_5"){
  
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatusbirth_cat +
                               granddamstatusbirth_cat +
                               prophorizontalrelsstatus2birth_recat +
                               prophorizontalrelsstatus3birth_recat +
                               
                               (1|Farm),
                             family = "binomial",
                             data = data_modelling_train,
                             control = glmerControl(optimizer = "bobyqa",
                                                    optCtrl=list(maxfun=100000)))
}
  
if(modeltorun == "priorglmm_birth_6"){
  
  
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatusbirth_cat * damntestsbirth +
                               granddamstatusbirth_cat *granddamntestsbirth +
                               prophorizontalrelsstatus2birth_recat +
                               prophorizontalrelsstatus3birth_recat +
                               
                               (1|Farm),
                             family = "binomial",
                             data = data_modelling_train,
                             control = glmerControl(optimizer = "bobyqa",
                                                    optCtrl=list(maxfun=100000)))
}
  
if(modeltorun == "priorglmm_birth_7"){
  
  
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatusbirth_cat * damntestsbirth +
                               granddamstatusbirth_cat *granddamntestsbirth +
                               prophorizontalrelsstatus2birth_recat +
                               prophorizontalrelsstatus3birth_recat +
                               propproximaldamsstatus2birth +
                               propproximaldamsstatus3birth +
                               (1|Farm),
                             family = "binomial",
                             data = data_modelling_train,
                             control = glmerControl(optimizer = "bobyqa",
                                                    optCtrl=list(maxfun=100000)))
}

if(modeltorun == "priorglmm_birth_8"){
  
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatusbirth_cat * damntestsbirth +
                               granddamstatusbirth_cat *granddamntestsbirth +
                               prophorizontalrelsstatus2birth_recat +
                               prophorizontalrelsstatus3birth_recat +
                               propvaguelyproximaldamsstatus2birth +
                               propvaguelyproximaldamsstatus3birth +
                               (1|Farm),
                             family = "binomial",
                             data = data_modelling_train,
                             control = glmerControl(optimizer = "bobyqa",
                                                    optCtrl=list(maxfun=100000)))
}
  
if(modeltorun == "priorglmm_birth_9"){
  
  
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatusbirth_cat * damntestsbirth +
                               granddamstatusbirth_cat *granddamntestsbirth +
                               prophorizontalrelsstatus2birth_recat +
                               prophorizontalrelsstatus3birth_recat +
                               propvaguelyproximaldamsstatus2birth +
                               propvaguelyproximaldamsstatus3birth +
                               propposavg_cat +
                               (1|Farm),
                             family = "binomial",
                             data = data_modelling_train,
                             control = glmerControl(optimizer = "bobyqa",
                                                    optCtrl=list(maxfun=100000)))
}
  
if(modeltorun == "priorglmm_birth_10"){
  
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatusbirth_cat * damntestsbirth +
                                granddamstatusbirth_cat *granddamntestsbirth +
                                prophorizontalrelsstatus2birth_recat +
                                prophorizontalrelsstatus3birth_recat +
                                propvaguelyproximaldamsstatus2birth +
                                propvaguelyproximaldamsstatus3birth +
                                meantitreavg_cat +
                                (1|Farm),
                              family = "binomial",
                              data = data_modelling_train,
                              control = glmerControl(optimizer = "bobyqa",
                                                     optCtrl=list(maxfun=100000)))
}

if(modeltorun == "priorglmm_birth_11"){
  
  
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatusbirth_cat * damntestsbirth +
                                granddamstatusbirth_cat *granddamntestsbirth +
                                prophorizontalrelsstatus2birth_recat +
                                prophorizontalrelsstatus3birth_recat +
                                propvaguelyproximaldamsstatus2birth +
                                propvaguelyproximaldamsstatus3birth +
                                meantitrenegcowsavg_cat +
                                (1|Farm),
                              family = "binomial",
                              data = data_modelling_train,
                              control = glmerControl(optimizer = "bobyqa",
                                                     optCtrl=list(maxfun=100000)))
}

if(modeltorun == "priorglmm_birth_12"){
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatusbirth_cat * damntestsbirth +
                                granddamstatusbirth_cat *granddamntestsbirth +
                                prophorizontalrelsstatus2birth_recat +
                                prophorizontalrelsstatus3birth_recat +
                                propvaguelyproximaldamsstatus2birth +
                                propvaguelyproximaldamsstatus3birth +
                                meantitrenegcowsavg_recat +
                                (1|Farm),
                              family = "binomial",
                              data = data_modelling_train,
                              control = glmerControl(optimizer = "bobyqa",
                                                     optCtrl=list(maxfun=100000)))
}
  
if(modeltorun == "priorglmm_birth_13"){
  
  
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatusbirth_cat * damntestsbirth +
                                granddamstatusbirth_cat *granddamntestsbirth +
                                propsiblingsstatus2birth +
                                propvaguelyproximaldamsstatus2birth +
                                propvaguelyproximaldamsstatus3birth +
                                meantitrenegcowsavg_recat +
                                (1|Farm),
                              family = "binomial",
                              data = data_modelling_train,
                              control = glmerControl(optimizer = "bobyqa",
                                                     optCtrl=list(maxfun=100000)))
}
  
if(modeltorun == "priorglmm_birth_14"){
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatusbirth_cat * damntestsbirth +
                                granddamstatusbirth_cat *granddamntestsbirth +
                                propsiblingsstatus2birth_cat +
                                propvaguelyproximaldamsstatus2birth +
                                propvaguelyproximaldamsstatus3birth +
                                meantitrenegcowsavg_recat +
                                (1|Farm),
                              family = "binomial",
                              data = data_modelling_train,
                              control = glmerControl(optimizer = "bobyqa",
                                                     optCtrl=list(maxfun=100000)))
  
}
  
if(modeltorun == "priorglmm_birth_15"){
  
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatusbirth_cat * damntestsbirth +
                                granddamstatusbirth_cat *granddamntestsbirth +
                                propsiblingsstatus2birth +
                                propsiblingsstatus3birth +
                                propvaguelyproximaldamsstatus2birth +
                                propvaguelyproximaldamsstatus3birth +
                                meantitrenegcowsavg_recat +
                                (1|Farm),
                              family = "binomial",
                              data = data_modelling_train,
                              control = glmerControl(optimizer = "bobyqa",
                                                     optCtrl=list(maxfun=100000)))
}

if(modeltorun == "priorglmm_birth_16"){
  
  
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatusbirth_cat * damntestsbirth +
                                granddamstatusbirth_cat *granddamntestsbirth +
                                propsiblingsstatus2birth +
                                propsiblingsstatus3birth +
                                propauntsstatus2birth +
                                propvaguelyproximaldamsstatus2birth +
                                propvaguelyproximaldamsstatus3birth +
                                meantitrenegcowsavg_recat +
                                (1|Farm),
                              family = "binomial",
                              data = data_modelling_train,
                              control = glmerControl(optimizer = "bobyqa",
                                                     optCtrl=list(maxfun=100000)))
}
  
if(modeltorun == "priorglmm_birth_17"){
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatusbirth_cat * damntestsbirth +
                                granddamstatusbirth_cat *granddamntestsbirth +
                                propsiblingsstatus2birth +
                                propsiblingsstatus3birth +
                                propauntsstatus3birth +
                                propvaguelyproximaldamsstatus2birth +
                                propvaguelyproximaldamsstatus3birth +
                                meantitrenegcowsavg_recat +
                                (1|Farm),
                              family = "binomial",
                              data = data_modelling_train,
                              control = glmerControl(optimizer = "bobyqa",
                                                     optCtrl=list(maxfun=100000)))
}
 
if(modeltorun == "priorglmm_birth_18"){
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatusbirth_cat * damntestsbirth +
                                propsiblingsstatus2birth +
                                propsiblingsstatus3birth +
                                propvaguelyproximaldamsstatus2birth +
                                propvaguelyproximaldamsstatus3birth +
                                meantitrenegcowsavg_recat +
                                (1|Farm),
                              family = "binomial",
                              data = data_modelling_train,
                              control = glmerControl(optimizer = "bobyqa",
                                                     optCtrl=list(maxfun=100000)))
}
  
if(modeltorun == "priorglmm_birth_19"){
  
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatusbirth_cat * damntestsbirth +
                                prophorizontalrelsstatus2birth +
                                prophorizontalrelsstatus3birth +
                                propvaguelyproximaldamsstatus2birth +
                                propvaguelyproximaldamsstatus3birth +
                                meantitrenegcowsavg_recat +
                                (1|Farm),
                              family = "binomial",
                              data = data_modelling_train,
                              control = glmerControl(optimizer = "bobyqa",
                                                     optCtrl=list(maxfun=100000)))
}
  
if(modeltorun == "priorglmm_birth_20"){
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatusbirth_cat * damntestsbirth +
                                prophorizontalrelsstatus2birth +
                                propvaguelyproximaldamsstatus2birth +
                                propvaguelyproximaldamsstatus3birth +
                                meantitrenegcowsavg_recat +
                                (1|Farm),
                              family = "binomial",
                              data = data_modelling_train,
                              control = glmerControl(optimizer = "bobyqa",
                                                     optCtrl=list(maxfun=100000)))
}
  
if(modeltorun == "priorglmm_birth_21"){
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatusbirth_cat * damntestsbirth +
                                prophorizontalrelsstatus2birth_cat +
                                propvaguelyproximaldamsstatus2birth +
                                propvaguelyproximaldamsstatus3birth +
                                meantitrenegcowsavg_recat +
                                (1|Farm),
                              family = "binomial",
                              data = data_modelling_train,
                              control = glmerControl(optimizer = "bobyqa",
                                                     optCtrl=list(maxfun=100000)))  
}
  
if(modeltorun == "priorglmm_12mold_1"){
  
   fittedmod <- glmer(Target_altdef1_strictneg ~ damstatus12mold_cat +
                               (1|Farm),
                               family ="binomial",
                               data = data_modelling_train,
                               control = glmerControl(optimizer = "bobyqa",
                                                      optCtrl = list(maxfun = 100000)))
}
  
if(modeltorun == "priorglmm_12mold_2"){
   
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatus12mold_cat * damntests12mold +
                               (1|Farm),
                               family ="binomial",
                               data = data_modelling_train,
                               control = glmerControl(optimizer = "bobyqa",
                                                      optCtrl = list(maxfun = 100000)))
}
  
if(modeltorun == "priorglmm_12mold_3"){
   
   
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatus12mold_cat * damntests12mold +
                               granddamstatus12mold_cat +
                               (1|Farm),
                               family ="binomial",
                               data = data_modelling_train,
                               control = glmerControl(optimizer = "bobyqa",
                                                      optCtrl = list(maxfun = 100000)))
}

if(modeltorun == "priorglmm_12mold_4"){
   
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatus12mold_cat * damntests12mold +
                               granddamstatus12mold_cat * granddamntests12mold +
                               (1|Farm),
                               family ="binomial",
                               data = data_modelling_train,
                               control = glmerControl(optimizer = "bobyqa",
                                                      optCtrl = list(maxfun = 100000)))
}
  
if(modeltorun == "priorglmm_12mold_5"){
   
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatus12mold_cat * damntests12mold +
                               granddamstatus12mold_cat * granddamntests12mold +
                               prophorizontalrelsstatus212mold_cat +
                               prophorizontalrelsstatus312mold_cat +
                               (1|Farm),
                               family ="binomial",
                               data = data_modelling_train,
                               control = glmerControl(optimizer = "bobyqa",
                                                      optCtrl = list(maxfun = 100000)))
}
  
if(modeltorun == "priorglmm_12mold_6"){
   
   
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatus12mold_cat * damntests12mold +
                               granddamstatus12mold_cat * granddamntests12mold +
                               prophorizontalrelsstatus212mold_cat +
                               (1|Farm),
                               family ="binomial",
                               data = data_modelling_train,
                               control = glmerControl(optimizer = "bobyqa",
                                                      optCtrl = list(maxfun = 100000)))
}

if(modeltorun == "priorglmm_12mold_7"){
   
   
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatus12mold_cat * damntests12mold +
                               granddamstatus12mold_cat * granddamntests12mold +
                               propsiblingsstatus212mold +
                               (1|Farm),
                               family ="binomial",
                               data = data_modelling_train,
                               control = glmerControl(optimizer = "bobyqa",
                                                      optCtrl = list(maxfun = 100000)))
}
   
if(modeltorun == "priorglmm_12mold_8"){
   
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatus12mold_cat * damntests12mold +
                               granddamstatus12mold_cat * granddamntests12mold +
                               propsiblingsstatus212mold +
                               propsiblingsstatus312mold +
                               (1|Farm),
                               family ="binomial",
                               data = data_modelling_train,
                               control = glmerControl(optimizer = "bobyqa",
                                                      optCtrl = list(maxfun = 100000)))
}
  
if(modeltorun == "priorglmm_12mold_9"){
   
   
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatus12mold_cat * damntests12mold +
                               granddamstatus12mold_cat * granddamntests12mold +
                               propsiblingsstatus212mold +
                               propauntsstatus212mold +
                               (1|Farm),
                               family ="binomial",
                               data = data_modelling_train,
                               control = glmerControl(optimizer = "bobyqa",
                                                      optCtrl = list(maxfun = 100000)))
}
  
if(modeltorun == "priorglmm_12mold_10"){
   
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatus12mold_cat * damntests12mold +
                                granddamstatus12mold_cat * granddamntests12mold +
                                propsiblingsstatus212mold +
                                propvaguelyproximaldamsstatus212mold +
                                (1|Farm),
                                family ="binomial",
                                data = data_modelling_train,
                                control = glmerControl(optimizer = "bobyqa",
                                                       optCtrl = list(maxfun = 100000)))
}
  
if(modeltorun == "priorglmm_12mold_11"){
   
   
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatus12mold_cat * damntests12mold +
                                granddamstatus12mold_cat * granddamntests12mold +
                                propsiblingsstatus212mold +
                                propproximaldamsstatus212mold +
                                (1|Farm),
                                family ="binomial",
                                data = data_modelling_train,
                                control = glmerControl(optimizer = "bobyqa",
                                                       optCtrl = list(maxfun = 100000)))
   
}
  
if(modeltorun == "priorglmm_12mold_12"){
   
   
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatus12mold_cat * damntests12mold +
                                granddamstatus12mold_cat * granddamntests12mold +
                                propsiblingsstatus212mold +
                                propvaguelyproximaldamsstatus212mold +
                                propvaguelyproximaldamsstatus312mold +
                                (1|Farm),
                                family ="binomial",
                                data = data_modelling_train,
                                control = glmerControl(optimizer = "bobyqa",
                                                       optCtrl = list(maxfun = 100000)))
}
  
if(modeltorun == "priorglmm_12mold_13"){
   
   
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatus12mold_cat * damntests12mold +
                                granddamstatus12mold_cat * granddamntests12mold +
                                propsiblingsstatus212mold +
                                propvaguelyproximaldamsstatus212mold +
                                propvaguelyproximaldamsstatus312mold +
                                meantitrenegcowsavg_recat +
                                (1|Farm),
                                family ="binomial",
                                data = data_modelling_train,
                                control = glmerControl(optimizer = "bobyqa",
                                                       optCtrl = list(maxfun = 100000)))
}
  
if(modeltorun == "priorglmm_12mold_14"){
   
  fittedmod <- glmer(Target_altdef1_strictneg ~ damstatus12mold_cat * damntests12mold +
                                granddamstatus12mold_cat * granddamntests12mold +
                                prophorizontalrelsstatus212mold +
                                propvaguelyproximaldamsstatus212mold +
                                propvaguelyproximaldamsstatus312mold +
                                meantitrenegcowsavg_recat +
                                (1|Farm),
                                family ="binomial",
                                data = data_modelling_train,
                                control = glmerControl(optimizer = "bobyqa",
                                                       optCtrl = list(maxfun = 100000)))
}
  
if(modeltorun == "priorglmm_12mold_15"){
   
   
   fittedmod <- glmer(Target_altdef1_strictneg ~ damstatus12mold_cat * damntests12mold +
                                granddamstatus12mold_cat * granddamntests12mold +
                                prophorizontalrelsstatus212mold +
                                prophorizontalrelsstatus312mold +
                                propvaguelyproximaldamsstatus212mold +
                                propvaguelyproximaldamsstatus312mold +
                                meantitrenegcowsavg_recat +
                                (1|Farm),
                                family ="binomial",
                                data = data_modelling_train,
                                control = glmerControl(optimizer = "bobyqa",
                                                       optCtrl = list(maxfun = 100000)))
}

if(modeltorun == "priorglmm_crt_1") { 
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damcrtstatus_cat * damntestscrt +
                             (1|Farm),
                           family ="binomial",
                           data = data_modelling_train,
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl = list(maxfun = 100000)))
}
  
if(modeltorun == "priorglmm_crt_2"){
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damcrtstatus_cat * damntestscrt +
                             granddamcrtstatus_cat * granddamntestscrt +
                             (1|Farm),
                           family ="binomial",
                           data = data_modelling_train,
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl = list(maxfun = 100000)))
}
  
if(modeltorun == "priorglmm_crt_3"){
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damcrtstatus_cat * damntestscrt +
                             granddamcrtstatus_cat * granddamntestscrt +
                             prophorizontalrelsstatus2crt_cat +
                             (1|Farm),
                           family ="binomial",
                           data = data_modelling_train,
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl = list(maxfun = 100000)))
}
  
if(modeltorun == "priorglmm_crt_3"){
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damcrtstatus_cat * damntestscrt +
                             granddamcrtstatus_cat * granddamntestscrt +
                             prophorizontalrelsstatus2crt_cat +
                             (1|Farm),
                           family ="binomial",
                           data = data_modelling_train,
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl = list(maxfun = 100000)))
}
  
if(modeltorun == "priorglmm_crt_4"){
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damcrtstatus_cat * damntestscrt +
                             granddamcrtstatus_cat * granddamntestscrt +
                             prophorizontalrelsstatus2crt_recat +
                             (1|Farm),
                           family ="binomial",
                           data = data_modelling_train,
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl = list(maxfun = 100000)))
}
  
if(modeltorun == "priorglmm_crt_5"){
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damcrtstatus_cat * damntestscrt +
                             granddamcrtstatus_cat * granddamntestscrt +
                             prophorizontalrelsstatus2crt +
                             (1|Farm),
                           family ="binomial",
                           data = data_modelling_train,
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl = list(maxfun = 100000)))
}
  
if(modeltorun == "priorglmm_crt_6"){
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damcrtstatus_cat * damntestscrt +
                             granddamcrtstatus_cat * granddamntestscrt +
                             prophorizontalrelsstatus2crt_recat +
                             prophorizontalrelsstatus3crt_cat +
                             (1|Farm),
                           family ="binomial",
                           data = data_modelling_train,
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl = list(maxfun = 100000)))
}
  
if(modeltorun == "priorglmm_crt_7"){
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damcrtstatus_cat * damntestscrt +
                             granddamcrtstatus_cat * granddamntestscrt +
                             propsiblingsstatus2crt +
                             (1|Farm),
                           family ="binomial",
                           data = data_modelling_train,
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl = list(maxfun = 100000)))
}
  
if(modeltorun == "priorglmm_crt_8"){
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damcrtstatus_cat * damntestscrt +
                             granddamcrtstatus_cat * granddamntestscrt +
                             propsiblingsstatus2crt_cat +
                             (1|Farm),
                           family ="binomial",
                           data = data_modelling_train,
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl = list(maxfun = 100000)))
}
  
if(modeltorun == "priorglmm_crt_9"){
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damcrtstatus_cat * damntestscrt +
                             granddamcrtstatus_cat * granddamntestscrt +
                             prophorizontalrelsstatus2crt_recat +
                             prophorizontalrelsstatus3crt_recat +
                             (1|Farm),
                           family ="binomial",
                           data = data_modelling_train,
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl = list(maxfun = 100000)))
}
  
if(modeltorun == "priorglmm_crt_10"){
  
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damcrtstatus_cat * damntestscrt +
                              granddamcrtstatus_cat * granddamntestscrt +
                              prophorizontalrelsstatus2crt_recat +
                              prophorizontalrelsstatus3crt_recat +
                              propproximaldamsstatus2crt +
                              (1|Farm),
                            family ="binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl = list(maxfun = 100000)))
  
}
  
if(modeltorun == "priorglmm_crt_11"){
  
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damcrtstatus_cat * damntestscrt +
                              granddamcrtstatus_cat * granddamntestscrt +
                              prophorizontalrelsstatus2crt_recat +
                              prophorizontalrelsstatus3crt_recat +
                              propvaguelyproximaldamsstatus2crt +
                              (1|Farm),
                            family ="binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl = list(maxfun = 100000)))
  
}
  
if(modeltorun == "priorglmm_crt_12"){
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damcrtstatus_cat * damntestscrt +
                              granddamcrtstatus_cat * granddamntestscrt +
                              prophorizontalrelsstatus2crt_recat +
                              prophorizontalrelsstatus3crt_recat +
                              propvaguelyproximaldamsstatus2crt +
                              propvaguelyproximaldamsstatus3crt +
                              (1|Farm),
                            family ="binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl = list(maxfun = 100000)))
}
  
if(modeltorun == "priorglmm_crt_13"){
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damcrtstatus_cat * damntestscrt +
                              granddamcrtstatus_cat * granddamntestscrt +
                              prophorizontalrelsstatus2crt_recat +
                              prophorizontalrelsstatus3crt_recat +
                              propvaguelyproximaldamsstatus2crt +
                              propvaguelyproximaldamsstatus3crt +
                              propproximalcalvesstatus2crt +
                              (1|Farm),
                            family ="binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl = list(maxfun = 100000)))
}
  
if(modeltorun == "priorglmm_crt_14"){
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damcrtstatus_cat * damntestscrt +
                              granddamcrtstatus_cat * granddamntestscrt +
                              prophorizontalrelsstatus2crt_recat +
                              prophorizontalrelsstatus3crt_recat +
                              propvaguelyproximaldamsstatus2crt +
                              propvaguelyproximaldamsstatus3crt +
                              propvaguelyproximalcalvesstatus2crt +
                              (1|Farm),
                            family ="binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl = list(maxfun = 100000)))
  
}
  
if(modeltorun == "priorglmm_crt_15"){
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damcrtstatus_cat * damntestscrt +
                              granddamcrtstatus_cat * granddamntestscrt +
                              prophorizontalrelsstatus2crt_recat +
                              prophorizontalrelsstatus3crt_recat +
                              propvaguelyproximaldamsstatus2crt +
                              propvaguelyproximaldamsstatus3crt +
                              propvaguelyproximalcalvesstatus2crt +
                              propproximalcalvesstatus3crt +
                              (1|Farm),
                            family ="binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl = list(maxfun = 100000)))
  
}
  
if(modeltorun == "priorglmm_crt_16"){
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damcrtstatus_cat * damntestscrt +
                              granddamcrtstatus_cat * granddamntestscrt +
                              prophorizontalrelsstatus2crt_recat +
                              prophorizontalrelsstatus3crt_recat +
                              propvaguelyproximaldamsstatus2crt +
                              propvaguelyproximaldamsstatus3crt +
                              propvaguelyproximalcalvesstatus2crt +
                              propvaguelyproximalcalvesstatus3crt +
                              (1|Farm),
                            family ="binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl = list(maxfun = 100000)))
}
  
if(modeltorun == "priorglmm_crt_17"){
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damcrtstatus_cat * damntestscrt +
                              granddamcrtstatus_cat * granddamntestscrt +
                              prophorizontalrelsstatus2crt_recat +
                              prophorizontalrelsstatus3crt_recat +
                              propvaguelyproximaldamsstatus2crt +
                              propvaguelyproximaldamsstatus3crt +
                              propvaguelyproximalcalvesstatus2crt +
                              propvaguelyproximalcalvesstatus3crt +
                              meantitrenegcowsavg_recat +
                              (1|Farm),
                            family ="binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl = list(maxfun = 100000)))
}
  
if(modeltorun == "priorglmm_crt_18"){
  
  
  fittedmod <- glmer(Target_altdef1_strictneg ~ damcrtstatus_cat * damntestscrt +
                              prophorizontalrelsstatus2crt_recat +
                              prophorizontalrelsstatus3crt_recat +
                              propvaguelyproximaldamsstatus2crt +
                              propvaguelyproximaldamsstatus3crt +
                              propvaguelyproximalcalvesstatus2crt +
                              propvaguelyproximalcalvesstatus3crt +
                              meantitrenegcowsavg_recat +
                              (1|Farm),
                            family ="binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl = list(maxfun = 100000)))
}
  
  
  
  
  
return(fittedmod)
  
}
  

for(m in 1:length(priormods)){
  assign(priormods[m], fittedmods[[m]])
}




  

match("priorglmm_crt_15", priormods)

#Retrieve model from list


for(m in priormods){

mod <- fittedmods[match(m, priormods)]

testpred <- unlist(predict(mod, newdata = data_modelling_test, type = "response", allow.new.levels = T))

CaliPlot(testpred, data_modelling_test$Target_altdef1_strictneg, nbins = 10, ptitle = m, psubtitle = "External Testing Data")

ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/",m,"External.png"))

}

aicmods_birth <- c("priorglmm_birth_1",
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
             "priorglmm_birth_21")

glmmaictable_birth <- data.frame(Model = aicmods_birth)
glmmaictable_birth$AIC <- 0

for (i in 1:nrow(glmmaictable_birth)){
  glmmaictable_birth$AIC[i] <- AIC(get(glmmaictable_birth$Model[i]))
  glmmaictable_birth$features[i] <- toString(terms(get(glmmaictable_birth$Model[i]))[[3]])
}

glmmaictable_birth <- glmmaictable_birth[order(glmmaictable_birth$AIC),]

View(glmmaictable_birth)


aicmods_12mold <- c("priorglmm_12mold_1",
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
             "priorglmm_12mold_15")

glmmaictable_12mold <- data.frame(Model = aicmods_12mold)
glmmaictable_12mold$AIC <- 0

for (i in 1:nrow(glmmaictable_12mold)){
  glmmaictable_12mold$AIC[i] <- AIC(get(glmmaictable_12mold$Model[i]))
  glmmaictable_12mold$features[i] <- toString(terms(get(glmmaictable_12mold$Model[i]))[[3]])
}

glmmaictable_12mold <- glmmaictable_12mold[order(glmmaictable_12mold$AIC),]

View(glmmaictable_12mold)









aicmods_crt <- c("priorglmm_crt_1",
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

glmmaictable_crt <- data.frame(Model = aicmods_crt)
glmmaictable_crt$AIC <- 0

for (i in 1:nrow(glmmaictable_crt)){
  glmmaictable_crt$AIC[i] <- AIC(get(glmmaictable_crt$Model[i]))
  glmmaictable_crt$features[i] <- toString(terms(get(glmmaictable_crt$Model[i]))[[3]])
}

glmmaictable_crt <- glmmaictable_crt[order(glmmaictable_crt$AIC),]

View(glmmaictable_crt)




data_modelling_train$prophorizontalrelsstatus2crt_cat_collapsed <- data_modelling_train$prophorizontalrelsstatus2crt_cat
levels(data_modelling_train$prophorizontalrelsstatus2crt_cat_collapsed) <- c(levels(data_modelling_train$prophorizontalrelsstatus2crt_cat_collapsed), "(0.333,1]")
data_modelling_train$prophorizontalrelsstatus2crt_cat_collapsed[data_modelling_train$prophorizontalrelsstatus2crt_cat_collapsed == "(0.333,0.667]" |
                                                                  data_modelling_train$prophorizontalrelsstatus2crt_cat_collapsed == "(0.667,1]"] <- "(0.333,1]"
data_modelling_train$prophorizontalrelsstatus2crt_cat_collapsed <- droplevels(data_modelling_train$prophorizontalrelsstatus2crt_cat_collapsed, c("(0.333,0.667]", "(0.667,1]"))




data_modelling_train$propposavg_recat <- cut(data_modelling_train$propposavg, breaks = c(-0.00001, 0.02,0.04,0.5))
levels(data_modelling_train$propposavg_recat) <- c(levels(data_modelling_train$propposavg_recat), "Missing")
data_modelling_train$propposavg_recat[is.na(data_modelling_train$propposavg_recat)] <- "Missing"
