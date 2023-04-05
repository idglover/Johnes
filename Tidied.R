#####READ AND TIDY DATA####

data <- read.csv("y:/ian/johnesthresholds/johnesproper/data/CombinedData/Combined_RSQL.csv")

data <- tidyr::unite(data, covsandtitre1, c(58:68), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre2, c(59:69), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre3, c(60:70), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre4, c(61:71), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre5, c(62:72), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre6, c(63:73), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre7, c(64:74), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre8, c(65:75), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre9, c(66:76), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre10, c(67:77), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre11, c(68:78), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre12, c(69:79), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre13, c(70:80), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre14, c(71:81), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre15, c(72:82), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre16, c(73:83), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre17, c(74:84), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre18, c(75:85), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre19, c(76:86), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre20, c(77:87), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre21, c(78:88), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre22, c(79:89), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre23, c(80:90), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre24, c(81:91), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre25, c(82:92), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre26, c(83:93), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre27, c(84:94), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre28, c(85:95), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre29, c(86:96), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre30, c(87:97), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre31, c(88:98), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre32, c(89:99), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre33, c(90:100), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre34, c(91:101), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre35, c(92:102), remove = TRUE, sep = ":")

#Lubridate

data$calfdob <- ymd(data$calfdob)
data$latesttestdate <- ymd(data$latesttestdate)
data$nexttestdate <- ymd(data$nexttestdate)

#####CLEAN AND FORMAT INDIVIDUAL PREDICTORS######

data <- data[!is.na(data$calfeartag),]
data <- data[data$calfdob != "1985-01-01",]

data$testdobinterval <- as.integer(data$testdobinterval)
data$ntestedlatest <- as.integer(data$ntestedlatest)
data$nrecordedlatest <- as.integer(data$nrecordedlatest)

data <- data[data$nsiblings < 18,]
data <- data[data$naunts < 9,]
data <- data[data$ngreataunts <13,]

data$propposavg_cat <- cut(data$propposavg, breaks = 4)
levels(data$propposavg_cat) <- c(levels(data$propposavg_cat), "Missing")
data$propposavg_cat[is.na(data$propposavg_cat)] <- "Missing"

data$meantitreavg_cat <- cut(data$meantitreavg, breaks = 4)
levels(data$meantitreavg_cat) <- c(levels(data$meantitreavg_cat), "Missing")
data$meantitreavg_cat[is.na(data$meantitreavg_cat)] <- "Missing"

data$meantitrenegcowsavg_cat <- cut(data$meantitrenegcowsavg, breaks = 4)
levels(data$meantitrenegcowsavg_cat) <- c(levels(data$meantitrenegcowsavg_cat), "Missing")
data$meantitrenegcowsavg_cat[is.na(data$meantitrenegcowsavg_cat)] <- "Missing"

data$damstatusbirth_cat <- as.factor(data$damstatusbirth)
levels(data$damstatusbirth_cat) <- c(levels(data$damstatusbirth_cat), "Missing", "Other")
data$damstatusbirth_cat[is.na(data$damstatusbirth_cat)] <- "Missing"
data$damstatusbirth_cat[data$damstatusbirth_cat != "1" & data$damstatusbirth_cat != "2" & data$damstatusbirth_cat != "3"] <- "Other"
data$damstatusbirth_cat <- droplevels(data$damstatusbirth_cat, c("4", "5", "6", "8"))

data$granddamstatusbirth_cat <- as.factor(data$granddamstatusbirth)
levels(data$granddamstatusbirth_cat) <- c(levels(data$granddamstatusbirth_cat), "Missing", "Other")
data$granddamstatusbirth_cat[is.na(data$granddamstatusbirth_cat)] <- "Missing"
data$granddamstatusbirth_cat[data$granddamstatusbirth_cat != "1" & data$granddamstatusbirth_cat != "2" & data$granddamstatusbirth_cat != "3"] <- "Other"
data$granddamstatusbirth_cat <- droplevels(data$granddamstatusbirth_cat, c("4", "5", "6", "8"))



print(paste("Full dataset (after cleaning):",dim(data)))

######CREATE TARGET#####


data <- data[,c(4,110:112,29,32,33,113,114,36,39,40,41,44,45,46,47,48,51,52,53,56,57,58:109)]

data$Target_Meyer <- ifelse(data$ntests >= 9 & grepl("H", substr(data$profile, nchar(data$profile)-7, nchar(data$profile))) == FALSE, "0", "U")
data$Target_Meyer <- ifelse(data$ntests >= 3 &  substr(data$profile, nchar(data$profile) - 1, nchar(data$profile)) == "HH", "1", data$Target_Meyer)
data$Target_Meyer <- as.factor(data$Target_Meyer)

#data$Target_Strict1 <- ifelse(data$ntests >= 9 & grepl("H", 1, nchar(data$profile)) == FALSE, "0", "U")
#data$Target_Strict1 <- ifelse(data$ntests >= 3 &  substr(data$profile, nchar(data$profile) - 1, nchar(data$profile)) == "HH", "1", data$Target_Strict1)
#data$Target_Strict1 <- as.factor(data$Target_Strict1)

data <- data[!is.na(data$calfeartag),]

#######BIRTH PRIOR MODELS######

data_modelling <- data[,c(2:23,76)]

data_modelling <- data_modelling[data_modelling$Target_Meyer != "U",]

data_modelling$Target_Meyer <- droplevels(data_modelling$Target_Meyer, "U")

set.seed(1981)
trainRowNumbers <- createDataPartition(data_modelling$Target_Meyer, p = 0.8, list = FALSE)
data_modelling_train <- data_modelling[trainRowNumbers,]
data_modelling_test <- data_modelling[-trainRowNumbers,]


print(paste("Trainset:",dim(data_modelling_train)))
print(paste("Testset:",dim(data_modelling_test)))

cont <- trainControl(method = "repeatedcv", number = 10, repeats = 5, allowParallel = TRUE, verbose = FALSE)

cl <- makePSOCKcluster(5)
registerDoParallel(cl)
cat("No. Cores:",getDoParWorkers())

model_LR_1 <- train(Target_Meyer ~ .,                                                   
                    data = data_modelling_train, trControl = cont,
                    method = "glm",
                    family = "binomial")

model_RegLR_1 <- train(Target_Meyer ~ .,                                                   
                       data = data_modelling_train, trControl = cont,
                       method = "glmnet",
                       family = "binomial")

model_MARS_1 <- train(Target_Meyer ~ ., data = data_modelling_train, trControl = cont, 
                      method = "earth")

stopCluster(cl)

data_modelling_test$predLR <- predict(model_LR_1, newdata = data_modelling_test, type = "prob")[,2]

data_modelling_test$predRegLR <- predict(model_RegLR_1, newdata = data_modelling_test, type = "prob")[,2]

data_modelling_test$predMARS <- predict(model_MARS_1, newdata = data_modelling_test, type = "prob")[,2]

CaliPlot(data_modelling_test$predLR, data_modelling_test$Target_Meyer, nbins = 5, ptitle  = "LogReg")

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/LogRegCaliCurve.png")

CaliPlot(data_modelling_test$predRegLR, data_modelling_test$Target_Meyer, nbins = 5, ptitle  = "LogReg")

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/RegLRCaliCurve.png")

CaliPlot(data_modelling_test$predMARS, data_modelling_test$Target_Meyer, nbins = 5, ptitle  = "MARS")

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/MARSCaliCurve.png")


data <- tidyr::gather(data, covsandtitre, value, -c(calfeartag,
                                                    propposavg_cat,
                                                    meantitreavg_cat,
                                                    meantitrenegcowsavg_cat,
                                                    nsiblings,
                                                    propsiblingsstatus2birth,
                                                    propsiblingsstatus3birth,
                                                    damstatusbirth_cat,
                                                    granddamstatusbirth_cat,
                                                    naunts,
                                                    propauntsstatus2birth,
                                                    propauntsstatus3birth,
                                                    ngreataunts,
                                                    propgreatauntsstatus2birth,
                                                    propgreatauntsstatus3birth,
                                                    nproximalcalves,
                                                    nvaguelyproximalcalves,
                                                    nproximaldams,
                                                    propproximaldamsstatus2birth,
                                                    propproximaldamsstatus3birth,
                                                    nvaguelyproximaldams,
                                                    propvaguelyproximaldamsstatus2birth,
                                                    propvaguelyproximaldamsstatus3birth,
                                                    profile,
                                                    ntests,
                                                    ageatfirsttest,
                                                    ageatlasttest,
                                                    testingperiod,
                                                    nonehigh,
                                                    Hanypoint,
                                                    HHanypoint,
                                                    HLHanypoint,
                                                    HMHanypoint,
                                                    ageatfirstH,
                                                    ageatfirstHH,
                                                    ageatfirstHMH,
                                                    ageatfirstHLH,
                                                    posstatusanypoint,
                                                    ageatfirstposstatus,
                                                    Farm,
                                                    Target_Meyer))



data <- tidyr::separate(data, value, c("date", "age", "parity", "dim", "yield", "butterfat", "protein", "lactose", "cellcount", "titre","class"), sep = ":")

data <- data[data$age != "NA",]

data <- data[
  with(data, order(calfeartag, date)),
]

data$predLR <- predict(model_LR_1, newdata = data, type = "prob")[,2]

data$predRegLR <- predict(model_RegLR_1, newdata = data, type = "prob")[,2]

data$predMARS <- predict(model_MARS_1, newdata = data, type = "prob")[,2]

data$birthpriorLR <- data$predLR / (1 - data$predLR) #Prior ODDS at birth

data$birthpriorRegLR <- data$predRegLR / (1 - data$predRegLR) 

data$birthpriorMARS <- data$predMARS / (1 - data$predMARS)

hist(data$predLR, main = "Logistic Regression Birth Prior Model")



#####LIKELIHOOD MODELS######

summary(data$Target_Meyer)

data$titre <- as.numeric(data$titre)

data$age_cat <- cut(as.numeric(data$age), breaks = c(0,30,35,40,45,50,75,300))

summary(data$age_cat)

cutpoints <- c(0,10,20,30,60,100)

data$cutpoint <- length(cutpoints)

summary(data$cutpoint)

cl <- makePSOCKcluster(5)
registerDoParallel(cl)
cat("No. Cores:",getDoParWorkers())

for (i in 1:nrow(data)){
  for (j in 1:length(cutpoints)-1){
    if(data$titre[i] >= cutpoints[j] && data$titre[i] < cutpoints[j+1]){data$cutpoint[i] <- j}
  }
}


head(cbind(data$titre, data$cutpoint))

stopCluster(cl)
data$cutpoint <- as.factor(data$cutpoint)

summary(data$cutpoint)

for (i in 1:length(unique(data$age_cat))){
  for (j in 1:length(unique(data$cutpoint))){
    print(paste(i,j,nrow(data[data$age_cat == unique(data$age_cat)[i] & data$cutpoint == unique(data$cutpoint)[j],])))
  }
}

####Create data for multinomial model (NNET Package)

data_multinom <- data[data$Target_Meyer != "U",]

data_multinom$Target_Meyer <- droplevels(data_multinom$Target_Meyer, "U")

#Remove final positive results of Target_Meyer positive cows

#data_multinom <- data_multinom[data_multinom$ageatfirstposstatus == 0 | data_multinom$age < data_multinom$ageatfirstposstatus,]

data_multinom$Target_Meyer <- as.factor(data_multinom$Target_Meyer)

summary(data_multinom$Target_Meyer)

cl <- makePSOCKcluster(5)
registerDoParallel(cl)
getDoParWorkers()

#Likelihood models continuous age

data_multinom$age <- as.numeric(data_multinom$age)

multinom_model_age <- train(cutpoint ~ age + Target_Meyer, method = "multinom", data = data_multinom) #NNET with Caret bootstrapping


multinom_model_age <- multinom(cutpoint ~ age + Target_Meyer, MaxNWts =10000000, data = data_multinom) #NNET package without Caret

multinom_model_age_inter <- multinom(cutpoint ~ age * Target_Meyer, MaxNWts =10000000, data = data_multinom) #NNET package without Caret
multinom_model_agesq_inter <- multinom(cutpoint ~ age * Target_Meyer + I(age^2), MaxNWts =10000000, data = data_multinom) #NNET package without Caret

data_multinom$age_exp <- exp(data_multinom$age)

multinom_model_ageexp_inter <- multinom(cutpoint ~ age_exp * Target_Meyer, MaxNWts =10000000, data = data_multinom) #NNET package without Caret

multinom_model_age_inter_yield <- multinom(cutpoint ~ age * Target_Meyer + yield, MaxNWts =10000000, data = data_multinom) #NNET package without Caret

multinom_model_agesq_inter_yield <- multinom(cutpoint ~ age * Target_Meyer + I(age^2) + yield, MaxNWts =10000000, data = data_multinom) #NNET package without Caret

#Likelihood model categorised age

multinom_model_age_cat <- multinom(cutpoint ~ age_cat + Target_Meyer, MaxNWts =10000000, data = data_multinom) #NNET package without Caret

multinom_model_age_cat_inter <- multinom(cutpoint ~ age_cat + Target_Meyer + age_cat*Target_Meyer, MaxNWts =10000000, data = data_multinom) #NNET package without Caret

multinom_model_age_cat_inter_yield <- multinom(cutpoint ~ age_cat * Target_Meyer + yield, MaxNWts =10000000, data = data_multinom) #NNET package without Caret

multinom_model_age_cat_inter_yieldsq <- multinom(cutpoint ~ age_cat * Target_Meyer + yield + I(yield^2), MaxNWts =10000000, data = data_multinom)

multinom_model_age_cat_inter_dim <- multinom(cutpoint ~ age_cat * Target_Meyer + dim, MaxNWts =10000000, data = data_multinom) #NNET package without Caret

multinom_model_age_cat_inter_dimsq <- multinom(cutpoint ~ age_cat * Target_Meyer + dim + I(dim^2), MaxNWts =10000000, data = data_multinom) #NNET package without Caret

models <- c("multinom_model_age",
            "multinom_model_age_inter",
            "multinom_model_agesq_inter",
            "multinom_model_ageexp_inter",
            "multinom_model_age_inter_yield",
            "multinom_model_agesq_inter_yield",
            "multinom_model_age_cat", 
            "multinom_model_age_cat_inter", 
            "multinom_model_age_cat_inter_dim",
            "multinom_model_age_cat_inter_dimsq",
            "multinom_model_age_cat_inter_yield",
            "multinom_model_age_cat_inter_yieldsq")

for (i in models){
  print(paste(i,AIC(get(i))))
}


stopCluster(cl)

data_temp_TM <- data

data_temp_TM$Target_Meyer <- "0"

MNPredTM0 <- predict(multinom_model_agesq_inter_yield, newdata = data_temp_TM, type = "prob")

data_temp_TM$Target_Meyer <- "1"

MNPredTM1 <- predict(multinom_model_agesq_inter_yield, newdata = data_temp_TM, type = "prob")

head(MNPredTM1,10)

head(MNPredTM0,10)

data_temp_TM$MNPredProbTM0 <- 0

for (i in 1:nrow(data_temp_TM)){
  colnum <- data_temp_TM$cutpoint[i]
  data_temp_TM$MNPredProbTM0[i] <- MNPredTM0[i,colnum]
}

data_temp_TM$MNPredProbTM1 <- 0

for (i in 1:nrow(data_temp_TM)){
  colnum <- data_temp_TM$cutpoint[i]
  data_temp_TM$MNPredProbTM1[i] <- MNPredTM1[i,colnum]
}

data$MNPredProbTM0 <- data_temp_TM$MNPredProbTM0
data$MNPredProbTM1 <- data_temp_TM$MNPredProbTM1

data$likelihood <- data$MNPredProbTM1/data$MNPredProbTM0

plotlydata <- data.frame(x=data$age, y=data$titre, z=data$likelihood)
plot_ly() %>% 
  add_trace(data = plotlydata,  x=plotlydata$x, y=plotlydata$y, z=plotlydata$z, type="mesh3d" ) 

for (i in unique(data$age_cat)){
  plotdata <- data[data$age_cat == i,]
  print(ggplot(plotdata, aes(x = cutpoint, y = likelihood)) +
          geom_point() +
          geom_hline(yintercept = 1) +
          labs(title = i))
}

for (i in 1:length(cutpoints)){
  plotdata <- data[data$cutpoint == i,]
  print(ggplot(plotdata, aes(x = age, y = likelihood)) +
          geom_point() +
          geom_hline(yintercept = 1) +
          labs(title = paste("cutpoint",i)))
}

for (i in 1:length(unique(data$age_cat))){
  print (i)
  print(summary(data$likelihood[data$age_cat == unique(data$age_cat)[i]]))
}

ggplot(data, aes(x = yield, y = likelihood)) +
  geom_point(aes(color = cutpoint)) +
  geom_smooth()

write.csv(data, "y:/ian/johnesthresholds/johnesproper/data/data.csv", row.names=FALSE) #Write to Excel for quick Posterior updating

data <- read.csv("y:/ian/johnesthresholds/johnesproper/data/data.csv")

data$PosteriorOdds <- as.numeric(0)

data$PosteriorOdds[1] <- data$birthpriorLR[1] * data$likelihood[1]

cl <- makePSOCKcluster(5)
registerDoParallel(cl)
getDoParWorkers()

#Posterior updating in R (sloooooow!)

foreach(i = 2:nrow(data)) %do% {
  data$PosteriorOdds[i] <- ifelse(data$calfeartag[i] != data$calfeartag[i-1], data$birthpriorLR[i] * data$likelihood[i], data$PosteriorOdds[i-1] * data$likelihood[i])
}

stopCluster(cl)

data$PosteriorProb <- data$PosteriorOdds/(1 + data$PosteriorOdds)

data$PosteriorProb <- as.numeric(data$PosteriorProb)

hist(as.numeric(data$PosteriorProb))

#Produce Plots

data$titre <- as.numeric(data$titre)
data$PosteriorProb <- as.numeric(data$PosteriorProb)

data$Target_Meyer <- as.factor(data$Target_Meyer)

str(data$Target_Meyer)

testcows <- c("UK100603202309",
              "UK102276100824",
              "UK283905602999",
              "UK283905705912",
              "UK304238103319",
              "UK304601303851",
              "UK323079201963")

mod <- "6 Cutpoints; include finals; age (continuous) target interaction plus yield"

group.colours <- c("L" = "green", "M" = "orange", "H" = "red")

for (cow in testcows){
  ggplot(data[data$calfeartag == cow,], aes(x = age)) +
    geom_point(aes(y = titre, color = class)) +
    geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
    geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
    scale_color_manual(values = group.colours) +
    geom_line(aes(y = PosteriorProb*100)) +
    geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
    scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
    labs(title = data$calfeartag[data$calfeartag == cow], subtitle = paste("Birth Probability:",data$predLR[data$calfeartag == cow]))
  ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/PosteriorProbCharts/TestCow/",cow,mod,".png"))
}

data$PosteriorProb <- as.numeric(data$PosteriorProb)

calfsamplemeyer1 <- sample(unique(data$calfeartag[data$Target_Meyer == "1"]), 100, replace = FALSE)

calfsamplemeyer0 <- sample(unique(data$calfeartag[data$Target_Meyer == "0"]), 100, replace = FALSE)

calfsamplemeyerU <- sample(unique(data$calfeartag[data$Target_Meyer == "U"]), 100, replace = FALSE)

calfsamplerandom <- sample(unique(data$calfeartag), 1000, replace  = FALSE)

group.colours <- c("L" = "green", "M" = "orange", "H" = "red")

for (i in calfsamplemeyer1){
  ggplot(data[data$calfeartag == i,], aes(x = age)) +
    geom_point(aes(y = titre, color = class)) + 
    geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
    geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
    scale_color_manual(values = group.colours) +
    geom_line(aes(y = PosteriorProb*100)) +
    geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
    scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
    labs(title = data$calfeartag[data$calfeartag == i], subtitle = paste("Birth Probability:",data$predLR[data$calfeartag == i]))
  
  ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/PosteriorProbCharts/MeyerPosCows/",i,".png"))
}

group.colours <- c("L" = "green", "M" = "orange", "H" = "red")

for (i in calfsamplemeyerU){
  ggplot(data[data$calfeartag == i,], aes(x = age)) +
    geom_point(aes(y = titre, color = class)) + 
    geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
    geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
    scale_color_manual(values = group.colours) +
    geom_line(aes(y = PosteriorProb*100)) +
    geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
    scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
    labs(title = data$calfeartag[data$calfeartag == i], subtitle = paste("Birth Probability:",data$predLR[data$calfeartag == i]))
  
  ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/PosteriorProbCharts/MeyerUnknownCows/",i,".png"))
}

group.colours <- c("L" = "green", "M" = "orange", "H" = "red")

for (i in calfsamplemeyer0){
  ggplot(data[data$calfeartag == i,], aes(x = age)) +
    geom_point(aes(y = titre, color = class)) + 
    geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
    geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
    scale_color_manual(values = group.colours) +
    geom_line(aes(y = PosteriorProb*100)) +
    geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
    scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
    labs(title = data$calfeartag[data$calfeartag == i], subtitle = paste("Birth Probability:",data$predLR[data$calfeartag == i]))
  
  ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/PosteriorProbCharts/MeyerNegCows/",i,".png"))
}

#Vaidating Posteriors

ggplot(data, aes(x = age, y = PosteriorProb)) +
  geom_point() +
  labs(x = "Age", y = "Posterior Probability", title = "Posterior Probabilities")

data$testnum <- as.numeric(substr(data$covsandtitre,13,15))

data$Target_Meyer <- as.factor(data$Target_Meyer)

for (i in unique(data$age_cat)){
  print(CaliPlot(data$PosteriorProb[data$Target_Meyer != "U" & data$age_cat == i & !is.na(data$PosteriorProb) & data$calfeartag != "NA"], data$Target_Meyer[data$age_cat == i & data$Target_Meyer != "U" & !is.na(data$PosteriorProb) & data$calfeartag != "NA"],
                 nbins = 7,           
                 ptitle = paste("Predictions From", i, "Age Bracket")))}

unique(data$age_cat)[7]

PPData <- data[data$PosteriorProb > 0.7 & data$age_cat == unique(data$age_cat)[7] & data$Target_Meyer == "0" & data$calfeartag != "NA",]

dim(PPData)

unique(PPData$calfeartag)

group.colours <- c("L" = "green", "M" = "orange", "H" = "red")

for (cow in unique(PPData$calfeartag)){
  ggplot(data[data$calfeartag == cow,], aes(x = age)) +
    geom_point(aes(y = titre, color = class)) +
    geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
    geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
    scale_color_manual(values = group.colours) +
    geom_line(aes(y = PosteriorProb*100)) +
    geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
    scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
    labs(title = data$calfeartag[data$calfeartag == cow], subtitle = paste("Birth Probability:",data$predLR[data$calfeartag == cow]))
  ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/PosteriorProbCharts/PoorPreds/",cow,".png"))
}

hist(data$PosteriorProb[data$Target_Meyer == "0"])

hist(data$PosteriorProb[data$Target_Meyer == "1"])


