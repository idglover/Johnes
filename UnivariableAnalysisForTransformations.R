
#####READ AND CLEAN DATA####

data <- read.csv("Y:/Ian/JohnesThresholds/JohnesProper/Data/data_birthpriors.csv")

print("Cleaning data...")

data$Target_Meyer <- as.factor(data$Target_Meyer)

data$titre <- as.numeric(data$titre)

data <- data[data$yield > 0 &
               !is.na(data$yield) &
               data$yield <= 70 &
               data$protein > 0 &
               !is.na(data$protein) &
               data$protein <=7 &
               data$cellcount >0 &
               !is.na(data$cellcount) &
               data$cellcount < 10000 &
               data$butterfat >0 &
               !is.na(data$butterfat) &
               data$butterfat < 10 &
               data$age >= 24 &
               !is.na(data$age),]

data$age_cat <- cut(as.numeric(data$age), breaks = agebreaks)

####CREATE TITRE CUTPOINTS####


print("Creating titre cutpoints...")

data$cutpoint <- length(cutpoints)

cl <- makePSOCKcluster(5)
registerDoParallel(cl)
cat("No. Cores:",getDoParWorkers())

time1 <- Sys.time()

for (i in 1:nrow(data)){
  for (j in 1:length(cutpoints)-1){
    if(data$titre[i] >= cutpoints[j] && data$titre[i] < cutpoints[j+1]){data$cutpoint[i] <- j}
  }
}

time2 <- Sys.time()

print(paste("Time taken assigning cutpoints:", round(difftime(time2, time1, units = "mins"),2), "mins"))

stopCluster(cl)

data$cutpoint <- as.factor(data$cutpoint)



####Create data for multinomial model (NNET Package)####

print("Creating data for multinomial likelihood models...")

data_multinom <- data[data$Target_Meyer != "U",]

data_multinom$Target_Meyer <- droplevels(data_multinom$Target_Meyer, "U")

data_multinom$Target_Meyer <- as.factor(data_multinom$Target_Meyer)

summary(data_multinom$Target_Meyer)

data_multinom$age <- as.numeric(data_multinom$age)

data_multinom$yield <- as.numeric(data_multinom$yield)

data_multinom$cellcount <- as.numeric(data_multinom$cellcount)

data_multinom$dim <- as.numeric(data_multinom$dim)

data_multinom$meantitrenegcows <- as.numeric(data_multinom$meantitrenegcows)

data_multinom$butterfat <- as.numeric(data_multinom$butterfat)

data_multinom$protein <- as.numeric(data_multinom$protein)


#####PLOT HISTOGRAMS OF CONTINUOUS VARIABLES#####

contvarcolnums <- c(45,48,49,50,51,53)

for (i in 1:length(contvarcolnums)){
  xvar <- colnames(data_multinom)[contvarcolnums[i]]
  print(ggplot(data_multinom, aes(x = get(noquote(xvar)))) +
    geom_histogram() +
    labs(title = xvar))
}


#####UNIVARIABLE SCATTER PLOTS####

for (i in 1:length(contvarcolnums)){
  xvar <- colnames(data_multinom)[contvarcolnums[i]]
  print(ggplot(data_multinom, aes(x = get(noquote(xvar)), y = titre)) +
          geom_point() +
          geom_smooth() +
    labs(title = xvar))
  ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/ContinuousUnivarPlots/",xvar,".png"))
}


####FIT UNIVARIABLE MODELS CONTINUOUS TITRE####

model_univar_age_titre <- lm(titre ~ age, data = data_multinom)
model_univar_agesq_titre <- lm(titre ~ age + I(age^2), data = data_multinom)
model_univar_agecb_titre <- lm(titre ~ age + I(age^3), data = data_multinom)

model_univar_yield_titre <- lm(titre ~ yield, data = data_multinom)
model_univar_yieldsq_titre <- lm(titre ~ yield + I(yield^2), data = data_multinom)
model_univar_yieldcb_titre <- lm(titre ~ yield + I(yield^3), data = data_multinom)

model_univar_scc_titre <- lm(titre ~ cellcount, data = data_multinom)
model_univar_scclog_titre <- lm(titre ~ cellcount + I(log(cellcount)), data = data_multinom)
model_univar_sccsq_titre <- lm(titre ~ cellcount + I(cellcount^2), data = data_multinom)
model_univar_scccb_titre <- lm(titre ~ cellcount + I(cellcount^3), data = data_multinom)

model_univar_protein_titre <- lm(titre ~ protein, data = data_multinom)
model_univar_proteinsq_titre <- lm(titre ~ protein + I(protein^2), data = data_multinom)
model_univar_proteincb_titre <- lm(titre ~ protein + I(protein^3), data = data_multinom)

model_univar_meantitrenegcows_titre <- lm(titre ~ meantitrenegcows, data = data_multinom)
model_univar_meantitrenegcowssq_titre <- lm(titre ~ meantitrenegcows + I(meantitrenegcows^2), data = data_multinom)
model_univar_meantitrenegcowscb_titre <- lm(titre ~ meantitrenegcows + I(meantitrenegcows^3), data = data_multinom)


#####FIT MULTINOMIAL UNIVARIABLE MODELS (CATEGORISED TITRE)

for (mod in c(

"model_univar_multinom_age_cutpoint",
"model_univar_multinom_agesq_cutpoint",
"model_univar_multinom_agecb_cutpoint",

"model_univar_multinom_yield_cutpoint",
"model_univar_multinom_yieldsq_cutpoint",
"model_univar_multinom_yieldcb_cutpoint",

"model_univar_multinom_scc_cutpoint",
"model_univar_multinom_scclog_cutpoint",
"model_univar_multinom_sccsq_cutpoint",
"model_univar_multinom_scccb_cutpoint",

"model_univar_multinom_protein_cutpoint",
"model_univar_multinom_proteinsq_cutpoint",
"model_univar_multinom_proteincb_cutpoint",

"model_univar_multinom_meantitrenegcows_cutpoint",
"model_univar_multinom_meantitrenegcowssq_cutpoint",
"model_univar_multinom_meantitrenegcowscb_cutpoint"
)){

  print(paste0(mod," AIC: ",AIC(get(mod))))
  
}
