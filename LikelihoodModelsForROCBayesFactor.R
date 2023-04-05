####READ AND CLEAN DATA####

#####Read Data#####

data <- read.csv("Y:/Ian/JohnesThresholds/JohnesProper/Data/data_birthpriors.csv")

print("Cleaning data...")

#####Clean Data#####

data$Target_QMMS <- as.factor(data$Target_QMMS)

data$titre <- as.numeric(data$titre)

data <- data[which(data$yield <= 70 &
                     data$protein <=7 &
                     data$cellcount < 10000 &
                     data$butterfat < 10 &
                     data$age >= 24 &
                     !is.na(data$age)),]




data$age_cat <- cut(as.numeric(data$age), breaks = agebreaks)

data$yield_cat <- cut(data$yield, breaks = c(0,10,20,30,40,50,70))
levels(data$yield_cat) <- c(levels(data$yield_cat), "Missing")
data$yield_cat[which(is.na(data$yield_cat))] <- "Missing"

data$protein_cat <- cut(data$protein, breaks = c(0,3,3.5,4,4.5,7))
levels(data$protein_cat) <- c(levels(data$protein_cat), "Missing")
data$protein_cat[which(is.na(data$protein_cat))] <- "Missing"

data$butterfat_cat <- cut(data$butterfat, breaks = c(0,2,4,6,10))
levels(data$butterfat_cat) <- c(levels(data$butterfat_cat), "Missing")
data$butterfat_cat[which(is.na(data$butterfat_cat))] <- "Missing"

data$cellcount_cat <- cut(data$cellcount, breaks = c(0,50,100,150,200,500,1000,10000))
levels(data$cellcount_cat) <- c(levels(data$cellcount_cat), "Missing")
data$cellcount_cat[which(is.na(data$cellcount_cat))] <- "Missing"


#####Assign Cutpoints#####

print("Creating titre cutpoints...")

data$cutpoint <- length(cutpoints)


time1 <- Sys.time()

source_python("Y:/Ian/JohnesThresholds/JohnesProper/Data/PythonScripts/PyCreateCutpoints.py")

data <- createcutpoints(data, cutpoints)




time2 <- Sys.time()

print(paste("Time taken assigning cutpoints:", round(difftime(time2, time1, units = "mins"),2), "mins"))

print("Titre cutpoints:")
print(head(cbind(data$titre, data$cutpoint),30))


data$cutpoint <- as.factor(data$cutpoint)



####CREATE DATA FOR GLMMs####

print("Creating data for multinomial likelihood models...")

#####Create Targets#####

if(target == "MEYER"){
  data_multinom <- data[data$Target_Meyer != "U",]
  data_multinom$Target_Meyer <- droplevels(data_multinom$Target_Meyer, "U")
  data_multinom$Target_Meyer <- as.factor(data_multinom$Target_Meyer)
}

if(target == "QMMS"){
  data_multinom <- data[data$Target_QMMS != "U",]
  data_multinom$Target_QMMS <- droplevels(data_multinom$Target_QMMS, "U")
  data_multinom$Target_QMMS <- as.factor(data_multinom$Target_QMMS)
  
}


#####Continuous Feature Transformation#####


data_multinom$age <- as.numeric(data_multinom$age)

data_multinom$yield <- as.numeric(data_multinom$yield)

data_multinom$cellcount <- as.numeric(data_multinom$cellcount)

data_multinom$dim <- as.numeric(data_multinom$dim)

data_multinom$meantitrenegcows <- as.numeric(data_multinom$meantitrenegcows)

data_multinom$butterfat <- as.numeric(data_multinom$butterfat)

data_multinom$protein <- as.numeric(data_multinom$protein)


data_multinom$agesq <- data_multinom$age^2
data_multinom$yieldsq <- data_multinom$yield^2
data_multinom$proteinsq <- data_multinom$protein^2
data_multinom$butterfatsq <- data_multinom$butterfat^2
data_multinom$meantitrenegcowssq <- data_multinom$meantitrenegcows^2
data_multinom$cellcountlog <- log(data_multinom$cellcount)
data_multinom$titresq <- data_multinom$titre^2

#####Split Train/Test#####

uniquefarms <- unique(data_multinom$Farm)

trainfarms <- sample(uniquefarms, 63, replace = FALSE)
testfarms <- uniquefarms[!(uniquefarms %in% trainfarms)]

data_multinom_train <- data_multinom[data_multinom$Farm %in% trainfarms,]
data_multinom_test <- data_multinom[data_multinom$Farm %in% testfarms,]



cat(paste0("Trainset: \n",dim(data_multinom_train)[1],
           " rows (",round(dim(data_multinom_train)[1]/(dim(data_multinom_test)[1] + dim(data_multinom_train)[1])*100,1),"%)"),
    "\nPositive Target:", summary(as.factor(data_multinom_train$Target_QMMS))[[2]], "rows",
    "(", round(summary(as.factor(data_multinom_train$Target_QMMS))[[2]]/nrow(data_multinom_train)*100,1),"%)")

cat(paste0("testset: \n",dim(data_multinom_test)[1],
           " rows (",round(dim(data_multinom_test)[1]/(dim(data_multinom_train)[1] + dim(data_multinom_test)[1])*100,1),"%)"),
    "\nPositive Target:", summary(as.factor(data_multinom_test$Target_QMMS))[[2]], "rows",
    "(", round(summary(as.factor(data_multinom_test$Target_QMMS))[[2]]/nrow(data_multinom_test)*100,1),"%)")


####BUILD GLMMs####

model.formula <- formula(Target_QMMS ~ titre + (1|Farm))

likeglmm_TQ_1 <- glmer(model.formula,
                             data = data_multinom_train,
                             family = "binomial",
                             control = glmerControl(optimizer  = "bobyqa",
                                                    optCtrl = list(maxfun = 100000)))

parallelCVglmm(data_multinom_train, model.formula, 10, 10, ptitle = "likeglmm_TQ_1")

CaliPlot(predict(likeglmm_TQ_1, newdata = data_multinom_test, type = "response", re.form = ~0), data_multinom_test$Target_QMMS, ptitle = "data_multinom_test")


model.formula <- formula(Target_QMMS ~ titre * age + (1|Farm))

likeglmm_TQ_2 <- glmer(model.formula,
                       data = data_multinom_train,
                       family = "binomial",
                       control = glmerControl(optimizer  = "bobyqa",
                                              optCtrl = list(maxfun = 100000)))


parallelCVglmm(data_multinom_train, model.formula, 10, 10, ptitle = "likeglmm_TQ_2")

CaliPlot(predict(likeglmm_TQ_2, newdata = data_multinom_test, type = "response", re.form = ~0), data_multinom_test$Target_QMMS, ptitle = "data_multinom_test")

model.formula <- formula(Target_QMMS ~ titre + titresq + age + agesq + meantitrenegcows + (1|Farm))

likeglmm_TQ_3 <- glmer(model.formula,
                       data = data_multinom_train,
                       family = "binomial",
                       control = glmerControl(optimizer  = "bobyqa",
                                              optCtrl = list(maxfun = 100000)))

CaliPlot(predict(likeglmm_TQ_3, newdata = data_multinom_test, type = "response", re.form = ~0), data_multinom_test$Target_QMMS, ptitle = "data_multinom_test")


####GET LIKELIHOOD FROM GLMM ROC CURVE####

preds <- predict(likeglmm_TQ_3, newdata = data_multinom_train, type = "response", re.form = ~0)
obs <- data_multinom_train$Target_QMMS

likelihood <- LikeFromROC(preds = preds, obs = obs, ncuts = 10)

data$pred_TQ <- predict(likeglmm_TQ_1, newdata = data, type = "response", re.form = ~0)



breaks <- substr(sesptable$predcat, 2, unlist(gregexpr(',', sesptable$predcat))-1)
breaks <- c(breaks, "1")

data$pred_TQ_cat <- cut(data$pred_TQ, breaks = breaks)

data <- merge(data, sesptable[,c(1,10)], by.x = "pred_TQ_cat", by.y = "predcat", all.x = TRUE)

data <- data[order(data$calfeartag, data$date),]

data$PosteriorOdds <- as.numeric(0)
data$POFloorApplied <- as.numeric(0)


source_python("Y:/Ian/JohnesThresholds/JohnesProper/Data/PythonScripts/PyPOUpdater.py")
data <- updatePO(data, posterioroddsfloor, priorstage)

data$PosteriorProb <- data$PosteriorOdds / 
  (1 + data$PosteriorOdds)


testcows <- c("UK723449401226",
              "UK706252503632",
              "UK382226501365",
              "UK382226501015",
              "UK382226401014",
              "UK382226101137",
              "UK360336601224",
              "UK344962601339",
              "UK344962601332",
              "UK344962401792",
              "UK344962201363",
              "UK344962201335",
              "UK344962201237",
              "UK323079201963",
              "UK323001301913",
              "UK283905705912",
              "UK283905705800",
              "UK283905602467",
              "UK283905602271",
              "UK283905503789",
              "UK283905203317",
              "UK207270701322",
              "UK200932103046",
              "UK141520703389")



group.colours <- c("L" = "green", "M" = "orange", "H" = "red")

for (cow in testcows){
  print(ggplot(data[data$calfeartag == cow,], aes(x = age)) +
          geom_point(aes(y = titre, color = class)) +
          geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
          geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
          #geom_text(aes(y = -4, label = ifelse(POFloorApplied == 1,"POF","")), size = 2, angle = -90 ) +
          geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
          geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
          geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
          scale_color_manual(values = group.colours) +
          geom_line(aes(y = PosteriorProb*100)) +
          geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
          geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
          scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
          labs(title = data$calfeartag[data$calfeartag == cow], subtitle = paste(crtmodel,"\nBirth Probability:", round(data$priorprob_birth[data$calfeartag == cow],2),"12m Probability:",round(data$priorprob_12mold[data$calfeartag == cow],2),"Crt Probability:",round(data$priorprob_crt[data$calfeartag == cow],2),"\n Titre Cutpoints:", toString(cutpoints), "\n", "Age cutpoints:", toString(agebreaks))))
  #if(POUpdateMode == "floor"){ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/PosteriorProbCharts/TestCows/",cow,birthmodel,crtmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),"PFloor",posterioroddsfloor,".png"))}
  #if(POUpdateMode == "lastxtests"){ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/PosteriorProbCharts/TestCows/",cow,birthmodel,crtmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),"Last",lastxtests,"tests.png"))}
  #if(POUpdateMode == "birthfraction"){ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/PosteriorProbCharts/TestCows/",cow,birthmodel,crtmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),birthoddsfraction,"birthprob.png"))}
  #if(POUpdateMode == "ignoreuntil"){ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/PosteriorProbCharts/TestCows/",cow,birthmodel,crtmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),"IgnoreUntil",ignorethreshold,".png"))}
}
