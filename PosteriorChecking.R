####READ DATA####

print("Reading data (data_posteriors.csv)")
data_postchecks <- read.csv("y:/ian/johnesthresholds/johnesproper/data/data_posteriors.csv")

data_postchecks$age_cat <- cut(data_postchecks$age, breaks = 10)

data_postchecks$nH <- str_count(data_postchecks$profile,"H")
data_postchecks$nM <- str_count(data_postchecks$profile,"M")





###Define groups####

print("Defining different profiles...")

data_postchecks$profilegroup <- as.character("NONE") 



data_postchecks$profilegroup <- ifelse(data_postchecks$Hanypoint == 0 & data_postchecks$nM == 0 &
                                         data_postchecks$ntests >= 8, 
                                       "AllLowAtLeast8Tests", data_postchecks$profilegroup)

data_postchecks$profilegroup <- ifelse(data_postchecks$Hanypoint == 0 & data_postchecks$nM == 0 &
                                         data_postchecks$ntests < 8, 
                                       "AllLowAtFewerThan8Tests", data_postchecks$profilegroup)

data_postchecks$profilegroup <- ifelse(data_postchecks$nH == 0 &
                                         data_postchecks$nM >= 1,
                                       "SomeMNeverH", data_postchecks$profilegroup) 


data_postchecks$profilegroup <- ifelse(data_postchecks$nH == 0 &
                                         data_postchecks$nM == 1 &
                                         grepl("M", substr(data_postchecks$profile, nchar(data_postchecks$profile), nchar(data_postchecks$profile))) == FALSE,
                                       "AllLowOneMAllLow", data_postchecks$profilegroup)

data_postchecks$profilegroup <- ifelse(data_postchecks$nH == 0 &
                                         data_postchecks$nM == 2 &
                                         grepl("MM", substr(data_postchecks$profile, 1, nchar(data_postchecks$profile))) == TRUE &
                                         grepl("M", substr(data_postchecks$profile, nchar(data_postchecks$profile), nchar(data_postchecks$profile))) == FALSE,
                                       "AllLowTwoMAllLow", data_postchecks$profilegroup) 

data_postchecks$profilegroup <- ifelse(data_postchecks$nH == 0 &
                                         data_postchecks$nM == 1 &
                                         grepl("M", substr(data_postchecks$profile, nchar(data_postchecks$profile), nchar(data_postchecks$profile))) == TRUE,
                                       "AllLowOneMFinal", data_postchecks$profilegroup) 

data_postchecks$profilegroup <- ifelse(data_postchecks$nH == 0 &
                                         data_postchecks$nM == 2 &
                                         grepl("MM", substr(data_postchecks$profile, nchar(data_postchecks$profile)-1, nchar(data_postchecks$profile))) == TRUE,
                                       "AllLowTwoMFinal", data_postchecks$profilegroup) 



data_postchecks$profilegroup <- ifelse(data_postchecks$nH >= 2 &
                                         data_postchecks$HHanypoint == 0 &
                                         grepl("HMH", substr(data_postchecks$profile, 1, nchar(data_postchecks$profile))) == FALSE &
                                         grepl("HLH", substr(data_postchecks$profile, 1, nchar(data_postchecks$profile))) == FALSE,
                                       "SomeHNeverHHorHMHorHLH", data_postchecks$profilegroup) 






data_postchecks$profilegroup <- ifelse(data_postchecks$nH == 1 &
                                         data_postchecks$nM == 0 &
                                         grepl("H", substr(data_postchecks$profile, nchar(data_postchecks$profile), nchar(data_postchecks$profile))) == FALSE,
                                       "AllLowOneHAllLow", data_postchecks$profilegroup)


data_postchecks$profilegroup <- ifelse(data_postchecks$nH == 1 &
                                         data_postchecks$nM >= 1 &
                                         grepl("H", substr(data_postchecks$profile, nchar(data_postchecks$profile), nchar(data_postchecks$profile))) == FALSE,
                                       "OneHWithSomeMedium", data_postchecks$profilegroup)

data_postchecks$profilegroup <- ifelse(data_postchecks$nH == 1 &
                                         data_postchecks$nM >= 1 &
                                         grepl("H", substr(data_postchecks$profile, nchar(data_postchecks$profile), nchar(data_postchecks$profile))) == TRUE,
                                       "OneHFinalWithSomeMedium", data_postchecks$profilegroup)

data_postchecks$profilegroup <- ifelse(data_postchecks$nH == 2 &
                                         data_postchecks$nM == 0 &
                                         grepl("HH", substr(data_postchecks$profile, 1, nchar(data_postchecks$profile))) == TRUE &
                                         grepl("H", substr(data_postchecks$profile, nchar(data_postchecks$profile), nchar(data_postchecks$profile))) == FALSE,
                                       "AllLowTwoHAllLow", data_postchecks$profilegroup)


data_postchecks$profilegroup <- ifelse(data_postchecks$nH == 2 &
                                         data_postchecks$nM == 0 &
                                         grepl("HH", substr(data_postchecks$profile, 1, nchar(data_postchecks$profile))) == TRUE &
                                         grepl("H", substr(data_postchecks$profile, nchar(data_postchecks$profile), nchar(data_postchecks$profile))) == FALSE,
                                       "AllLowTwoHAllLow", data_postchecks$profilegroup) 


data_postchecks$profilegroup <- ifelse(data_postchecks$nH == 2 &
                                         data_postchecks$nM >= 1 &
                                         grepl("HH", substr(data_postchecks$profile, 1, nchar(data_postchecks$profile))) == TRUE &
                                         grepl("H", substr(data_postchecks$profile, nchar(data_postchecks$profile), nchar(data_postchecks$profile))) == FALSE,
                                       "TwoHWithSomeMedium", data_postchecks$profilegroup) 



data_postchecks$profilegroup <- ifelse(data_postchecks$nH == 1 &
                                         data_postchecks$nM == 0 &
                                         grepl("H", substr(data_postchecks$profile, nchar(data_postchecks$profile), nchar(data_postchecks$profile))) == TRUE,
                                       "AllLowOneHFinal", data_postchecks$profilegroup) 

data_postchecks$profilegroup <- ifelse(data_postchecks$nH == 2 &
                                         data_postchecks$nM == 0 &
                                         grepl("HH", substr(data_postchecks$profile, nchar(data_postchecks$profile)-1, nchar(data_postchecks$profile))) == TRUE,
                                       "AllLowTwoHFinal", data_postchecks$profilegroup)

data_postchecks$profilegroup <- ifelse(data_postchecks$nH == 2 &
                                         data_postchecks$nM >= 1 &
                                         grepl("HH", substr(data_postchecks$profile, nchar(data_postchecks$profile)-1, nchar(data_postchecks$profile))) == TRUE,
                                       "TwoHFinalWithSomeMedium", data_postchecks$profilegroup)


data_postchecks$profilegroup <- ifelse((grepl("HLH", substr(data_postchecks$profile, 1, nchar(data_postchecks$profile))) == TRUE |
                                         grepl("HMH", substr(data_postchecks$profile, 1, nchar(data_postchecks$profile))) == TRUE) &
                                         grepl("HH", substr(data_postchecks$profile, 1, nchar(data_postchecks$profile))) == FALSE,
                                       "HMHorHLHNeverHH", data_postchecks$profilegroup) 



data_postchecks$profilegroup <- ifelse(data_postchecks$nM == 0 &
                                      grepl("LHHHH", substr(data_postchecks$profile, nchar(data_postchecks$profile)-4, nchar(data_postchecks$profile))) == TRUE,
                                       "AllLowLast4TestsH", data_postchecks$profilegroup)

data_postchecks$profilegroup <- ifelse(data_postchecks$nM > 0 &
                                         grepl("HHHH", substr(data_postchecks$profile, nchar(data_postchecks$profile)-3, nchar(data_postchecks$profile))) == TRUE,
                                       "SomeMLast4TestsH", data_postchecks$profilegroup)


data_postchecks$profilegroup <- factor(data_postchecks$profilegroup, levels = c("AllLowAtLeast8Tests", 
                                                                                "AllLowAtFewerThan8Tests",
                                                                                "AllLowOneMAllLow", 
                                                                                "AllLowOneMFinal",
                                                                                "AllLowTwoMAllLow", 
                                                                                "AllLowTwoMFinal", 
                                                                                "SomeMNeverH",
                                                                                "AllLowOneHAllLow",
                                                                                "AllLowOneHFinal",
                                                                                "OneHWithSomeMedium", 
                                                                                "OneHFinalWithSomeMedium",
                                                                                "SomeHNeverHHorHMHorHLH",
                                                                                "HMHorHLHNeverHH", 
                                                                                "AllLowTwoHAllLow",
                                                                                "AllLowTwoHFinal",
                                                                                "TwoHWithSomeMedium",
                                                                                "TwoHFinalWithSomeMedium",
                                                                                "AllLowLast4TestsH",
                                                                                "SomeMLast4TestsH",
                                                                                "NONE"))

#####Check Profiles####

head(data_postchecks$profile[which(data_postchecks$profilegroup == "NONE")],100)



###Reference Ages#####

data_postchecks$refage <- as.numeric(0)

data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "NONE", 0, data_postchecks$refage)

data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "AllLowAtLeast8Tests", 0, data_postchecks$refage)

data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "AllLowFewerThan8Tests", 0, data_postchecks$refage)

data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "AllLowOneMAllLow", data_postchecks$ageatfirstM, data_postchecks$refage)

data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "AllLowTwoMAllLow", data_postchecks$ageatfirstM, data_postchecks$refage)

data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "AllLowOneMFinal", data_postchecks$ageatfirstM, data_postchecks$refage)

data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "AllLowTwoMFinal", data_postchecks$ageatfirstM, data_postchecks$refage)

data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "SomeMNeverH", data_postchecks$ageatfirstM, data_postchecks$refage)


data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "AllLowOneHAllLow", data_postchecks$ageatfirstH, data_postchecks$refage)

data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "OneHWithSomeMedium", data_postchecks$ageatfirstH, data_postchecks$refage)

data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "AllLowTwoHAllLow", data_postchecks$ageatfirstH, data_postchecks$refage)

data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "AllLowOneHFinal", data_postchecks$ageatfirstH, data_postchecks$refage)


data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "SomeHNeverHHorHMHorHLH", data_postchecks$ageatfirstH, data_postchecks$refage)

data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "OneHFinalWithSomeMedium", data_postchecks$ageatfirstH, data_postchecks$refage)

data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "AllLowTwoHFinal", data_postchecks$ageatfirstH, data_postchecks$refage)


data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "TwoHWithSomeMedium", data_postchecks$ageatfirstHH, data_postchecks$refage)

data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "HMHorHLHNeverHH", data_postchecks$ageatfirstH, data_postchecks$refage)

data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "TwoHFinalWithSomeMedium", data_postchecks$ageatfirstHH, data_postchecks$refage)

data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "AllLowLast4TestsH", data_postchecks$ageatfirstH, data_postchecks$refage)

data_postchecks$refage <- ifelse(data_postchecks$profilegroup == "SomeMLast4TestsH", data_postchecks$ageatfirstH, data_postchecks$refage)

data_postchecks$profilegroup <- as.factor(data_postchecks$profilegroup)

data_postchecks <- data_postchecks[which(data_postchecks$profilegroup != "NONE"),]


#####PLOTS#####

if(POUpdateMode == "floor"){

ggplot(data_postchecks[which((data_postchecks$age - data_postchecks$refage) == 0 | data_postchecks$refage == 0),], aes(x= profilegroup, y = PosteriorProb)) +
  geom_boxplot() +
  theme(axis.text.x=element_text(angle= -90)) +
  labs(title = "Posterior Probability on day of period of interest", subtitle = paste0("PO Floor ",posterioroddsfloor), x = "Profile", y = "Posterior Probability")
}

if(POUpdateMode == "lastxtests"){
  ggplot(data_postchecks[which((data_postchecks$age - data_postchecks$refage) == 0 | data_postchecks$refage == 0),], aes(x= profilegroup, y = PosteriorProb)) +
    geom_boxplot() +
    theme(axis.text.x=element_text(angle= -90)) +
    labs(title = "Posterior Probability on day of period of interest", subtitle = paste0("Last ",lastxtests," tests"), x = "Profile", y = "Posterior Probability")
}
  
if(POUpdateMode == "birthfraction"){
  ggplot(data_postchecks[which((data_postchecks$age - data_postchecks$refage) == 0 | data_postchecks$refage == 0),], aes(x= profilegroup, y = PosteriorProb)) +
    geom_boxplot() +
    theme(axis.text.x=element_text(angle= -90)) +
    labs(title = "Posterior Probability on day of period of interest", subtitle = paste0("PO Floor ",birthoddsfraction, " * Birth Prior Odds"), x = "Profile", y = "Posterior Probability")
}



if(POUpdateMode == "floor"){ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/GroupsPosteriorChecks/Plots/",birthmodel,crtmodel,"BoxGroupsAtPOIPOFloor",posterioroddsfloor,".png"))}
if(POUpdateMode == "lastxtests"){ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/GroupsPosteriorChecks/Plots/",birthmodel,crtmodel,"BoxGroupsAtPOILast",lastxtests,"tests.png"))}
if(POUpdateMode == "birthfraction"){ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/GroupsPosteriorChecks/Plots/",birthmodel,crtmodel,"BoxGroupsAtPOIPOFloor",birthoddsfraction,"birthodds.png"))}


#Check correlation between birth prior and probability at POI and final posteriors

data_postchecks$predLR_cat <- cut(data_postchecks$priorprob_crt, 10)

if(POUpdateMode == "floor"){

print(ggplot(data_postchecks[which((data_postchecks$age - data_postchecks$refage) == 0),], aes(x= predLR_cat, y = PosteriorProb)) +
  geom_boxplot() +
  theme(axis.text.x=element_text(angle= -90)) +
  labs(title = "Posterior Probability at period of interest", subtitle = paste0(crtmodel,"\nPOFloor:", posterioroddsfloor), x = "Birth Probability", y = "Posterior Probability")) +
  facet_wrap(~profilegroup)

ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/groupsposteriorchecks/plots/PosteriorAtPOIbyBirthProb",crtmodel,"POF",posterioroddsfloor,".png"))

}

if(POUpdateMode == "lastxtests"){
  
  print(ggplot(data_postchecks[which((data_postchecks$age - data_postchecks$refage) == 0),], aes(x= predLR_cat, y = PosteriorProb)) +
          geom_boxplot() +
          theme(axis.text.x=element_text(angle= -90)) +
          labs(title = "Posterior Probability on day of period of interest", subtitle = paste0(crtmodel,"\nLast ",lastxtests," tests"), x = "Birth Probability", y = "Posterior Probability") +
          facet_wrap(~profilegroup))
  
  ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/groupsposteriorchecks/plots/PosteriorAtPOIbyBirthProb",crtmodel,"Last",lastxtests,"Tests.png"))
  
}

if(POUpdateMode == "birthfraction"){
  
  print(ggplot(data_postchecks[which((data_postchecks$age - data_postchecks$refage) == 0),], aes(x= predLR_cat, y = PosteriorProb)) +
          geom_boxplot() +
          theme(axis.text.x=element_text(angle= -90)) +
          labs(title = "Posterior Probability at period of interest", subtitle = paste0(crtmodel,"\nPOFloor:", birthoddsfraction,"*Birth Odds"), x = "Birth Probability", y = "Posterior Probability")) +
    facet_wrap(~profilegroup)
  
  ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/groupsposteriorchecks/plots/PosteriorAtPOIbyBirthProb",crtmodel,"POF",birthoddsfraction,"birthodds.png"))
  
}

print("Correlation between birth prior and posterior at point of interest")
for (i in unique(data_postchecks$profilegroup)[3:15]){
  print(paste0(i,": ",round(cor(data_postchecks$priorprob_crt[which((data_postchecks$age - data_postchecks$refage) == 0 & data_postchecks$profilegroup == i)], 
                                data_postchecks$PosteriorProb[which((data_postchecks$age - data_postchecks$refage) == 0 & data_postchecks$profilegroup == i)]),2)))
}


dpriorpoicuts <- c(0,90,180,270,360)
  
for (i in 1:length(dpriorpoicuts)-1){
  
  
  
  pdata <- data_postchecks
  pdata$duntilref <- (pdata$refage - pdata$age) /12 *365
  pdata <- pdata[which((pdata$duntilref > dpriorpoicuts[i] & pdata$duntilref <= dpriorpoicuts[i+1]) | pdata$refage == 0),]
    
    
    
  pdatacounts <- count(pdata, profilegroup)  
  
if(POUpdateMode == "floor"){
  
  ggplot() +
  geom_boxplot(data = pdata, 
               aes(x = profilegroup, y = PosteriorProb)) +
   labs(title = paste0("Posterior Probability within ",dpriorpoicuts[i]+1," to ",dpriorpoicuts[i+1]," days prior to period of interest"), subtitle = paste0("PO Floor ",posterioroddsfloor), x = "Profile", y = "Posterior Probability") +
    
  theme(axis.text.x=element_text(angle= -90)) 
ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/GroupsPosteriorChecks/Plots/",birthmodel,crtmodel,"BoxGroupsWithin",dpriorpoicuts[i]+1,"to",dpriorpoicuts[i+1],"daysbeforePOIPOFloor",posterioroddsfloor,".png"))
}
  
if(POUpdateMode == "lastxtests"){
  ggplot() +
    geom_boxplot(data = pdata, 
                 aes(x = profilegroup, y = PosteriorProb)) +
    labs(title = paste0("Posterior Probability within ",dpriorpoicuts[i]+1," to ",dpriorpoicuts[i+1]," days prior to period of interest"), subtitle = paste0("Last ",lastxtests," tests"), x = "Profile", y = "Posterior Probability") +
    
    theme(axis.text.x=element_text(angle= -90)) 
  ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/GroupsPosteriorChecks/Plots/",birthmodel,crtmodel,"BoxGroupsWithin",dpriorpoicuts[i]+1,"to",dpriorpoicuts[i+1],"daysbeforePOILast",lastxtests,"tests.png"))
  
}
  
  if(POUpdateMode == "birthfraction"){
    
    ggplot() +
      geom_boxplot(data = pdata, 
                   aes(x = profilegroup, y = PosteriorProb)) +
      labs(title = paste0("Posterior Probability within ",dpriorpoicuts[i]+1," to ",dpriorpoicuts[i+1]," days prior to period of interest"), subtitle = paste0("PO Floor ",birthoddsfraction," * birthodds"), x = "Profile", y = "Posterior Probability") +
      
      theme(axis.text.x=element_text(angle= -90)) 
    ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/GroupsPosteriorChecks/Plots/",birthmodel,crtmodel,"BoxGroupsWithin",dpriorpoicuts[i]+1,"to",dpriorpoicuts[i+1],"daysbeforePOIPOFloor",birthoddsfraction,"birthodds.png"))
  }

}
                     
#for (i in unique(data_postchecks$profilegroup)){

#  if(POUpdateMode == "floor"){
#  print(ggplot(data_postchecks[which(data_postchecks$profilegroup == i),], aes(x = age - refage, y = PosteriorProb)) +
#    geom_point() +
#    geom_smooth() +
#    labs(title = "All titres relative to start of incident", subtitle = paste0(i,"\nPOFloor",posterioroddsfloor), x  = "Age relative to incident start", y = "Posterior Probability") +
#    theme(axis.text.x = element_text(angle = -90)))
  #ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/GroupsPosteriorChecks/Plots/",birthmodel,crtmodel,"Scatter",i,"RelToPOIPOFloor",posterioroddsfloor,".png"))
#}
  
#  if(POUpdateMode == "lastxtests"){
#    print(ggplot(data_postchecks[which(data_postchecks$profilegroup == i),], aes(x = age - refage, y = PosteriorProb)) +
#            geom_point() +
#            geom_smooth() +
#            labs(title = "All titres relative to start of incident", subtitle = paste0(i,"\nLast ",lastxtests," tests"), x  = "Age relative to incident start", y = "Posterior Probability") +
#            theme(axis.text.x = element_text(angle = -90)))
    #ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/GroupsPosteriorChecks/Plots/",birthmodel,crtmodel,"Scatter",i,"RelToPOILast",lastxtests,"tests.png"))
#  }
  
#  if(POUpdateMode == "birthfraction"){
#    print(ggplot(data_postchecks[which(data_postchecks$profilegroup == i),], aes(x = age - refage, y = PosteriorProb)) +
#            geom_point() +
#            geom_smooth() +
#            labs(title = "All titres relative to start of incident", subtitle = paste0(i,"\nPOFloor",birthoddsfraction,"*birth odds"), x  = "Age relative to incident start", y = "Posterior Probability") +
#            theme(axis.text.x = element_text(angle = -90)))
    #ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/GroupsPosteriorChecks/Plots/",birthmodel,crtmodel,"Scatter",i,"RelToPOIPOFloor",birthoddsfraction,"birthodds.png"))
#  }
  





print(ggplot() +
  geom_boxplot(data = data_postchecks, aes(x = profilegroup, y = priorprob_crt)) +
  labs(title = "Birth Probability", x = "Profile", y = "Probability at birth") +
  theme(axis.text.x = element_text(angle = -90)))
ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/GroupsPosteriorChecks/Plots/",birthmodel,crtmodel,"BoxGroupsProbAtBirth.png"))


testlist <- c("covsandtitre1",
              "covsandtitre2",
              "covsandtitre3",
              "covsandtitre4",
              "covsandtitre5",
              "covsandtitre6",
              "covsandtitre7",
              "covsandtitre8")

for(i in 1:length(testlist)){
  
  if(POUpdateMode == "floor"){
  print(ggplot() +
    geom_boxplot(data = data_postchecks[which(data_postchecks$covsandtitre == testlist[i]),], aes(x = profilegroup, y = priorprob_crt)) +
    labs(title = paste0("Probability after ",i," test(s)"), subtitle = paste0("Posterior Odds Floor ",posterioroddsfloor), x = "Profile", y = "Posterior Probability") +
    theme(axis.text.x = element_text(angle = -90)))
ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/GroupsPosteriorChecks/Plots/",birthmodel,crtmodel,"POFloor",posterioroddsfloor,"BoxGroupsProbAfter",i,"Tests.png"))
  }
  
  if(POUpdateMode == "lastxtests"){
    print(ggplot() +
            geom_boxplot(data = data_postchecks[which(data_postchecks$covsandtitre == testlist[i]),], aes(x = profilegroup, y = priorprob_crt)) +
            labs(title = paste0("Probability after ",i," test(s)"), subtitle = paste0("Last ",lastxtests," tests"), x = "Profile", y = "Posterior Probability") +
            theme(axis.text.x = element_text(angle = -90)))
    ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/GroupsPosteriorChecks/Plots/",birthmodel,crtmodel,"Last",lastxtests,"testsBoxGroupsProbAfter",i,"Tests.png"))
  }
  
  if(POUpdateMode == "birthfraction"){
    print(ggplot() +
            geom_boxplot(data = data_postchecks[which(data_postchecks$covsandtitre == testlist[i]),], aes(x = profilegroup, y = priorprob_crt)) +
            labs(title = paste0("Probability after ",i," test(s)"), subtitle = paste0("Posterior Odds Floor ",birthoddsfraction,"*birth odds"), x = "Profile", y = "Posterior Probability") +
            theme(axis.text.x = element_text(angle = -90)))
    ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/GroupsPosteriorChecks/Plots/",birthmodel,crtmodel,"POFloor",birthoddsfraction,"birthoddsBoxGroupsProbAfter",i,"Tests.png"))
  }
}


###CHECK PROFILES OF ANIMALS WITH DIFFERENT FINAL POSTERIORS####


data_postchecks_lasttest <- data_postchecks[which(data_postchecks$age == data_postchecks$ageatlasttest),]


if(POUpdateMode == "floor"){
print(ggplot(data_postchecks_lasttest, aes(x = profilegroup, y = PosteriorProb)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = -90)) +
  labs(title = "Final posterior of animals by profile", x = "Profile", y = "Posterior Probability", subtitle = paste0("POFloor ", posterioroddsfloor)))

ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/GroupsPosteriorChecks/Plots/",birthmodel,crtmodel,"BoxGroupsFinalPosteriorsPOFloor",posterioroddsfloor,".png"))
}

if(POUpdateMode == "lastxtests"){
  ggplot(data_postchecks_lasttest, aes(x = profilegroup, y = PosteriorProb)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = -90)) +
    labs(title = "Final posterior of animals by profile", x = "Profile", y = "Posterior Probability", subtitle = paste0("Last  ", lastxtests, " tests"))
  
  ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/GroupsPosteriorChecks/Plots/",birthmodel,crtmodel,"BoxGroupsFinalPosteriorsLast",lastxtests,"tests.png"))
  
}

if(POUpdateMode == "birthfraction"){
  print(ggplot(data_postchecks_lasttest, aes(x = profilegroup, y = PosteriorProb)) +
          geom_boxplot() +
          theme(axis.text.x = element_text(angle = -90)) +
          labs(title = "Final posterior of animals by profile", x = "Profile", y = "Posterior Probability", subtitle = paste0("POFloor ", birthoddsfraction," * birth odds")))
  
  ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/GroupsPosteriorChecks/Plots/",birthmodel,crtmodel,"BoxGroupsFinalPosteriorsPOFloor",birthoddsfraction,"birthodds.png"))
}

if(POUpdateMode == "floor"){

ggplot(data_postchecks_lasttest, aes(x = predLR_cat, y = PosteriorProb)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = -90)) +
  labs(title = "Final posterior of animals by birth probability", subtitle = paste0(crtmodel,"\nPOFloor ",posterioroddsfloor), x = "Birth Probability", y = "Posterior Probability") +
  facet_wrap(~profilegroup)

  ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/groupsposteriorchecks/plots/FinalPosteriorsbyBirthProb",crtmodel,"POF",posterioroddsfloor,".png"))
}  
 

if(POUpdateMode == "lastxtests"){
  
  ggplot(data_postchecks_lasttest, aes(x = priorprob_crt_cat, y = PosteriorProb)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = -90)) +
    labs(title = "Final posterior of animals by birth probability", subtitle = paste0(crtmodel,"\nLast ",lastxtests, " tests"), x = "Birth Probability", y = "Posterior Probability") +
    facet_wrap(~profilegroup)
  
  ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/groupsposteriorchecks/plots/FinalPosteriorsbyBirthProb",crtmodel,"Last",lastxtests,"Tests.png"))
}

if(POUpdateMode == "birthfraction"){
  
  ggplot(data_postchecks_lasttest, aes(x = priorprob_crt_cat, y = PosteriorProb)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = -90)) +
    labs(title = "Final posterior of animals by birth probability", subtitle = paste0(crtmodel,"\nPOFloor ",birthoddsfraction," * birth odds"), x = "Birth Probability", y = "Posterior Probability") +
    facet_wrap(~profilegroup)
  
  ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/groupsposteriorchecks/plots/FinalPosteriorsbyBirthProb",crtmodel,"POF",birthoddsfraction,"birthodds.png"))
}  
 







print("Correlation between birth prior and final posterior")
for (i in unique(data_postchecks_lasttest$profilegroup)){
  print(paste0(i,": ",round(cor(data_postchecks_lasttest$priorprob_crt[which(data_postchecks_lasttest$profilegroup == i)], 
                                data_postchecks_lasttest$PosteriorProb[which(data_postchecks_lasttest$profilegroup == i)]),2)))
}



####CHECK UNUSUAL POSTERIOR PREDICTIONS####

HtestsPosteriorOdds <- data_postchecks[which(data_postchecks$class == "H"),]

#,c(2:139)

HtestsPosteriorOdds <- HtestsPosteriorOdds[order(HtestsPosteriorOdds$PosteriorOdds),]

head(HtestsPosteriorOdds,100)



###CHECK LOWER OUTLIERS FOR SEEMINGLY POSITIVE COWS####

if(plotoutliers == "Y"){
lprobcut <- 0.3
hprobcut <- 0.9

outlierdata <- data_postchecks[which((data_postchecks$profilegroup == "AllLowTwoHFinal" & 
                                       data_postchecks$PosteriorProb < lprobcut & 
                                       data_postchecks$age == data_postchecks$ageatlasttest) |
                                       (data_postchecks$profilegroup == "TwoHFinalWithSomeMedium" &
                                          data_postchecks$PosteriorProb < lprobcut &
                                          data_postchecks$age == data_postchecks$ageatlasttest) |
                                       (data_postchecks$profilegroup == "AllLowTwoHFinal" &
                                          data_postchecks$PosteriorProb < lprobcut &
                                          data_postchecks$age == data_postchecks$refage) |
                                       (data_postchecks$profilegroup == "TwoHWithSomeMedium" &
                                          data_postchecks$PosteriorProb < lprobcut &
                                          data_postchecks$age == data_postchecks$refage) |
                                       (data_postchecks$profilegroup == "TwoHFinalWithSomeMedium" &
                                          data_postchecks$PosteriorProb < lprobcut &
                                          data_postchecks$age == data_postchecks$refage) |
                                       (data_postchecks$profilegroup == "AllLowLast4TestsH" &
                                       data_postchecks$PosteriorProb < lprobcut &
                                       data_postchecks$age == data_postchecks$refage)),]





#outlierdata <- data_postchecks[which(data_postchecks$profilegroup == "SomeMNeverH" &
#                                                data_postchecks$PosteriorProb > hprobcut),]

head(cbind(outlierdata$Farm, outlierdata$calfeartag),100)



for (cow in unique(outlierdata$calfeartag)){
  
  if(POUpdateMode == "floor"){
  ggplot(data_postchecks[data_postchecks$calfeartag == cow,], aes(x = age)) +
    geom_point(aes(y = titre, color = class)) +
    geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
    geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
    geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
    scale_color_manual(values = group.colours) +
    geom_line(aes(y = PosteriorProb*100)) +
    geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
    scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
    labs(title = cow, subtitle = paste(crtmodel,"\nBirth Probability:", round(data_postchecks$priorprob_birth[data_postchecks$calfeartag == cow],2),"12mold Probability:",round(data_postchecks$priorprob_12mold[data_postchecks$calfeartag == cow],2),"Crt Probability:",round(data_postchecks$priorprob_crt[data_postchecks$calfeartag == cow],2), "\n Titre Cutpoints:", toString(cutpoints), "\n", "Age cutpoints:", toString(agebreaks),"\nPOFloor",posterioroddsfloor))
    
  ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/GroupsPosteriorChecks/Outliers/",cow,birthmodel,crtmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),"PFloor",posterioroddsfloor,".png"))
  }
  
  if(POUpdateMode == "lastxtests"){
  
    ggplot(data_postchecks[data_postchecks$calfeartag == cow,], aes(x = age)) +
      geom_point(aes(y = titre, color = class)) +
      geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
      geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
      geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
      geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
      geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
      scale_color_manual(values = group.colours) +
      geom_line(aes(y = PosteriorProb*100)) +
      geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
      geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
      scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
      labs(title = cow, subtitle = paste(crtmodel,"\n",birthmodel, "Birth Probability:", round(data_postchecks$priorprob_crt[data_postchecks$calfeartag == cow],2), "\n Titre Cutpoints:", toString(cutpoints), "\n", "Age cutpoints:", toString(agebreaks),"\nLast ",lastxtests," tests"))
    
    ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/GroupsPosteriorChecks/Outliers/",cow,birthmodel,crtmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),"Last",lastxtests,"tests.png"))
    
  }
  
  if(POUpdateMode == "birthfraction"){
    ggplot(data_postchecks[data_postchecks$calfeartag == cow,], aes(x = age)) +
      geom_point(aes(y = titre, color = class)) +
      geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
      geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
      geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
      geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
      geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
      scale_color_manual(values = group.colours) +
      geom_line(aes(y = PosteriorProb*100)) +
      geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
      geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
      scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
      labs(title = cow, subtitle = paste(crtmodel,"\n",birthmodel, "Birth Probability:", round(data_postchecks$priorprob_crt[data_postchecks$calfeartag == cow],2), "\n Titre Cutpoints:", toString(cutpoints), "\n", "Age cutpoints:", toString(agebreaks),"\nPOFloor",birthoddsfraction," * birth odds"))
    
    ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/GroupsPosteriorChecks/Outliers/",cow,birthmodel,crtmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),"PFloor",birthoddsfraction,"birthodds.png"))
  }
}
}


#####CaliPlots of posterior at POI and Target_QMMS#####

pdata <- data_postchecks[which( 
                                 data_postchecks$ageatfirstM == data_postchecks$age &
                                 data_postchecks$Target_QMMS != "U"),]

CaliPlot(pdata$PosteriorProb, as.numeric(pdata$Target_QMMS))

#if(POUpdateMode == "floor"){
#ggplot(outlierdata, aes(x = yield)) +
#  geom_histogram() +
#  labs(title = "Outlier data", subtitle = paste0("Posterior Prob",lprobcut," POfloor ",posterioroddsfloor), x = "Yield (l)", y = "Posterior Probability")
#}

#if(POUpdateMode == "lastxtests"){
#  ggplot(outlierdata, aes(x = yield)) +
#    geom_histogram() +
#    labs(title = "Outlier data", subtitle = paste0("Posterior Prob",lprobcut," Last ",lastxtests," tests"), x = "Yield (l)", y = "Posterior Probability")
  
#}


#####PLOT COWS WITH POSTERIOR FLOOR APPLIED#####

if(POUpdateMode == "floor" & posterioroddsfloor != 0){

  for (cow in unique(data_postchecks$calfeartag[which(data_postchecks$POFloorApplied == 1 & data_postchecks$nH >= 1)])[1:50]){
  ggplot(data[data$calfeartag == cow,], aes(x = age)) +
    geom_point(aes(y = titre, color = class)) +
    geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
    geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
    geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
    scale_color_manual(values = group.colours) +
    geom_line(aes(y = PosteriorProb*100)) +
    geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
    scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
    labs(title = data$calfeartag[data$calfeartag == cow], subtitle = paste(crtmodel,"\n",birthmodel, "Birth Probability:", round(data$priorprob_crt[data$calfeartag == cow],2), "\n Titre Cutpoints:", toString(cutpoints), "\n", "Age cutpoints:", toString(agebreaks),"\nPOFloor ",posterioroddsfloor))
  ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/GroupsPosteriorChecks/PosteriorFloorApplied/",cow,birthmodel,crtmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),"PFloor",posterioroddsfloor,".png"))
  }
}

if(POUpdateMode == "birthfraction"){
  
  for (cow in unique(data_postchecks$calfeartag[which(data_postchecks$POFloorApplied == 1 & data_postchecks$nH >= 1)])[1:50]){
    ggplot(data_postchecks[data_postchecks$calfeartag == cow,], aes(x = age)) +
      geom_point(aes(y = titre, color = class)) +
      geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
      geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
      geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
      geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
      geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
      scale_color_manual(values = group.colours) +
      geom_line(aes(y = PosteriorProb*100)) +
      geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
      geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
      scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
      labs(title = data_postchecks$calfeartag[data_postchecks$calfeartag == cow], subtitle = paste(crtmodel,"\n",birthmodel, "Birth Probability:", round(data_postchecks$priorprob_crt[data_postchecks$calfeartag == cow],2), "\n Titre Cutpoints:", toString(cutpoints), "\n", "Age cutpoints:", toString(agebreaks),"\nPOFloor ",birthoddsfraction," * birth odds"))
    ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/GroupsPosteriorChecks/PosteriorFloorApplied/",cow,birthmodel,crtmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),"PFloor",birthoddsfraction,"birthodds.png"))
  }
}




cowsPOFapplied <- unique(data_postchecks$calfeartag[which(data_postchecks$POFloorApplied == 1)])
cowsNoPOFapplied <- unique(data_postchecks$calfeartag[which(!(data_postchecks$calfeartag %in% cowsPOFapplied))])



###Plot POFApplied Cows vs NoPOFApplied Cows with similar likelihoods at first positive test###

if(POUpdateMode == "floor"){

ggplot() +
  geom_point(data=data_postchecks[which(data_postchecks$age == data_postchecks$refage &
                                          data_postchecks$profilegroup == "AllLowOneHFinal"),], 
             aes(x = log(likelihood), y = PosteriorProb),colour = "darkgreen") +
  geom_smooth(data=data_postchecks[which(data_postchecks$age == data_postchecks$refage &
                                           data_postchecks$profilegroup == "AllLowOneHFinal"),], 
              aes(x = log(likelihood), y = PosteriorProb),colour = "darkgreen") +
  labs(title = "AllLowOneHFinal Cows. Posterior Prob at point of interest", subtitle = paste0("POF ",posterioroddsfloor))

ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/GroupsPosteriorChecks/Plots/",birthmodel,crtmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),"AllLowOneHFinalPOF",posterioroddsfloor,".png"))

}

if(POUpdateMode == "lastxtests"){
  ggplot() +
    geom_point(data=data_postchecks[which(data_postchecks$age == data_postchecks$refage &
                                            data_postchecks$profilegroup == "AllLowOneHFinal"),], 
               aes(x = log(likelihood), y = PosteriorProb),colour = "darkgreen") +
    geom_smooth(data=data_postchecks[which(data_postchecks$age == data_postchecks$refage &
                                             data_postchecks$profilegroup == "AllLowOneHFinal"),], 
                aes(x = log(likelihood), y = PosteriorProb),colour = "darkgreen") +
    labs(title = "AllLowOneHFinal Cows. Posterior Prob at point of interest", subtitle = paste0("Last ",lastxtests," tests"))
  
  ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/GroupsPosteriorChecks/Plots/",birthmodel,crtmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),"AllLowOneHFinalLast",lastxtests,"tests.png"))
  
}

if(POUpdateMode == "birthfraction"){
  
  ggplot() +
    geom_point(data=data_postchecks[which(data_postchecks$age == data_postchecks$refage &
                                            data_postchecks$profilegroup == "AllLowOneHFinal"),], 
               aes(x = log(likelihood), y = PosteriorProb),colour = "darkgreen") +
    geom_smooth(data=data_postchecks[which(data_postchecks$age == data_postchecks$refage &
                                             data_postchecks$profilegroup == "AllLowOneHFinal"),], 
                aes(x = log(likelihood), y = PosteriorProb),colour = "darkgreen") +
    labs(title = "AllLowOneHFinal Cows. Posterior Prob at point of interest", subtitle = paste0("POF ",birthoddsfraction," * birth odds"))
  
  ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/GroupsPosteriorChecks/Plots/",birthmodel,crtmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),"AllLowOneHFinalPOF",birthoddsfraction,"birthodds.png"))
  
}

if(POUpdateMode == "floor"){

ggplot() +
  geom_point(data=data_postchecks[which(data_postchecks$calfeartag %in% cowsNoPOFapplied & 
                                          data_postchecks$age == data_postchecks$refage &
                                          data_postchecks$profilegroup == "AllLowOneHFinal"),], 
             aes(x = log(likelihood), y = PosteriorProb),colour = "darkgreen") +
  geom_smooth(data=data_postchecks[which(data_postchecks$calfeartag %in% cowsNoPOFapplied & 
                                           data_postchecks$age == data_postchecks$refage &
                                           data_postchecks$profilegroup == "AllLowOneHFinal"),], 
              aes(x = log(likelihood), y = PosteriorProb),colour = "darkgreen") +
  geom_point(data=data_postchecks[which(data_postchecks$calfeartag %in% cowsPOFapplied & 
                                          data_postchecks$age == data_postchecks$refage &
                                          data_postchecks$profilegroup == "AllLowOneHFinal"),], 
             aes(x = log(likelihood), y = PosteriorProb),colour = "darkred") +
  labs(title = "AllLowOneHFinal Cows. Posterior Prob at point of interest", subtitle = paste0("Posterior Odds Floor = ",posterioroddsfloor))

ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/GroupsPosteriorChecks/Plots/",birthmodel,crtmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),"AllLowOneHFinalPOFvsNoPOFatPOIPOF",posterioroddsfloor,".png"))


data_postchecks$loglike_cat <- cut(log(data_postchecks$likelihood), breaks = 6)

ggplot(data_postchecks[which(data_postchecks$age == data_postchecks$refage &
                               data_postchecks$profilegroup == "AllLowOneHFinal"),], 
       aes(x = loglike_cat, y = PosteriorProb)) +
         geom_boxplot() +
        labs(title = "AllLowOneHFinal Cows", subtitle = paste0("Posterior Probability by Likelihood at POI\nPOF ",posterioroddsfloor), x = "log(Likelihood)", y = "Posterior Probability")

ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/GroupsPosteriorChecks/Plots/",birthmodel,crtmodel,"AllLowOneHFinalByLikelihoodPOF",posterioroddsfloor,".png"))

}

if(POUpdateMode == "birthfraction"){
  
  ggplot() +
    geom_point(data=data_postchecks[which(data_postchecks$calfeartag %in% cowsNoPOFapplied & 
                                            data_postchecks$age == data_postchecks$refage &
                                            data_postchecks$profilegroup == "AllLowOneHFinal"),], 
               aes(x = log(likelihood), y = PosteriorProb),colour = "darkgreen") +
    geom_smooth(data=data_postchecks[which(data_postchecks$calfeartag %in% cowsNoPOFapplied & 
                                             data_postchecks$age == data_postchecks$refage &
                                             data_postchecks$profilegroup == "AllLowOneHFinal"),], 
                aes(x = log(likelihood), y = PosteriorProb),colour = "darkgreen") +
    geom_point(data=data_postchecks[which(data_postchecks$calfeartag %in% cowsPOFapplied & 
                                            data_postchecks$age == data_postchecks$refage &
                                            data_postchecks$profilegroup == "AllLowOneHFinal"),], 
               aes(x = log(likelihood), y = PosteriorProb),colour = "darkred") +
    labs(title = "AllLowOneHFinal Cows. Posterior Prob at point of interest", subtitle = paste0("Posterior Odds Floor = ",birthoddsfraction," * birth odds"))
  
  ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/GroupsPosteriorChecks/Plots/",birthmodel,crtmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),"AllLowOneHFinalPOFvsNoPOFatPOIPOF",birthoddsfraction,"birthodds.png"))
  
  
  data_postchecks$loglike_cat <- cut(log(data_postchecks$likelihood), breaks = 6)
  
  ggplot(data_postchecks[which(data_postchecks$age == data_postchecks$refage &
                                 data_postchecks$profilegroup == "AllLowOneHFinal"),], 
         aes(x = loglike_cat, y = PosteriorProb)) +
    geom_boxplot() +
    labs(title = "AllLowOneHFinal Cows", subtitle = paste0("Posterior Probability by Likelihood at POI\nPOF ",birthoddsfraction, " * birth odds"), x = "log(Likelihood)", y = "Posterior Probability")
  
  ggsave(paste0("y:/ian/johnesthresholds/johnesproper/data/GroupsPosteriorChecks/Plots/",birthmodel,crtmodel,"AllLowOneHFinalByLikelihoodPOF",birthoddsfraction,"birthodds.png"))
  
}

#####Plot Posterior Odds Floor Cows against normal cows####


POFCowTag <- "DE1404973820"

POFCowBirthPrior <- data_postchecks$birthpriorLR[which(data_postchecks$calfeartag == POFCowTag)][1]
POFCowageatfirsttest <- data_postchecks$ageatfirsttest[which(data_postchecks$calfeartag == POFCowTag)][1]
POFCowageatfirstH <- data_postchecks$ageatfirstH[which(data_postchecks$calfeartag == POFCowTag)][1]
POFCowrefage <- data_postchecks$refage[which(data_postchecks$calfeartag == POFCowTag)][1]
POFCowLikelihoodPOI <- data_postchecks$likelihood[which(data_postchecks$calfeartag == POFCowTag & data_postchecks$age == POFCowrefage)][1]



matchedcows <- unique(data_postchecks$calfeartag[which(
                              (data_postchecks$calfeartag %in% cowsPOFapplied) == FALSE &
                               data_postchecks$age == data_postchecks$refage &
                               data_postchecks$birthpriorLR - POFCowBirthPrior < 0.05 &
                                 data_postchecks$birthpriorLR - POFCowBirthPrior > - 0.05 &
                                 data_postchecks$ageatfirsttest - POFCowageatfirsttest < 5 &
                                 data_postchecks$ageatfirsttest - POFCowageatfirsttest > -5 &
                                 data_postchecks$ageatfirstH - POFCowageatfirstH < 10 &
                                 data_postchecks$ageatfirstH - POFCowageatfirstH > -10 &
                                 data_postchecks$likelihood - POFCowLikelihoodPOI < 50 &
                                 data_postchecks$likelihood - POFCowLikelihoodPOI > -50)])


for (i in matchedcows){
group.colours <- c("L" = "green", "M" = "orange", "H" = "red")

  print(ggplot(data_postchecks[data_postchecks$calfeartag == i,], aes(x = age)) +
    geom_point(aes(y = titre, color = class)) + 
    geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
    geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
    geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
    scale_color_manual(values = group.colours) +
    geom_line(aes(y = PosteriorProb*100)) +
    geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
    scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
    labs(title = data_postchecks$calfeartag[data_postchecks$calfeartag == i], subtitle = paste(crtmodel,"\n",birthmodel, "Birth Probability:", round(data_postchecks$priorprob_crt[data_postchecks$calfeartag == i],2), "\n Titre Cutpoints:", toString(cutpoints), "\n", "Age cutpoints:", toString(agebreaks))))

}


####Histograms of days prior to POI when cows pass probability thresholds



#source_python("Y:/Ian/JohnesThresholds/JohnesProper/Data/PythonScripts/PyFindFirstTitresAboveX.py")

#print('Finding first titres above x...')

#thresh = 0.1

#data_postchecks <- findfirsttitreabovex(data_postchecks, thresh)

#data_postchecks_firstabove <- data_postchecks[which(data_postchecks$FirstAbove == 1),]

#data_postchecks_firstabove <- data_postchecks_firstabove[which(data_postchecks_firstabove$profilegroup == "AllLowTwoHAllLow" |
#                                                                 data_postchecks_firstabove$profilegroup == "AllLowTwoHFinal" |
#                                                                 data_postchecks_firstabove$profilegroup == "TwoHWithSomeMedium" |
#                                                                 data_postchecks_firstabove$profilegroup == "TwoHFinalWithSomeMedium" |
#                                                                 data_postchecks_firstabove$profilegroup == "AllLowLastFourTestsHigh" |
#                                                                 data_postchecks_firstabove$profilegroup == "SomeMLast4TestsH"),]#
#
#
#print(paste0("Animals crossing threshold prior to POI: ",round(nrow(data_postchecks_firstabove[which((data_postchecks_firstabove$age - data_postchecks_firstabove$refage) <0),])/
#        nrow(data_postchecks_firstabove) * 100,1),"%"))

#ggplot(data_postchecks_firstabove, aes(x = age - refage)) +
#  geom_histogram() +
#  labs(title = "All Positive Groups", subtitle = paste0("First Posterior Probability > ",thresh," Relative To POI"), x = "Months")
#
#ggplot(data_postchecks_firstabove[which((data_postchecks_firstabove$age - data_postchecks_firstabove$refage) <= 0),], aes(x = age - refage)) +
#  geom_histogram() +
#  labs(title = "All Positive Groups", subtitle = paste0("First Posterior Probability > ",thresh," Prior To POI"), x = "Months")



                           

#for (pf in c("AllLowTwoHFinal",
#             "TwoHFinalWithSomeMedium",
#             "SomeMLast4TestsH")){
#
#  print(ggplot(data_postchecks_firstabove[which(data_postchecks_firstabove$profilegroup == pf),], aes(x = age - refage)) +
#    geom_histogram() +
#    labs(title = pf, subtitle = paste0("First Posterior Probability > ",thresh," Relative To POI"), x = "Months"))
#  
#    print(ggplot(data_postchecks_firstabove[which(data_postchecks_firstabove$profilegroup == pf & 
#                                                    (data_postchecks_firstabove$age - data_postchecks_firstabove$refage) <=0 ),], 
#                 aes(x = age - refage)) +
#    geom_histogram() +
#    labs(title = pf, subtitle = paste0("First Posterior Probability > ",thresh," Prior To POI"), x = "Months"))
#  

#}



#write.csv(data_postchecks_firstabove, "y:/ian/johnesthresholds/johnesproper/data/data_postchecks_firstabove.csv", row.names = FALSE)
#write.csv(data_postchecks, "y:/ian/johnesthresholds/johnesproper/data/data_postchecks.csv", row.names = FALSE)

rm(data_postchecks)
rm(pdata)
rm(HtestsPosteriorOdds)
rm(data_postchecks_lasttest)
rm(outlierdata)
rm(cowsNoPOFapplied)
rm(cowsPOFapplied)
#rm(data_postchecks_firstabove)
