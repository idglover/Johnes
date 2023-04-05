time0 <- Sys.time()

print("Reading data...")

data <- read.csv("y:/ian/johnesthresholds/johnesproper/data/data_posteriors.csv")

data$titre <- as.numeric(data$titre)
data$PosteriorProb <- as.numeric(data$PosteriorProb)

data$Target_QMMS <- as.factor(data$Target_Meyer)

str(data$Target_QMMS)

print("Plotting test cows...")

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
    geom_text(aes(x = age, y = -2, label = ifelse(POFloorApplied == 1,"*",""))) +  
    scale_color_manual(values = group.colours) +
    geom_line(aes(y = PosteriorProb*100)) +
    geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
    scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
    labs(title = data$calfeartag[data$calfeartag == cow], subtitle = paste(crtmodel,"\nBirth Probability:", round(data$priorprob_birth[data$calfeartag == cow],2),"12m Probability:",round(data$priorprob_12mold[data$calfeartag == cow],2),"Crt Probability:",round(data$priorprob_crt[data$calfeartag == cow],2),"\n Titre Cutpoints:", toString(cutpoints), "\n", "Age cutpoints:", toString(agebreaks))))
  if(POUpdateMode == "floor"){ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/PosteriorProbCharts/TestCows/",cow,birthmodel,crtmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),"PFloor",posterioroddsfloor,".png"))}
  if(POUpdateMode == "lastxtests"){ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/PosteriorProbCharts/TestCows/",cow,birthmodel,crtmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),"Last",lastxtests,"tests.png"))}
  if(POUpdateMode == "birthfraction"){ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/PosteriorProbCharts/TestCows/",cow,birthmodel,crtmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),birthoddsfraction,"birthprob.png"))}
  if(POUpdateMode == "ignoreuntil"){ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/PosteriorProbCharts/TestCows/",cow,birthmodel,crtmodel,"Titres",toString(cutpoints),"Ages",toString(agebreaks),"IgnoreUntil",ignorethreshold,".png"))}
}

if (producegenplots == "Y"){print("Producing general plots...")}

calfsamplemeyer1 <- sample(unique(data$calfeartag[data$Target_Meyer == "1"]), 100, replace = FALSE)

calfsamplemeyer0 <- sample(unique(data$calfeartag[data$Target_Meyer == "0"]), 100, replace = FALSE)

calfsamplemeyerU <- sample(unique(data$calfeartag[data$Target_Meyer == "U"]), 100, replace = FALSE)

calfsamplerandom <- sample(unique(data$calfeartag), 200, replace  = FALSE)

calfsampleqmms1 <- sample(unique(data$calfeartag[data$Target_QMMS == "1"]), 100, replace = FALSE)

calfsampleqmms0 <- sample(unique(data$calfeartag[data$Target_QMMS == "0"]), 100, replace = FALSE)

calfsampleqmmsU <- sample(unique(data$calfeartag[data$Target_QMMS == "U"]), 100, replace = FALSE)

calfsamplerandom <- sample(unique(data$calfeartag), 200, replace  = FALSE)

if(target == "MEYER"){

if (producegenplots == "Y"){
  group.colours <- c("L" = "green", "M" = "orange", "H" = "red")
  
  for (i in calfsamplemeyer1){
    ggplot(data[data$calfeartag == i,], aes(x = age)) +
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
      labs(title = i, subtitle = paste(crtmodel,"\nBirth Probability:", round(data$priorprob_birth[data$calfeartag == cow],2),"12m Probability:",round(data$priorprob_12mold[data$calfeartag == cow],2),"Crt Probability:",round(data$priorprob_crt,2),"\n Titre Cutpoints:", toString(cutpoints), "\n", "Age cutpoints:", toString(agebreaks)))
    
    ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/PosteriorProbCharts/MeyerPosCows/",i,".png"))
  }}

if (producegenplots == "Y"){
  group.colours <- c("L" = "green", "M" = "orange", "H" = "red")
  
  for (i in calfsamplemeyerU){
    ggplot(data[data$calfeartag == i,], aes(x = age)) +
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
      labs(title = i, subtitle = paste(crtmodel,"\nBirth Probability:", round(data$priorprob_birth[data$calfeartag == cow],2),"12m Probability:",round(data$priorprob_12mold[data$calfeartag == cow],2),"Crt Probability:",round(data$priorprob_crt,2),"\n Titre Cutpoints:", toString(cutpoints), "\n", "Age cutpoints:", toString(agebreaks)))
    
    ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/PosteriorProbCharts/MeyerUnknownCows/",i,".png"))
  }}

if (producegenplots == "Y"){
  
  group.colours <- c("L" = "green", "M" = "orange", "H" = "red")
  
  for (i in calfsamplerandom){
    ggplot(data[data$calfeartag == i,], aes(x = age)) +
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
      labs(title = i, subtitle = paste(crtmodel,"\nBirth Probability:", round(data$priorprob_birth[data$calfeartag == cow],2),"12m Probability:",round(data$priorprob_12mold[data$calfeartag == cow],2),"Crt Probability:",round(data$priorprob_crt,2),"\n Titre Cutpoints:", toString(cutpoints), "\n", "Age cutpoints:", toString(agebreaks)))
    
    ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/PosteriorProbCharts/RandomCows/",i,".png"))
  }}

if (producegenplots == "Y"){

group.colours <- c("L" = "green", "M" = "orange", "H" = "red")

for (i in calfsamplemeyer0){
  ggplot(data[data$calfeartag == i,], aes(x = age)) +
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
    labs(title = i, subtitle = paste(crtmodel,"\nBirth Probability:", round(data$priorprob_birth[data$calfeartag == cow],2),"12m Probability:",round(data$priorprob_12mold[data$calfeartag == cow],2),"Crt Probability:",round(data$priorprob_crt,2),"\n Titre Cutpoints:", toString(cutpoints), "\n", "Age cutpoints:", toString(agebreaks)))
  
  ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/PosteriorProbCharts/MeyerNegCows/",i,".png"))
}}

}

if(target == "QMMS"){
  
  if (producegenplots == "Y"){
    group.colours <- c("L" = "green", "M" = "orange", "H" = "red")
    
    for (i in calfsampleqmms1){
      ggplot(data[data$calfeartag == i,], aes(x = age)) +
        geom_point(aes(y = titre, color = class)) + 
        geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
        geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
        geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
        geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
        geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
        #geom_text(aes(y = -4, label = ifelse(POFloorApplied == 1,"POF","")), size = 2, angle = -90 ) +
        scale_color_manual(values = group.colours) +
        geom_line(aes(y = PosteriorProb*100)) +
        geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
        geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
        scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
        labs(title = i, subtitle = paste(crtmodel,"\nBirth Probability:", round(data$priorprob_birth[data$calfeartag == i],2),"12m Probability:",round(data$priorprob_12mold[data$calfeartag == i],2),"Crt Probability:",round(data$priorprob_crt[data$calfeartag == i],2),"\n Titre Cutpoints:", toString(cutpoints), "\n", "Age cutpoints:", toString(agebreaks)))
      
      ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/PosteriorProbCharts/QMMSPosCows/",i,".png"))
    }}
  
  if (producegenplots == "Y"){
    group.colours <- c("L" = "green", "M" = "orange", "H" = "red")
    
    for (i in calfsampleqmmsU){
      ggplot(data[data$calfeartag == i,], aes(x = age)) +
        geom_point(aes(y = titre, color = class)) + 
        geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
        geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
        geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
        geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
        geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
        #geom_text(aes(y = -4, label = ifelse(POFloorApplied == 1,"POF","")), size = 2, angle = -90 ) +
        scale_color_manual(values = group.colours) +
        geom_line(aes(y = PosteriorProb*100)) +
        geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
        geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
        scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
        labs(title = i, subtitle = paste(crtmodel,"\nBirth Probability:", round(data$priorprob_birth[data$calfeartag == i],2),"12m Probability:",round(data$priorprob_12mold[data$calfeartag == i],2),"Crt Probability:",round(data$priorprob_crt[data$calfeartag == i],2),"\n Titre Cutpoints:", toString(cutpoints), "\n", "Age cutpoints:", toString(agebreaks)))
      
      ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/PosteriorProbCharts/QMMSUnknownCows/",i,".png"))
    }}
  
  if (producegenplots == "Y"){
    
    group.colours <- c("L" = "green", "M" = "orange", "H" = "red")
    
    for (i in calfsamplerandom){
      ggplot(data[data$calfeartag == i,], aes(x = age)) +
        geom_point(aes(y = titre, color = class)) + 
        geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
        geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
        geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
        geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
        geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
        #geom_text(aes(y = -4, label = ifelse(POFloorApplied == 1,"POF","")), size = 2, angle = -90 ) +
        scale_color_manual(values = group.colours) +
        geom_line(aes(y = PosteriorProb*100)) +
        geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
        geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
        scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
        labs(title = i, subtitle = paste(crtmodel,"\nBirth Probability:", round(data$priorprob_birth[data$calfeartag == i],2),"12m Probability:",round(data$priorprob_12mold[data$calfeartag == i],2),"Crt Probability:",round(data$priorprob_crt[data$calfeartag == i],2),"\n Titre Cutpoints:", toString(cutpoints), "\n", "Age cutpoints:", toString(agebreaks)))
      
      ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/PosteriorProbCharts/RandomCows/",i,".png"))
    }}
  
  if (producegenplots == "Y"){
    
    group.colours <- c("L" = "green", "M" = "orange", "H" = "red")
    
    for (i in calfsampleqmms0){
      ggplot(data[data$calfeartag == i,], aes(x = age)) +
        geom_point(aes(y = titre, color = class)) + 
        geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
        geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) + 
        geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
        geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
        geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
        #(aes(y = -4, label = ifelse(POFloorApplied == 1,"POF","")), size = 2, angle = -90 ) +
        scale_color_manual(values = group.colours) +
        geom_line(aes(y = PosteriorProb*100)) +
        geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
        geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
        scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
        labs(title = i, subtitle = paste(crtmodel,"\nBirth Probability:", round(data$priorprob_birth[data$calfeartag == i],2),"12m Probability:",round(data$priorprob_12mold[data$calfeartag == i],2),"Crt Probability:",round(data$priorprob_crt[data$calfeartag == i],2),"\n Titre Cutpoints:", toString(cutpoints), "\n", "Age cutpoints:", toString(agebreaks)))
      
      ggsave(paste("y:/ian/johnesthresholds/johnesproper/data/PosteriorProbCharts/QMMSNegCows/",i,".png"))
    }}
  
}

time2 <- Sys.time()

print(paste("Time processing plots:", round(difftime(time2, time0, units = "mins"),2), "mins"))

