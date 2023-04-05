####READ DATA####

data_brms <- read.csv("y:/ian/johnesthresholds/johnesproper/data/data_brms.csv")

dbmtr <- read.csv("y:/ian/johnesthresholds/johnesproper/data/dbmtr.csv")
dbmte <- read.csv("y:/ian/johnesthresholds/johnesproper/data/dbmte.csv")

####ADD TEST NUMBER####

data_brms$testnum <- as.numeric(substr(data_brms$covsandtitre,13,15))

####MERGE POSTERIOR PROBABILITIES AND POFAPPLIED####

dbmtr <- merge(dbmtr, data_brms[,c('Farm', 
                                   'calfeartag',
                                   'date',
                                   'testnum',
                                   'PosteriorProb', 
                                   'POFloorApplied')],
               by = c('Farm', 
                      'calfeartag', 
                      'date'),
               all.x = TRUE)

dbmte <- merge(dbmte, data_brms[,c('Farm', 
                                   'calfeartag',
                                   'date',
                                   'testnum',
                                   'PosteriorProb', 
                                   'POFloorApplied')],
               by = c('Farm', 
                      'calfeartag', 
                      'date'),
               all.x = TRUE)


####CALIPLOTS####

#####TRAINSET#####

######ALL TEST RESULTS######

CaliPlot(dbmtr$PosteriorProb,
         dbmtr$Target_altdef1,
         ptitle = "Trainset",
         psubtitle = "All Test Results")

#####FINAL POSTERIORS#####

CaliPlot(dbmtr$PosteriorProb[dbmtr$testnum == dbmtr$ntests],
         dbmtr$Target_altdef1[dbmtr$testnum == dbmtr$ntests],
         ptitle = "Trainset",
         psubtitle = "Final Posteriors")

#####BY TESTNUMBER#####

for (tn in 1:max(dbmtr$testnum)){
  print(CaliPlot(dbmtr$PosteriorProb[dbmtr$testnum == tn],
           dbmtr$Target_altdef1[dbmtr$testnum == tn],
           ptitle = "Trainset",
           psubtitle = paste0("Test number: ",tn)))
}

#####TESTSET#####

######ALL TEST RESULTS######

CaliPlot(dbmte$PosteriorProb,
         dbmte$Target_altdef1,
         ptitle = "Testset",
         psubtitle = "All Test Results")

#####FINAL POSTERIORS#####

CaliPlot(dbmte$PosteriorProb[dbmte$testnum == dbmte$ntests],
         dbmte$Target_altdef1[dbmte$testnum == dbmte$ntests],
         ptitle = "Testset",
         psubtitle = "Final Posteriors")

#####BY TESTNUMBER#####

for (tn in 1:max(dbmte$testnum)){
  print(CaliPlot(dbmte$PosteriorProb[dbmte$testnum == tn],
                 dbmte$Target_altdef1[dbmte$testnum == tn],
                 ptitle = "Testset",
                 psubtitle = paste0("Test number: ",tn)))
}

