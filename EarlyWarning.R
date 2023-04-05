####SET PARAMETERS####

#####DATA HARVESTING#####

tvupath <- "Y:/ian/johnesthresholds/johnesproper/data/EarlyWarning/EarlyWarningTVUs/" #TVUs for all QMMS herds in Oct 2022

minherdsize <- 50 #Minumimum size of herd

minpropherdtested <- 0.7 #Minimum proportion of herd tested

maxtestingintvl <- 180 #Maximum interval between herd tests 

nearlytests <- 4 #Number of early herd test dates to consider

firstmrfirstjintvl <- 1095 #Minimum number of days between first milk recording and first johne's test

testsvalidfrom <- "2010-12-01"

####SELECT FARMS####

farms <- list.files(path = tvupath) # All files in TVU directory

farms <- substr(farms, 1,nchar(farms)-4) #Remove file extension

farmsjttab <- data.frame(farm = farms)

#####CLEAN TVUs#####

cat("Cleaning TVUs...\n")

source("Y:/Ian/R/JohnesProper/EarlyWarningTVUCleaner.R")

#####ELIMINATE FARMS WITH NO JOHNE'S TESTING#####

tm <- timerstart()

cat("\nEliminating farms with no Johne's testing...\n")

farmsjttab$jtesting <- 0

for(i in 1:nrow(farmsjttab)) {
  
  FARM <- farmsjttab$farm[i]
  pctdone <- round(i/nrow(farmsjttab),2)*100
  cat(paste0("\r",FARM," (",pctdone,"%)"))
  con <- dbConnect(RSQLite::SQLite(),paste0(tvupath,FARM,".tvu"))
  
  jr <- dbReadTable(con, "johnes_result")
  
  if(nrow(jr) != 0){
    farmsjttab$jtesting[i] <- 1
  }
  
  dbDisconnect(con)
}

farmsjttab <- farmsjttab[farmsjttab$jtesting == 1,]

timerend(tm)

#####FURTHER SELECT FARMS#####

a <- timerstart()

cat("\nFurther selecting farms...\n")

farmsjttab$sufficienttests <- NA
farmsjttab$firstherdjtest <- NA
farmsjttab$firstmr <- NA
farmsjttab$okherdsize <- NA
farmsjttab$afterkitchange <- NA

for (i in 1:nrow(farmsjttab)){
  
  FARM <- farmsjttab$farm[i]
  pctdone <- round(i/nrow(farmsjttab),2)*100
  cat(paste0("\r",FARM," (",pctdone,"%)"))
  con <- dbConnect(RSQLite::SQLite(),paste0(tvupath,FARM,".tvu"))
  
  sqlcode <- 
    
    "SELECT 

    jr.date,
    count(jr.animalId) as njtested

    FROM

    johnes_result jr

    GROUP BY jr.date

    ORDER BY jr.date"
  
  herdjtdates <- dbGetQuery(con, sqlcode)
  
  dbWriteTable(con, "herdjtdates", herdjtdates)
  
  sqlcode <- 
    
    'SELECT

    hjt.date,
    count(ca.id) as herdsize

    FROM herdjtdates hjt, cow_alive ca



    INNER JOIN animal a on ca.animalId = a.id

    INNER JOIN lactation l on a.id = l.animalId 

    WHERE 
    a.sex = "C" AND
    ca.cAlive = 1 AND
    ca.cOnFarm = 1 AND
    l.parity >= 1 AND
    julianday(hjt.date) >= julianday(l.calvingDate) AND 
    (julianday(l.end) >= julianday(hjt.date) OR l.end IS NULL) AND
    julianday(hjt.date) >= julianday(ca.start) AND 
    (julianday(ca.end) > julianday(hjt.date) OR ca.end IS NULL)

    GROUP BY hjt.date

    ORDER BY hjt.date, a.id'
  
  herdsize <- dbGetQuery(con, sqlcode)
  
  sig <- ifelse(nrow(herdsize) < nearlytests, nrow(herdsize), nearlytests)
  
  farmsjttab$okherdsize[i] <- ifelse(min(herdsize$herdsize[1:sig]) >= minherdsize, 1, 0)
  
  sqlcode <- 
    
    'SELECT

    min(o.date) as date
  
    FROM official_recording o
  
    WHERE o.isOfficial = 1'
  
  firstmrdate <- dbGetQuery(con, sqlcode)[[1]]
  
  dbDisconnect(con)
  
  herdjtdates <- merge(herdjtdates, herdsize, by = "date", all.x = TRUE)
  
  herdjtdates$wht <- ifelse((herdjtdates$njtested/herdjtdates$herdsize) >= minpropherdtested, 1, 0)

  #####GET INTERVALS BETWEEN HERD TESTS#####
  
  herdjtdates$intvl <- NA
  
  if(nrow(herdjtdates) >= nearlytests){
    for (r in 2:nrow(herdjtdates)){
      herdjtdates$intvl[r] <- round(difftime(herdjtdates$date[r],
                                             herdjtdates$date[r-1],
                                             units = "days"),0)
    }
  }
  
  if(nrow(herdjtdates) < nearlytests){
    farmsjttab$sufficienttests[i] <- 0
  }
  
  if(nrow(herdjtdates) >= nearlytests){
  
    farmsjttab$sufficienttests[i] <-
      ifelse(nrow(herdjtdates) >= nearlytests &
               mean(herdjtdates$wht[1:nearlytests]) == 1 &
               max(herdjtdates$intvl[2:nearlytests]) <= maxtestingintvl &
               all(!is.na(herdjtdates$wht[1:nearlytests])) == TRUE,
             1,0)
  }


  #####GET EARLIEST MILK RECORDING AND JOHNE'S TESTING DATES#####

  farmsjttab$firstherdjtest[i] <- herdjtdates$date[1]
  farmsjttab$firstmr[i] <- firstmrdate 


  farmsjttab$afterkitchange[i] <- ifelse(difftime(herdjtdates$date[1], testsvalidfrom) >= 0, 1, 0)

}

cat("\nLimiting to farms with regular early whole herd testing...")

farmsjttab <- farmsjttab[farmsjttab$sufficienttests == 1,]

cat(paste0("\nLimiting to farms milk recording at least ",firstmrfirstjintvl," days prior to start of Johne's testing..."))

farmsjttab$mrjint <- round(difftime(as.POSIXct(farmsjttab$firstherdjtest),
                              as.POSIXct(farmsjttab$firstmr),
                              units = "days"),0)

farmsjttab <- farmsjttab[farmsjttab$mrjint >= firstmrfirstjintvl,]

cat(paste0("\nLimiting to herds of at least ", minherdsize, " cows..."))

farmsjttab <- farmsjttab[farmsjttab$okherdsize == 1,]

cat(paste0("\nLimiting to herds where first test was after ",testsvalidfrom))

farmsjttab <- farmsjttab[farmsjttab$afterkitchange == 1,]

timerend(a)

cat(paste0("\n",nrow(farmsjttab)," farms selected"))

####HARVEST DATA FOR SELECTED FARMS####

cat("\nGetting data from selected farms...\n")

data_modelling <- data.frame()

for (i in 1:nrow(farmsjttab)){
  
  FARM <- farmsjttab$farm[i]
  pctdone <- round(i/nrow(farmsjttab),2)*100
  cat(paste0("\r",FARM," (",pctdone,"%)"))
  con <- dbConnect(RSQLite::SQLite(),paste0(tvupath,FARM,".tvu"))
  
  testdata <- dbReadTable(con, "herdjtdates")[1:nearlytests,]
  
  testdata$intvl <- NA
  
  for (r in 2:nrow(testdata)){
    testdata$intvl[r] <- round(difftime(testdata$date[r],
                                  testdata$date[r-1],
                                  units = "days"),0)
  }
  
  testdata <- testdata[,c('date',
                          'intvl',
                          'njtested')]
  
  #####GET HERDSIZE#####
  
  sqlcode <-
    
    'SELECT
  
    hjt.date,
    count(ca.id) as herdsize
  
    FROM
  
    herdjtdates hjt
  
    INNER JOIN cow_alive ca on julianday(hjt.date) >= julianday(ca.start) AND (julianday(ca.end) > julianday(hjt.date) OR ca.end IS NULL)
    INNER JOIN animal a on ca.animalId = a.id
    INNER JOIN lactation l on l.animalId = a.id AND julianday(hjt.date) >= julianday(l.calvingDate) AND (julianday(l.end) > julianday(hjt.date) OR l.end IS NULL)
  
    WHERE ca.cAlive = 1 AND
    ca.cOnFarm = 1 AND
    a.sex = "C" AND
    l.parity >= 1
  
    GROUP BY hjt.date'
  
  herdsize <- dbGetQuery(con, sqlcode)
  
  testdata <- merge(testdata, herdsize, by = "date", all.x = TRUE)
  
  #####GET N TEST POSITIVE#####
  
  sqlcode <-
    
    'SELECT
  
    hjt.date,
    count(jr.value) as ntestpos
  
    FROM
  
    herdjtdates hjt
  
    INNER JOIN johnes_result jr on jr.date = hjt.date
  
    WHERE jr.value >= 30
  
    GROUP BY hjt.date'
  
  ntestpos <- dbGetQuery(con, sqlcode)
  
  testdata <- merge(testdata, ntestpos, by = "date", all.x = TRUE)
  
  testdata$proptestedtestpos <- testdata$ntestpos/testdata$njtested
  testdata$propherdtestpos <- testdata$ntestpos/testdata$herdsize
    
  #####GET N STATUS PROVISIONALLY POSITIVE#####
  
  sqlcode <-
    
    'SELECT

    hjt.date,
    count(js.cTVStatus) as nstatusprovpos

    FROM

    herdjtdates hjt

    INNER JOIN johnes_state_2 js on julianday(hjt.date) >= julianday(js.start) AND (julianday(js.end) > julianday(hjt.date) OR js.end IS NULL)

	  INNER JOIN cow_alive ca on js.animalId = ca.animalId AND julianday(hjt.date) >= julianday(ca.start) AND (julianday(ca.end) > julianday(hjt.date) OR ca.end IS NULL)
	
	  INNER JOIN animal a on a.id = js.animalId
	
    WHERE js.cTVStatus = 3 AND
	  ca.cAlive = 1 AND
	  ca.cOnFarm = 1
	
	  GROUP BY hjt.date
	
	  ORDER BY hjt.date'
  
  nstatusprovpos <- dbGetQuery(con, sqlcode)
  
  testdata <- merge(testdata, nstatusprovpos, by = "date", all.x = TRUE)
  
  testdata$proptestedstatusprovpos <- testdata$nstatusprovpos/testdata$njtested
  testdata$propherdstatusprovpos <- testdata$nstatusprovpos/testdata$herdsize
  
  #####GET N STATUS POSITIVE#####
  
  sqlcode <-
    
    'SELECT

    hjt.date,
    count(js.cTVStatus) as nstatuspos

    FROM

    herdjtdates hjt

    INNER JOIN johnes_state_2 js on julianday(hjt.date) >= julianday(js.start) AND (julianday(js.end) > julianday(hjt.date) OR js.end IS NULL)

	  INNER JOIN cow_alive ca on js.animalId = ca.animalId AND julianday(hjt.date) >= julianday(ca.start) AND (julianday(ca.end) > julianday(hjt.date) OR ca.end IS NULL)
	
	  INNER JOIN animal a on a.id = js.animalId
	
    WHERE js.cTVStatus = 2 AND
	  ca.cAlive = 1 AND
	  ca.cOnFarm = 1
	
	  GROUP BY hjt.date
	
	  ORDER BY hjt.date'
  
  nstatuspos <- dbGetQuery(con, sqlcode)
  
  testdata <- merge(testdata, nstatuspos, by = "date", all.x = TRUE)
  
  testdata$proptestedstatuspos <- testdata$nstatuspos/testdata$njtested
  testdata$propherdstatuspos <- testdata$nstatuspos/testdata$herdsize
  
  #####GET MEAN TITRE NEGATIVE COWS#####
  
  for (cpt in c(10,20,30)){
  
    sqlcode <- paste0(
    
      'SELECT
  
      hjt.date,
      avg(jr.value) as mtl',cpt,'
    
      FROM
    
      herdjtdates hjt
	
	    INNER JOIN johnes_result jr on jr.date = hjt.date
	
	    WHERE jr.value < ',cpt,'
	
	    GROUP BY hjt.date'
    )
  
    assign(paste0("mtl",cpt), dbGetQuery(con, sqlcode))
    
    testdata <- merge(testdata, get(paste0("mtl",cpt)), by = "date", all.x = TRUE)
    
  }
  
  #####GET MEAN TITRE#####
  
  sqlcode <-
    
    'SELECT
  
    hjt.date,
    avg(jr.value) as meantitre
    
    FROM
    
    herdjtdates hjt
	
	  INNER JOIN johnes_result jr on jr.date = hjt.date
	
	  GROUP BY hjt.date'
  
  meantitre <- dbGetQuery(con, sqlcode)
  
  testdata <- merge(testdata, meantitre, by = "date", all.x = TRUE)
  
  #####ADD FARM NAME#####
  
  testdata$farm <- FARM
  
  #####REORDER COLUMNS#####
  
  colnum <- match("farm", colnames(testdata))
  
  testdata <- testdata[,c(colnum, 2:colnum-1)]
  
  #####CLEAN AND CLOSE TVU#####
  
  dbRemoveTable(con, "herdjtdates")
  
  dbDisconnect(con)
  
  data_modelling <- rbind(data_modelling, testdata)
    
}

data_modelling[is.na(data_modelling)] <- 0
data_modelling$intvl[data_modelling$intvl == 0] <- NA


####DESCRIPTIVE STATS####

descstats(data_modelling)
  
####CONVERT DATA LONG TO WIDE####

data_modelling$testnum <- 1

for (r in 2:nrow(data_modelling)){
  data_modelling$testnum[r] <-
    ifelse(data_modelling$farm[r] != data_modelling$farm[r-1], 1,
           data_modelling$testnum[r-1] + 1)
}

data_modelling_wide <- unite(data_modelling,
                        concat,
                        date,
                        intvl,
                        njtested,
                        herdsize,
                        ntestpos,
                        proptestedtestpos,
                        propherdtestpos,
                        nstatusprovpos,
                        proptestedstatusprovpos,
                        propherdstatusprovpos,
                        nstatuspos,
                        proptestedstatuspos,
                        propherdstatuspos,
                        mtl10,
                        mtl20,
                        mtl30,
                        meantitre,
                        sep = ":",
                        remove = TRUE)

data_modelling_wide <- spread(data_modelling_wide, testnum, concat)

for(i in 1:nearlytests){

  data_modelling_wide <- separate(data_modelling_wide,
                   2 + (i-1) * ntimevaryingvars,
                   paste0(timevaryingvarnames,"_",i),
                   sep = ":", 
                   remove = TRUE, 
                   extra = "warn", 
                   convert = TRUE)


}

####CHECK CORRELATIONS BETWEEN FEATURES####

numericfts <- data.frame(ft = 1:ncol(data_modelling_wide))
numericfts$numeric <- 0

for(r in 1:nrow(numericfts)){
  numericfts$numeric[r] <- ifelse(class(data_modelling_wide[,r]) == "numeric", 1, 0)
}

numericfts <- numericfts[numericfts$numeric == 1,]

M = cor(data_modelling_wide[,numericfts$ft])
corrplot(M, method = 'number')
