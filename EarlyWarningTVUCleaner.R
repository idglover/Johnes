tm <- timerstart()

toremove <- c("herdjtdates")

tvupath <- "Y:/ian/johnesthresholds/johnesproper/data/EarlyWarning/EarlyWarningTVUs/" #TVUs for all QMMS herds in Oct 2022

farms <- list.files(path = tvupath) # All files in TVU directory

farms <- substr(farms, 1,nchar(farms)-4) #Remove file extension

for(mu in 1:length(farms)){
  
  FARM <- farms[mu]
  pctdone <- round(mu/length(farms),2)*100
  cat(paste0("\r",FARM," (",pctdone,"%)"))
  con <- dbConnect(RSQLite::SQLite(),paste0(tvupath,FARM,".tvu"))
  
  for(t in toremove){
    tryCatch({dbRemoveTable(con, t)},
           error = function(cond){})
  }
  dbDisconnect(con)
  
}
timerend(tm)