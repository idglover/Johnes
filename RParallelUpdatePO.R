RparallelUpdatePO <- function(dt, pos, pof){
  
  time0 <- Sys.time()
  
  dt$lastcalf <- ""
  dt$lastcalf[2:nrow(dt)] <- dt$calfeartag[1:nrow(dt) - 1]
  dt$newcalf <- ifelse(dt$calfeartag != dt$lastcalf, 1, 0)
  
  nc <- detectCores()
  
  sr <- rep(0,nc-1)
  
  for(i in 1:length(sr)){
    sr[i] <- round(nrow(dt)/length(sr)*i)
  }
  
  pp <- foreach(i = 1:(length(sr)-1), .combine = "c") %do% {
    a <- ifelse(i == 1, 1, sr[i-1] + 1)
    
    b <- sr[i]
    
    tempdata <- dt[a:b,]
    
    a <- nrow(tempdata)-19
    b <- nrow(tempdata)
    
    match(1, tempdata$newcalf[a:b])
  
  }
  
  srmod <- sr
  srmod[1:length(srmod)-1] <- sr[1:length(sr)-1] - 21 + pp
  
  
 
  
  dt$PosteriorOdds <- 0
  
  regcores(-1)
  
  pp <- foreach(i = 1:length(srmod), .packages = "foreach") %dopar% {
    a <- ifelse(i == 1, 1, srmod[i - 1] + 1)
    b <- srmod[i]
    tempdata <- dt[a:b,]
    pp2 <- foreach(r = 1:nrow(tempdata)) %do% {
      tryCatch({write.table(r, paste0("C:/Users/Ian.glover.HEADOFFICE/Documents/PythonTools/ForEachRowCounter/rowsdonecore",i,".txt"), append = FALSE, sep = " ", dec = ".",
                            row.names = FALSE, col.names = FALSE)}, 
               error = function(cond){})
      if(tempdata$newcalf[r] == 1){
        if(pos == "crt"){
          po <- ifelse((tempdata$priorodds_crt[r] *
                   tempdata$likelihood[r]) < pof,
                 pof,
                 tempdata$priorodds_crt[r] *
                   tempdata$likelihood[r])
        }
        if(pos == "12mold"){
          po <- ifelse((tempdata$priorodds_12mold[r] *
                          tempdata$likelihood[r]) < pof,
                       pof,
                       tempdata$priorodds_12mold[r] *
                         tempdata$likelihood[r])
        }
        if(pos == "birth"){
          po <- ifelse((tempdata$priorodds_birth[r] *
                          tempdata$likelihood[r]) < pof,
                       pof,
                       tempdata$priorodds_birth[r] *
                         tempdata$likelihood[r])
        }
      }
      if(tempdata$newcalf[r] == 0){
        po <- ifelse((pom1 *
                 tempdata$likelihood[r]) < pof,
               pof,
               pom1 *
                 tempdata$likelihood[r])
      }
      pom1 <- po
      po
      
    }
    unlist(pp2)
  }
  stopCluster(cl)
  
  po <- unlist(pp[1])
  
  for(i in 2:length(srmod)){
    po <- c(po, unlist(pp[i]))
  }
  
  dt$PosteriorOdds <- po
  
  time1 <- Sys.time()
  print(paste0("Time taken updating posteriors: ", round(difftime(time1, time0, units = "mins"),1)," mins"))
  return(dt)
}

