####READ DATA_BRMS####

data <- read.csv("y:/ian/johnesthresholds/johnesproper/data/data_brms.csv")

####LIMIT TO ANIMALS THAT HAD A POSITIVE RESULTS, AND AT LEAST TWO TESTS####

data <- data[data$ageatfirstH != 0 &
               data$ntests >= 2,]

####EXCLUDE ANIMALS WITH MISSING YIELD ETC.####


uniquecows <- unique(data$calfeartag)

regcores(-1)

missing <- foreach (cow = uniquecows, .combine = "c") %dopar% {
  tempdata <- data[data$calfeartag == cow,]
  ifelse(!is.na(match(TRUE, is.na(tempdata$yield))) |
           !is.na(match(TRUE, is.na(tempdata$scc))) |
           !is.na(match(TRUE, is.na(tempdata$meantitrenegcows))), 1, 0)
}

stopCluster(cl)

ms_df <- data.frame(cow = uniquecows, msg = missing)
ms_df <- ms_df[ms_df$msg == 0,]

data <- data[data$calfeartag %in% ms_df$cow,]

rm(ms_df)
rm(missing)

####COUNT NUMBER OF HIGH/MEDIUM TEST RESULTS####

#data$nH <- str_count(data$profile,"H")
#data$nM <- str_count(data$profile,"M")

####IDENTIFY COWS THAT HIT POF####

uniquecows <- unique(data$calfeartag)

regcores(-1)

prevpof <- foreach(cow = uniquecows, .combine = "rbind") %dopar% {
  tempdata <- data[data$calfeartag == cow,]
  tempdata$firsthightest <- ifelse(tempdata$age == tempdata$ageatfirstH, 1, 0)
  testn <- match(1, tempdata$firsthightest)
  if(nrow(tempdata[tempdata$age == tempdata$ageatfirstH,]) == 0){
    pof <- 0
  }
  if((testn -1) > 0 & 
     nrow(tempdata[tempdata$age == tempdata$ageatfirstH,]) == 1){
    pof <- ifelse(tempdata$POFloorApplied[testn - 1] == 1,1,0)
  }
  if((testn - 1) < 1 & 
     nrow(tempdata[tempdata$age == tempdata$ageatfirstH,]) == 1){
    pof <- 0
  }
  c(cow, pof)
}

stopCluster(cl)

prevpof <- data.frame(prevpof)

colnames(prevpof) <- c("calfeartag", "prevpof")

data <- merge(data, prevpof, by = "calfeartag", all.x = TRUE)

####GET PRECEDING POSTERIORODDS####

regcores(-1)

prevPO <- foreach(r = 2:nrow(data), .combine = "c") %dopar% {
  ifelse(data$calfeartag[r] == data$calfeartag[r-1], data$PosteriorOdds[r-1],NA)
}

data$prevPO <- NA

data$prevPO[2:nrow(data)] <- prevPO

####SPLIT DATA INTO POF AND NO-POF COWS####

data_nopof <- data[data$prevpof == "0" &
                     data$age == data$ageatfirstH &
                     data$prevPO < 0.003,]
data_pof <- data[data$prevpof == "1" &
                   data$age == data$ageatfirstH,]

####PLOTS####

ggplot(data_nopof,
       aes(x = likelihood,
           y = PosteriorOdds)) +
  geom_point() +
  labs(title = "Cows not hitting POF") +
  geom_smooth()


ggplot(data_pof,
       aes(x = likelihood,
           y = PosteriorOdds)) +
  geom_point() +
  labs(title = "Cows hitting POF") +
  ylim(c(0,1))

ggplot(data_nopof,
       aes(x = PosteriorOdds)) +
  geom_boxplot() +
  labs(title = "Cows not hitting POF")

####ZERO INTERCEPT LINEAR MODEL TO ASCERTAIN BEST POF####

mod <- lm(PosteriorOdds ~ 0 + likelihood,
          data = data_nopof)

summary(mod)

#POF == 0.00274

