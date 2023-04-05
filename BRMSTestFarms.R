####PARAMETERS####

repsperunittitre <- 0.5

path <- "c:/users/ian.glover.headoffice/desktop/johnestestfarm/"

farm = "UK376111A"

####READ DATA####

data_brms_testing <- read.csv(paste0(path,"data_TESTFARMS_birthpriors.csv"))



print(paste0("N Rows: ",nrow(data_brms_testing), " N Cows: ",length(unique(data_brms_testing$calfeartag))))

####ENGINEER DATA####

#####TITRE#####

data_brms_testing$titre <- as.numeric(data_brms_testing$titre)

#####DIM CATEGORIES#####

data_brms_testing$dim_cat <- cut(data_brms_testing$dim, breaks = c(-1,5,10,10000))

data_brms_testing$dim_cat <- relevel(data_brms_testing$dim_cat, "(10,1e+04]")

data_brms_testing$dim_dummy_05 <- as.factor(ifelse(data_brms_testing$dim_cat == "(-1,5]","1","0"))
data_brms_testing$dim_dummy_510 <- as.factor(ifelse(data_brms_testing$dim_cat == "(5,10]","1","0"))

data_brms_testing$cellcount[data_brms_testing$cellcount == 0] <- 1

######HERD AVG DIM######

df <- data.frame(date  = unique(data_brms_testing$date))

regcores(-1)

df$herdavgdim <- foreach(r = 1:nrow(df), .combine = "c") %dopar% {
  median(data_brms_testing$dim[data_brms_testing$date == df$date[r]], na.rm = TRUE)
} 

stopCluster(cl)

data_brms_testing <- merge(data_brms_testing,
                   df,
                   by = c('date'),
                   all.x = TRUE)

colnum1 <- match("calfeartag", colnames(data_brms_testing))
colnum2 <- match("date", colnames(data_brms_testing))

data_brms_testing <- data_brms_testing[order(data_brms_testing[,colnum1], as.POSIXct(data_brms_testing[,colnum2])),]



data_brms_testing$herdavgdim_cat <- cut(data_brms_testing$herdavgdim,
                                breaks = c(-1,50,200,1000))

data_brms_testing$herdavgdim_dummy050 <- as.factor(ifelse(data_brms_testing$herdavgdim_cat == "(-1,50]" , "1", "0"))
data_brms_testing$herdavgdim_dummy50200 <- as.factor(ifelse(data_brms_testing$herdavgdim_cat == "(50,200]" , "1", "0"))
data_brms_testing$herdavgdim_dummy2001000 <- as.factor(ifelse(data_brms_testing$herdavgdim_cat == "(200,1e+03]" , "1", "0"))


####LIMIT TO COWS CURRENTLY IN HERD####

con <- dbConnect(RSQLite::SQLite(), paste0(path,farm,".tvu"), sep = "")

latestevent <- date(dbGetQuery(con, '
                          select max(ev.date)

FROM event ev')[[1]])

cowsalive <- dbGetQuery(con, '
                        
SELECT 

a.earTag,
ca.start,
ca.end

FROM animal a

INNER JOIN cow_alive ca on a.id = ca.animalId


WHERE ca.cOnFarm = 1 AND a.sex = "C"')

cowsalive$start <- date(cowsalive$start)
cowsalive$end <- date(cowsalive$end)

cowsalive <- unique(cowsalive$earTag[which((cowsalive$end - latestevent) < 200 & (cowsalive$end - latestevent) > -21)])

cowsaliveindata <- unique(data_brms_testing$calfeartag[which(data_brms_testing$calfeartag %in% cowsalive)])

data_brms_testing <- data_brms_testing[data_brms_testing$calfeartag %in% cowsaliveindata,]

print(paste0("N Rows Only Current Cows: ",nrow(data_brms_testing)))


####GET BAYES FACTORS (SIMULATED)####

data_brms_testing$likelihood <- simulate_bayes_factors(data_brms_testing,
                                                       negmod = 'brms_mod_lognorm_neg_yield_scc_mtnc_dimind',
                                                       posmod = 'brms_mod_lognorm_pos_age_yield_scc_mtnc_dimind',
                                                       npd = 1000000)




data_brms_testing$likelihood[is.na(data_brms_testing$likelihood)] <- 250 #Titres too high for positive distribution
data_brms_testing$likelihood[data_brms_testing$likelihood == Inf] <- 250 #Titres too high for negative cow distribution



#data_brms_testing$likelihood[is.na(data_brms_testing$likelihood)] <- Inf

####UPDATE POSTERIORS####

data_brms_testing <- data_brms_testing[!is.na(data_brms_testing$priorodds_crt),]

data_brms_testing$PosteriorOdds <- as.numeric(0)
data_brms_testing$POFloorApplied <- as.numeric(0)
data_brms_testing$PosteriorProb = as.numeric(0)

posterioroddsfloor <- 0
priorstage <- "crt"

data_brms_testing <- RparallelUpdatePO(data_brms_testing, priorstage, posterioroddsfloor)

data_brms_testing$PosteriorProb <- data_brms_testing$PosteriorOdds /
  (1 + data_brms_testing$PosteriorOdds)

data_brms_testing$titre <- as.numeric(data_brms_testing$titre)
data_brms_testing$PosteriorProb <- as.numeric(data_brms_testing$PosteriorProb)

data_brms_testing$PosteriorProb[data_brms_testing$PosteriorOdds == Inf] <- 1

print(ggplot(data_brms_testing,
             aes(x = PosteriorProb)) +
        geom_histogram())

#data_brms_testing$Target_altdef1 <- as.factor(data_brms_testing$Target_altdef1)

write.csv(data_brms_testing, paste0(path,"data_brms_testing.csv"), row.names = FALSE)

####CREATE TABLE####

crtjohnesprobdata <- data_brms_testing[which(data_brms_testing$age == data_brms_testing$ageatlasttest & data_brms_testing$calfeartag %in% cowsaliveindata),]
crtjohnesprobdata <- crtjohnesprobdata[which(!duplicated(crtjohnesprobdata$calfeartag)),]

crtjohnesprobtable <- data.frame(Eartag = crtjohnesprobdata$calfeartag,
                                 LatestProbability_Bayes = round(crtjohnesprobdata$PosteriorProb,2))

crtjohnesprobtable <- crtjohnesprobtable[order(-crtjohnesprobtable$LatestProbability_Bayes),]

linenos <- dbGetQuery(con, '
                      SELECT
                      
                      a.earTag,
                      a.mgtCode
                      
                      FROM animal a')

crtjohnesprobtable <- merge(crtjohnesprobtable, linenos, by.x = "Eartag", by.y = "earTag", all.x = TRUE)

crtjohnesprobtable <- crtjohnesprobtable[,c(3,1,2)]

write.csv(crtjohnesprobtable, paste0(path,"LatestProbs.csv"), row.names = FALSE)

dbDisconnect(con)

####COMBINE TEST RESULTS WITH LATEST PROBS####

ijr <- read.csv('c:/users/ian.glover.headoffice/desktop/johnestestfarm/individual_johnes_report.csv')

colnum <- match('EarTag', colnames(ijr))

colnames(ijr)[colnum] <- 'Eartag'

ijr <- merge(ijr, 
             crtjohnesprobtable[,c('Eartag',
                                   'LatestProbability_Bayes')],
             by = "Eartag",
             all.x = TRUE)

write.csv(ijr, paste0(path,'LatestProbs.csv'), row.names = FALSE)


####SAVE PLOTS####

plotsavepath = "c:/users/ian.glover.headoffice/desktop/johnestestfarm/testfarmplots/"

group.colours <- c("L" = "green", "M" = "orange", "H" = "red")

for (cow in cowsaliveindata){
  ggplot(data_brms_testing[data_brms_testing$calfeartag == cow,], aes(x = age)) +
    geom_point(aes(y = titre, color = class)) +
    geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
    geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
    geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    #geom_text(aes(x = age, y = PosteriorProb_brms * 100, label =round(PosteriorProb_brms, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3, color = "blue") + 
    geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = -4, label =round(likelihood, 1)), nudge_x = 0, check_overlap = T, size = 3, color = "black") + 
    #geom_text(aes(x = age, y = -6, label =round(likelihood_brms, 1)), nudge_x = 0, check_overlap = T, size = 3, color = "blue") + 
    geom_text(aes(x = age, y = -2, label = ifelse(POFloorApplied == 1, "*", "")), color = "black") +
    #geom_text(aes(x = age, y = -3, label = ifelse(POFloorApplied_brms == 1, "*", "")), color = "blue") +
    geom_text(aes(x = age, y = -8, label = cellcount), color = "black", size = 3) +
    geom_text(aes(x = age, y = -10, label = dim), color = "black", size = 3) +
    geom_text(aes(x = min(age) - (min(age)/10), y = -4, label = "BF"), color = "black" ,size = 3) +
    #geom_text(aes(x = min(age) - (min(age)/10), y = -6, label = "Bayes BF"), color = "blue" ,size = 3) +
    geom_text(aes(x = min(age) - (min(age)/10), y = -8, label = "SCC"), color = "black", size = 3) +
    geom_text(aes(x = min(age) - (min(age)/10), y = -10, label = "DIM"), color = "black", size = 3) +
    scale_color_manual(values = group.colours) +
    geom_line(aes(y = PosteriorProb*100)) +
    #geom_line(aes(y = PosteriorProb_brms * 100), color = "blue") +
    geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
    scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
    labs(title = data_brms_testing$calfeartag[data_brms_testing$calfeartag == cow], subtitle = paste0("Birth Probability:", round(data_brms_testing$priorprob_crt[data_brms_testing$calfeartag == cow],2)))
  ggsave(paste0(plotsavepath,cow,".png"))
}

