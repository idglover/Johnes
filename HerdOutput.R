data_herdoutput <- read.csv("y:/ian/johnesthresholds/johnesproper/data/data_TESTFARMS_posteriors.csv")

data_herdoutput$PosteriorProbCat <- cut(data_herdoutput$PosteriorProb, breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))

data_herdoutput$PosteriorProbCat <- factor(data_herdoutput$PosteriorProbCat, levels = c("(0.9,1]",
                                                                                        "(0.8,0.9]",
                                                                                        "(0.7,0.8]",
                                                                                        "(0.6,0.7]",
                                                                                        "(0.5,0.6]",
                                                                                        "(0.4,0.5]",
                                                                                        "(0.3,0.4]",
                                                                                        "(0.2,0.3]",
                                                                                        "(0.1,0.2]",
                                                                                        "(0,0.1]"))






FARM = data$Farm[1]


con <- dbConnect(RSQLite::SQLite(), paste("y:/ian/johnesthresholds/johnesproper/data/TestingData/",FARM,"/",FARM,".tvu", sep = ""))

latestmrdate <- date(dbGetQuery(con, '
                           SELECT

max(off.date) as latestmrdate

FROM official_recording off')[[1]])

cowsalive <- dbGetQuery(con, '
                        
SELECT 

a.earTag,
ca.start,
ca.end

FROM animal a

INNER JOIN cow_alive ca on a.id = ca.animalId


WHERE ca.cOnFarm = 1 AND a.sex = "C"')

dbDisconnect(con)

cowsalive$start <- date(cowsalive$start)
cowsalive$end <- date(cowsalive$end)


cowsalive <- unique(cowsalive$earTag[which(difftime(latestmrdate, cowsalive$start) > 0 & difftime(cowsalive$end, latestmrdate) >-30)])



cowsaliveindata <- unique(data$calfeartag[which(data$calfeartag %in% cowsalive)])



data_herdoutput_finals <- data_herdoutput[which(data$calfeartag %in% cowsaliveindata & data$age == data_herdoutput$ageatlasttest),]



ggplot(data_herdoutput_finals, aes(x = Farm, fill = PosteriorProbCat)) +
  geom_bar(position = "fill", stat = "count", color = "black") +
  scale_fill_manual(values = c("(0.9,1]" = "#4F0100", 
                                "(0.8,0.9]" = "#800000",
                                "(0.7,0.8]" = "#D50000",
                                "(0.6,0.7]" = "#D53000",
                                "(0.5,0.6]" = "#FF3900",
                                "(0.4,0.5]" = "#FF8401",
                                "(0.3,0.4]" = "#FFB201",
                                "(0.2,0.3]" = "#FFE801",
                                "(0.1,0.2]" = "#74FF01",
                                "(0,0.1]" = "#09FF05")) +
  theme(axis.text.x = element_text(angle = -90)) +
  labs(title = "Cows Alive", subtitle = "Alive any time since thirty days prior to latest milk recording")


rm(data_herdoutput)
rm(data_herdoutput_finals)











