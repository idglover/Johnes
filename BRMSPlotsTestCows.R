
con <- dbConnect(RSQLite::SQLite(), "C:/Users/Ian.glover.HEADOFFICE/Desktop/Risdon/UK376111A.tvu")

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

#cowsalive$dsincelatestmr <- cowsalive$start - latestmrdate

cowsalive <- unique(cowsalive$earTag[which((cowsalive$end - latestevent) < 200 & (cowsalive$end - latestevent) > -21)])



cowsaliveindata <- unique(data_brms$calfeartag[which(data_brms$calfeartag %in% cowsalive)])

group.colours <- c("L" = "green", "M" = "orange", "H" = "red")

for (cow in cowsaliveindata){
  ggplot(data_brms[data_brms$calfeartag == cow,], aes(x = age)) +
    geom_point(aes(y = titre, color = class)) +
    geom_point(aes(y = yield), color = "black", shape = 4, size = 8) +
    geom_point(aes(y = meantitrenegcows), color = "black", shape = 5, size = 4) +
    geom_text(aes(x = age, y = PosteriorProb * 100, label =round(PosteriorProb, 2)), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = titre, label =round(titre, 1), color = class), nudge_x = 0, nudge_y = 5, check_overlap = T, size = 3) + 
    geom_text(aes(x = age, y = -5, label =round(likelihood, 1)), nudge_x = 0, nudge_y = -5, check_overlap = T, size = 3, color = "darkgrey") + 
    geom_text(aes(x = age, y = -2, label = ifelse(POFloorApplied == 1, "*", ""))) +
    scale_color_manual(values = group.colours) +
    geom_line(aes(y = PosteriorProb*100)) +
    geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 20, linetype = "dashed", color = "orange") +
    scale_y_continuous(name = "Titre", sec.axis = sec_axis(~./100, name="Posterior Probability")) +
    labs(title = data_brms$calfeartag[data_brms$calfeartag == cow], subtitle = paste0("Birth Probability:", round(data_brms$priorprob_crt[data_brms$calfeartag == cow],2)))
  ggsave(paste0("C:/Users/Ian.glover.HEADOFFICE/Desktop/Risdon/CowPlots/",cow,"_Bayes.png"))
}


crtjohnesprobdata_brms <- data_brms[which(data_brms$age == data_brms$ageatlasttest & data_brms$calfeartag %in% cowsaliveindata),]
crtjohnesprobdata_brms <- crtjohnesprobdata_brms[which(!duplicated(crtjohnesprobdata_brms$calfeartag)),]

crtjohnesprobtable_brms <- data.frame(Eartag = crtjohnesprobdata_brms$calfeartag, 
                                 LatestBayesProbability = round(crtjohnesprobdata_brms$PosteriorProb,2))

crtjohnesprobtable_brms <- crtjohnesprobtable_brms[order(-crtjohnesprobtable_brms$LatestBayesProbability),]

linenos <- dbGetQuery(con, '
                      SELECT
                      
                      a.earTag,
                      a.mgtCode
                      
                      FROM animal a')

crtjohnesprobtable_brms <- merge(crtjohnesprobtable_brms, linenos, by.x = "Eartag", by.y = "earTag", all.x = TRUE)

crtjohnesprobtable_brms <- crtjohnesprobtable_brms[,c(3,1,2)]

write.csv(crtjohnesprobtable_brms, paste0("C:/Users/Ian.glover.HEADOFFICE/Desktop/Risdon/LatestProbs_Bayes.csv"), row.names = FALSE)

dbDisconnect(con)


rm(crtjohnesprobdata_brms)
rm(crtjohnesprobtable_brms)
rm(cowsalive)
rm(cowsaliveindata)

