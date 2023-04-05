time0 <- Sys.time()

nfd <- 0

farms <- c("10_Small",
           "11_Frost",
           "13_Franks",
           "15_Clapp",
           "20_Edmunds",
           "21_Ellis",
           "25_AlstonSutton",
           "36_Risdon",
           "48_Kingsplay",
           "60_Tucker",
           "86_Cole",
           "94_Major",
           "101_Reade",
           "105_Raymont",
           "112_Cole",
           "131_Ham",
           "142_Creed",
           "146_Banfield",
           "150_Groves",
           "152_Maidment",
           "153_Pople",
           "163_Board",
           "164_Tincknell",
           "180_Patch",
           "182_Kimber",
           "186_Waters",
           "193_Stroud",
           "200_Mortimer",
           "208_Pocock",
           "211_Winn",
           "226_Ryall",
           "229_Persey",
           "230_Stockton",
           "231_Rogers",
           "232_WiltshireCollege",
           "233_Farrant",
           "234_Button",
           "240_Lywood",
           "274_Granger",
           "311_Gleave",
           "319_UoN",
           "323_Matthews",
           "329_Crooklands",
           "366_Wigley",
           "389_Whitehouse",
           "398_Briggs",
           "401_Hillman",
           "437_Richards",
           "440_Pickford",
           "443_Bland",
           "468_Read",
           "496_Clowes",
           "510_Willcocks",
           "536_Adorian",
           "598_Chatcull",
           "613_Ford",
           "620_Hesketh",
           "626_Preston",
           "628_Price",
           "637_Brown",
           "678_Woodward",
           "696_Eastfield",
           "738_Titley",
           "745_Cornthwaite",
           "763_Harding",
           "795_Manasseh",
           "859_Vaughan",
           "943_Jones",
           "1035_Curtis",
           "1113_Cann",
           "1123_Neston",
           "1140_Lovatt",
           "1272_DeanHall",
           "1476_Rhodes",
           "1560_Gilbert",
           "1570_Pomeroy",
           "1610_Robinson",
           "1709_Pilkington",
           "1751_Perrett",
           "1991_Owen")






for (i in farms){
  
  
  time1 <- Sys.time()  
  
  FARM = i
  
  print(FARM)
  
  con <- dbConnect(RSQLite::SQLite(), paste("y:/ian/johnesthresholds/johnesproper/data/pat/TVUsDelTitres/",FARM,"DelTitres.tvu", sep = ""))
  
  
  
  
  ######HOMEGROWN CALVES######
  
  
  
  calves <- as.data.table(dbGetQuery(con,'
                                             
  SELECT 
            
  mv.animalId AS calfid,
  a.earTag AS calfeartag
  
  FROM movement mv
            
  INNER JOIN animal a on mv.animalId = a.id
            
  WHERE mv.type = 1 AND a.sex = "C" AND a.damId <> 38 AND a.damId <> 0                                   
                                             
                                             
'))
  
  
  
  
  
  
  
  
  
  
  
  
  #####CALF DOB#####
  
  
  calves_dob <- as.data.table(dbGetQuery(con,'
                                       
  SELECT
  
  a.id as calfid,
  a.DOB as calfdob
  
  FROM animal a
                                       
'))
  
  
  calffinaljohnesstatus <- as.data.table(dbGetQuery(con, '
                                                  
SELECT a.id as calfid,
a.cJohnesFinal as finaljohnesstatus

FROM ANIMAL a
                                                  
                                                  '))
  
  
  
  ####DAM PARITY AND AGE#####
  
  dam_parity_age <- as.data.table(dbGetQuery(con,'
                                           
  SELECT 
  
  a.id as calfid,
  cs.cParity as damparity,
  cs.cAge as damage
  
  FROM animal a
  
  INNER JOIN cow_state_2 cs on a.damId = cs.animalId

  WHERE a.DOB = cs.start
  

                                           
'))
  
  ####RELATIVES STATUS AT BIRTH#####
  
  
  dam_status_birth <- as.data.table(dbGetQuery(con,'
                                             
  SELECT

  a.id as calfid,
  js.cTVStatus as damstatusbirth
  
  FROM animal a
  
  INNER JOIN johnes_state_2 js on a.damId = js.animalId
  
  WHERE a.DOB >= js.start AND a.DOB < js.end

                                             
'))
  
  damstatus12mold <- as.data.table(dbGetQuery(con,'
                                              
                                              
   SELECT di.calfid,
js.CTVStatus as damstatus12mold

 FROM

(  SELECT

  a.id as calfid,
  a.earTag as calfeartag,
  a.damId as damid,
  a.DOB,
  date(a.DOB, "+1 year") as dateat12mold

  
  FROM animal a
  

  
) di

INNER JOIN johnes_state_2 js on di.damid = js.animalId

WHERE di.dateat12mold >= js.start AND di.dateat12mold < js.end                                           
                                              
                                              '))
  
  
  nTestsDamAtBirth <- as.data.table(dbGetQuery(con,'
                                         
   SELECT
               
   a.id as calfid,
   COUNT (jr.animalId) as nDamTestsAtBirth
                       
   FROM animal a
                       
   INNER JOIN johnes_result jr on a.damId = jr.animalId
                       
   WHERE julianday(a.DOB) - julianday(jr.date) >= 0
                       
   GROUP BY a.damId
                    
'))
  
  
  granddam_status_birth <- as.data.table(dbGetQuery(con, '

  SELECT
  
  a.id as calfid,
  js.cTVStatus as granddamstatusbirth
  
  FROM animal a
  
  INNER JOIN animal a2 on a.damId = a2.id
  INNER JOIN johnes_state_2 js on js.animalId = a2.damId
  
  WHERE a.DOB >= js.start AND a.DOB < js.end
                                                  
'))
  
  
  siblings <- as.data.table(dbGetQuery(con, '

  SELECT
  
  a.id as calfid,
  a2.id as siblingid,
  js.cTVStatus
  
  FROM animal a
  
  INNER JOIN animal a2 on a2.damId = a.damId
  INNER JOIN johnes_state_2 js on a2.id = js.animalId
  INNER JOIN animal a3 on a.damId = a3.id
  
  WHERE a.id <> 38 AND a.id <> 0 AND
    a.damId <> 38 AND a.damId <> 0 AND a3.earTag <> "00" AND a3.earTag <> "000" AND SUBSTRING(a3.earTag,1,4) <> "0000" AND
    a3.earTag <> "BARTLETT" AND a3.earTag <> "HUDSON" AND a3.earTag <> "UK" AND a3.earTag <> "" AND a3.earTag <> "JERSEY DAM" AND
    a2.sex = "C" AND js.start <= a.DOB and js.end > a.DOB
  
  ORDER by a.id
  
  
'))
  
  nsiblingsbirth <- as.data.table(dbGetQuery(con, '
                                      
 SELECT 
 
 a.id as calfid,
 COUNT(a2.id) as nsiblingsbirth
 
 FROM animal a
 
 INNER JOIN animal a2 on a2.damId = a.damId
 INNER JOIN animal a3 on a.damId = a3.id

                                      
 WHERE a.id <> 38 AND a.id <> 0 AND 
    a.damId <> 38 AND a.damId <> 0 AND a3.earTag <> "00" AND a3.earTag <> "000" AND SUBSTRING(a3.earTag,1,4) <> "0000" AND
    a3.earTag <> "BARTLETT" AND a3.earTag <> "HUDSON" AND a3.earTag <> "UK" AND a3.earTag <> "" AND a3.earTag <> "JERSEY DAM" AND
    a2.sex = "C" AND a2.DOB < a.DOB
 
 GROUP BY a.id
                                      
'))
  
  
  
  
  nsiblingsstatus2birth <- as.data.table(dbGetQuery(con, '

  SELECT
  
  a.id as calfid,
  COUNT(a2.id) as nsiblingsstatus2birth
  
  FROM animal a
  
  INNER JOIN animal a2 on a2.damId = a.damId
  INNER JOIN johnes_state_2 js on a2.id = js.animalId
  INNER JOIN animal a3 on a.damId = a3.id
  
  WHERE a.id <> 38 AND a.id <> 0 AND a.damId <> 38 AND a.damId <> 0 AND 
  a3.earTag <> "00" AND a3.earTag <> "000" AND SUBSTRING(a3.earTag,1,4) <> "0000" AND
    a3.earTag <> "BARTLETT" AND a3.earTag <> "HUDSON" AND a3.earTag <> "UK" AND a3.earTag <> "" AND a3.earTag <> "JERSEY DAM" AND
    a2.sex = "C" AND js.start <= a.DOB AND (js.end > a.DOB OR js.end IS NULL) AND js.cTVStatus = 2 AND a2.DOB < a.DOB
  
  GROUP BY a.id
  
'))
  
  
  
  nsiblingsstatus3birth <- as.data.table(dbGetQuery(con, '

  SELECT
  
  a.id as calfid,
  COUNT(a2.id) as nsiblingsstatus3birth
  
  FROM animal a
  
  INNER JOIN animal a2 on a2.damId = a.damId
  INNER JOIN johnes_state_2 js on a2.id = js.animalId
  INNER JOIN animal a3 on a.damId = a3.id
  
  WHERE a.id <> 38 AND a.id <> 0 AND 
    a.damId <> 38 AND a.damId <> 0 AND a3.earTag <> "00" AND a3.earTag <> "000" AND SUBSTRING(a3.earTag,1,4) <> "0000" AND
    a3.earTag <> "BARTLETT" AND a3.earTag <> "HUDSON" AND a3.earTag <> "UK" AND a3.earTag <> "" AND a3.earTag <> "JERSEY DAM" AND
    a2.sex = "C" AND js.start <= a.DOB AND (js.end > a.DOB OR js.end IS NULL) AND js.cTVStatus = 3 AND a2.DOB < a.DOB
    
  GROUP by a.id
  
'))
  
  
  nauntsbirth <- as.data.table(dbGetQuery(con, '
                                      
 SELECT
calfid,
count(auntid) as nauntsbirth FROM(

SELECT 
 
 a4.id as calfid,
 a4.earTag as calfeartag,
 a3.id as damid,
 a3.earTag as dameartag,
 a2.id as granddamid,
 a2.earTag as granddameartag,
a.id as auntid,
a.earTag as aunteartag
 
 FROM animal a
 LEFT JOIN animal a2 on a.damId = a2.id
 INNER JOIN animal a3 on a3.damId = a.damId
 INNER JOIN animal a4 on a4.damId = a3.id
                                      
 WHERE a4.id <> 38 AND a4.id <> 39 AND a4.id <> 0 AND a4.damId <> 38 AND a4.damId <> 0 AND 
  a3.earTag <> "00" AND a3.EarTag <> "000" AND SUBSTR(a3.earTag,1,4) <> "0000" AND
  a3.earTag <> "BARTLETT" AND a3.earTag <> "HUDSON" AND a3.earTag <> "UK" AND a3.earTag <> "" AND a3.earTag <> "" AND a3.earTag <> "JERSEY DAM" AND
  a3.damId <> 38 AND a3.damId <> 0 AND 
  a2.earTag <> "00" AND a2.earTag <> "000" AND SUBSTR(a2.earTag,1,4) <> "0000" AND
  a2.earTag <> "BARTLETT" AND a2.earTag <> "HUDSON" AND a2.earTag <> "UK" AND a2.earTag <> "" AND a2.earTag <> "" AND a2.earTag <> "JERSEY DAM" AND
  a.sex = "C" AND a.id <> a3.id AND julianday(a.DOB) <= julianday(a4.DOB)
 
 ORDER BY a4.id
 )

GROUP BY calfid

'))
  
  
  
  
  nauntsstatus2birth <- as.data.table(dbGetQuery(con, '
                                 
SELECT 

calfid,
count(auntid) as nauntsstatus2birth

FROM(

SELECT 
 
 a4.id as calfid,
 a4.DOB as calfdob,
 a4.earTag as calfeartag,
 a3.id as damid,
 a3.earTag as dameartag,
 a2.id as granddamid,
 a2.earTag as granddameartag,
a.id as auntid,
a.earTag as aunteartag,
js.start,
js.end,
js.cTVStatus
 
 FROM johnes_state_2 js
 LEFT JOIN animal a on a.id = js.animalId
 LEFT JOIN animal a2 on a.damId = a2.id
 INNER JOIN animal a3 on a3.damId = a.damId
 INNER JOIN animal a4 on a4.damId = a3.id
 
                                      
 WHERE a4.id <> 38 AND a4.id <> 39 AND a4.id <> 0 AND a4.damId <> 38 AND a4.damId <> 0 AND 
  a3.earTag <> "00" AND a3.EarTag <> "000" AND SUBSTR(a3.earTag,1,4) <> "0000" AND
  a3.earTag <> "BARTLETT" AND a3.earTag <> "HUDSON" AND a3.earTag <> "UK" AND a3.earTag <> "" AND a3.earTag <> "" AND a3.earTag <> "JERSEY DAM" AND
  a3.damId <> 38 AND a3.damId <> 0 AND 
  a2.earTag <> "00" AND a2.earTag <> "000" AND SUBSTR(a2.earTag,1,4) <> "0000" AND
  a2.earTag <> "BARTLETT" AND a2.earTag <> "HUDSON" AND a2.earTag <> "UK" AND a2.earTag <> "" AND a2.earTag <> "" AND a2.earTag <> "JERSEY DAM" AND
  a.sex = "C" AND a.id <> a3.id AND julianday(a.DOB) <= julianday(a4.DOB) AND
  js.cTVStatus =2 AND julianday(js.start) <= julianday(a4.DOB) AND (julianday(js.end) > julianday(a4.DOB) OR js.end IS NULL)
 
 ORDER BY a4.id)
 
 GROUP BY calfid

'))
  
  
  
  
  nauntsstatus3birth <- as.data.table(dbGetQuery(con, '
                                 
                                 
SELECT 

calfid,
count(auntid) as nauntsstatus3birth

FROM(

SELECT 
 
 a4.id as calfid,
 a4.DOB as calfdob,
 a4.earTag as calfeartag,
 a3.id as damid,
 a3.earTag as dameartag,
 a2.id as granddamid,
 a2.earTag as granddameartag,
a.id as auntid,
a.earTag as aunteartag,
js.start,
js.end,
js.cTVStatus
 
 FROM johnes_state_2 js
 LEFT JOIN animal a on a.id = js.animalId
 LEFT JOIN animal a2 on a.damId = a2.id
 INNER JOIN animal a3 on a3.damId = a.damId
 INNER JOIN animal a4 on a4.damId = a3.id
 
                                      
 WHERE a4.id <> 38 AND a4.id <> 39 AND a4.id <> 0 AND a4.damId <> 38 AND a4.damId <> 0 AND 
  a3.earTag <> "00" AND a3.EarTag <> "000" AND SUBSTR(a3.earTag,1,4) <> "0000" AND
  a3.earTag <> "BARTLETT" AND a3.earTag <> "HUDSON" AND a3.earTag <> "UK" AND a3.earTag <> "" AND a3.earTag <> "" AND a3.earTag <> "JERSEY DAM" AND
  a3.damId <> 38 AND a3.damId <> 0 AND 
  a2.earTag <> "00" AND a2.earTag <> "000" AND SUBSTR(a2.earTag,1,4) <> "0000" AND
  a2.earTag <> "BARTLETT" AND a2.earTag <> "HUDSON" AND a2.earTag <> "UK" AND a2.earTag <> "" AND a2.earTag <> "" AND a2.earTag <> "JERSEY DAM" AND
  a.sex = "C" AND a.id <> a3.id AND julianday(a.DOB) <= julianday(a4.DOB) AND
  js.cTVStatus =3 AND julianday(js.start) <= julianday(a4.DOB) AND (julianday(js.end) > julianday(a4.DOB) OR js.end IS NULL)
 
 ORDER BY a4.id)
 
 GROUP BY calfid          
'))
  
  
  
  
  
  ngreatauntsbirth <- as.data.table(dbGetQuery(con, '
                                      
 SELECT
calfid,
count(greatauntid) as ngreatauntsbirth FROM(

SELECT 
 
 a5.id as calfid,
 a5.earTag as calfeartag,
 a4.id as damid,
 a4.earTag as dameartag,
 a3.id as granddamid,
 a3.earTag as granddameartag,
a2.id as grandgranddamid,
a2.earTag as grandgranddameartag,
a.id as greatauntid,
a.eartag as greataunteartag
 
 FROM animal a
 LEFT JOIN animal a2 on a.damId = a2.id
 INNER JOIN animal a3 on a3.damId = a.damId
 INNER JOIN animal a4 on a4.damId = a3.id
 INNER JOIN animal a5 on a5.damId = a4.id
                                      
WHERE a5.id <> 38 AND a5.id <> 39 AND a5.id <> 0 AND 
  a5.damId <> 38 AND a5.damId <> 0 AND a4.earTag <> "00" AND a4.EarTag <> "000" AND SUBSTR(a4.earTag,1,4) <> "0000" AND
  a4.earTag <> "BARTLETT" AND a4.earTag <> "HUDSON" AND a4.earTag <> "UK" AND a4.earTag <> "" AND a4.earTag <> "JERSEY DAM" AND
  a4.damId <> 38 AND a4.damId <> 0 AND 
  a3.earTag <> "00" AND a3.earTag <> "000" AND SUBSTR(a3.earTag,1,4) <> "0000" AND
  a3.earTag <> "BARTLETT" AND a3.earTag <> "HUDSON" AND a3.earTag <> "UK" AND a3.earTag <> "" AND a3.earTag <> "JERSEY DAM" AND
  a3.damId <> 38 AND a3.damId <> 0 AND 
  a2.earTag <> "00" AND a2.earTag <> "000" AND SUBSTR(a2.earTag,1,4) <> "0000" AND
  a2.earTag <> "BARTLETT" AND a2.earTag <> "HUDSON" AND a2.earTag <> "UK" AND a2.earTag <> "" AND a2.earTag <> "JERSEY DAM" AND
  a.sex = "C" AND a.id <> a3.id AND julianday(a.DOB) <= julianday(a5.DOB)
 
 ORDER BY a5.id
 )

GROUP BY calfid
                                      
'))
  
  
  ngreatauntsstatus2birth <- as.data.table(dbGetQuery(con, '
   
SELECT calfid,
count(greatauntid) as ngreatauntsstatus2birth 

FROM(


SELECT 
 
 a5.id as calfid,
 a5.earTag as calfeartag,
 a4.id as damid,
 a4.earTag as dameartag,
 a3.id as granddamid,
 a3.earTag as granddameartag,
a2.id as grandgranddamid,
a2.earTag as grandgranddameartag,
a.id as greatauntid,
a.eartag as greataunteartag,
js.start,
js.end,
js.cTVStatus
 
 
 FROM johnes_state_2 js
 
 LEFT JOIN animal a on js.animalId = a.id
 LEFT JOIN animal a2 on a.damId = a2.id
 INNER JOIN animal a3 on a3.damId = a.damId
 INNER JOIN animal a4 on a4.damId = a3.id
 INNER JOIN animal a5 on a5.damId = a4.id
                                      
WHERE a5.id <> 38 AND a5.id <> 39 AND a5.id <> 0 AND 
  a5.damId <> 38 AND a5.damId <> 0 AND a4.earTag <> "00" AND a4.EarTag <> "000" AND SUBSTR(a4.earTag,1,4) <> "0000" AND
  a4.earTag <> "BARTLETT" AND a4.earTag <> "HUDSON" AND a4.earTag <> "UK" AND a4.earTag <> "" AND a4.earTag <> "JERSEY DAM" AND
  a4.damId <> 38 AND a4.damId <> 0 AND 
  a3.earTag <> "00" AND a3.earTag <> "000" AND SUBSTR(a3.earTag,1,4) <> "0000" AND
  a3.earTag <> "BARTLETT" AND a3.earTag <> "HUDSON" AND a3.earTag <> "UK" AND a3.earTag <> "" AND a3.earTag <> "JERSEY DAM" AND
  a3.damId <> 38 AND a3.damId <> 0 AND 
  a2.earTag <> "00" AND a2.earTag <> "000" AND SUBSTR(a2.earTag,1,4) <> "0000" AND
  a2.earTag <> "BARTLETT" AND a2.earTag <> "HUDSON" AND a2.earTag <> "UK" AND a2.earTag <> "" AND a2.earTag <> "JERSEY DAM" AND
  a.sex = "C" AND a.id <> a3.id AND julianday(a.DOB) <= julianday(a5.DOB) AND 
  js.cTVSTatus = 2 AND julianday(js.start) < julianday(a5.DOB) AND (julianday(js.end) > julianday(a5.DOB) OR js.end IS NULL)
 
 ORDER BY a5.id)
 
 GROUP BY calfid
 
                                 
'))
  
  ngreatauntsstatus3birth <- as.data.table(dbGetQuery(con, '
                                 
SELECT calfid,
count(greatauntid) as ngreatauntsstatus3birth 

FROM(


SELECT 
 
 a5.id as calfid,
 a5.earTag as calfeartag,
 a4.id as damid,
 a4.earTag as dameartag,
 a3.id as granddamid,
 a3.earTag as granddameartag,
a2.id as grandgranddamid,
a2.earTag as grandgranddameartag,
a.id as greatauntid,
a.eartag as greataunteartag,
js.start,
js.end,
js.cTVStatus
 
 
 FROM johnes_state_2 js
 
 LEFT JOIN animal a on js.animalId = a.id
 LEFT JOIN animal a2 on a.damId = a2.id
 INNER JOIN animal a3 on a3.damId = a.damId
 INNER JOIN animal a4 on a4.damId = a3.id
 INNER JOIN animal a5 on a5.damId = a4.id
                                      
WHERE a5.id <> 38 AND a5.id <> 39 AND a5.id <> 0 AND 
  a5.damId <> 38 AND a5.damId <> 0 AND a4.earTag <> "00" AND a4.EarTag <> "000" AND SUBSTR(a4.earTag,1,4) <> "0000" AND
  a4.earTag <> "BARTLETT" AND a4.earTag <> "HUDSON" AND a4.earTag <> "UK" AND a4.earTag <> "" AND a4.earTag <> "JERSEY DAM" AND
  a4.damId <> 38 AND a4.damId <> 0 AND 
  a3.earTag <> "00" AND a3.earTag <> "000" AND SUBSTR(a3.earTag,1,4) <> "0000" AND
  a3.earTag <> "BARTLETT" AND a3.earTag <> "HUDSON" AND a3.earTag <> "UK" AND a3.earTag <> "" AND a3.earTag <> "JERSEY DAM" AND
  a3.damId <> 38 AND a3.damId <> 0 AND 
  a2.earTag <> "00" AND a2.earTag <> "000" AND SUBSTR(a2.earTag,1,4) <> "0000" AND
  a2.earTag <> "BARTLETT" AND a2.earTag <> "HUDSON" AND a2.earTag <> "UK" AND a2.earTag <> "" AND a2.earTag <> "JERSEY DAM" AND
  a.sex = "C" AND a.id <> a3.id AND julianday(a.DOB) <= julianday(a5.DOB) AND 
  js.cTVSTatus = 3 AND julianday(js.start) < julianday(a5.DOB) AND (julianday(js.end) > julianday(a5.DOB) OR js.end IS NULL)
 
 ORDER BY a5.id)
 
 GROUP BY calfid
 
                                 
'))
  
  
  
  relstatusatbirth <- merge(calves, nsiblingsbirth, by = "calfid", all.x = TRUE)
  relstatusatbirth <- relstatusatbirth[,c(1,3)]
  relstatusatbirth <- merge(relstatusatbirth, nsiblingsstatus2birth, by = "calfid", all.x = TRUE)
  relstatusatbirth <- merge(relstatusatbirth, nsiblingsstatus3birth, by = "calfid", all.x = TRUE)
  relstatusatbirth$propsiblingsstatus2birth <- relstatusatbirth$nsiblingsstatus2birth / relstatusatbirth$nsiblingsbirth
  relstatusatbirth$propsiblingsstatus3birth <- relstatusatbirth$nsiblingsstatus3birth / relstatusatbirth$nsiblingsbirth
  relstatusatbirth <- merge(relstatusatbirth, dam_status_birth, by = "calfid", all.x = TRUE )
  relstatusatbirth <- merge(relstatusatbirth, granddam_status_birth, by = "calfid", all.x =  TRUE)
  relstatusatbirth <- merge(relstatusatbirth, nauntsbirth, by = "calfid", all.x = TRUE)
  relstatusatbirth <- merge(relstatusatbirth, nauntsstatus2birth, by = "calfid", all.x = TRUE)
  relstatusatbirth <- merge(relstatusatbirth, nauntsstatus3birth, by = "calfid", all.x = TRUE)
  relstatusatbirth$propauntsstatus2birth <- relstatusatbirth$nauntsstatus2birth / relstatusatbirth$nauntsbirth
  relstatusatbirth$propauntsstatus3birth <- relstatusatbirth$nauntsstatus3birth / relstatusatbirth$nauntsbirth
  relstatusatbirth <- merge(relstatusatbirth, ngreatauntsbirth, by = "calfid", all.x = TRUE)
  relstatusatbirth <- merge(relstatusatbirth, ngreatauntsstatus2birth, by = "calfid", all.x = TRUE)
  relstatusatbirth <- merge(relstatusatbirth, ngreatauntsstatus3birth, by = "calfid", all.x = TRUE)
  relstatusatbirth$propgreatauntsstatus2birth <- relstatusatbirth$ngreatauntsstatus2birth / relstatusatbirth$ngreatauntsbirth
  relstatusatbirth$propgreatauntsstatus3birth <- relstatusatbirth$ngreatauntsstatus3birth / relstatusatbirth$ngreatauntsbirth
  
  #####CURRENT STATUS OF RELATIVES####
  
  
  
  allanimalscrtstatus <- as.data.table(dbGetQuery(con, '
                                 
SELECT

a.id as animalid,
max(js.start) as latestdate,
js.cTVStatus as crtjstatus

FROM animal a

INNER JOIN johnes_state_2 js on a.id = js.animalId

GROUP BY a.id
                                 
'))
  
  nsiblingscrt <- as.data.table(dbGetQuery(con, '
                                      
 SELECT 
 
 a.id as calfid,
 COUNT(a2.id) as nsiblingscrt
 
 FROM animal a
 
 INNER JOIN animal a2 on a2.damId = a.damId
 INNER JOIN animal a3 on a.damId = a3.id
                                      
 WHERE a.id <> 38 AND a.id <> 39 AND
 a.id <> a2.id AND
  a.damId <> 38 AND a.damId <> 0 AND a3.earTag <> "00" AND a3.earTag <> "000" AND SUBSTRING(a3.earTag,1,4) <> "0000" AND 
  a3.earTag <> "BARTLETT" AND a3.earTag <> "HUDSON" AND a3.earTag <> "UK" AND a3.earTag <> "" AND a3.earTag <> "JERSEY DAM" AND
  a2.sex = "C"
 
 GROUP BY a.id
                                      
'))
  
  damcrtstatus <- as.data.table(dbGetQuery(con,'
                                             
  SELECT

  a.id as calfid,
  max(js.start) as damlatesttest,
  js.cTVStatus as damcrtstatus
  
  FROM animal a
  
  INNER JOIN animal a2 on a.damId = a2.id
  
  INNER JOIN johnes_state_2 js on a.damId = js.animalId
  
  GROUP BY a.id                            
'))
  
  damcrtstatus <- damcrtstatus[,c(1,3)]
  
  granddamcrtstatus <- as.data.table(dbGetQuery(con,'

  SELECT

  a.id as calfid,
  max(js.start) as granddamlatesttest,
  js.cTVStatus as granddamcrtstatus
  
  FROM animal a
  
  INNER JOIN animal a2 on a.damId = a2.id
  
  INNER JOIN johnes_state_2 js on a2.damId = js.animalId
  
  GROUP BY a.id
                                              
'))
  
  granddamcrtstatus <- granddamcrtstatus[,c(1,3)]
  
  nsiblingsstatus3crt <- as.data.table(dbGetQuery(con, '
                                                
SELECT calfid,
count(siblingid) as nsiblingsstatus3crt


FROM(

SELECT

	a2.id as calfid,
	a2.earTag as calfeartag,
	a.id as siblingid,
	a.earTag as siblingeartag,
	a.cJohnesFinal

	FROM animal a

	
	LEFT JOIN animal a2 on a2.damId = a.damId
	INNER JOIN animal a3 on a2.damId = a3.id
 

	WHERE a.id <> a2.id AND
	  a.damId <> 38 AND a.damId <> 0 AND a3.earTag <> "00" AND a3.earTag <> "000" AND SUBSTR(a3.earTag,1,4) <> "0000" AND 
	  a3.earTag <> "BARTLETT" AND a3.earTag <> "HUDSON" AND a3.earTag <> "UK" AND a3.earTag <> "" AND a3.earTag <> "JERSEY DAM" AND
	  a.sex = "C" AND a.cJohnesFinal = 3
	  
	ORDER BY a2.id)
	
	GROUP BY calfid                               
'))
  
  
  nsiblingsstatus2crt <- as.data.table(dbGetQuery(con, '
                                                
SELECT calfid,
count(siblingid) as nsiblingsstatus2crt


FROM(

SELECT

	a2.id as calfid,
	a2.earTag as calfeartag,
	a.id as siblingid,
	a.earTag as siblingeartag,
	a.cJohnesFinal

	FROM animal a

	
	LEFT JOIN animal a2 on a2.damId = a.damId
	INNER JOIN animal a3 on a2.damId = a3.id
 

	WHERE a.id <> a2.id AND
	  a.damId <> 38 AND a.damId <> 0 AND a3.earTag <> "00" AND a3.earTag <> "000" AND SUBSTR(a3.earTag,1,4) <> "0000" AND 
	  a3.earTag <> "BARTLETT" AND a3.earTag <> "HUDSON" AND a3.earTag <> "UK" AND a3.earTag <> "" AND a3.earTag <> "JERSEY DAM" AND
	  a.sex = "C" AND a.cJohnesFinal = 2
	  
	ORDER BY a2.id)
	
	GROUP BY calfid 
                                                
                                                
'))
  
  
  nauntscrt <- as.data.table(dbGetQuery(con, '
                                      
SELECT
calfid,
count(auntid) as nauntscrt FROM(

SELECT 
 
 a4.id as calfid,
 a4.earTag as calfeartag,
 a3.id as damid,
 a3.earTag as dameartag,
 a2.id as granddamid,
 a2.earTag as granddameartag,
a.id as auntid,
a.earTag as aunteartag
 
 FROM animal a
 LEFT JOIN animal a2 on a.damId = a2.id
 INNER JOIN animal a3 on a3.damId = a.damId
 INNER JOIN animal a4 on a4.damId = a3.id
                                      
 WHERE a4.id <> 38 AND a4.id <> 39 AND a4.id <> 0 AND 
  a4.damId <> 38 AND a4.damId <> 0 AND a3.earTag <> "00" AND a3.earTag <> "000" AND SUBSTR(a3.earTag,1,4) <> "0000" AND
  a3.earTag <> "BARTLETT" AND a3.earTag <> "HUDSON" AND a3.earTag <> "UK" AND a3.earTag <> "" AND a3.earTag <> "JERSEY DAM" AND
  a3.damId <> 38 AND a3.damId <> 0 AND a2.earTag <> "00" AND a2.earTag <> "000" AND SUBSTR(a2.earTag,1,4) <> "0000" AND
  a2.earTag <> "BARTLETT" AND a2.earTag <> "HUDSON" AND a2.earTag <> "UK" AND a2.earTag <> "" AND a2.earTag <> "JERSEY DAM" AND
  a.sex = "C" AND a.id <> a3.id
 
 ORDER BY a4.id
 )

GROUP BY calfid           
'))
  
  nauntsstatus3crt <- as.data.table(dbGetQuery(con,'

SELECT calfid,
count(auntid) as nauntsstatus3crt FROM(

SELECT 
 
 a4.id as calfid,
 a4.earTag as calfeartag,
 a3.id as damid,
 a3.earTag as dameartag,
 a2.id as granddamid,
 a2.earTag as granddameartag,
a.id as auntid,
a.earTag as aunteartag,
a.cJohnesFinal
 
 FROM animal a
 LEFT JOIN animal a2 on a.damId = a2.id
 INNER JOIN animal a3 on a3.damId = a.damId
 INNER JOIN animal a4 on a4.damId = a3.id
                                      
 WHERE a4.id <> 38 AND a4.id <> 39 AND a4.id <> 0 AND 
  a4.damId <> 38 AND a4.damId <> 0 AND a3.earTag <> "00" AND a3.earTag <> "000" AND SUBSTR(a3.earTag,1,4) <> "0000" AND
  a3.earTag <> "BARTLETT" AND a3.earTag <> "HUDSON" AND a3.earTag <> "UK" AND a3.earTag <> "" AND a3.earTag <> "JERSEY DAM" AND
  a3.damId <> 38 AND a3.damId <> 0 AND a2.earTag <> "00" AND a2.earTag <> "000" AND SUBSTR(a2.earTag,1,4) <> "0000" AND
  a2.earTag <> "BARTLETT" AND a2.earTag <> "HUDSON" AND a2.earTag <> "UK" AND a2.earTag <> "" AND a2.earTag <> "JERSEY DAM" AND
  a.sex = "C" AND a.id <> a3.id AND a.cJohnesFinal = 3
 
 ORDER BY a4.id)
 
GROUP BY calfid
                                             
                                             
                                             
'))
  
  nauntsstatus2crt <- as.data.table(dbGetQuery(con,'

SELECT calfid,
count(auntid) as nauntsstatus2crt FROM(

SELECT 
 
 a4.id as calfid,
 a4.earTag as calfeartag,
 a3.id as damid,
 a3.earTag as dameartag,
 a2.id as granddamid,
 a2.earTag as granddameartag,
a.id as auntid,
a.earTag as aunteartag,
a.cJohnesFinal
 
 FROM animal a
 LEFT JOIN animal a2 on a.damId = a2.id
 INNER JOIN animal a3 on a3.damId = a.damId
 INNER JOIN animal a4 on a4.damId = a3.id
                                      
 WHERE a4.id <> 38 AND a4.id <> 39 AND a4.id <> 0 AND 
  a4.damId <> 38 AND a4.damId <> 0 AND a3.earTag <> "00" AND a3.earTag <> "000" AND SUBSTR(a3.earTag,1,4) <> "0000" AND
  a3.earTag <> "BARTLETT" AND a3.earTag <> "HUDSON" AND a3.earTag <> "UK" AND a3.earTag <> "" AND a3.earTag <> "JERSEY DAM" AND
  a3.damId <> 38 AND a3.damId <> 0 AND a2.earTag <> "00" AND a2.earTag <> "000" AND SUBSTR(a2.earTag,1,4) <> "0000" AND
  a2.earTag <> "BARTLETT" AND a2.earTag <> "HUDSON" AND a2.earTag <> "UK" AND a2.earTag <> "" AND a2.earTag <> "JERSEY DAM" AND
  a.sex = "C" AND a.id <> a3.id AND a.cJohnesFinal = 2
 
 ORDER BY a4.id)
 
GROUP BY calfid
                                             
'))
  
  ngreatauntscrt <- as.data.table(dbGetQuery(con, '
                                      
 SELECT
calfid,
count(greatauntid) as ngreatauntscrt FROM(

SELECT 
 
 a5.id as calfid,
 a5.earTag as calfeartag,
 a4.id as damid,
 a4.earTag as dameartag,
 a3.id as granddamid,
 a3.earTag as granddameartag,
a2.id as grandgranddamid,
a2.earTag as grandgranddameartag,
a.id as greatauntid,
a.eartag as greataunteartag
 
 FROM animal a
 LEFT JOIN animal a2 on a.damId = a2.id
 INNER JOIN animal a3 on a3.damId = a.damId
 INNER JOIN animal a4 on a4.damId = a3.id
 INNER JOIN animal a5 on a5.damId = a4.id
                                      
WHERE a5.id <> 38 AND a5.id <> 39 AND a5.id <> 0 AND 
  a5.damId <> 38 AND a5.damId <> 0 AND a4.earTag <> "00" AND a4.EarTag <> "000" AND SUBSTR(a4.earTag,1,4) <> "0000" AND
  a4.earTag <> "BARTLETT" AND a4.earTag <> "HUDSON" AND a4.earTag <> "UK" AND a4.earTag <> "" AND a4.earTag <> "JERSEY DAM" AND
  a4.damId <> 38 AND a4.damId <> 0 AND 
  a3.earTag <> "00" AND a3.earTag <> "000" AND SUBSTR(a3.earTag,1,4) <> "0000" AND
  a3.earTag <> "BARTLETT" AND a3.earTag <> "HUDSON" AND a3.earTag <> "UK" AND a3.earTag <> "" AND a3.earTag <> "JERSEY DAM" AND
  a3.damId <> 38 AND a3.damId <> 0 AND 
  a2.earTag <> "00" AND a2.earTag <> "000" AND SUBSTR(a2.earTag,1,4) <> "0000" AND
  a2.earTag <> "BARTLETT" AND a2.earTag <> "HUDSON" AND a2.earTag <> "UK" AND a2.earTag <> "" AND a2.earTag <> "JERSEY DAM" AND
  a.sex = "C" AND a.id <> a3.id
 
 ORDER BY a5.id
 )

GROUP BY calfid
                                      
'))
  
  
  ngreatauntsstatus3crt <- as.data.table(dbGetQuery(con,'

SELECT

calfid,
count(greatauntid) as ngreatauntsstatus3crt FROM(

SELECT 
 
 a5.id as calfid,
 a5.earTag as calfeartag,
 a4.id as damid,
 a4.earTag as dameartag,
 a3.id as granddamid,
 a3.earTag as granddameartag,
a2.id as grandgranddamid,
a2.earTag as grandgranddameartag,
a.id as greatauntid,
a.eartag as greataunteartag,
a.cJohnesFinal
 
 FROM animal a
 LEFT JOIN animal a2 on a.damId = a2.id
 INNER JOIN animal a3 on a3.damId = a.damId
 INNER JOIN animal a4 on a4.damId = a3.id
 INNER JOIN animal a5 on a5.damId = a4.id
                                      
WHERE a5.id <> 38 AND a5.id <> 39 AND a5.id <> 0 AND 
  a5.damId <> 38 AND a5.damId <> 0 AND a4.earTag <> "00" AND a4.EarTag <> "000" AND SUBSTR(a4.earTag,1,4) <> "0000" AND
  a4.earTag <> "BARTLETT" AND a4.earTag <> "HUDSON" AND a4.earTag <> "UK" AND a4.earTag <> "" AND a4.earTag <> "JERSEY DAM" AND
  a4.damId <> 38 AND a4.damId <> 0 AND 
  a3.earTag <> "00" AND a3.earTag <> "000" AND SUBSTR(a3.earTag,1,4) <> "0000" AND
  a3.earTag <> "BARTLETT" AND a3.earTag <> "HUDSON" AND a3.earTag <> "UK" AND a3.earTag <> "" AND a3.earTag <> "JERSEY DAM" AND
  a3.damId <> 38 AND a3.damId <> 0 AND 
  a2.earTag <> "00" AND a2.earTag <> "000" AND SUBSTR(a2.earTag,1,4) <> "0000" AND
  a2.earTag <> "BARTLETT" AND a2.earTag <> "HUDSON" AND a2.earTag <> "UK" AND a2.earTag <> "" AND a2.earTag <> "JERSEY DAM" AND
  a.sex = "C" AND a.id <> a3.id AND a.cJohnesFinal = 3
 
 ORDER BY a5.id)
 
 GROUP BY calfid
                                             
                                             
                                             
'))
  
  
  
  ngreatauntsstatus2crt <- as.data.table(dbGetQuery(con,'

SELECT

calfid,
count(greatauntid) as ngreatauntsstatus2crt FROM(

SELECT 
 
 a5.id as calfid,
 a5.earTag as calfeartag,
 a4.id as damid,
 a4.earTag as dameartag,
 a3.id as granddamid,
 a3.earTag as granddameartag,
a2.id as grandgranddamid,
a2.earTag as grandgranddameartag,
a.id as greatauntid,
a.eartag as greataunteartag,
a.cJohnesFinal
 
 FROM animal a
 LEFT JOIN animal a2 on a.damId = a2.id
 INNER JOIN animal a3 on a3.damId = a.damId
 INNER JOIN animal a4 on a4.damId = a3.id
 INNER JOIN animal a5 on a5.damId = a4.id
                                      
WHERE a5.id <> 38 AND a5.id <> 39 AND a5.id <> 0 AND 
  a5.damId <> 38 AND a5.damId <> 0 AND a4.earTag <> "00" AND a4.EarTag <> "000" AND SUBSTR(a4.earTag,1,4) <> "0000" AND
  a4.earTag <> "BARTLETT" AND a4.earTag <> "HUDSON" AND a4.earTag <> "UK" AND a4.earTag <> "" AND a4.earTag <> "JERSEY DAM" AND
  a4.damId <> 38 AND a4.damId <> 0 AND 
  a3.earTag <> "00" AND a3.earTag <> "000" AND SUBSTR(a3.earTag,1,4) <> "0000" AND
  a3.earTag <> "BARTLETT" AND a3.earTag <> "HUDSON" AND a3.earTag <> "UK" AND a3.earTag <> "" AND a3.earTag <> "JERSEY DAM" AND
  a3.damId <> 38 AND a3.damId <> 0 AND 
  a2.earTag <> "00" AND a2.earTag <> "000" AND SUBSTR(a2.earTag,1,4) <> "0000" AND
  a2.earTag <> "BARTLETT" AND a2.earTag <> "HUDSON" AND a2.earTag <> "UK" AND a2.earTag <> "" AND a2.earTag <> "JERSEY DAM" AND
  a.sex = "C" AND a.id <> a3.id AND a.cJohnesFinal = 2
 
 ORDER BY a5.id)
 
 GROUP BY calfid
                                             
                                             
                                             
'))
  
  relstatuscrt <- merge(calves, nsiblingscrt, by = "calfid", all.x = TRUE)
  relstatuscrt <- relstatuscrt[,c(1,3)]
  relstatuscrt <- merge(relstatuscrt, nsiblingsstatus2crt, by = "calfid", all.x = TRUE)
  relstatuscrt <- merge(relstatuscrt, nsiblingsstatus3crt, by = "calfid", all.x = TRUE)
  relstatuscrt$propsiblingsstatus2crt <- relstatuscrt$nsiblingsstatus2crt / relstatuscrt$nsiblingscrt
  relstatuscrt$propsiblingsstatus3crt <- relstatuscrt$nsiblingsstatus3crt / relstatuscrt$nsiblingscrt
  relstatuscrt <- merge(relstatuscrt, damcrtstatus, by = "calfid", all.x = TRUE )
  relstatuscrt <- merge(relstatuscrt, granddamcrtstatus, by = "calfid", all.x =  TRUE)
  relstatuscrt <- merge(relstatuscrt, nauntscrt, by = "calfid", all.x = TRUE)
  relstatuscrt <- merge(relstatuscrt, nauntsstatus2crt, by = "calfid", all.x = TRUE)
  relstatuscrt <- merge(relstatuscrt, nauntsstatus3crt, by = "calfid", all.x = TRUE)
  relstatuscrt$propauntsstatus2crt <- relstatuscrt$nauntsstatus2crt / relstatuscrt$nauntscrt
  relstatuscrt$propauntsstatus3crt <- relstatuscrt$nauntsstatus3crt / relstatuscrt$nauntscrt
  relstatuscrt <- merge(relstatuscrt, ngreatauntscrt, by = "calfid", all.x = TRUE)
  relstatuscrt <- merge(relstatuscrt, ngreatauntsstatus2crt, by = "calfid", all.x = TRUE)
  relstatuscrt <- merge(relstatuscrt, ngreatauntsstatus3crt, by = "calfid", all.x = TRUE)
  relstatuscrt$propgreatauntsstatus2crt <- relstatuscrt$ngreatauntsstatus2crt / relstatuscrt$ngreatauntscrt
  relstatuscrt$propgreatauntsstatus3crt <- relstatuscrt$ngreatauntsstatus3crt / relstatuscrt$ngreatauntscrt
  
  
  
  #####HERD JOHNES DATA#####
  
  
  
  
  ntested <- as.data.table(dbGetQuery(con,'
            
            SELECT jr.date, COUNT(jr.id) as ntested
            FROM johnes_result jr
            
            WHERE jr.date >= "2010-11-10"
            
            GROUP by jr.date
            
            '))
  
  nrecorded <- as.data.table(dbGetQuery(con,'
            
            SELECT 
            
            y.date,
            count(y.id) as nrecorded
            
            FROM yield y
            
            GROUP BY y.date
            
            '))
  
  nposanimalsbydate <- as.data.table(dbGetQuery(con,'
              
              SELECT jr.date, COUNT(jr.animalid) as npos
              
              FROM johnes_result jr
              
              WHERE jr.value >= 30
              
              GROUP BY jr.date
              
              '))
  
  meantitrebydate <- as.data.table(dbGetQuery(con,'
                     
                     SELECT
                     
                     jr.date,
                     AVG(jr.value) as meanttitre
                     
                     FROM johnes_result jr
                     
                     GROUP BY jr.date
                     
                     '))
  
  meantitrenegcowsbydate <- as.data.table(dbGetQuery(con,'
                        
                        SELECT
                        
                        jr.date,
                        AVG(jr.value) as meantitrenegcows
                        
                        FROM johnes_result jr
                        
                        WHERE jr.value < 30
                        
                        GROUP BY jr.date
                                                   
                        '))
  
  setkey(ntested, date)
  setkey(nrecorded, date)
  
  herdjohnesdata <- merge(ntested,nrecorded, all.x = TRUE)
  
  herdjohnesdata$proptested <- herdjohnesdata$ntested / herdjohnesdata$nrecorded
  
  setkey(herdjohnesdata, date)
  setkey(nposanimalsbydate, date)
  
  herdjohnesdata <- merge(herdjohnesdata, nposanimalsbydate, all.x = TRUE)
  
  setkey(herdjohnesdata, date)
  setkey(meantitrebydate, date)
  
  herdjohnesdata$proppos <- herdjohnesdata$npos / herdjohnesdata$ntested
  
  herdjohnesdata <- merge(herdjohnesdata, meantitrebydate, all.x = TRUE)
  
  setkey(herdjohnesdata, date)
  setkey(meantitrenegcowsbydate, date)
  
  herdjohnesdata <- merge(herdjohnesdata, meantitrenegcowsbydate, all.x = TRUE)
  
  
  
  earlierherdtestdates <- as.data.table(dbGetQuery(con,'
                                                 
  SELECT 
                        
  a.id as calfid,
  max(jr.date) as latesttestdate,
  julianday(a.DOB)-julianday(max(jr.date)) as testdobinterval

  FROM animal a, johnes_result jr
  
  WHERE julianday(a.DOB) - julianday(jr.date) <= 365 AND julianday(a.DOB) - julianday(jr.date) >=0 AND jr.date >= "2010-11-10"
                        
  GROUP BY a.id
                        
'))
  
  
  laterherdtestdates <- as.data.table(dbGetQuery(con,'
                                               
  SELECT 
                  
  a.id as calfid,
  min(jr.date) as nexttestdate,
  julianday(min(jr.date))-julianday(a.DOB) as dobtestinterval
                        
  FROM animal a, johnes_result jr
                        
  WHERE julianday(jr.date) - julianday(a.DOB) >0 AND julianday(jr.date) - julianday(a.DOB) <= 365 AND jr.date >= "2010-11-10"
                      
  GROUP BY a.id                       
                                               
'))
  
  
  
  earlierherdjohnesdata <- merge(earlierherdtestdates, herdjohnesdata, by.x = "latesttestdate", by.y = "date", all.x = TRUE)
  
  laterherdjohnesdata <- merge(laterherdtestdates, herdjohnesdata, by.x = "nexttestdate", by.y = "date", all.x = TRUE)
  
  combinedherdjohnesdata <- merge(earlierherdjohnesdata, laterherdjohnesdata, by = "calfid")
  
  colnames(combinedherdjohnesdata) <- c("calfid",
                                        "latesttestdate",
                                        "testdobinterval",
                                        "ntestedlatest",
                                        "nrecordedlatest",
                                        "proptestedlatest",
                                        "nposlatest",
                                        "propposlatest",
                                        "meantitrelatest",
                                        "meantitrenegcowslatest",
                                        "nexttestdate",
                                        "dobtestinterval",
                                        "ntestednext",
                                        "nrecordednext",
                                        "proptestednext",
                                        "nposnext",
                                        "propposnext",
                                        "meantitrenext",
                                        "meantitrenegcowsnext")
  
  ######N PROXIMAL/VAGUELY PROXIMAL CALVES/DAMS#####
  
  
  
  
  nproximal_calves <- as.data.table(dbGetQuery(con,'
                                            
  SELECT
                      
  a.id AS calfid,
  COUNT(mv.animalId) AS nproximalcalves
                    
  FROM animal a, movement mv
                      
  WHERE julianday(a.DOB) - julianday(mv.date) <= 10 
  AND julianday(a.DOB) - julianday(mv.date) >=-10 
  AND mv.type = 1
  AND a.id <> mv.animalId
                      
  GROUP BY a.id
                                            
'))
  
  
  nvaguelyproximal_calves <- as.data.table(dbGetQuery(con,'
                                            
  SELECT
                      
  a.id AS calfid,
  COUNT(mv.animalId) AS nvaguelyproximalcalves
                    
  FROM animal a, movement mv
                      
  WHERE julianday(a.DOB) - julianday(mv.date) <= 70 
  AND julianday(a.DOB) - julianday(mv.date) >=-70 
  AND mv.type = 1
  AND a.id <> mv.animalId
                      
  GROUP BY a.id
                                            
'))
  
  
  
  
  
  
  
  
  
  
  
  
  nproximaldams <- as.data.table(dbGetQuery(con, '
                                          
  SELECT
  
  a.id AS calfid,
  COUNT(DISTINCT a3.id) as nproximaldams
                    
  FROM animal a, movement mv
  
  INNER JOIN animal a2 on mv.animalId = a2.id
  INNER JOIN animal a3 on a2.damId = a3.id
                      
  WHERE julianday(a.DOB) - julianday(mv.date) <= 10 
  AND julianday(a.DOB) - julianday(mv.date) >=-10 
  AND mv.type = 1
  AND a.id <> mv.animalId
  AND a3.id <> 38
  
  GROUP BY a.id
                                          
                                          
                                          
                                          '))
  
  nvaguelyproximaldams <- as.data.table(dbGetQuery(con, '
                                          
  SELECT
  
  a.id AS calfid,
  COUNT(DISTINCT a3.id) as nvaguelyproximaldams
                    
  FROM animal a, movement mv
  
  INNER JOIN animal a2 on mv.animalId = a2.id
  INNER JOIN animal a3 on a2.damId = a3.id
                      
  WHERE julianday(a.DOB) - julianday(mv.date) <= 70 
  AND julianday(a.DOB) - julianday(mv.date) >= -70 
  AND mv.type = 1
  AND a.id <> mv.animalId
  AND a3.id <> 38
  
  GROUP BY a.id
                                          
                                          
                                          
                                          '))
  
  
  nproximaldamsstatus2birth <- as.data.table(dbGetQuery(con,'
                                            
  SELECT
                      
  a.id AS calfid,
  COUNT(a3.id) as nproximaldamsstatus2birth
  
  
                    
  FROM animal a, movement mv
  
  INNER JOIN animal a2 on mv.animalId = a2.id
  INNER JOIN animal a3 on a2.damId = a3.id
  INNER JOIN johnes_state_2 js on a3.id = js.animalId
                      
  WHERE julianday(a.DOB) - julianday(mv.date) <= 10 
  AND julianday(a.DOB) - julianday(mv.date) >=-10 
  AND mv.type = 1
  AND a.id <> mv.animalId
  AND a3.id <> 38
  AND js.start = a.DOB
  AND js.cTVStatus = 2
  
  GROUP BY a.id

                      

'))
  
  nproximaldamsstatus3birth <- as.data.table(dbGetQuery(con,'
                                            
  SELECT
                      
  a.id AS calfid,
  COUNT(a3.id) as nproximaldamsstatus3birth
  
  
                    
  FROM animal a, movement mv
  
  INNER JOIN animal a2 on mv.animalId = a2.id
  INNER JOIN animal a3 on a2.damId = a3.id
  INNER JOIN johnes_state_2 js on a3.id = js.animalId
                      
  WHERE julianday(a.DOB) - julianday(mv.date) <= 10 
  AND julianday(a.DOB) - julianday(mv.date) >=-10 
  AND mv.type = 1
  AND a.id <> mv.animalId
  AND a3.id <> 38
  AND js.start = a.DOB
  AND js.cTVStatus = 3
  
  GROUP BY a.id

                      

'))
  
  
  
  nproximaldamsstatus2crt <- as.data.table(dbGetQuery(con,'
  
  
SELECT calfid,
count(proximaldam) as nproximaldamsstatus2crt

FROM(

SELECT
  
  a.id AS calfid,
  a3.id as proximaldam,
  a3.earTag as proximaldameartag,
  a3.cJohnesFinal
                    
  FROM animal a, movement mv
  
  LEFT JOIN animal a2 on mv.animalId = a2.id
  INNER JOIN animal a3 on a2.damId = a3.id
                      
  WHERE julianday(a.DOB) - julianday(mv.date) <= 10 
  AND julianday(a.DOB) - julianday(mv.date) >=-10 
  AND mv.type = 1
  AND a.id <> mv.animalId
  AND a3.id <> 38
  AND a3.cJohnesFinal = 2
  
  ORDER BY a.id)
  
  GROUP BY calfid
                     

'))
  
  nproximaldamsstatus3crt <- as.data.table(dbGetQuery(con,'
  
  
SELECT calfid,
count(proximaldam) as nproximaldamsstatus3crt

FROM(

SELECT
  
  a.id AS calfid,
  a3.id as proximaldam,
  a3.earTag as proximaldameartag,
  a3.cJohnesFinal
                    
  FROM animal a, movement mv
  
  LEFT JOIN animal a2 on mv.animalId = a2.id
  INNER JOIN animal a3 on a2.damId = a3.id
                      
  WHERE julianday(a.DOB) - julianday(mv.date) <= 10 
  AND julianday(a.DOB) - julianday(mv.date) >=-10 
  AND mv.type = 1
  AND a.id <> mv.animalId
  AND a3.id <> 38
  AND a3.cJohnesFinal = 3
  
  ORDER BY a.id)
  
  GROUP BY calfid


'))
  
  
  
  nvaguelyproximaldamsstatus2crt <- as.data.table(dbGetQuery(con,'
  
  
  
SELECT calfid,
count(proximaldam) as nvaguelyproximaldamsstatus2crt

FROM(

SELECT
  
  a.id AS calfid,
  a3.id as proximaldam,
  a3.earTag as proximaldameartag,
  a3.cJohnesFinal
                    
  FROM animal a, movement mv
  
  LEFT JOIN animal a2 on mv.animalId = a2.id
  INNER JOIN animal a3 on a2.damId = a3.id
                      
  WHERE julianday(a.DOB) - julianday(mv.date) <= 70 
  AND julianday(a.DOB) - julianday(mv.date) >= -70 
  AND mv.type = 1
  AND a.id <> mv.animalId
  AND a3.id <> 38
  AND a3.cJohnesFinal = 2
  
  ORDER BY a.id)
  
  GROUP BY calfid

'))
  
  nvaguelyproximaldamsstatus3crt <- as.data.table(dbGetQuery(con,'
  
  
  
SELECT calfid,
count(proximaldam) as nvaguelyproximaldamsstatus3crt

FROM(

SELECT
  
  a.id AS calfid,
  a3.id as proximaldam,
  a3.earTag as proximaldameartag,
  a3.cJohnesFinal
                    
  FROM animal a, movement mv
  
  LEFT JOIN animal a2 on mv.animalId = a2.id
  INNER JOIN animal a3 on a2.damId = a3.id
                      
  WHERE julianday(a.DOB) - julianday(mv.date) <= 70 
  AND julianday(a.DOB) - julianday(mv.date) >= -70 
  AND mv.type = 1
  AND a.id <> mv.animalId
  AND a3.id <> 38
  AND a3.cJohnesFinal = 3
  
  ORDER BY a.id)
  
  GROUP BY calfid

'))
  
  
  
  
  nvaguelyproximaldamsstatus2birth <- as.data.table(dbGetQuery(con,'
                                            
  SELECT
                      
  a.id AS calfid,
  COUNT(a3.id) as nvaguelyproximaldamsstatus2birth
  
  
                    
  FROM animal a, movement mv
  
  INNER JOIN animal a2 on mv.animalId = a2.id
  INNER JOIN animal a3 on a2.damId = a3.id
  INNER JOIN johnes_state_2 js on a3.id = js.animalId
                      
  WHERE julianday(a.DOB) - julianday(mv.date) <= 70 
  AND julianday(a.DOB) - julianday(mv.date) >= -70 
  AND mv.type = 1
  AND a.id <> mv.animalId
  AND a3.id <> 38
  AND js.start = a.DOB
  AND js.cTVStatus = 2
  
  GROUP BY a.id

                      

'))
  
  
  
  nvaguelyproximaldamsstatus3birth <- as.data.table(dbGetQuery(con,'
                                            
  SELECT
                      
  a.id AS calfid,
  COUNT(a3.id) as nvaguelyproximaldamsstatus3birth
  
  
                    
  FROM animal a, movement mv
  
  INNER JOIN animal a2 on mv.animalId = a2.id
  INNER JOIN animal a3 on a2.damId = a3.id
  INNER JOIN johnes_state_2 js on a3.id = js.animalId
                      
  WHERE julianday(a.DOB) - julianday(mv.date) <= 70 
  AND julianday(a.DOB) - julianday(mv.date) >= -70 
  AND mv.type = 1
  AND a.id <> mv.animalId
  AND a3.id <> 38
  AND js.start = a.DOB
  AND js.cTVStatus = 3
  
  GROUP BY a.id

                      

'))
  
  nproximal_calvesstatus3crt <- as.data.table(dbGetQuery(con,'

   SELECT calfid,
  count(proximalcalfid) as nproximalcalvesstatus3crt FROM(
  
  SELECT
                      
  a.id AS calfid,
  mv.animalId AS proximalcalfid,
  a2.cJohnesFinal
                    
  FROM animal a, movement mv
  
  LEFT JOIN animal a2 on mv.animalId = a2.id
                      
  WHERE julianday(a.DOB) - julianday(mv.date) <= 10 
  AND julianday(a.DOB) - julianday(mv.date) >=-10 
  AND mv.type = 1
  AND a.id <> mv.animalId AND
  a2.cJohnesFinal = 3
                      
  ORDER BY a.id)
  
  GROUP by calfid
                                                       
'))
  
  nproximal_calvesstatus2crt <- as.data.table(dbGetQuery(con,'

   SELECT calfid,
  count(proximalcalfid) as nproximalcalvesstatus2crt FROM(
  
  SELECT
                      
  a.id AS calfid,
  mv.animalId AS proximalcalfid,
  a2.cJohnesFinal
                    
  FROM animal a, movement mv
  
  LEFT JOIN animal a2 on mv.animalId = a2.id
                      
  WHERE julianday(a.DOB) - julianday(mv.date) <= 10 
  AND julianday(a.DOB) - julianday(mv.date) >=-10 
  AND mv.type = 1
  AND a.id <> mv.animalId AND
  a2.cJohnesFinal = 2
                      
  ORDER BY a.id)
  
  GROUP by calfid
                                                       
'))
  
  
  nvaguelyproximal_calvesstatus3crt <- as.data.table(dbGetQuery(con,'

   SELECT calfid,
  count(proximalcalfid) as nvaguelyproximalcalvesstatus3crt FROM(
  
  SELECT
                      
  a.id AS calfid,
  mv.animalId AS proximalcalfid,
  a2.cJohnesFinal
                    
  FROM animal a, movement mv
  
  LEFT JOIN animal a2 on mv.animalId = a2.id
                      
  WHERE julianday(a.DOB) - julianday(mv.date) <= 70 
  AND julianday(a.DOB) - julianday(mv.date) >= -70 
  AND mv.type = 1
  AND a.id <> mv.animalId AND
  a2.cJohnesFinal = 3
                      
  ORDER BY a.id)
  
  GROUP by calfid
                                                       
'))
  
  
  nvaguelyproximal_calvesstatus2crt <- as.data.table(dbGetQuery(con,'

  SELECT calfid,
  count(proximalcalfid) as nvaguelyproximalcalvesstatus2crt FROM(
  
  SELECT
                      
  a.id AS calfid,
  mv.animalId AS proximalcalfid,
  a2.cJohnesFinal
                    
  FROM animal a, movement mv
  
  LEFT JOIN animal a2 on mv.animalId = a2.id
                      
  WHERE julianday(a.DOB) - julianday(mv.date) <= 70 
  AND julianday(a.DOB) - julianday(mv.date) >= -70 
  AND mv.type = 1
  AND a.id <> mv.animalId AND
  a2.cJohnesFinal = 2
                      
  ORDER BY a.id)
  
  GROUP by calfid
                                                       
'))
  
  
  ######JOHNES TITRES######
  
  johnesresults <- as.data.table(dbGetQuery(con, '
                                          
  SELECT 
  
  jr.animalId as calfid,
  jr.date as johnestestdate,
  jr.value as johnestitre,
  julianday(jr.date) - julianday(a.dob) as ageattest,
  cs.cParity as parityattest,
  cs.cDIM as dimattest
  
  FROM johnes_result jr
  
  INNER JOIN cow_state_2 cs on jr.animalId = cs.animalId
  INNER JOIN animal a on a.id = jr.animalId
  
  WHERE julianday(jr.date) - julianday(cs.start) >=0 AND
  julianday(cs.end) - julianday(jr.date) >0
  AND jr.date >= "2010-12-01"

  
  
  

                                          
'))
  
  
  #####YIELDS AND CELL COUNTS####
  
  mrdata <- as.data.table(dbGetQuery(con, '
                                   
  SELECT
  
  y.animalId as calfid,
  y.date as johnestestdate,
  y.totalYield as yield,
  y.butterFat as butterfat,
  y.protein as protein,
  y.lactose as lactose,
  y.SCC as cellcount
  
  FROM yield y
  
                                   
                                   
'))
  
  dbDisconnect(con)
  
  ######COLLATE DATA#####
  
  collated <- merge(calves, calves_dob, by = "calfid", all.x = TRUE)
  
  collated <- merge(collated, calffinaljohnesstatus, by = "calfid", all.x = TRUE)
  
  collated <- merge(collated, dam_parity_age, by = "calfid", all.x = TRUE)
  
  collated <- merge(collated, relstatusatbirth, by = "calfid", all.x = TRUE)
  
  collated <- merge(collated, nproximal_calves, by = "calfid", all.x = TRUE)
  
  collated <- merge(collated, nvaguelyproximal_calves, by = "calfid", all.x = TRUE)
  
  collated <- merge(collated, nproximaldams, by = "calfid", all.x = TRUE)
  
  collated <- merge(collated, nproximaldamsstatus2birth, by = "calfid", all.x = TRUE)
  
  collated <- merge(collated, nproximaldamsstatus3birth, by = "calfid", all.x = TRUE)
  
  collated$propproximaldamsstatus2birth <- collated$nproximaldamsstatus2birth / collated$nproximaldams
  
  collated$propproximaldamsstatus3birth <- collated$nproximaldamsstatus3birth / collated$nproximaldams
  
  collated <- merge(collated, nvaguelyproximaldams, by = "calfid", all.x = TRUE)
  
  collated <- merge(collated, nvaguelyproximaldamsstatus2birth, by = "calfid", all.x = TRUE)
  
  collated <- merge(collated, nvaguelyproximaldamsstatus3birth, by = "calfid", all.x = TRUE)
  
  collated$propvaguelyproximaldamsstatus2birth <- collated$nvaguelyproximaldamsstatus2birth / collated$nvaguelyproximaldams
  
  collated$propvaguelyproximaldamsstatus3birth <- collated$nvaguelyproximaldamsstatus3birth / collated$nvaguelyproximaldams
  
  collated <- merge(collated, relstatuscrt, by = "calfid", all.x = TRUE)
  
  collated <- merge(collated, nproximal_calvesstatus2crt, by = "calfid", all.x = TRUE)
  
  collated <- merge(collated, nproximal_calvesstatus3crt, by = "calfid", all.x = TRUE)
  
  collated$propproximalcalvesstatus2crt <- collated$nproximalcalvesstatus2crt / collated$nproximalcalves
  
  collated$propproximalcalvesstatus3crt <- collated$nproximalcalvesstatus3crt / collated$nproximalcalves
  
  collated <- merge(collated, nvaguelyproximal_calvesstatus2crt, by = "calfid", all.x = TRUE)
  
  collated <- merge(collated, nvaguelyproximal_calvesstatus3crt, by = "calfid", all.x = TRUE)
  
  collated$propvaguelyproximalcalvesstatus2crt <- collated$nvaguelyproximalcalvesstatus2crt / collated$nvaguelyproximalcalves
  
  collated$propvaguelyproximalcalvesstatus3crt <- collated$nvaguelyproximalcalvesstatus3crt / collated$nvaguelyproximalcalves
  
  collated <- merge(collated, nproximaldamsstatus2crt, by = "calfid", all.x = TRUE)
  
  collated <- merge(collated, nproximaldamsstatus3crt, by = "calfid", all.x = TRUE)
  
  collated$propproximaldamsstatus2crt <- collated$nproximaldamsstatus2crt / collated$nproximaldams
  
  collated$propproximaldamsstatus3crt <- collated$nproximaldamsstatus3crt / collated$nproximaldams
  
  collated <- merge(collated, nvaguelyproximaldamsstatus2crt, by = "calfid", all.x = TRUE)
  
  collated <- merge(collated, nvaguelyproximaldamsstatus3crt, by = "calfid", all.x = TRUE)
  
  collated$propvaguelyproximaldamsstatus2crt <- collated$nvaguelyproximaldamsstatus2crt / collated$nvaguelyproximaldams
  
  collated$propvaguelyproximaldamsstatus3crt <- collated$nvaguelyproximaldamsstatus3crt / collated$nvaguelyproximaldams
  
  
  
  
  
  
  collated <- merge(collated, combinedherdjohnesdata, by = "calfid", all.x = TRUE)
  
  collated$propposavg <- (collated$propposlatest + collated$propposnext) / 2
  collated$meantitreavg <- (collated$meantitrelatest + collated$meantitrenext) / 2
  collated$meantitrenegcowsavg <- (collated$meantitrenegcowslatest + collated$meantitrenegcowsnext) / 2
  
  
  
  
  
  
  
  
  
  
  collated <- merge(collated, johnesresults, by = "calfid", all.x = TRUE)
  
  collated$johnestestdate <- ymd(collated$johnestestdate)
  meantitrenegcowsbydate$date <- ymd(meantitrenegcowsbydate$date)
  
  
  collated <- merge(collated, meantitrenegcowsbydate, by.x ="johnestestdate", by.y = "date", all.X = TRUE)
  
  collated$johnestestdate <- ymd(collated$johnestestdate)
  mrdata$johnestestdate <- ymd(mrdata$johnestestdate)
  
  collated <- merge(collated, mrdata, by = c("calfid", "johnestestdate"), all.x = TRUE)
  
  
  
  
  
  #####ADD EXTRA FEATURES####
  
  
  collated <- collated[with(collated, order (calfid, johnestestdate)),]
  
  
  collated$testcounter <- 1
  for (i in 2:nrow(collated)){
    collated$testcounter[i] <- ifelse(collated$calfid[i] == collated$calfid[i-1], collated$testcounter[i-1] + 1, 1)
    
  }
  
  collated$testcounter <- ifelse(is.na(collated$johnestestdate), 0, collated$testcounter)
  
  collated$johnesclass <- ""
  
  collated$johnesclass <- ifelse(collated$johnestitre >= 30,"H",ifelse(collated$johnestitre >= 20, "M", "L"))
  
  
  
  
  
  collated <- collated[,c(1,2,3,4,5,6:90,3,101,92:100,91,102)]
  
  #collated <- merge(bornonfarm, collated, by = "calfid", all.y = TRUE)
  
  #collated$bornonfarm[which(is.na(collated$bornonfarm))] <- 0
  
  
  
  collated$damage <- collated$damage / 365 * 12
  collated$ageattest <- collated$ageattest / 365 * 12
  
  collated <- collated[collated$testcounter != 0,]
  
  
  
  
  
  
  
  
  ######CONVERT DATA LONG TO WIDE######
  
  data_indi_long <- as.data.frame(collated)
  
  data_indi_long$calfdob <- ymd(data_indi_long$calfdob)
  data_indi_long$latesttestdate <- ymd(data_indi_long$latesttestdate)
  data_indi_long$nexttestdate <- ymd(data_indi_long$nexttestdate)
  data_indi_long$johnestestdate <- ymd(data_indi_long$johnestestdate)
  
  data_indi_long <- data_indi_long[with(data_indi_long, order (calfid, johnestestdate)),]
  
  data_indi_long_united <- unite(data_indi_long, concat, 
                                 johnestestdate, 
                                 ageattest, 
                                 parityattest, 
                                 dimattest,
                                 meantitrenegcows,
                                 yield, 
                                 butterfat,
                                 protein,
                                 lactose,
                                 cellcount,
                                 johnestitre,
                                 johnesclass,
                                 sep = ":", remove = TRUE)
  
  data_indi_wide <- spread(data_indi_long_united, testcounter, concat)
  
  nbaselinevars <- 89
  
  timevaryingvarnames <- c("johnestestdate",
                           "ageattest",
                           "parityattest",
                           "dimattest",
                           "meantitrenegcows",
                           "yield",
                           "butterfat",
                           "protein",
                           "lactose",
                           "cellcount",
                           "johnestitre",
                           "johnesclass")
  
  ntimevaryingvars <- length(timevaryingvarnames)
  
  
  
  maxntests <- ncol(data_indi_wide) - nbaselinevars
  
  for (i in 1:maxntests){
    data_indi_wide[,nbaselinevars+i][is.na(data_indi_wide[,nbaselinevars+i])] <- paste(replicate(ntimevaryingvars-1, ":"), collapse = "")
  }
  
  for (i in 1:maxntests){
    data_indi_wide <- separate(data_indi_wide, nbaselinevars + 1 + ((i-1) * ntimevaryingvars), paste(timevaryingvarnames, i, sep = ""), sep = ":", remove = TRUE, extra = "warn", convert = TRUE)  
  }
  
  for (i in 1:nrow(data_indi_wide)){
    profilelist <- NULL
    for (j in 1:maxntests){
      profilelist <- paste(profilelist, data_indi_wide[i,nbaselinevars+((j-1)*ntimevaryingvars)+match(c("johnesclass"), timevaryingvarnames)], sep = "")
    }
    data_indi_wide$profile[i] <- profilelist
  }
  
  data_indi_wide$ntests <- nchar(data_indi_wide$profile)
  
  
  data_indi_wide$ageatfirsttest <- data_indi_wide$ageattest1
  
  data_indi_wide$ageatlasttest <- 0
  
  for (i in 1:nrow(data_indi_wide)){
    data_indi_wide$ageatlasttest[i] <- data_indi_wide[i,nbaselinevars + ((data_indi_wide$ntests[i]-1)*ntimevaryingvars) + match(c("ageattest"),timevaryingvarnames)]
  }
  
  
  data_indi_wide$testingperiod <- as.numeric(data_indi_wide$ageatlasttest) - as.numeric(data_indi_wide$ageatfirsttest)
  
  data_indi_wide$nonehigh <- ifelse(grepl("H", data_indi_wide$profile, fixed = TRUE) == TRUE, 0, 1)
  data_indi_wide$Hanypoint <- ifelse(grepl("H", data_indi_wide$profile, fixed = TRUE) == TRUE, 1, 0)
  data_indi_wide$HHanypoint <- ifelse(grepl("HH", data_indi_wide$profile, fixed = TRUE) == TRUE, 1, 0)
  data_indi_wide$HLHanypoint <- ifelse(grepl("HLH", data_indi_wide$profile, fixed = TRUE) == TRUE, 1, 0)
  data_indi_wide$HMHanypoint <- ifelse(grepl("HMH", data_indi_wide$profile, fixed = TRUE) == TRUE, 1, 0)
  
  
  
  data_indi_wide$ageatfirstH <- 0
  
  for (i in 1:nrow(data_indi_wide)){
    Htestno <- ifelse(gregexpr(pattern ='H',data_indi_wide$profile[i])[[1]][1] == -1, 0, gregexpr(pattern ='H',data_indi_wide$profile[i])[[1]][1])
    data_indi_wide$ageatfirstH[i] <- ifelse(Htestno == 0, 0, data_indi_wide[i,nbaselinevars + ((Htestno-1)*ntimevaryingvars) + match(c("ageattest"),timevaryingvarnames)])
  }
  
  data_indi_wide$ageatfirstM <- 0
  
  for (i in 1:nrow(data_indi_wide)){
    Mtestno <- ifelse(gregexpr(pattern ='M',data_indi_wide$profile[i])[[1]][1] == -1, 0, gregexpr(pattern ='M',data_indi_wide$profile[i])[[1]][1])
    data_indi_wide$ageatfirstM[i] <- ifelse(Mtestno == 0, 0, data_indi_wide[i,nbaselinevars + ((Mtestno-1)*ntimevaryingvars) + match(c("ageattest"),timevaryingvarnames)])
  }
  
  data_indi_wide$ageatfirstHH <- 0
  
  for (i in 1:nrow(data_indi_wide)){
    HHtestno <- ifelse(gregexpr(pattern ='HH',data_indi_wide$profile[i])[[1]][1] == -1, 0, gregexpr(pattern ='HH',data_indi_wide$profile[i])[[1]][1])
    data_indi_wide$ageatfirstHH[i] <- ifelse(HHtestno == 0, 0, data_indi_wide[i,nbaselinevars + ((HHtestno-1)*ntimevaryingvars) + match(c("ageattest"),timevaryingvarnames)])
  }
  
  
  
  data_indi_wide$ageatfirstHMH <- 0
  
  for (i in 1:nrow(data_indi_wide)){
    HMHtestno <- ifelse(gregexpr(pattern ='HMH',data_indi_wide$profile[i])[[1]][1] == -1, 0, gregexpr(pattern ='HMH',data_indi_wide$profile[i])[[1]][1])
    data_indi_wide$ageatfirstHMH[i] <- ifelse(HMHtestno == 0, 0, data_indi_wide[i,nbaselinevars + ((HMHtestno-1)*ntimevaryingvars) + match(c("ageattest"),timevaryingvarnames)])
  }
  
  
  data_indi_wide$ageatfirstHLH <- 0
  
  for (i in 1:nrow(data_indi_wide)){
    HLHtestno <- ifelse(gregexpr(pattern ='HLH',data_indi_wide$profile[i])[[1]][1] == -1, 0, gregexpr(pattern ='HLH',data_indi_wide$profile[i])[[1]][1])
    data_indi_wide$ageatfirstHLH[i] <- ifelse(HLHtestno == 0, 0, data_indi_wide[i,nbaselinevars + ((HLHtestno-1)*ntimevaryingvars) + match(c("ageattest"),timevaryingvarnames)])
  }
  
  data_indi_wide$posstatusanypoint <- ifelse(data_indi_wide$HHanypoint == 1 | data_indi_wide$HMHanypoint == 1 | data_indi_wide$HLHanypoint == 1, 1, 0)
  
  
  
  
  data_indi_wide$nposlatest <- ifelse(!is.na(data_indi_wide$ntestedlatest) & is.na(data_indi_wide$nposlatest), 0, data_indi_wide$nposlatest)
  
  data_indi_wide$nposnext <- ifelse(!is.na(data_indi_wide$ntestednext) & is.na(data_indi_wide$nposnext), 0, data_indi_wide$nposnext)
  
  data_indi_wide$propposlatest <- ifelse(data_indi_wide$nposlatest == 0, 0, data_indi_wide$propposlatest)
  
  data_indi_wide$propposnext <- ifelse(data_indi_wide$nposnext == 0, 0, data_indi_wide$propposnext)
  
  data_indi_wide$propposlatest <- ifelse(data_indi_wide$proptestedlatest < 0.75, NA, data_indi_wide$propposlatest)
  
  data_indi_wide$propposnext <- ifelse(data_indi_wide$proptestednext < 0.75, NA, data_indi_wide$propposnext)
  
  data_indi_wide$propposavg <- (data_indi_wide$propposlatest + data_indi_wide$propposnext) / 2
  
  data_indi_wide$nsiblingsbirth[is.na(data_indi_wide$nsiblingsbirth)] <- 0
  
  data_indi_wide$nsiblingscrt[is.na(data_indi_wide$nsiblingscrt)] <- 0
  
  
  data_indi_wide$nsiblingsstatus2birth[is.na(data_indi_wide$nsiblingsstatus2birth)] <- 0
  
  data_indi_wide$nsiblingsstatus3birth[is.na(data_indi_wide$nsiblingsstatus3birth)] <- 0
  
  data_indi_wide$nsiblingsstatus2crt[is.na(data_indi_wide$nsiblingsstatus2crt)] <- 0
  
  data_indi_wide$nsiblingsstatus3crt[is.na(data_indi_wide$nsiblingsstatus3crt)] <- 0
  
  
  
  
  data_indi_wide$propsiblingsstatus2birth[is.na(data_indi_wide$propsiblingsstatus2birth)] <- 0
  
  data_indi_wide$propsiblingsstatus3birth[is.na(data_indi_wide$propsiblingsstatus3birth)] <- 0
  
  data_indi_wide$propsiblingsstatus2crt[is.na(data_indi_wide$propsiblingsstatus2crt)] <- 0
  
  data_indi_wide$propsiblingsstatus3crt[is.na(data_indi_wide$propsiblingsstatus3crt)] <- 0
  
  
  
  
  
  data_indi_wide$nauntsbirth[is.na(data_indi_wide$nauntsbirth)] <- 0
  
  data_indi_wide$nauntscrt[is.na(data_indi_wide$nauntscrt)] <- 0
  
  
  data_indi_wide$nauntsstatus2birth[is.na(data_indi_wide$nauntsstatus2birth)] <- 0
  
  data_indi_wide$nauntsstatus3birth[is.na(data_indi_wide$nauntsstatus3birth)] <- 0
  
  data_indi_wide$propauntsstatus2birth[is.na(data_indi_wide$propauntsstatus2birth)] <- 0
  
  data_indi_wide$propauntsstatus3birth[is.na(data_indi_wide$propauntsstatus3birth)] <- 0
  
  
  data_indi_wide$nauntsstatus2crt[is.na(data_indi_wide$nauntsstatus2crt)] <- 0
  
  data_indi_wide$nauntsstatus3crt[is.na(data_indi_wide$nauntsstatus3crt)] <- 0
  
  data_indi_wide$propauntsstatus2crt[is.na(data_indi_wide$propauntsstatus2crt)] <- 0
  
  data_indi_wide$propauntsstatus3crt[is.na(data_indi_wide$propauntsstatus3crt)] <- 0
  
  
  
  
  
  data_indi_wide$ngreatauntsbirth[is.na(data_indi_wide$ngreatauntsbirth)] <- 0
  
  data_indi_wide$ngreatauntsstatus2birth[is.na(data_indi_wide$ngreatauntsstatus2birth)] <- 0
  
  data_indi_wide$ngreatauntsstatus3birth[is.na(data_indi_wide$ngreatauntsstatus3birth)] <- 0
  
  data_indi_wide$propgreatauntsstatus2birth[is.na(data_indi_wide$propgreatauntsstatus2birth)] <- 0
  
  data_indi_wide$propgreatauntsstatus3birth[is.na(data_indi_wide$propgreatauntsstatus3birth)] <- 0
  
  
  data_indi_wide$ngreatauntscrt[is.na(data_indi_wide$ngreatauntscrt)] <- 0
  
  data_indi_wide$ngreatauntsstatus2crt[is.na(data_indi_wide$ngreatauntsstatus2crt)] <- 0
  
  data_indi_wide$ngreatauntsstatus3crt[is.na(data_indi_wide$ngreatauntsstatus3crt)] <- 0
  
  data_indi_wide$propgreatauntsstatus2crt[is.na(data_indi_wide$propgreatauntsstatus2crt)] <- 0
  
  data_indi_wide$propgreatauntsstatus3crt[is.na(data_indi_wide$propgreatauntsstatus3crt)] <- 0
  
  
  
  
  
  data_indi_wide$nproximalcalves[is.na(data_indi_wide$nproximalcalves)] <- 0
  
  data_indi_wide$nvaguelyproximalcalves[is.na(data_indi_wide$nvaguelyproximalcalves)] <- 0
  
  data_indi_wide$nproximaldams[is.na(data_indi_wide$nproximaldams)] <- 0
  
  data_indi_wide$nvaguelyproximaldams[is.na(data_indi_wide$nvaguelyproximaldams)] <- 0
  
  
  
  data_indi_wide$nproximaldamsstatus2birth[is.na(data_indi_wide$nproximaldamsstatus2birth)] <- 0
  
  data_indi_wide$nproximaldamsstatus3birth[is.na(data_indi_wide$nproximaldamsstatus3birth)] <- 0
  
  
  data_indi_wide$nvaguelyproximaldamsstatus2birth[is.na(data_indi_wide$nvaguelyproximaldamsstatus2birth)] <- 0
  
  data_indi_wide$nvaguelyproximaldamsstatus3birth[is.na(data_indi_wide$nvaguelyproximaldamsstatus3birth)] <- 0
  
  
  data_indi_wide$propproximaldamsstatus2birth[is.na(data_indi_wide$propproximaldamsstatus2birth)] <- 0
  
  data_indi_wide$propproximaldamsstatus3birth[is.na(data_indi_wide$propproximaldamsstatus3birth)] <- 0
  
  data_indi_wide$propvaguelyproximaldamsstatus2birth[is.na(data_indi_wide$propvaguelyproximaldamsstatus2birth)] <- 0
  
  data_indi_wide$propvaguelyproximaldamsstatus3birth[is.na(data_indi_wide$propvaguelyproximaldamsstatus3birth)] <- 0
  
  
  
  
  data_indi_wide$nproximaldamsstatus2crt[is.na(data_indi_wide$nproximaldamsstatus2crt)] <- 0
  
  data_indi_wide$nproximaldamsstatus3crt[is.na(data_indi_wide$nproximaldamsstatus3crt)] <- 0
  
  
  data_indi_wide$nvaguelyproximaldamsstatus2crt[is.na(data_indi_wide$nvaguelyproximaldamsstatus2crt)] <- 0
  
  data_indi_wide$nvaguelyproximaldamsstatus3crt[is.na(data_indi_wide$nvaguelyproximaldamsstatus3crt)] <- 0
  
  
  data_indi_wide$propproximaldamsstatus2crt[is.na(data_indi_wide$propproximaldamsstatus2crt)] <- 0
  
  data_indi_wide$propproximaldamsstatus3crt[is.na(data_indi_wide$propproximaldamsstatus3crt)] <- 0
  
  data_indi_wide$propvaguelyproximaldamsstatus2crt[is.na(data_indi_wide$propvaguelyproximaldamsstatus2crt)] <- 0
  
  data_indi_wide$propvaguelyproximaldamsstatus3crt[is.na(data_indi_wide$propvaguelyproximaldamsstatus3crt)] <- 0
  
  
  data_indi_wide$nproximalcalvesstatus2crt[is.na(data_indi_wide$nproximalcalvesstatus2crt)] <- 0
  data_indi_wide$nproximalcalvesstatus3crt[is.na(data_indi_wide$nproximalcalvesstatus3crt)] <- 0
  
  data_indi_wide$nvaguelyproximalcalvesstatus2crt[is.na(data_indi_wide$nvaguelyproximalcalvesstatus2crt)] <- 0
  data_indi_wide$nvaguelyproximalcalvesstatus3crt[is.na(data_indi_wide$nvaguelyproximalcalvesstatus3crt)] <- 0
  
  data_indi_wide$propproximalcalvesstatus2crt[is.na(data_indi_wide$propproximalcalvesstatus2crt)] <- 0
  data_indi_wide$propproximalcalvesstatus3crt[is.na(data_indi_wide$propproximalcalvesstatus3crt)] <- 0
  
  data_indi_wide$propvaguelyproximalcalvesstatus2crt[is.na(data_indi_wide$propvaguelyproximalcalvesstatus2crt)] <- 0
  data_indi_wide$propvaguelyproximalcalvesstatus3crt[is.na(data_indi_wide$propvaguelyproximalcalvesstatus3crt)] <- 0
  
  
  
  
  
  for (i in 1:nrow(data_indi_wide)){
    if(data_indi_wide$ageatfirstHH[i] == 0 & data_indi_wide$ageatfirstHLH[i] == 0 & data_indi_wide$ageatfirstHMH[i] == 0){
      data_indi_wide$ageatfirstposstatus[i] <- 0
    }
    if(data_indi_wide$ageatfirstHH[i] > 0 | data_indi_wide$ageatfirstHLH[i] > 0 | data_indi_wide$ageatfirstHMH[i] > 0){
      ageatpos <- c(data_indi_wide$ageatfirstHH[i], data_indi_wide$ageatfirstHLH[i], data_indi_wide$ageatfirstHMH[i])
      data_indi_wide$ageatfirstposstatus[i] <- min(ageatpos[ageatpos != 0])
    }
  }
  
  
  
  
  
  data_indi_wide$Farm <- FARM
  
  write.csv(data_indi_wide, paste("y:/ian/johnesthresholds/johnesproper/data/pat/TVUsDelTitres/",FARM,"_RSQL_DelTitres.csv",sep = ""))
  
  time2 <- Sys.time()
  
  nfd <- nfd + 1
  
  
  print(paste0("Time taken for farm ",nfd,": ", round(difftime(time2, time1, units = "mins"),1)," mins"))
  print(paste0("Time taken so far: ", round(difftime(time2, time0, units = "mins"),1)," mins"))
  print(paste0(nfd,"/",length(farms)," completed (",round(nfd/length(farms),2)*100,"%)"))
  
  
  
}

rm(calves)
rm(calves_dob)
rm(dam_parity_age)
rm(dam_status_birth)
rm(nTestsDamAtBirth)
rm(granddam_status_birth)
rm(siblings)
rm(nsiblingsbirth)
rm(nsiblingsstatus2birth)
rm(nsiblingsstatus3birth)
rm(nauntsbirth)
rm(nauntsstatus2birth)
rm(nauntsstatus3birth)
rm(ngreatauntsbirth)
rm(ngreatauntsstatus2birth)
rm(ngreatauntsstatus3birth)
rm(relstatusatbirth)
rm(allanimalscrtstatus)
rm(nsiblingscrt)
rm(damcrtstatus)
rm(granddamcrtstatus)
rm(nsiblingsstatus3crt)
rm(nsiblingsstatus2crt)
rm(nauntscrt)
rm(nauntsstatus3crt)
rm(nauntsstatus2crt)
rm(ngreatauntscrt)
rm(ngreatauntsstatus3crt)
rm(ngreatauntsstatus2crt)
rm(relstatuscrt)
rm(ntested)
rm(nrecorded)
rm(nposanimalsbydate)
rm(meantitrebydate)
rm(meantitrenegcowsbydate)
rm(herdjohnesdata)
rm(earlierherdtestdates)
rm(laterherdtestdates)
rm(earlierherdjohnesdata)
rm(laterherdjohnesdata)
rm(combinedherdjohnesdata)
rm(nproximal_calves)
rm(nvaguelyproximal_calves)
rm(nproximaldams)
rm(nvaguelyproximaldams)
rm(nproximaldamsstatus2birth)
rm(nproximaldamsstatus3birth)
rm(nproximaldamsstatus2crt)
rm(nproximaldamsstatus3crt)
rm(nvaguelyproximaldamsstatus2crt)
rm(nvaguelyproximaldamsstatus3crt)
rm(nvaguelyproximaldamsstatus2birth)
rm(nvaguelyproximaldamsstatus3birth)
rm(nproximal_calvesstatus3crt)
rm(nproximal_calvesstatus2crt)
rm(nvaguelyproximal_calvesstatus3crt)
rm(nvaguelyproximal_calvesstatus2crt)
rm(johnesresults)
rm(mrdata)
rm(collated)
rm(data_indi_long)
rm(data_indi_long_united)
rm(data_indi_wide)
rm(vaguely_proximal_calves)
rm(proximal_calves)
rm(farms)
rm(con)
rm(bornonfarm)
