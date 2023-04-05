###Dodgy dam eartag finder#####

farms <- c("10_Small",
           "11_Frost",
           "13_Franks",
           "15_Clapp",
           "20_Edmunds",
           "21_Ellis",
           "25_AlstonSutton",
           "30_Keedwell",
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
           "235_Burdass",
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

allfarmsdamlist <- data.frame()

for (i in farms){
  
  
 
  FARM = i
  
  print(FARM)
  
  con <- dbConnect(RSQLite::SQLite(), paste("y:/ian/johnesthresholds/johnesproper/data/individualfarms/",FARM,"/",FARM,".tvu", sep = ""))

  damlist <- dbGetQuery(con, '
  
  SELECT 
  
  a.id as calfid,
  a.earTag as calfeartag,
  a.damId as damid,
  a2.earTag as dameartag
  
  FROM animal a
  
  INNER JOIN animal a2 on a.damId = a2.id
  
  
                        
  ')
  
  damlist$Farm <- FARM 
  
  allfarmsdamlist <- rbind(allfarmsdamlist, damlist)
}

allfarmsdamlist <- allfarmsdamlist[!duplicated(allfarmsdamlist[,c('dameartag')]),]

View(allfarmsdamlist[allfarmsdamlist$dameartag != "Unknown DAM" & 
                       allfarmsdamlist$dameartag != "BARLTETT" & 
                       allfarmsdamlist$dameartag != "" &
                       allfarmsdamlist$dameartag != "0" &
                       allfarmsdamlist$dameartag != "00" &
                       allfarmsdamlist$dameartag != "000" &
                       substr(allfarmsdamlist$dameartag,1,4) != "0000",])
                       
  