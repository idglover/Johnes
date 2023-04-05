time0 <- Sys.time()

print("Reading data...")

data <- read.csv("y:/ian/johnesthresholds/johnesproper/data/pat/TVUsDelTitres/Combined_RSQL_DelTitres.csv")

print("Combining milk recording data...")

alpha <- (ncol(data)-108)/12

#for (i in 1:alpha){

#  cvttrstring <- paste0("covsandtitre",i)
#  data <- tidyr::unite(data, cvttrstring, c(i+90, 
#                                            i+91,
#                                            i+92,
#                                            i+93,
#                                            i+94,
#                                            i+95,
#                                            i+96,
#                                            i+97,
#                                            i+98,
#                                            i+99,
#                                            i+101), 
#                       remove = TRUE, sep = ":")

#}


data <- tidyr::unite(data, covsandtitre1, c(92:103), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre2, c(93:104), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre3, c(94:105), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre4, c(95:106), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre5, c(96:107), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre6, c(97:108), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre7, c(98:109), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre8, c(99:110), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre9, c(100:111), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre10, c(101:112), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre11, c(102:113), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre12, c(103:114), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre13, c(104:115), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre14, c(105:116), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre15, c(106:117), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre16, c(107:118), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre17, c(108:119), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre18, c(109:120), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre19, c(110:121), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre20, c(111:122), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre21, c(112:123), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre22, c(113:124), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre23, c(114:125), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre24, c(115:126), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre25, c(116:127), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre26, c(117:128), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre27, c(118:129), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre28, c(119:130), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre29, c(120:131), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre30, c(121:132), remove = TRUE, sep = ":")
#data <- tidyr::unite(data, covsandtitre31, c(122:133), remove = TRUE, sep = ":")
#data <- tidyr::unite(data, covsandtitre32, c(123:134), remove = TRUE, sep = ":")
#data <- tidyr::unite(data, covsandtitre33, c(124:135), remove = TRUE, sep = ":")
#data <- tidyr::unite(data, covsandtitre34, c(125:136), remove = TRUE, sep = ":")
#data <- tidyr::unite(data, covsandtitre35, c(126:137), remove = TRUE, sep = ":")
#data <- tidyr::unite(data, covsandtitre36, c(127:138), remove = TRUE, sep = ":")
#data <- tidyr::unite(data, covsandtitre37, c(128:139), remove = TRUE, sep = ":")

print("Cleaning data...")

#Lubridate

data$calfdob <- ymd(data$calfdob)
data$latesttestdate <- ymd(data$latesttestdate)
data$nexttestdate <- ymd(data$nexttestdate)

#####CLEAN AND FORMAT INDIVIDUAL PREDICTORS######




data <- data[which(!is.na(data$calfeartag)),]
data <- data[which(data$calfdob != "1985-01-01"),]


data$testdobinterval <- as.integer(data$testdobinterval)
data$ntestedlatest <- as.integer(data$ntestedlatest)
data$nrecordedlatest <- as.integer(data$nrecordedlatest)


#data <- data[which(data$damparity != 0),]

#nsiblingsbirth and nsiblingscrt OK

#nsiblingsstatusxbirth and nsiblingsstatusxcrt OK

#propsiblingsstatusxbirth and propsiblingsstatusxcrt OK

#Dam and GrandDam status birth/crt OK

#nauntscrt and nauntsbirth, nauntsstatusxbirth/crt OK

#ngreatauntsbirth/crt and statuses OK

#nProxCalves and nProxDams OK (although on occasion nproxcalves >0 when nproxdams == 0, think because geneology is dodgy, i.e. dams are unidentified)

#nproxstatusxbirth/crt ok

#ALSO NEED TO CHECK PROPTESTED i.e. nTested/nRecorded - Some are huge as nRecorded are small. Therefore need better measure of whether this is a whole herd test,
#e.g. need rolling 2-yearly average of nRecorded??????










data$damparity_cat <- cut(data$damparity, breaks = 4)
levels(data$damparity_cat) <- c(levels(data$damparity_cat), "Missing")
data$damparity_cat[is.na(data$damparity_cat)] <- "Missing"



data$propposavg_cat <- cut(data$propposavg, breaks = c(0,0.025,0.1,0.15))
levels(data$propposavg_cat) <- c(levels(data$propposavg_cat), "Missing")
data$propposavg_cat[is.na(data$propposavg_cat)] <- "Missing"

data$meantitreavg_cat <- cut(data$meantitreavg, breaks = 4)
levels(data$meantitreavg_cat) <- c(levels(data$meantitreavg_cat), "Missing")
data$meantitreavg_cat[is.na(data$meantitreavg_cat)] <- "Missing"

data$meantitrenegcowsavg_cat <- cut(data$meantitrenegcowsavg, breaks = 4)
levels(data$meantitrenegcowsavg_cat) <- c(levels(data$meantitrenegcowsavg_cat), "Missing")
data$meantitrenegcowsavg_cat[is.na(data$meantitrenegcowsavg_cat)] <- "Missing"

data$damstatusbirth_cat <- as.factor(data$damstatusbirth)
levels(data$damstatusbirth_cat) <- c(levels(data$damstatusbirth_cat), "Missing", "Other")
data$damstatusbirth_cat[is.na(data$damstatusbirth_cat)] <- "Missing"
data$damstatusbirth_cat[data$damstatusbirth_cat != "1" & data$damstatusbirth_cat != "2" & data$damstatusbirth_cat != "3" & data$damstatusbirth_cat != "Missing"] <- "Other"
data$damstatusbirth_cat <- droplevels(data$damstatusbirth_cat, c("4", "5", "6", "8"))

data$granddamstatusbirth_cat <- as.factor(data$granddamstatusbirth)
levels(data$granddamstatusbirth_cat) <- c(levels(data$granddamstatusbirth_cat), "Missing", "Other")
data$granddamstatusbirth_cat[is.na(data$granddamstatusbirth_cat)] <- "Missing"
data$granddamstatusbirth_cat[data$granddamstatusbirth_cat != "1" & data$granddamstatusbirth_cat != "2" & data$granddamstatusbirth_cat != "3" & data$granddamstatusbirth_cat != "Missing"] <- "Other"
data$granddamstatusbirth_cat <- droplevels(data$granddamstatusbirth_cat, c("4", "5", "6", "8"))

data$damcrtstatus_cat <- as.factor(data$damcrtstatus)
levels(data$damcrtstatus_cat) <- c(levels(data$damcrtstatus_cat), "Missing", "Other")
data$damcrtstatus_cat[is.na(data$damcrtstatus_cat)] <- "Missing"
data$damcrtstatus_cat[data$damcrtstatus_cat != "1" & data$damcrtstatus_cat != "2" & data$damcrtstatus_cat != "3" & data$damcrtstatus_cat != "Missing"] <- "Other"
data$damcrtstatus_cat <- droplevels(data$damcrtstatus_cat, c("4", "5", "6", "8"))

data$granddamcrtstatus_cat <- as.factor(data$granddamcrtstatus)
levels(data$granddamcrtstatus_cat) <- c(levels(data$granddamcrtstatus_cat), "Missing", "Other")
data$granddamcrtstatus_cat[is.na(data$granddamcrtstatus_cat)] <- "Missing"
data$granddamcrtstatus_cat[data$granddamcrtstatus_cat != "1" & data$granddamcrtstatus_cat != "2" & data$granddamcrtstatus_cat != "3" & data$granddamcrtstatus_cat != "Missing"] <- "Other"
data$granddamcrtstatus_cat <- droplevels(data$granddamcrtstatus_cat, c("4", "5", "6", "8"))


data$propsiblingsstatus2crt_cat <- cut(data$propsiblingsstatus2crt, breaks = 4)
levels(data$propsiblingsstatus2crt_cat) <- c(levels(data$propsiblingsstatus2crt_cat), "Unknown")
data$propsiblingsstatus2crt_cat[which(data$nsiblingscrt == 0)] <- "Unknown"

data$propsiblingsstatus3crt_cat <- cut(data$propsiblingsstatus3crt, breaks = 4)
levels(data$propsiblingsstatus3crt_cat) <- c(levels(data$propsiblingsstatus3crt_cat), "Unknown")
data$propsiblingsstatus3crt_cat[which(data$nsiblingscrt == 0)] <- "Unknown"





data$propauntsstatus2crt_cat <- cut(data$propauntsstatus2crt, breaks = 4)
levels(data$propauntsstatus2crt_cat) <- c(levels(data$propauntsstatus2crt_cat), "Unknown")
data$propauntsstatus2crt_cat[which(data$nauntscrt == 0)] <- "Unknown"

data$propauntsstatus3crt_cat <- cut(data$propauntsstatus3crt, breaks = 4)
levels(data$propauntsstatus3crt_cat) <- c(levels(data$propauntsstatus3crt_cat), "Unknown")
data$propauntsstatus3crt_cat[which(data$nauntscrt == 0)] <- "Unknown"


data$propgreatauntsstatus2crt_cat <- cut(data$propgreatauntsstatus2crt, breaks = 4)
levels(data$propgreatauntsstatus2crt_cat) <- c(levels(data$propgreatauntsstatus2crt_cat), "Unknown")
data$propgreatauntsstatus2crt_cat[which(data$ngreatauntscrt == 0)] <- "Unknown"

data$propgreatauntsstatus3crt_cat <- cut(data$propgreatauntsstatus3crt, breaks = 4)
levels(data$propgreatauntsstatus3crt_cat) <- c(levels(data$propgreatauntsstatus3crt_cat), "Unknown")
data$propgreatauntsstatus3crt_cat[which(data$ngreatauntscrt == 0)] <- "Unknown"


data$prophorizontalrelsstatus2crt <- (data$nsiblingsstatus2crt + data$nauntsstatus2crt + data$ngreatauntsstatus2crt) / (data$nsiblingscrt + data$nauntscrt + data$ngreatauntscrt)
data$prophorizontalrelsstatus2crt_cat <- cut(data$prophorizontalrelsstatus2crt, breaks = 3)
levels(data$prophorizontalrelsstatus2crt_cat) <- c(levels(data$prophorizontalrelsstatus2crt_cat), "Unknown")
data$prophorizontalrelsstatus2crt_cat[which(is.na(data$prophorizontalrelsstatus2crt_cat))] <- "Unknown"

data$prophorizontalrelsstatus3crt <- (data$nsiblingsstatus3crt + data$nauntsstatus3crt + data$ngreatauntsstatus3crt) / (data$nsiblingscrt + data$nauntscrt + data$ngreatauntscrt)
data$prophorizontalrelsstatus3crt_cat <- cut(data$prophorizontalrelsstatus3crt, breaks = 3)
levels(data$prophorizontalrelsstatus3crt_cat) <- c(levels(data$prophorizontalrelsstatus3crt_cat), "Unknown")
data$prophorizontalrelsstatus3crt_cat[which(is.na(data$prophorizontalrelsstatus3crt_cat))] <- "Unknown"

data$prophorizontalrelsstatus2crt_cat_collapsed <- data$prophorizontalrelsstatus2crt_cat
levels(data$prophorizontalrelsstatus2crt_cat_collapsed) <- c(levels(data$prophorizontalrelsstatus2crt_cat_collapsed), "(0.333,1]")
data$prophorizontalrelsstatus2crt_cat_collapsed[data$prophorizontalrelsstatus2crt_cat_collapsed == "(0.333,0.667]" |
                                                  data$prophorizontalrelsstatus2crt_cat_collapsed == "(0.667,1]"] <- "(0.333,1]"
data$prophorizontalrelsstatus2crt_cat_collapsed <- droplevels(data$prophorizontalrelsstatus2crt_cat_collapsed, c("(0.333,0.667]", "(0.667,1]"))

data$meantitrenegcowsavg_recat <- cut(data$meantitrenegcowsavg, breaks = c(-0.00001, 2,4, 50))
levels(data$meantitrenegcowsavg_recat) <- c(levels(data$meantitrenegcowsavg_recat), "Missing")
data$meantitrenegcowsavg_recat[is.na(data$meantitrenegcowsavg_recat)] <- "Missing"


print(paste("Full dataset (after cleaning):",dim(data)[1],"rows"))







#data <- data[,c(145,4,6,146,13,150,14,151,8,9,11,10,12,15,16,18,17,19,20,21,23,22,24,27,28,30,29,31,32,33,35,34,36,42,152,43,153,37,38,40,154,39,41,155,44,45,47,156,46,48,157,49,50,52,158,51,53,159,160,161,162,163,25,54,56,55,57,26,58,60,59,61,62,64,63,65,66,68,67,69,88,147,89,148,90,149,128:144,91:127)]

#data <- data[,c(146,4,6,7,147,14,151,15,152,9,10,12,11,13,16,17,19,18,20,21,22,24,23,25,28,29,31,30,32,33,34,36,35,37,43,153,44,154,38,39,41,155,40,42,156,45,46,48,157,47,49,158,50,51,53,159,52,54,160,161,162,163,164,26,55,57,56,58,27,59,61,60,62,63,65,64,66,67,69,68,70,89,148,90,149,91,150,129:145,92:128)]

data <- data[,c("Farm",
                "calfeartag",
                "finaljohnesstatus",
                "damparity",
                "damparity_cat",
                "damstatusbirth",
                "damstatusbirth_cat",
                "granddamstatusbirth",
                "granddamstatusbirth_cat",
                "nsiblingsbirth",
                "nsiblingsstatus2birth",
                "propsiblingsstatus2birth",
                "nsiblingsstatus3birth",
                "propsiblingsstatus3birth",
                "nauntsbirth",
                "nauntsstatus2birth",
                "propauntsstatus2birth",
                "nauntsstatus3birth",
                "propauntsstatus3birth",
                "ngreatauntsbirth",
                "ngreatauntsstatus2birth",
                "propgreatauntsstatus2birth",
                "ngreatauntsstatus3birth",
                "propgreatauntsstatus3birth",
                "nproximaldams",
                "nproximaldamsstatus2birth",
                "propproximaldamsstatus2birth",
                "nproximaldamsstatus3birth",
                "propproximaldamsstatus3birth",
                "nvaguelyproximaldams",
                "nvaguelyproximaldamsstatus2birth",
                "propvaguelyproximaldamsstatus2birth",
                "nvaguelyproximaldamsstatus3birth",
                "propvaguelyproximaldamsstatus3birth",
                "damcrtstatus",
                "damcrtstatus_cat",
                "granddamcrtstatus",
                "granddamcrtstatus_cat",
                "nsiblingscrt",
                "nsiblingsstatus2crt",
                "propsiblingsstatus2crt",
                "propsiblingsstatus2crt_cat",
                "nsiblingsstatus3crt",
                "propsiblingsstatus3crt",
                "propsiblingsstatus3crt_cat",
                "nauntscrt",
                "nauntsstatus2crt",
                "propauntsstatus2crt",
                "propauntsstatus2crt_cat",
                "nauntsstatus3crt",
                "propauntsstatus3crt",
                "propauntsstatus3crt_cat",
                "ngreatauntscrt",
                "ngreatauntsstatus2crt",
                "propgreatauntsstatus2crt",
                "propgreatauntsstatus2crt_cat",
                "ngreatauntsstatus3crt",
                "propgreatauntsstatus3crt",
                "propgreatauntsstatus3crt_cat",
                "prophorizontalrelsstatus2crt",
                "prophorizontalrelsstatus2crt_cat",
                "prophorizontalrelsstatus3crt",
                "prophorizontalrelsstatus3crt_cat",
                "prophorizontalrelsstatus2crt_cat_collapsed",
                "nproximalcalves",
                "nproximalcalvesstatus2crt",
                "propproximalcalvesstatus2crt",
                "nproximalcalvesstatus3crt",
                "propproximalcalvesstatus3crt",
                "nvaguelyproximalcalves",
                "nvaguelyproximalcalvesstatus2crt",
                "propvaguelyproximalcalvesstatus2crt",
                "nvaguelyproximalcalvesstatus3crt",
                "propvaguelyproximalcalvesstatus3crt",
                "nproximaldamsstatus2crt",
                "propproximaldamsstatus2crt",
                "nproximaldamsstatus3crt",
                "propproximaldamsstatus3crt",
                "nvaguelyproximaldamsstatus2crt",
                "propvaguelyproximaldamsstatus2crt",
                "nvaguelyproximaldamsstatus3crt",
                "propvaguelyproximaldamsstatus3crt",
                "propposavg",
                "propposavg_cat",
                "meantitreavg",
                "meantitreavg_cat",
                "meantitrenegcowsavg",
                "meantitrenegcowsavg_cat",
                "meantitrenegcowsavg_recat",
                "profile",
                "ntests",
                "ageatfirsttest",
                "ageatlasttest",
                "testingperiod",
                "nonehigh",
                "Hanypoint",
                "HHanypoint",
                "HLHanypoint",
                "HMHanypoint",
                "ageatfirstH",
                "ageatfirstM",
                "ageatfirstHH",
                "ageatfirstHMH",
                "ageatfirstHLH",
                "posstatusanypoint",
                "ageatfirstposstatus",
                "covsandtitre1",
                "covsandtitre2",
                "covsandtitre3",
                "covsandtitre4",
                "covsandtitre5",
                "covsandtitre6",
                "covsandtitre7",
                "covsandtitre8",
                "covsandtitre9",
                "covsandtitre10",
                "covsandtitre11",
                "covsandtitre12",
                "covsandtitre13",
                "covsandtitre14",
                "covsandtitre15",
                "covsandtitre16",
                "covsandtitre17",
                "covsandtitre18",
                "covsandtitre19",
                "covsandtitre20",
                "covsandtitre21",
                "covsandtitre22",
                "covsandtitre23",
                "covsandtitre24"
)]


#Creating label

data$Target_Meyer <- ifelse(data$ntests >= 9 & grepl("H", substr(data$profile, nchar(data$profile)-7, nchar(data$profile))) == FALSE, "0", "U")
data$Target_Meyer <- ifelse(data$ntests >= 3 &  substr(data$profile, nchar(data$profile) - 1, nchar(data$profile)) == "HH", "1", data$Target_Meyer)
data$Target_Meyer <- as.factor(data$Target_Meyer)

data$Target_QMMS <- "U"

data$Target_QMMS <- ifelse(grepl("HH", data$profile) == TRUE |
                             grepl("HMH", data$profile) == TRUE |
                             grepl("HLH", data$profile) == TRUE,
                           "1",
                           ifelse(data$ntests >= 9 &
                                    substr(data$profile, nchar(data$profile) - 7, nchar(data$profile)) == "LLLLLLLL", "0",
                                  data$Target_QMMS))


#data$Target_Strict1 <- ifelse(data$ntests >= 9 & grepl("H", data$profile, 1, nchar(data$profile)) == FALSE, "0", "U")
#data$Target_Strict1 <- ifelse(data$ntests >= 3 &  substr(data$profile, nchar(data$profile) - 1, nchar(data$profile)) == "HH", "1", data$Target_Strict1)
#data$Target_Strict1 <- as.factor(data$Target_Strict1)

#Categorise nsiblings/aunts/greataunts for interactions (distinguishing zero denominators e.g. zero siblings -> zero siblings status x)

#data$nsiblingsbirth <- as.factor(data$nsiblingsbirth)
#data$nsiblingscrt <- as.factor(data$nsiblingscrt)
#data$nauntsbirth <- as.factor(data$nauntsbirth)
#data$nauntscrt <- as.factor(data$nauntscrt)
#data$ngreatauntsbirth <- as.factor(data$ngreatauntsbirth)
#data$ngreatauntscrt <- as.factor(data$ngreatauntscrt)


#data <- data[!is.na(data$calfeartag),]








#Plot Birth GLMM marginal effects

finalbirthmodel <- readRDS("y:/ian/johnesthresholds/johnesproper/data/pickledmodels/finalbirthmodel.rds")
finalbirthmodel_TQ <- readRDS("y:/ian/johnesthresholds/johnesproper/data/pickledmodels/finalbirthmodel_TQ.rds")




print("Converting data wide to long...")

data <- tidyr::gather(data, covsandtitre, value, -c(Farm,
                                                    calfeartag,
                                                    damparity,
                                                    damparity_cat,
                                                    damstatusbirth,
                                                    damstatusbirth_cat,
                                                    granddamstatusbirth,
                                                    granddamstatusbirth_cat,
                                                    nsiblingsbirth,
                                                    nsiblingsstatus2birth,
                                                    propsiblingsstatus2birth,
                                                    nsiblingsstatus3birth,
                                                    propsiblingsstatus3birth,
                                                    nauntsbirth,
                                                    nauntsstatus2birth,
                                                    propauntsstatus2birth,
                                                    nauntsstatus3birth,
                                                    propauntsstatus3birth,
                                                    ngreatauntsbirth,
                                                    ngreatauntsstatus2birth,
                                                    propgreatauntsstatus2birth,
                                                    ngreatauntsstatus3birth,
                                                    propgreatauntsstatus3birth,
                                                    nproximaldams,
                                                    nproximaldamsstatus2birth,
                                                    propproximaldamsstatus2birth,
                                                    nproximaldamsstatus3birth,
                                                    propproximaldamsstatus3birth,
                                                    nvaguelyproximaldams,
                                                    nvaguelyproximaldamsstatus2birth,
                                                    propvaguelyproximaldamsstatus2birth,
                                                    nvaguelyproximaldamsstatus3birth,
                                                    propvaguelyproximaldamsstatus3birth,
                                                    damcrtstatus,
                                                    damcrtstatus_cat,
                                                    granddamcrtstatus,
                                                    granddamcrtstatus_cat,
                                                    nsiblingscrt,
                                                    nsiblingsstatus2crt,
                                                    propsiblingsstatus2crt,
                                                    propsiblingsstatus2crt_cat,
                                                    nsiblingsstatus3crt,
                                                    propsiblingsstatus3crt,
                                                    propsiblingsstatus3crt_cat,
                                                    nauntscrt,
                                                    nauntsstatus2crt,
                                                    propauntsstatus2crt,
                                                    propauntsstatus2crt_cat,
                                                    nauntsstatus3crt,
                                                    propauntsstatus3crt,
                                                    propauntsstatus3crt_cat,
                                                    ngreatauntscrt,
                                                    ngreatauntsstatus2crt,
                                                    propgreatauntsstatus2crt,
                                                    propgreatauntsstatus2crt_cat,
                                                    ngreatauntsstatus3crt,
                                                    propgreatauntsstatus3crt,
                                                    propgreatauntsstatus3crt_cat,
                                                    prophorizontalrelsstatus2crt,
                                                    prophorizontalrelsstatus2crt_cat,
                                                    prophorizontalrelsstatus2crt_cat_collapsed,
                                                    prophorizontalrelsstatus3crt,
                                                    prophorizontalrelsstatus3crt_cat,
                                                    nproximalcalves,
                                                    nproximalcalvesstatus2crt,
                                                    propproximalcalvesstatus2crt,
                                                    nproximalcalvesstatus3crt,
                                                    propproximalcalvesstatus3crt,
                                                    nvaguelyproximalcalves,
                                                    nvaguelyproximalcalvesstatus2crt,
                                                    propvaguelyproximalcalvesstatus2crt,
                                                    nvaguelyproximalcalvesstatus3crt,
                                                    propvaguelyproximalcalvesstatus3crt,
                                                    nproximaldamsstatus2crt,
                                                    propproximaldamsstatus2crt,
                                                    nproximaldamsstatus3crt,
                                                    propproximaldamsstatus3crt,
                                                    nvaguelyproximaldamsstatus2crt,
                                                    propvaguelyproximaldamsstatus2crt,
                                                    nvaguelyproximaldamsstatus3crt,
                                                    propvaguelyproximaldamsstatus3crt,
                                                    propposavg,
                                                    propposavg_cat,
                                                    meantitreavg,
                                                    meantitreavg_cat,
                                                    meantitrenegcowsavg,
                                                    meantitrenegcowsavg_cat,
                                                    meantitrenegcowsavg_recat,
                                                    profile,
                                                    ntests,
                                                    ageatfirsttest,
                                                    ageatlasttest,
                                                    testingperiod,
                                                    nonehigh,
                                                    Hanypoint,
                                                    HHanypoint,
                                                    HLHanypoint,
                                                    HMHanypoint,
                                                    ageatfirstH,
                                                    ageatfirstM,
                                                    ageatfirstHH,
                                                    ageatfirstHMH,
                                                    ageatfirstHLH,
                                                    posstatusanypoint,
                                                    ageatfirstposstatus,
                                                    finaljohnesstatus,
                                                    Target_Meyer,
                                                    Target_QMMS
))



data <- tidyr::separate(data, value, c("date", "age", "parity", "dim", "meantitrenegcows", "yield", "butterfat", "protein", "lactose", "cellcount", "titre","class"), sep = ":")

data <- data[data$age != "NA",]

data <- data[
  with(data, order(calfeartag, date)),
]

print("Predicting birth models on full dataset...")

reliability <- read.csv("y:/ian/johnesthresholds/johnesproper/data/PAT/reliability.csv")

data <- merge(data, reliability[,c(1,3,6)], by = "calfeartag", all.x = TRUE)

data$damntests[which(is.na(data$damntests))] <- 0
data$granddamntests[which(is.na(data$granddamntests))] <- 0

if(target == "MEYER"){
  data$predLR <- predict(finalbirthmodel, newdata = data, type = "response", re.form=~0, allow.new.levels = T)}

if(target == "QMMS"){
  data$predLR <- predict(finalbirthmodel_TQ, newdata = data, type = "response", re.form=~0, allow.new.levels = T)}


ggplot(data, aes(x = predLR)) +
  geom_histogram() +
  labs(title = "Predicted Probabilities at Birth", subtitle = paste0("Target ", target), x = "Probability")

#data$predLR <- predict(model_LR_1_future, newdata = data, type = "prob")[,2]

#data$predMARS <- predict(model_MARS_1, newdata = data, type = "prob")[,2]

data$birthpriorLR <- data$predLR / (1 - data$predLR) #Prior ODDS at birth

data$birthpriorLR[which(data$homegrown == 0)] <- 0.10

#data$birthpriorMARS <- data$predMARS / (1 - data$predMARS)



print("Saving data (data_birthpriors.csv)")


write.csv(data, "y:/ian/johnesthresholds/johnesproper/data/pat/TVUsDelTitres/data_birthpriors_DelTitres.csv", row.names = FALSE)

time1 <- Sys.time()
print(paste("Time processing birth models:", round(difftime(time1, time0, units = "mins"),2), "mins"))




rm(data)

