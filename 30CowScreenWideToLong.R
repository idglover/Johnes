data <- read.csv("y:/ian/johnesthresholds/johnesproper/data/30CowScreens/TVURaiderOutput/Combined_RSQL_IncPurchased.csv")

data <- tidyr::unite(data, covsandtitre1, c(91:102), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre2, c(92:103), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre3, c(93:104), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre4, c(94:105), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre5, c(95:106), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre6, c(96:107), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre7, c(97:108), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre8, c(98:109), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre9, c(99:110), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre10, c(100:111), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre11, c(101:112), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre12, c(102:113), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre13, c(103:114), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre14, c(104:115), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre15, c(105:116), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre16, c(106:117), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre17, c(107:118), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre18, c(108:119), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre19, c(109:120), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre20, c(110:121), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre21, c(111:122), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre22, c(112:123), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre23, c(113:124), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre24, c(114:125), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre25, c(115:126), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre26, c(116:127), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre27, c(117:128), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre28, c(118:129), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre29, c(119:130), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre30, c(120:131), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre31, c(121:132), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre32, c(122:133), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre33, c(123:134), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre34, c(124:135), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre35, c(125:136), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre36, c(126:137), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre37, c(127:138), remove = TRUE, sep = ":")






data$calfdob <- ymd(data$calfdob)
data$latesttestdate <- ymd(data$latesttestdate)
data$nexttestdate <- ymd(data$nexttestdate)

data <- data[which(!is.na(data$calfeartag)),]
data <- data[which(data$calfdob != "1985-01-01"),]


data$testdobinterval <- as.integer(data$testdobinterval)
data$ntestedlatest <- as.integer(data$ntestedlatest)
data$nrecordedlatest <- as.integer(data$nrecordedlatest)


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
data$damstatusbirth_cat[data$damstatusbirth_cat != "1" & data$damstatusbirth_cat != "2" & data$damstatusbirth_cat != "3"] <- "Other"
data$damstatusbirth_cat <- droplevels(data$damstatusbirth_cat, c("4", "5", "6", "8"))

data$granddamstatusbirth_cat <- as.factor(data$granddamstatusbirth)
levels(data$granddamstatusbirth_cat) <- c(levels(data$granddamstatusbirth_cat), "Missing", "Other")
data$granddamstatusbirth_cat[is.na(data$granddamstatusbirth_cat)] <- "Missing"
data$granddamstatusbirth_cat[data$granddamstatusbirth_cat != "1" & data$granddamstatusbirth_cat != "2" & data$granddamstatusbirth_cat != "3"] <- "Other"
data$granddamstatusbirth_cat <- droplevels(data$granddamstatusbirth_cat, c("4", "5", "6", "8"))

data$damcrtstatus_cat <- as.factor(data$damcrtstatus)
levels(data$damcrtstatus_cat) <- c(levels(data$damcrtstatus_cat), "Missing", "Other")
data$damcrtstatus_cat[is.na(data$damcrtstatus_cat)] <- "Missing"
data$damcrtstatus_cat[data$damcrtstatus_cat != "1" & data$damcrtstatus_cat != "2" & data$damcrtstatus_cat != "3"] <- "Other"
data$damcrtstatus_cat <- droplevels(data$damcrtstatus_cat, c("4", "5", "6", "8"))

data$granddamcrtstatus_cat <- as.factor(data$granddamcrtstatus)
levels(data$granddamcrtstatus_cat) <- c(levels(data$granddamcrtstatus_cat), "Missing", "Other")
data$granddamcrtstatus_cat[is.na(data$granddamcrtstatus_cat)] <- "Missing"
data$granddamcrtstatus_cat[data$granddamcrtstatus_cat != "1" & data$granddamcrtstatus_cat != "2" & data$granddamcrtstatus_cat != "3"] <- "Other"
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


print(paste("Full dataset (after cleaning):",dim(data)[1],"rows"))

data <- data[,c(145,3,4,6,146,13,150,14,151,8,9,11,10,12,15,16,18,17,19,20,21,23,22,24,27,28,30,29,31,32,33,35,34,36,42,152,43,153,37,38,40,154,39,41,155,44,45,47,156,46,48,157,49,50,52,158,51,53,159,160,161,162,163,25,54,56,55,57,26,58,60,59,61,62,64,63,65,66,68,67,69,88,147,89,148,90,149,128:144,91:127)]

data$Target_Meyer <- ifelse(data$ntests >= 9 & grepl("H", substr(data$profile, nchar(data$profile)-7, nchar(data$profile))) == FALSE, "0", "U")
data$Target_Meyer <- ifelse(data$ntests >= 3 &  substr(data$profile, nchar(data$profile) - 1, nchar(data$profile)) == "HH", "1", data$Target_Meyer)
data$Target_Meyer <- as.factor(data$Target_Meyer)


data <- tidyr::gather(data, covsandtitre, value, -c(Farm,
                                                    calfeartag,
                                                    bornonfarm,
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
                                                    Target_Meyer
))

data <- tidyr::separate(data, value, c("date", "age", "parity", "dim", "meantitrenegcows", "yield", "butterfat", "protein", "lactose", "cellcount", "titre","class"), sep = ":")

data <- data[data$age != "NA",]

data <- data[
  with(data, order(calfeartag, date)),
]

write.csv(data, "y:/ian/johnesthresholds/johnesproper/data/30CowScreens/dataincpurchased.csv", row.names = FALSE)
