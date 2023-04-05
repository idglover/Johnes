####READING DATA####

time0 <- Sys.time()

print("Reading data...")

data <- read.csv("y:/ian/johnesthresholds/johnesproper/data/CombinedData/Combined_RSQL.csv")

####IDENTIFY VERY LOW TITRES####

print("Combining milk recording data...")

cd1 <- ifelse(data[,135] < 5, "V","O")
cd2 <- ifelse(data[,147] < 5, "V","O")
cd3 <- ifelse(data[,159] < 5, "V","O")
cd4 <- ifelse(data[,171] < 5, "V","O")
cd5 <- ifelse(data[,183] < 5, "V","O")
cd6 <- ifelse(data[,195] < 5, "V","O")
cd7 <- ifelse(data[,207] < 5, "V","O")
cd8 <- ifelse(data[,219] < 5, "V","O")
cd9 <- ifelse(data[,231] < 5, "V","O")
cd10 <- ifelse(data[,243] < 5, "V","O")
cd11 <- ifelse(data[,255] < 5, "V","O")
cd12 <- ifelse(data[,267] < 5, "V","O")
cd13 <- ifelse(data[,279] < 5, "V","O")
cd14 <- ifelse(data[,291] < 5, "V","O")
cd15 <- ifelse(data[,303] < 5, "V","O")
cd16 <- ifelse(data[,315] < 5, "V","O")
cd17 <- ifelse(data[,327] < 5, "V","O")
cd18 <- ifelse(data[,339] < 5, "V","O")
cd19 <- ifelse(data[,351] < 5, "V","O")
cd20 <- ifelse(data[,363] < 5, "V","O")
cd21 <- ifelse(data[,375] < 5, "V","O")
cd22 <- ifelse(data[,387] < 5, "V","O")
cd23 <- ifelse(data[,399] < 5, "V","O")
cd24 <- ifelse(data[,411] < 5, "V","O")
cd25 <- ifelse(data[,423] < 5, "V","O")
cd26 <- ifelse(data[,435] < 5, "V","O")
cd27 <- ifelse(data[,447] < 5, "V","O")
cd28 <- ifelse(data[,459] < 5, "V","O")
cd29 <- ifelse(data[,471] < 5, "V","O")
cd30 <- ifelse(data[,483] < 5, "V","O")
cd31 <- ifelse(data[,495] < 5, "V","O")
cd32 <- ifelse(data[,507] < 5, "V","O")
cd33 <- ifelse(data[,519] < 5, "V","O")
cd34 <- ifelse(data[,531] < 5, "V","O")
cd35 <- ifelse(data[,543] < 5, "V","O")
cd36 <- ifelse(data[,555] < 5, "V","O")
cd37 <- ifelse(data[,567] < 5, "V","O")
  
cd1[is.na(cd1)] <- ""
cd2[is.na(cd2)] <- ""
cd3[is.na(cd3)] <- ""
cd4[is.na(cd4)] <- ""
cd5[is.na(cd5)] <- ""
cd6[is.na(cd6)] <- ""
cd7[is.na(cd7)] <- ""
cd8[is.na(cd8)] <- ""
cd9[is.na(cd9)] <- ""
cd10[is.na(cd10)] <- ""
cd11[is.na(cd11)] <- ""
cd12[is.na(cd12)] <- ""
cd13[is.na(cd13)] <- ""
cd14[is.na(cd14)] <- ""
cd15[is.na(cd15)] <- ""
cd16[is.na(cd16)] <- ""
cd17[is.na(cd17)] <- ""
cd18[is.na(cd18)] <- ""
cd19[is.na(cd19)] <- ""
cd20[is.na(cd20)] <- ""
cd21[is.na(cd21)] <- ""
cd22[is.na(cd22)] <- ""
cd23[is.na(cd23)] <- ""
cd24[is.na(cd24)] <- ""
cd25[is.na(cd25)] <- ""
cd26[is.na(cd26)] <- ""
cd27[is.na(cd27)] <- ""
cd28[is.na(cd28)] <- ""
cd29[is.na(cd29)] <- ""
cd30[is.na(cd30)] <- ""
cd31[is.na(cd31)] <- ""
cd32[is.na(cd32)] <- ""
cd33[is.na(cd33)] <- ""
cd34[is.na(cd34)] <- ""
cd35[is.na(cd35)] <- ""
cd36[is.na(cd36)] <- ""
cd37[is.na(cd37)] <- ""
  
data$VLProfile <- paste0(cd1,
       cd2,
       cd3,
       cd4,
       cd5,
       cd6,
       cd7,
       cd8,
       cd9,
       cd10,
       cd11,
       cd12,
       cd13,
       cd14,
       cd15,
       cd16,
       cd17,
       cd18,
       cd19,
       cd20,
       cd21,
       cd22,
       cd23,
       cd24,
       cd25,
       cd26,
       cd27,
       cd28,
       cd29,
       cd30,
       cd31,
       cd32,
       cd33,
       cd34,
       cd35,
       cd36,
       cd37)

####REARRANGE COLUMNS####

data <- data[,c(1:569,587,570:586)]

####UNITE TIME-VARYING FEATURES####

data <- tidyr::unite(data, covsandtitre1, c(125:136), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre2, c(126:137), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre3, c(127:138), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre4, c(128:139), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre5, c(129:140), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre6, c(130:141), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre7, c(131:142), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre8, c(132:143), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre9, c(133:144), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre10, c(134:145), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre11, c(135:146), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre12, c(136:147), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre13, c(137:148), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre14, c(138:149), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre15, c(139:150), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre16, c(140:151), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre17, c(141:152), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre18, c(142:153), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre19, c(143:154), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre20, c(144:155), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre21, c(145:156), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre22, c(146:157), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre23, c(147:158), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre24, c(148:159), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre25, c(149:160), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre26, c(150:161), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre27, c(151:162), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre28, c(152:163), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre29, c(153:164), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre30, c(154:165), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre31, c(155:166), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre32, c(156:167), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre33, c(157:168), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre34, c(158:169), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre35, c(159:170), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre36, c(160:171), remove = TRUE, sep = ":")
data <- tidyr::unite(data, covsandtitre37, c(161:172), remove = TRUE, sep = ":")

####CLEAN AND FORMAT DATA####

print("Cleaning data...")

#Lubridate

data$calfdob <- ymd(data$calfdob)
data$latesttestdate <- ymd(data$latesttestdate)
data$nexttestdate <- ymd(data$nexttestdate)

#####CLEAN AND FORMAT FEATURES#####

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
data$damstatusbirth_cat[data$damstatusbirth_cat != "1" & data$damstatusbirth_cat != "2" & data$damstatusbirth_cat != "3" & data$damstatusbirth_cat != "Missing"] <- "Other"
data$damstatusbirth_cat <- droplevels(data$damstatusbirth_cat, c("4", "5", "6", "8"))

data$granddamstatusbirth_cat <- as.factor(data$granddamstatusbirth)
levels(data$granddamstatusbirth_cat) <- c(levels(data$granddamstatusbirth_cat), "Missing", "Other")
data$granddamstatusbirth_cat[is.na(data$granddamstatusbirth_cat)] <- "Missing"
data$granddamstatusbirth_cat[data$granddamstatusbirth_cat != "1" & data$granddamstatusbirth_cat != "2" & data$granddamstatusbirth_cat != "3" & data$granddamstatusbirth_cat != "Missing"] <- "Other"
data$granddamstatusbirth_cat <- droplevels(data$granddamstatusbirth_cat, c("4", "5", "6", "8"))

data$damstatus12mold_cat <- as.factor(data$damstatus12mold)
levels(data$damstatus12mold_cat) <- c(levels(data$damstatus12mold_cat), "Missing", "Other")
data$damstatus12mold_cat[is.na(data$damstatus12mold_cat)] <- "Missing"
data$damstatus12mold_cat[data$damstatus12mold_cat != "1" & data$damstatus12mold_cat != "2" & data$damstatus12mold_cat != "3" & data$damstatus12mold_cat != "Missing"] <- "Other"
data$damstatus12mold_cat <- droplevels(data$damstatus12mold_cat, c("4", "5", "6", "8"))

data$granddamstatus12mold_cat <- as.factor(data$granddamstatus12mold)
levels(data$granddamstatus12mold_cat) <- c(levels(data$granddamstatus12mold_cat), "Missing", "Other")
data$granddamstatus12mold_cat[is.na(data$granddamstatus12mold_cat)] <- "Missing"
data$granddamstatus12mold_cat[data$granddamstatus12mold_cat != "1" & data$granddamstatus12mold_cat != "2" & data$granddamstatus12mold_cat != "3" & data$granddamstatus12mold_cat != "Missing"] <- "Other"
data$granddamstatus12mold_cat <- droplevels(data$granddamstatus12mold_cat, c("4", "5", "6", "8"))

data$damcrtstatus_cat <- as.factor(data$damcrtstatus)
levels(data$damcrtstatus_cat) <- c(levels(data$damcrtstatus_cat), "Missing", "Other")
data$damcrtstatus_cat[is.na(data$damcrtstatus_cat)] <- "Missing"
data$damcrtstatus_cat[data$damcrtstatus_cat != "1" & data$damcrtstatus_cat != "2" & data$damcrtstatus_cat != "3" & data$damcrtstatus != "Missing"] <- "Other"
data$damcrtstatus_cat <- droplevels(data$damcrtstatus_cat, c("4", "5", "6", "8"))

data$granddamcrtstatus_cat <- as.factor(data$granddamcrtstatus)
levels(data$granddamcrtstatus_cat) <- c(levels(data$granddamcrtstatus_cat), "Missing", "Other")
data$granddamcrtstatus_cat[is.na(data$granddamcrtstatus_cat)] <- "Missing"
data$granddamcrtstatus_cat[data$granddamcrtstatus_cat != "1" & data$granddamcrtstatus_cat != "2" & data$granddamcrtstatus_cat != "3" & data$granddamcrtstatus != "Missing"] <- "Other"
data$granddamcrtstatus_cat <- droplevels(data$granddamcrtstatus_cat, c("4", "5", "6", "8"))

data$propsiblingsstatus2birth_cat <- cut(data$propsiblingsstatus2birth, breaks = 4)
levels(data$propsiblingsstatus2birth_cat) <- c(levels(data$propsiblingsstatus2birth_cat), "Unknown")
data$propsiblingsstatus2birth_cat[which(data$nsiblingsbirth == 0)] <- "Unknown"

data$propsiblingsstatus3birth_cat <- cut(data$propsiblingsstatus3birth, breaks = 4)
levels(data$propsiblingsstatus3birth_cat) <- c(levels(data$propsiblingsstatus3birth_cat), "Unknown")
data$propsiblingsstatus3birth_cat[which(data$nsiblingsbirth == 0)] <- "Unknown"

data$propsiblingsstatus212mold_cat <- cut(data$propsiblingsstatus212mold, breaks = 4)
levels(data$propsiblingsstatus212mold_cat) <- c(levels(data$propsiblingsstatus212mold_cat), "Unknown")
data$propsiblingsstatus212mold_cat[which(data$nsiblings12mold == 0)] <- "Unknown"

data$propsiblingsstatus312mold_cat <- cut(data$propsiblingsstatus312mold, breaks = 4)
levels(data$propsiblingsstatus312mold_cat) <- c(levels(data$propsiblingsstatus312mold_cat), "Unknown")
data$propsiblingsstatus312mold_cat[which(data$nsiblings12mold == 0)] <- "Unknown"

data$propsiblingsstatus2crt_cat <- cut(data$propsiblingsstatus2crt, breaks = 4)
levels(data$propsiblingsstatus2crt_cat) <- c(levels(data$propsiblingsstatus2crt_cat), "Unknown")
data$propsiblingsstatus2crt_cat[which(data$nsiblingscrt == 0)] <- "Unknown"

data$propsiblingsstatus3crt_cat <- cut(data$propsiblingsstatus3crt, breaks = 4)
levels(data$propsiblingsstatus3crt_cat) <- c(levels(data$propsiblingsstatus3crt_cat), "Unknown")
data$propsiblingsstatus3crt_cat[which(data$nsiblingscrt == 0)] <- "Unknown"

data$propauntsstatus2birth_cat <- cut(data$propauntsstatus2birth, breaks = 4)
levels(data$propauntsstatus2birth_cat) <- c(levels(data$propauntsstatus2birth_cat), "Unknown")
data$propauntsstatus2birth_cat[which(data$nauntsbirth == 0)] <- "Unknown"

data$propauntsstatus3birth_cat <- cut(data$propauntsstatus3birth, breaks = 4)
levels(data$propauntsstatus3birth_cat) <- c(levels(data$propauntsstatus3birth_cat), "Unknown")
data$propauntsstatus3birth_cat[which(data$nauntsbirth == 0)] <- "Unknown"

data$propauntsstatus212mold_cat <- cut(data$propauntsstatus212mold, breaks = 4)
levels(data$propauntsstatus212mold_cat) <- c(levels(data$propauntsstatus212mold_cat), "Unknown")
data$propauntsstatus212mold_cat[which(data$naunts12mold == 0)] <- "Unknown"

data$propauntsstatus312mold_cat <- cut(data$propauntsstatus312mold, breaks = 4)
levels(data$propauntsstatus312mold_cat) <- c(levels(data$propauntsstatus312mold_cat), "Unknown")
data$propauntsstatus312mold_cat[which(data$naunts12mold == 0)] <- "Unknown"

data$propauntsstatus2crt_cat <- cut(data$propauntsstatus2crt, breaks = 4)
levels(data$propauntsstatus2crt_cat) <- c(levels(data$propauntsstatus2crt_cat), "Unknown")
data$propauntsstatus2crt_cat[which(data$nauntscrt == 0)] <- "Unknown"

data$propauntsstatus3crt_cat <- cut(data$propauntsstatus3crt, breaks = 4)
levels(data$propauntsstatus3crt_cat) <- c(levels(data$propauntsstatus3crt_cat), "Unknown")
data$propauntsstatus3crt_cat[which(data$nauntscrt == 0)] <- "Unknown"

data$propgreatauntsstatus2birth_cat <- cut(data$propgreatauntsstatus2birth, breaks = 4)
levels(data$propgreatauntsstatus2birth_cat) <- c(levels(data$propgreatauntsstatus2birth_cat), "Unknown")
data$propgreatauntsstatus2birth_cat[which(data$ngreatauntsbirth == 0)] <- "Unknown"

data$propgreatauntsstatus3birth_cat <- cut(data$propgreatauntsstatus3birth, breaks = 4)
levels(data$propgreatauntsstatus3birth_cat) <- c(levels(data$propgreatauntsstatus3birth_cat), "Unknown")
data$propgreatauntsstatus3birth_cat[which(data$ngreatauntsbirth == 0)] <- "Unknown"

data$propgreatauntsstatus212mold_cat <- cut(data$propgreatauntsstatus212mold, breaks = 4)
levels(data$propgreatauntsstatus212mold_cat) <- c(levels(data$propgreatauntsstatus212mold_cat), "Unknown")
data$propgreatauntsstatus212mold_cat[which(data$ngreataunts12mold == 0)] <- "Unknown"

data$propgreatauntsstatus312mold_cat <- cut(data$propgreatauntsstatus312mold, breaks = 4)
levels(data$propgreatauntsstatus312mold_cat) <- c(levels(data$propgreatauntsstatus312mold_cat), "Unknown")
data$propgreatauntsstatus312mold_cat[which(data$ngreataunts12mold == 0)] <- "Unknown"

data$propgreatauntsstatus2crt_cat <- cut(data$propgreatauntsstatus2crt, breaks = 4)
levels(data$propgreatauntsstatus2crt_cat) <- c(levels(data$propgreatauntsstatus2crt_cat), "Unknown")
data$propgreatauntsstatus2crt_cat[which(data$ngreatauntscrt == 0)] <- "Unknown"

data$propgreatauntsstatus3crt_cat <- cut(data$propgreatauntsstatus3crt, breaks = 4)
levels(data$propgreatauntsstatus3crt_cat) <- c(levels(data$propgreatauntsstatus3crt_cat), "Unknown")
data$propgreatauntsstatus3crt_cat[which(data$ngreatauntscrt == 0)] <- "Unknown"

data$prophorizontalrelsstatus212mold <- (data$nsiblingsstatus212mold + data$nauntsstatus212mold + data$ngreatauntsstatus212mold) / (data$nsiblings12mold + data$naunts12mold + data$ngreataunts12mold)
data$prophorizontalrelsstatus212mold_cat <- cut(data$prophorizontalrelsstatus212mold, breaks = 3)
levels(data$prophorizontalrelsstatus212mold_cat) <- c(levels(data$prophorizontalrelsstatus212mold_cat), "Unknown")
data$prophorizontalrelsstatus212mold_cat[which(is.na(data$prophorizontalrelsstatus212mold_cat))] <- "Unknown"

data$prophorizontalrelsstatus312mold <- (data$nsiblingsstatus312mold + data$nauntsstatus312mold + data$ngreatauntsstatus312mold) / (data$nsiblings12mold + data$naunts12mold + data$ngreataunts12mold)
data$prophorizontalrelsstatus312mold_cat <- cut(data$prophorizontalrelsstatus312mold, breaks = 3)
levels(data$prophorizontalrelsstatus312mold_cat) <- c(levels(data$prophorizontalrelsstatus312mold_cat), "Unknown")
data$prophorizontalrelsstatus312mold_cat[which(is.na(data$prophorizontalrelsstatus312mold_cat))] <- "Unknown"

data$prophorizontalrelsstatus2birth <- (data$nsiblingsstatus2birth + data$nauntsstatus2birth + data$ngreatauntsstatus2birth) / (data$nsiblingsbirth + data$nauntsbirth + data$ngreatauntsbirth)
data$prophorizontalrelsstatus2birth_cat <- cut(data$prophorizontalrelsstatus2birth, breaks = 3)
levels(data$prophorizontalrelsstatus2birth_cat) <- c(levels(data$prophorizontalrelsstatus2birth_cat), "Unknown")
data$prophorizontalrelsstatus2birth_cat[which(is.na(data$prophorizontalrelsstatus2birth_cat))] <- "Unknown"

data$prophorizontalrelsstatus3birth <- (data$nsiblingsstatus3birth + data$nauntsstatus3birth + data$ngreatauntsstatus3birth) / (data$nsiblingsbirth + data$nauntsbirth + data$ngreatauntsbirth)
data$prophorizontalrelsstatus3birth_cat <- cut(data$prophorizontalrelsstatus3birth, breaks = 3)
levels(data$prophorizontalrelsstatus3birth_cat) <- c(levels(data$prophorizontalrelsstatus3birth_cat), "Unknown")
data$prophorizontalrelsstatus3birth_cat[which(is.na(data$prophorizontalrelsstatus3birth_cat))] <- "Unknown"

data$prophorizontalrelsstatus2crt <- (data$nsiblingsstatus2crt + data$nauntsstatus2crt + data$ngreatauntsstatus2crt) / (data$nsiblingscrt + data$nauntscrt + data$ngreatauntscrt)
data$prophorizontalrelsstatus2crt_cat <- cut(data$prophorizontalrelsstatus2crt, breaks = 3)
levels(data$prophorizontalrelsstatus2crt_cat) <- c(levels(data$prophorizontalrelsstatus2crt_cat), "Unknown")
data$prophorizontalrelsstatus2crt_cat[which(is.na(data$prophorizontalrelsstatus2crt_cat))] <- "Unknown"

data$prophorizontalrelsstatus3crt <- (data$nsiblingsstatus3crt + data$nauntsstatus3crt + data$ngreatauntsstatus3crt) / (data$nsiblingscrt + data$nauntscrt + data$ngreatauntscrt)
data$prophorizontalrelsstatus3crt_cat <- cut(data$prophorizontalrelsstatus3crt, breaks = 3)
levels(data$prophorizontalrelsstatus3crt_cat) <- c(levels(data$prophorizontalrelsstatus3crt_cat), "Unknown")
data$prophorizontalrelsstatus3crt_cat[which(is.na(data$prophorizontalrelsstatus3crt_cat))] <- "Unknown"

data$prophorizontalrelsstatus2birth_recat <- cut(data$prophorizontalrelsstatus2birth, breaks = c(-0.000001,0.1,0.3,0.5,1.00001))
levels(data$prophorizontalrelsstatus2birth_recat) <- c(levels(data$prophorizontalrelsstatus2birth_recat), "Unknown")
data$prophorizontalrelsstatus2birth_recat[is.na(data$prophorizontalrelsstatus2birth_recat)] <- "Unknown"

data$prophorizontalrelsstatus3birth_recat <- cut(data$prophorizontalrelsstatus3birth, breaks = c(-0.000001,0.1,0.3,0.5,1.00001))
levels(data$prophorizontalrelsstatus3birth_recat) <- c(levels(data$prophorizontalrelsstatus3birth_recat), "Unknown")
data$prophorizontalrelsstatus3birth_recat[is.na(data$prophorizontalrelsstatus3birth_recat)] <- "Unknown"

data$prophorizontalrelsstatus2crt_recat <- data$prophorizontalrelsstatus2crt_cat
levels(data$prophorizontalrelsstatus2crt_recat) <- c(levels(data$prophorizontalrelsstatus2crt_recat), "(0.333,1]")
data$prophorizontalrelsstatus2crt_recat[data$prophorizontalrelsstatus2crt_recat == "(0.333,0.667]" |
                                                          data$prophorizontalrelsstatus2crt_recat == "(0.667,1]"] <- "(0.333,1]"
data$prophorizontalrelsstatus2crt_recat <- droplevels(data$prophorizontalrelsstatus2crt_recat, c("(0.333,0.667]", "(0.667,1]"))

data$prophorizontalrelsstatus3crt_recat <- cut(data$prophorizontalrelsstatus3crt, breaks = c(-0.0000001,0.5,1))
levels(data$prophorizontalrelsstatus3crt_recat) <- c(levels(data$prophorizontalrelsstatus3crt_recat), "Unknown")
data$prophorizontalrelsstatus3crt_recat[is.na(data$prophorizontalrelsstatus3crt_recat)] <- "Unknown"

data$prophorizontalrelsstatus2birth[is.na(data$prophorizontalrelsstatus2birth)] <- 0
data$prophorizontalrelsstatus212mold[is.na(data$prophorizontalrelsstatus212mold)] <- 0
data$prophorizontalrelsstatus2crt[is.na(data$prophorizontalrelsstatus2crt)] <- 0

data$prophorizontalrelsstatus3birth[is.na(data$prophorizontalrelsstatus3birth)] <- 0
data$prophorizontalrelsstatus312mold[is.na(data$prophorizontalrelsstatus312mold)] <- 0
data$prophorizontalrelsstatus3crt[is.na(data$prophorizontalrelsstatus3crt)] <- 0

data$damntestsbirth[data$damstatusbirth_cat == "Missing"] <- 0
data$damntests12mold[data$damstatus12mold_cat == "Missing"] <- 0
data$damntestscrt[data$damcrtstatus_cat == "Missing"] <- 0

data$granddamntestsbirth[data$granddamstatusbirth_cat == "Missing"] <- 0
data$granddamntests12mold[data$granddamstatus12mold_cat == "Missing"] <- 0
data$granddamntestscrt[data$granddamcrtstatus_cat == "Missing"] <- 0

data$meantitrenegcowsavg_recat <- cut(data$meantitrenegcowsavg, breaks = c(-0.00001, 2,4, 50))
levels(data$meantitrenegcowsavg_recat) <- c(levels(data$meantitrenegcowsavg_recat), "Missing")
data$meantitrenegcowsavg_recat[is.na(data$meantitrenegcowsavg_recat)] <- "Missing"

####DISPLAY DATASET SIZE####

print(paste("Full dataset (after cleaning):",dim(data)[1],"rows"))

####REARRANGE COLUMNS####

data <- data[,c("Farm",
                  "calfeartag",
                "calfdob",
                  "damparity",
                  "damparity_cat",
                "dameartag",
                  "damstatusbirth",
                  "damstatusbirth_cat",
                "damntestsbirth",
                "granddameartag",
                  "granddamstatusbirth",
                  "granddamstatusbirth_cat",
                "granddamntestsbirth",
                  "nsiblingsbirth",
                  "nsiblingsstatus2birth",
                  "propsiblingsstatus2birth",
                "propsiblingsstatus2birth_cat",
                  "nsiblingsstatus3birth",
                  "propsiblingsstatus3birth",
                "propsiblingsstatus3birth_cat",
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
                "prophorizontalrelsstatus2birth",
                "prophorizontalrelsstatus2birth_cat",
                "prophorizontalrelsstatus2birth_recat",
                "prophorizontalrelsstatus3birth",
                "prophorizontalrelsstatus3birth_cat",
                "prophorizontalrelsstatus3birth_recat",
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
                "damstatus12mold",
                "damstatus12mold_cat",
                "damntests12mold",
                "granddamstatus12mold",
                "granddamstatus12mold_cat",
                "granddamntests12mold",
                "nsiblings12mold",
                "nsiblingsstatus212mold",
                "propsiblingsstatus212mold",
                "propsiblingsstatus212mold_cat",
                "nsiblingsstatus312mold",
                "propsiblingsstatus312mold",
                "propsiblingsstatus312mold_cat",
                "naunts12mold",
                "nauntsstatus212mold",
                "propauntsstatus212mold",
                "nauntsstatus312mold",
                "propauntsstatus312mold",
                "ngreataunts12mold",
                "ngreatauntsstatus212mold",
                "propgreatauntsstatus212mold",
                "ngreatauntsstatus312mold",
                "propgreatauntsstatus312mold",
                "prophorizontalrelsstatus212mold",
                "prophorizontalrelsstatus212mold_cat",
                "prophorizontalrelsstatus312mold",
                "prophorizontalrelsstatus312mold_cat",
                "nproximaldams",
                "nproximaldamsstatus212mold",
                "propproximaldamsstatus212mold",
                "nproximaldamsstatus312mold",
                "propproximaldamsstatus312mold",
                "nvaguelyproximaldams",
                "nvaguelyproximaldamsstatus212mold",
                "propvaguelyproximaldamsstatus212mold",
                "nvaguelyproximaldamsstatus312mold",
                "propvaguelyproximaldamsstatus312mold",
                "damcrtstatus",
                  "damcrtstatus_cat",
                "damntestscrt",
                  "granddamcrtstatus",
                  "granddamcrtstatus_cat",
                "granddamntestscrt",
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
                "prophorizontalrelsstatus2crt_recat",
                  "prophorizontalrelsstatus3crt",
                  "prophorizontalrelsstatus3crt_cat",
                "prophorizontalrelsstatus3crt_recat",
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
                "VLProfile",
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
                "ageatfirstHH",
                "ageatfirstHMH",
                "ageatfirstHLH",
                "posstatusanypoint",
                "ageatfirstposstatus",
                "finaljohnesstatus",
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
                "covsandtitre24",
                "covsandtitre25",
                "covsandtitre26",
                "covsandtitre27",
                "covsandtitre28",
                "covsandtitre29",
                "covsandtitre30",
                "covsandtitre31",
                "covsandtitre32",
                "covsandtitre33",
                "covsandtitre34",
                "covsandtitre35",
                "covsandtitre36",
                "covsandtitre37")]
              
####CREATE LABEL####

#####TARGET_MEYER#####

data$Target_Meyer <- ifelse(data$ntests >= 9 & grepl("H", substr(data$profile, nchar(data$profile)-7, nchar(data$profile))) == FALSE, "0", "U")
data$Target_Meyer <- ifelse(data$ntests >= 3 &  substr(data$profile, nchar(data$profile) - 1, nchar(data$profile)) == "HH", "1", data$Target_Meyer)
data$Target_Meyer <- as.factor(data$Target_Meyer)

#####TARGET_QMMS#####

data$Target_QMMS <- "U"

data$Target_QMMS <- ifelse(grepl("HH", data$profile) == TRUE |
                                       grepl("HMH", data$profile) == TRUE |
                                       grepl("HLH", data$profile) == TRUE,
                                     "1",
                                     ifelse(data$ntests >= 9 &
                                              substr(data$profile, nchar(data$profile) - 7, nchar(data$profile)) == "LLLLLLLL", "0",
                                            data$Target_QMMS))

#####TARGET_ALTDEF1#####

data$Target_altdef1 <- "U"

data$Target_altdef1 <- as.factor(ifelse(grepl("HH", data$profile) == TRUE |
                                          grepl("HMH", data$profile) == TRUE |
                                          grepl("HLH", data$profile) == TRUE,
                                        "1",
                                        ifelse(data$ntests >= 3 &
                                                 substr(data$profile, nchar(data$profile) - 2, nchar(data$profile)) == "LLL" &
                                                 (grepl("M", data$profile) == TRUE | grepl("H", data$profile) == TRUE), "0",
                                               data$Target_altdef1)))

#####TARGET_STRICTNEG#####

data$Target_QMMS_strictneg <- "U"

data$Target_QMMS_strictneg <- ifelse(grepl("HH", data$profile) == TRUE |
                                       grepl("HMH", data$profile) == TRUE |
                                       grepl("HLH", data$profile) == TRUE,
                                     "1",
                                     ifelse(data$ntests >= 9 &
                                              substr(data$VLProfile, nchar(data$VLProfile) - 7, nchar(data$VLProfile)) == "VVVVVVVV", "0",
                                            data$Target_QMMS_strictneg))

####CREATE MODELLING DATA####

print("Creating birth model data...")

data_modelling <- data[,c(1:141,197,198,199,200)]


if(target == "QMMS"){

  data_modelling <- data_modelling[data_modelling$Target_QMMS != "U",]

  data_modelling$Target_QMMS <- as.factor(data_modelling$Target_QMMS)

}

if(target == "QMMS_STRICTNEG"){

  data_modelling <- data_modelling[data_modelling$Target_QMMS_strictneg != "U",]
  
  data_modelling$Target_QMMS_strictneg <- as.factor(data_modelling$Target_QMMS_strictneg)
  
}

if(target == "altdef1"){
  
  data_modelling <- data_modelling[data_modelling$Target_altdef1 != "U",]
  
  data_modelling$Target_altdef1 <- as.factor(data_modelling$Target_altdef1)
  
}

#####GATHER LIKELIHOODS AND POSTERIORS OF RELATIVES#####

#data_posteriors <- read.csv("y:/ian/johnesthresholds/johnesproper/data/data_posteriors.csv")

#data_posteriors <- data_posteriors[,c(2,3,160,161,170,182,185)]

#no_cores <- detectCores() 
#cl <- makePSOCKcluster(no_cores)
#registerDoParallel(cl)
#getDoParWorkers()

#ancestorpp <- foreach(i = 1:nrow(data), .combine = "rbind") %dopar% {
#  tmpdata <- data_posteriors[data_posteriors$Farm == data$Farm[i] & 
#                               data_posteriors$calfeartag == data$dameartag[i],]
#  tmpdata$date <- as.Date(tmpdata$date)
#  calfeartag <- data$calfeartag[i]
#  Farm <- data$Farm[i]
#  if(nrow(tmpdata) != 0){
#    damppcrt <- tmpdata$PosteriorProb[nrow(tmpdata)]
 #   damtitrecrt <- tmpdata$titre[nrow(tmpdata)]
#    damlikecrt <- tmpdata$likelihood[nrow(tmpdata)]}
#  if(nrow(tmpdata) == 0){
#    damppcrt <- ""
#    damtitrecrt <- ""
#    damlikecrt <- ""}
#  tmpdata$daterelcalfdob <- difftime(tmpdata$date, data$calfdob[i])
#  tmpdata <- tmpdata[tmpdata$daterelcalfdob <= 365,]
#  if(nrow(tmpdata) != 0){
#    dampp12mold <- tmpdata$PosteriorProb[nrow(tmpdata)]
#    damtitre12mold <- tmpdata$titre[nrow(tmpdata)]
#    damlike12mold <- tmpdata$likelihood[nrow(tmpdata)]}
#  if(nrow(tmpdata) == 0){
#    dampp12mold <- ""
#    damtitre12mold <- ""
#    damlike12mold <- ""}
#  tmpdata <- tmpdata[tmpdata$daterelcalfdob <= 0,]
#  if(nrow(tmpdata) != 0){
#  damppbirth <- tmpdata$PosteriorProb[nrow(tmpdata)]
#    damtitrebirth <- tmpdata$titre[nrow(tmpdata)]
#    damlikebirth <- tmpdata$likelihood[nrow(tmpdata)]}
#  if(nrow(tmpdata) == 0){
#    damppbirth <- ""
#    damtitrebirth <- ""
#    damlikebirth <- ""}
#  tmpdata <- data_posteriors[data_posteriors$calfeartag == data$granddameartag[i],]
#  tmpdata$date <- as.Date(tmpdata$date)
#  if(nrow(tmpdata) != 0){
#    granddamppcrt <- tmpdata$PosteriorProb[nrow(tmpdata)]
#    granddamtitrecrt <- tmpdata$titre[nrow(tmpdata)]
#    granddamlikecrt <- tmpdata$likelihood[nrow(tmpdata)]}
#  if(nrow(tmpdata) == 0){
#    granddamppcrt <- ""
#    granddamtitrecrt <- ""
#    granddamlikecrt <- ""}
#  tmpdata$daterelcalfdob <- difftime(tmpdata$date, data$calfdob[i])
#  tmpdata <- tmpdata[tmpdata$daterelcalfdob <= 365,]
#  if(nrow(tmpdata) != 0){
#    granddampp12mold <- tmpdata$PosteriorProb[nrow(tmpdata)]
#    granddamtitre12mold <- tmpdata$titre[nrow(tmpdata)]
#    granddamlike12mold <- tmpdata$likelihood[nrow(tmpdata)]}
#  if(nrow(tmpdata) == 0){
#    granddampp12mold <- ""
#    granddamtitre12mold <- ""
#    granddamlike12mold <- ""}
#  tmpdata <- tmpdata[tmpdata$daterelcalfdob <= 0,]
#  if(nrow(tmpdata) != 0){
#    granddamppbirth <- tmpdata$PosteriorProb[nrow(tmpdata)]
#    granddamtitrebirth <- tmpdata$titre[nrow(tmpdata)]
#    granddamlikebirth <- tmpdata$likelihood[nrow(tmpdata)]}
#  if(nrow(tmpdata) == 0){
#    granddamppbirth <- ""
#    granddamtitrebirth <- ""
#    granddamlikebirth <- ""}
#  c(Farm, 
#    calfeartag, 
#    damppbirth, 
#    dampp12mold, 
#    damppcrt, 
#    granddamppbirth, 
#    granddampp12mold, 
#    granddamppcrt, 
#    damtitrebirth, 
#    damtitre12mold, 
#    damtitrecrt, 
#    granddamtitrebirth, 
#    granddamtitre12mold, 
#    granddamtitrecrt,
#    damlikebirth,
#    damlike12mold,
#    damlikecrt,
#    granddamlikebirth,
#    granddamlike12mold,
#    granddamlikecrt)
#}

#stopCluster(cl)

#colnames(ancestorpp) <- c("Farm",
#                          "calfeartag",
#                          "damppbirth", 
#                          "dampp12mold", 
#                          "damppcrt", 
#                          "granddamppbirth", 
#                          "granddampp12mold", 
#                          "granddamppcrt",
#                          "damtitrebirth", 
#                          "damtitre12mold", 
#                          "damtitrecrt", 
#                          "granddamtitrebirth", 
#                          "granddamtitre12mold", 
#                          "granddamtitrecrt",
#                          "damlikebirth",
#                          "damlike12mold",
#                          "damlikecrt",
#                          "granddamlikebirth",
#                          "granddamlike12mold",
#                          "granddamlikecrt")
#
#data_modelling <- merge(data_modelling, ancestorpp, by = c("Farm", "calfeartag"), all.x = TRUE)
#
#data_modelling <- data_modelling[,c(1:9,145,151,157,10:13,148,154,160,14:49,146,152,158,50:52,149,155,161,53:86,147,153,159,87:89,150,156,162,90:144)]

#####SPLIT TRAIN/TEST#####

set.seed(1981)

uniquefarms <- unique(data_modelling$Farm)

trainfarms <- sample(uniquefarms, 63, replace = FALSE)
testfarms <- uniquefarms[!(uniquefarms %in% trainfarms)]

data_modelling_train <- data_modelling[data_modelling$Farm %in% trainfarms,]
data_modelling_test <- data_modelling[data_modelling$Farm %in% testfarms,]

cat(paste0("Trainset: \n",dim(data_modelling_train)[1],
           " rows (",round(dim(data_modelling_train)[1]/(dim(data_modelling_test)[1] + dim(data_modelling_train)[1])*100,1),"%)"),
    "\nPositive Target:", summary(as.factor(data_modelling_train$Target_altdef1))[[2]], "rows",
    "(", round(summary(as.factor(data_modelling_train$Target_altdef1))[[2]]/nrow(data_modelling_train)*100,1),"%)")

cat(paste0("testset: \n",dim(data_modelling_test)[1],
           " rows (",round(dim(data_modelling_test)[1]/(dim(data_modelling_train)[1] + dim(data_modelling_test)[1])*100,1),"%)"),
    "\nPositive Target:", summary(as.factor(data_modelling_test$Target_altdef1))[[2]], "rows",
    "(", round(summary(as.factor(data_modelling_test$Target_altdef1))[[2]]/nrow(data_modelling_test)*100,1),"%)")

#####CATEGORISE DATA FEATURES######

######TRAINSET######

#data_modelling_train$damppbirth_cat <- cut(as.numeric(data_modelling_train$damppbirth), breaks = 4)
#levels(data_modelling_train$damppbirth_cat) <- c(levels(data_modelling_train$damppbirth_cat), "Missing")
#data_modelling_train$damppbirth_cat[is.na(data_modelling_train$damppbirth_cat)] <- "Missing"

#data_modelling_train$damtitrebirth_cat <- cut(as.numeric(data_modelling_train$damtitrebirth), breaks = 5)
#levels(data_modelling_train$damtitrebirth_cat) <- c(levels(data_modelling_train$damtitrebirth_cat), "Missing")
#data_modelling_train$damtitrebirth_cat[is.na(data_modelling_train$damtitrebirth_cat)] <- "Missing"

#data_modelling_train$damlikebirth_cat <- cut(as.numeric(data_modelling_train$damlikebirth), breaks = c(0,0.5,1,10,100,1000,1000000 ))
#levels(data_modelling_train$damlikebirth_cat) <- c(levels(data_modelling_train$damlikebirth_cat), "Missing")
#data_modelling_train$damlikebirth_cat[is.na(data_modelling_train$damlikebirth_cat)] <- "Missing"

#data_modelling_train$damlikebirth_recat <- cut(as.numeric(data_modelling_train$damlikebirth), breaks = c(0,0.5,1,1000000 ))
#levels(data_modelling_train$damlikebirth_recat) <- c(levels(data_modelling_train$damlikebirth_recat), "Missing")
#data_modelling_train$damlikebirth_recat[is.na(data_modelling_train$damlikebirth_recat)] <- "Missing"

#data_modelling_train$granddamppbirth_cat <- cut(as.numeric(data_modelling_train$granddamppbirth), breaks = 4)
#levels(data_modelling_train$granddamppbirth_cat) <- c(levels(data_modelling_train$granddamppbirth_cat), "Missing")
#data_modelling_train$granddamppbirth_cat[is.na(data_modelling_train$granddamppbirth_cat)] <- "Missing"

#data_modelling_train$granddamlikebirth_cat <- cut(as.numeric(data_modelling_train$granddamlikebirth), breaks = c(0,0.5,1,10,100,1000,1000000 ))
#levels(data_modelling_train$granddamlikebirth_cat) <- c(levels(data_modelling_train$granddamlikebirth_cat), "Missing")
#data_modelling_train$granddamlikebirth_cat[is.na(data_modelling_train$granddamlikebirth_cat)] <- "Missing"

#data_modelling_train$damtitre12mold_cat <- cut(as.numeric(data_modelling_train$damtitre12mold), 
#                                               breaks = c(-0.00001, 48.6, 97.2, 146, 194, 1000))
#levels(data_modelling_train$damtitre12mold_cat) <- c(levels(data_modelling_train$damtitre12mold_cat), "Missing")
#data_modelling_train$damtitre12mold_cat[is.na(data_modelling_train$damtitre12mold_cat)] <- "Missing"
#data_modelling_train$damtitre12mold_cat <- droplevels(data_modelling_train$damtitre12mold_cat)

#data_modelling_train$damlike12mold_cat <- cut(as.numeric(data_modelling_train$damlike12mold), 
#                                               breaks = c(-0.00001, 1, 1000,2000,3500,10000))
#levels(data_modelling_train$damlike12mold_cat) <- c(levels(data_modelling_train$damlike12mold_cat), "Missing")
#data_modelling_train$damlike12mold_cat[is.na(data_modelling_train$damlike12mold_cat)] <- "Missing"
#data_modelling_train$damlike12mold_cat <- droplevels(data_modelling_train$damlike12mold_cat)

#data_modelling_train$dampp12mold_cat <- cut(as.numeric(data_modelling_train$dampp12mold), 
#                                               breaks = c(-0.00001, 0.1, 0.2, 0.5, 1.01))
#levels(data_modelling_train$dampp12mold_cat) <- c(levels(data_modelling_train$dampp12mold_cat), "Missing")
#data_modelling_train$dampp12mold_cat[is.na(data_modelling_train$dampp12mold_cat)] <- "Missing"
#data_modelling_train$dampp12mold_cat <- droplevels(data_modelling_train$dampp12mold_cat)

#data_modelling_train$damtitrecrt_cat <- cut(as.numeric(data_modelling_train$damtitrecrt),
#                                            breaks = c(-0.00001,68,135,203,10000))
#levels(data_modelling_train$damtitrecrt_cat) <- c(levels(data_modelling_train$damtitrecrt_cat), "Missing")
#data_modelling_train$damtitrecrt_cat[is.na(data_modelling_train$damtitrecrt_cat)] <- "Missing"
#data_modelling_train$damtitrecrt_cat <- droplevels(data_modelling_train$damtitrecrt_cat)

#data_modelling_train$damlikecrt_cat <- cut(as.numeric(data_modelling_train$damlikecrt),
#                                            breaks = c(-0.0001,1,1500,3000,10000))
#levels(data_modelling_train$damlikecrt_cat) <- c(levels(data_modelling_train$damlikecrt_cat), "Missing")
#data_modelling_train$damlikecrt_cat[is.na(data_modelling_train$damlikecrt_cat)] <- "Missing"
#data_modelling_train$damlikecrt_cat <- droplevels(data_modelling_train$damlikecrt_cat)

#data_modelling_train$damppcrt_cat <- cut(as.numeric(data_modelling_train$damppcrt),
#                                                    breaks = c(-0.0001,0.25,0.5,0.75,10000))
#levels(data_modelling_train$damppcrt_cat) <- c(levels(data_modelling_train$damppcrt_cat), "Missing")
#data_modelling_train$damppcrt_cat[is.na(data_modelling_train$damppcrt_cat)] <- "Missing"
#data_modelling_train$damppcrt_cat <- droplevels(data_modelling_train$damppcrt_cat)

######TESTSET######

#data_modelling_test$damppbirth_cat <- cut(as.numeric(data_modelling_test$damppbirth), breaks = 4)
#levels(data_modelling_test$damppbirth_cat) <- c(levels(data_modelling_test$damppbirth_cat), "Missing")
#data_modelling_test$damppbirth_cat[is.na(data_modelling_test$damppbirth_cat)] <- "Missing"
#
#data_modelling_test$damtitrebirth_cat <- cut(as.numeric(data_modelling_test$damtitrebirth), breaks = 5)
#levels(data_modelling_test$damtitrebirth_cat) <- c(levels(data_modelling_test$damtitrebirth_cat), "Missing")
#data_modelling_test$damtitrebirth_cat[is.na(data_modelling_test$damtitrebirth_cat)] <- "Missing"
#
#data_modelling_test$damlikebirth_cat <- cut(as.numeric(data_modelling_test$damlikebirth), breaks = c(0,0.5,1,10,100,1000,1000000 ))
#levels(data_modelling_test$damlikebirth_cat) <- c(levels(data_modelling_test$damlikebirth_cat), "Missing")
#data_modelling_test$damlikebirth_cat[is.na(data_modelling_test$damlikebirth_cat)] <- "Missing"
#
#data_modelling_test$damlikebirth_recat <- cut(as.numeric(data_modelling_test$damlikebirth), breaks = c(0,0.5,1,1000000 ))
#levels(data_modelling_test$damlikebirth_recat) <- c(levels(data_modelling_test$damlikebirth_recat), "Missing")
#data_modelling_test$damlikebirth_recat[is.na(data_modelling_test$damlikebirth_recat)] <- "Missing"
#
#data_modelling_test$granddamppbirth_cat <- cut(as.numeric(data_modelling_test$granddamppbirth), breaks = 4)
#levels(data_modelling_test$granddamppbirth_cat) <- c(levels(data_modelling_test$granddamppbirth_cat), "Missing")
#data_modelling_test$granddamppbirth_cat[is.na(data_modelling_test$granddamppbirth_cat)] <- "Missing"
#
#data_modelling_test$granddamlikebirth_cat <- cut(as.numeric(data_modelling_test$granddamlikebirth), breaks = c(0,0.5,1,10,100,1000,1000000 ))
#levels(data_modelling_test$granddamlikebirth_cat) <- c(levels(data_modelling_test$granddamlikebirth_cat), "Missing")
#data_modelling_test$granddamlikebirth_cat[is.na(data_modelling_test$granddamlikebirth_cat)] <- "Missing"
#
#data_modelling_test$damtitre12mold_cat <- cut(as.numeric(data_modelling_test$damtitre12mold), 
#                                               breaks = c(-0.00001, 48.6, 97.2, 146, 194, 1000))
#levels(data_modelling_test$damtitre12mold_cat) <- c(levels(data_modelling_test$damtitre12mold_cat), "Missing")
#data_modelling_test$damtitre12mold_cat[is.na(data_modelling_test$damtitre12mold_cat)] <- "Missing"
#data_modelling_test$damtitre12mold_cat <- droplevels(data_modelling_test$damtitre12mold_cat)
#
#data_modelling_test$dampp12mold_cat <- cut(as.numeric(data_modelling_test$dampp12mold), 
#                                            breaks = c(-0.00001, 0.1, 0.2, 0.5, 1.01))
#levels(data_modelling_test$dampp12mold_cat) <- c(levels(data_modelling_test$dampp12mold_cat), "Missing")
#data_modelling_test$dampp12mold_cat[is.na(data_modelling_test$dampp12mold_cat)] <- "Missing"
#data_modelling_test$dampp12mold_cat <- droplevels(data_modelling_test$dampp12mold_cat)

#data_modelling_test$damtitrecrt_cat <- cut(as.numeric(data_modelling_test$damtitrecrt),
#                                            breaks = c(-0.00001,68,135,203,10000))
#levels(data_modelling_test$damtitrecrt_cat) <- c(levels(data_modelling_test$damtitrecrt_cat), "Missing")
#data_modelling_test$damtitrecrt_cat[is.na(data_modelling_test$damtitrecrt_cat)] <- "Missing"
#data_modelling_test$damtitrecrt_cat <- droplevels(data_modelling_test$damtitrecrt_cat)
#data_modelling_test$damlikecrt_cat <- cut(as.numeric(data_modelling_test$damlikecrt),
#                                           breaks = c(-0.0001,1,1500,3000,10000))
#levels(data_modelling_test$damlikecrt_cat) <- c(levels(data_modelling_test$damlikecrt_cat), "Missing")
#data_modelling_test$damlikecrt_cat[is.na(data_modelling_test$damlikecrt_cat)] <- "Missing"
#data_modelling_test$damlikecrt_cat <- droplevels(data_modelling_test$damlikecrt_cat)

#data_modelling_test$damppcrt_cat <- cut(as.numeric(data_modelling_test$damppcrt),
#                                         breaks = c(-0.0001,0.25,0.5,0.75,10000))
#levels(data_modelling_test$damppcrt_cat) <- c(levels(data_modelling_test$damppcrt_cat), "Missing")
#data_modelling_test$damppcrt_cat[is.na(data_modelling_test$damppcrt_cat)] <- "Missing"
#data_modelling_test$damppcrt_cat <- droplevels(data_modelling_test$damppcrt_cat)

####TRAIN MODELS####

#####MODELS AT TIME OF BIRTH#####

#Models without dam posteriors

######priorglmm_birth_1######

priorglmm_birth_1 <- glmer(Target_altdef1 ~ damstatusbirth_cat +
                             (1|Farm),
                           family = "binomial",
                           data = data_modelling_train,
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl=list(maxfun=100000)))

######priorglmm_birth_2######

priorglmm_birth_2 <- glmer(Target_altdef1 ~ damstatusbirth_cat +
                             granddamstatusbirth_cat +
                             (1|Farm),
                           family = "binomial",
                           data = data_modelling_train,
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl=list(maxfun=100000)))


ggplot(data_modelling_train, aes(x = prophorizontalrelsstatus2birth, y = as.numeric(Target_altdef1))) +
  geom_point() +
  geom_smooth()

######priorglmm_birth_3######

priorglmm_birth_3 <- glmer(Target_altdef1 ~ damstatusbirth_cat +S
                             granddamstatusbirth_cat +
                             prophorizontalrelsstatus2birth_cat +
                             
                             (1|Farm),
                           family = "binomial",
                           data = data_modelling_train,
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl=list(maxfun=100000)))
ggplot(data_modelling_train, aes(x = prophorizontalrelsstatus3birth, y = as.numeric(Target_altdef1))) +
  geom_point() +
  geom_smooth()

######priorglmm_birth_4######

priorglmm_birth_4 <- glmer(Target_altdef1 ~ damstatusbirth_cat +
                             granddamstatusbirth_cat +
                             prophorizontalrelsstatus2birth_cat +
                             prophorizontalrelsstatus3birth_cat +
                             
                             (1|Farm),
                           family = "binomial",
                           data = data_modelling_train,
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl=list(maxfun=100000)))

######priorglmm_birth_5######


priorglmm_birth_5 <- glmer(Target_altdef1 ~ damstatusbirth_cat +
                             granddamstatusbirth_cat +
                             prophorizontalrelsstatus2birth_recat +
                             prophorizontalrelsstatus3birth_recat +
                             
                             (1|Farm),
                           family = "binomial",
                           data = data_modelling_train,
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl=list(maxfun=100000)))

######priorglmm_birth_6######

priorglmm_birth_6 <- glmer(Target_altdef1 ~ damstatusbirth_cat * damntestsbirth +
                             granddamstatusbirth_cat *granddamntestsbirth +
                             prophorizontalrelsstatus2birth_recat +
                             prophorizontalrelsstatus3birth_recat +
                             
                             (1|Farm),
                           family = "binomial",
                           data = data_modelling_train,
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl=list(maxfun=100000)))
######priorglmm_birth_7######


priorglmm_birth_7 <- glmer(Target_altdef1 ~ damstatusbirth_cat * damntestsbirth +
                             granddamstatusbirth_cat *granddamntestsbirth +
                             prophorizontalrelsstatus2birth_recat +
                             prophorizontalrelsstatus3birth_recat +
                             propproximaldamsstatus2birth +
                             propproximaldamsstatus3birth +
                             (1|Farm),
                           family = "binomial",
                           data = data_modelling_train,
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl=list(maxfun=100000)))
######priorglmm_birth_8######

priorglmm_birth_8 <- glmer(Target_altdef1 ~ damstatusbirth_cat * damntestsbirth +
                             granddamstatusbirth_cat *granddamntestsbirth +
                             prophorizontalrelsstatus2birth_recat +
                             prophorizontalrelsstatus3birth_recat +
                             propvaguelyproximaldamsstatus2birth +
                             propvaguelyproximaldamsstatus3birth +
                             (1|Farm),
                           family = "binomial",
                           data = data_modelling_train,
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl=list(maxfun=100000)))

######priorglmm_birth_9######

priorglmm_birth_9 <- glmer(Target_altdef1 ~ damstatusbirth_cat * damntestsbirth +
                             granddamstatusbirth_cat *granddamntestsbirth +
                             prophorizontalrelsstatus2birth_recat +
                             prophorizontalrelsstatus3birth_recat +
                             propvaguelyproximaldamsstatus2birth +
                             propvaguelyproximaldamsstatus3birth +
                             propposavg_cat +
                             (1|Farm),
                           family = "binomial",
                           data = data_modelling_train,
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl=list(maxfun=100000)))
######priorglmm_birth_10######

priorglmm_birth_10 <- glmer(Target_altdef1 ~ damstatusbirth_cat * damntestsbirth +
                             granddamstatusbirth_cat *granddamntestsbirth +
                             prophorizontalrelsstatus2birth_recat +
                             prophorizontalrelsstatus3birth_recat +
                             propvaguelyproximaldamsstatus2birth +
                             propvaguelyproximaldamsstatus3birth +
                             meantitreavg_cat +
                             (1|Farm),
                           family = "binomial",
                           data = data_modelling_train,
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl=list(maxfun=100000)))
######priorglmm_birth_11######


priorglmm_birth_11 <- glmer(Target_altdef1 ~ damstatusbirth_cat * damntestsbirth +
                              granddamstatusbirth_cat *granddamntestsbirth +
                              prophorizontalrelsstatus2birth_recat +
                              prophorizontalrelsstatus3birth_recat +
                              propvaguelyproximaldamsstatus2birth +
                              propvaguelyproximaldamsstatus3birth +
                              meantitrenegcowsavg_cat +
                              (1|Farm),
                            family = "binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=100000)))



######priorglmm_birth_12######

priorglmm_birth_12 <- glmer(Target_altdef1 ~ damstatusbirth_cat * damntestsbirth +
                              granddamstatusbirth_cat *granddamntestsbirth +
                              prophorizontalrelsstatus2birth_recat +
                              prophorizontalrelsstatus3birth_recat +
                              propvaguelyproximaldamsstatus2birth +
                              propvaguelyproximaldamsstatus3birth +
                              meantitrenegcowsavg_recat +
                              (1|Farm),
                            family = "binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=100000)))


ggplot(data_modelling_train, aes(x = propgreatauntsstatus2birth, y = as.numeric(Target_altdef1))) +
  geom_point() +
  geom_smooth(method="gam", formula = y ~ s(x, bs = "cs", k=5))

######priorglmm_birth_13######

priorglmm_birth_13 <- glmer(Target_altdef1 ~ damstatusbirth_cat * damntestsbirth +
                              granddamstatusbirth_cat *granddamntestsbirth +
                              propsiblingsstatus2birth +
                              propvaguelyproximaldamsstatus2birth +
                              propvaguelyproximaldamsstatus3birth +
                              meantitrenegcowsavg_recat +
                              (1|Farm),
                            family = "binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=100000)))

######priorglmm_birth_14######

priorglmm_birth_14 <- glmer(Target_altdef1 ~ damstatusbirth_cat * damntestsbirth +
                              granddamstatusbirth_cat *granddamntestsbirth +
                              propsiblingsstatus2birth_cat +
                              propvaguelyproximaldamsstatus2birth +
                              propvaguelyproximaldamsstatus3birth +
                              meantitrenegcowsavg_recat +
                              (1|Farm),
                            family = "binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=100000)))

######priorglmm_birth_15######



priorglmm_birth_15 <- glmer(Target_altdef1 ~ damstatusbirth_cat * damntestsbirth +
                              granddamstatusbirth_cat *granddamntestsbirth +
                              propsiblingsstatus2birth +
                              propsiblingsstatus3birth +
                              propvaguelyproximaldamsstatus2birth +
                              propvaguelyproximaldamsstatus3birth +
                              meantitrenegcowsavg_recat +
                              (1|Farm),
                            family = "binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=100000)))



ggplot(data_modelling_train, aes(x = propgreatauntsstatus3birth, y = as.numeric(Target_altdef1))) +
  geom_point() +
  geom_smooth(method="gam", formula = y ~ s(x, bs = "cs", k=5))

######priorglmm_birth_16######

priorglmm_birth_16 <- glmer(Target_altdef1 ~ damstatusbirth_cat * damntestsbirth +
                              granddamstatusbirth_cat *granddamntestsbirth +
                              propsiblingsstatus2birth +
                              propsiblingsstatus3birth +
                              propauntsstatus2birth +
                              propvaguelyproximaldamsstatus2birth +
                              propvaguelyproximaldamsstatus3birth +
                              meantitrenegcowsavg_recat +
                              (1|Farm),
                            family = "binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=100000)))



######priorglmm_birth_17######


priorglmm_birth_17 <- glmer(Target_altdef1 ~ damstatusbirth_cat * damntestsbirth +
                              granddamstatusbirth_cat *granddamntestsbirth +
                              propsiblingsstatus2birth +
                              propsiblingsstatus3birth +
                              propauntsstatus3birth +
                              propvaguelyproximaldamsstatus2birth +
                              propvaguelyproximaldamsstatus3birth +
                              meantitrenegcowsavg_recat +
                              (1|Farm),
                            family = "binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=100000)))

######priorglmm_birth_18######

priorglmm_birth_18 <- glmer(Target_altdef1 ~ damstatusbirth_cat * damntestsbirth +
                              propsiblingsstatus2birth +
                              propsiblingsstatus3birth +
                              propvaguelyproximaldamsstatus2birth +
                              propvaguelyproximaldamsstatus3birth +
                              meantitrenegcowsavg_recat +
                              (1|Farm),
                            family = "binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=100000)))

######priorglmm_birth_19######

priorglmm_birth_19 <- glmer(Target_altdef1 ~ damstatusbirth_cat * damntestsbirth +
                              prophorizontalrelsstatus2birth +
                              prophorizontalrelsstatus3birth +
                              propvaguelyproximaldamsstatus2birth +
                              propvaguelyproximaldamsstatus3birth +
                              meantitrenegcowsavg_recat +
                              (1|Farm),
                            family = "binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=100000)))

######priorglmm_birth_20######

priorglmm_birth_20 <- glmer(Target_altdef1 ~ damstatusbirth_cat * damntestsbirth +
                              prophorizontalrelsstatus2birth +
                              propvaguelyproximaldamsstatus2birth +
                              propvaguelyproximaldamsstatus3birth +
                              meantitrenegcowsavg_recat +
                              (1|Farm),
                            family = "binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=100000)))

######priorglmm_birth_21######

priorglmm_birth_21 <- glmer(Target_altdef1 ~ damstatusbirth_cat * damntestsbirth +
                              prophorizontalrelsstatus2birth_cat +
                              propvaguelyproximaldamsstatus2birth +
                              propvaguelyproximaldamsstatus3birth +
                              meantitrenegcowsavg_recat +
                              (1|Farm),
                            family = "binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=100000)))


#Models with dam posteriors



######priorglmm_birth_22######

model.formula <- formula(Target_altdef1 ~
                           damppbirth_cat +
                           (1 | Farm))

priorglmm_birth_22 <- glmer(model.formula,
                            family = "binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               ptitle = "priorglmm_birth_22",
               psubtitle = "10:10 Cross Validation")

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_22.png")

data_modelling_test$pred <- predict(priorglmm_birth_22,
                                    newdata = data_modelling_test,
                                    type = "response",
                                    re.form = ~0)

CaliPlot(data_modelling_test$pred,
         data_modelling_test$Target_altdef1,
         ptitle = "priorglmm_birth_22",
         psubtitle = "External Testset")

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_22External.png")


######priorglmm_birth_23######

model.formula <- formula(Target_altdef1 ~
                           damtitrebirth_cat +
                           (1 | Farm))

priorglmm_birth_23 <- glmer(model.formula,
                            family = "binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               ptitle = "priorglmm_birth_23",
               psubtitle = "10:10 Cross Validation")


ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_23.png")

data_modelling_test$pred <- predict(priorglmm_birth_23,
                                    newdata = data_modelling_test,
                                    type = "response",
                                    re.form = ~0)

CaliPlot(data_modelling_test$pred,
         data_modelling_test$Target_altdef1,
         ptitle = "priorglmm_birth_23",
         psubtitle = "External Testset")

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_23External.png")

######priorglmm_birth_24######

model.formula <- formula(Target_altdef1 ~
                           damlikebirth_cat +
                           (1 | Farm))

priorglmm_birth_24 <- glmer(model.formula,
                            family = "binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               ptitle = "priorglmm_birth_24",
               psubtitle = "10:10 Cross Validation")


ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_24.png")

data_modelling_test$pred <- predict(priorglmm_birth_24,
                                    newdata = data_modelling_test,
                                    type = "response",
                                    re.form = ~0)

CaliPlot(data_modelling_test$pred,
         data_modelling_test$Target_altdef1,
         ptitle = "priorglmm_birth_24",
         psubtitle = "External Testset")

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_24External.png")

######priorglmm_birth_25######

model.formula <- formula(Target_altdef1 ~
                           damlikebirth_cat +
                           granddamppbirth_cat +
                           (1 | Farm))

priorglmm_birth_25 <- glmer(model.formula,
                            family = "binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               ptitle = "priorglmm_birth_25",
               psubtitle = "10:10 Cross Validation")


ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_25.png")

######priorglmm_birth_26######

model.formula <- formula(Target_altdef1 ~
                           damlikebirth_cat +
                           granddamlikebirth_cat +
                           (1 | Farm))

priorglmm_birth_26 <- glmer(model.formula,
                            family = "binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               ptitle = "priorglmm_birth_26",
               psubtitle = "10:10 Cross Validation")


ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_26.png")

######priorglmm_birth_27######

model.formula <- formula(Target_altdef1 ~
                           damlikebirth_cat +
                           damstatusbirth_cat +
                           (1 | Farm))

priorglmm_birth_27 <- glmer(model.formula,
                            family = "binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               ptitle = "priorglmm_birth_27",
               psubtitle = "10:10 Cross Validation")


ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_27.png")

######priorglmm_birth_28######

model.formula <- formula(Target_altdef1 ~
                           damstatusbirth_cat +
                           (1 | Farm))

priorglmm_birth_28 <- glmer(model.formula,
                            family = "binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               ptitle = "priorglmm_birth_28",
               psubtitle = "10:10 Cross Validation")


ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_28.png")

######priorglmm_birth_29######

model.formula <- formula(Target_altdef1 ~
                           damlikebirth_cat +
                           prophorizontalrelsstatus2birth_recat +
                           (1 | Farm))


priorglmm_birth_29 <- glmer(model.formula,
                            family = "binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               ptitle = "priorglmm_birth_29",
               psubtitle = "10:10 Cross Validation")


ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_29.png")

data_modelling_test$pred <- predict(priorglmm_birth_29,
                                    newdata = data_modelling_test,
                                    type = "response",
                                    re.form = ~0)

CaliPlot(data_modelling_test$pred,
         data_modelling_test$Target_altdef1,
         ptitle = "priorglmm_birth_29",
         psubtitle = "External Testset")

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_29External.png")


######priorglmm_birth_30######

model.formula <- formula(Target_altdef1 ~
                           damlikebirth_cat +
                           granddamstatusbirth_cat +
                           prophorizontalrelsstatus2birth_recat +
                           (1 | Farm))


priorglmm_birth_30 <- glmer(model.formula,
                            family = "binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               ptitle = "priorglmm_birth_30",
               psubtitle = "10:10 Cross Validation")


ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_30.png")

data_modelling_test$pred <- predict(priorglmm_birth_30,
                                    newdata = data_modelling_test,
                                    type = "response",
                                    re.form = ~0)

CaliPlot(data_modelling_test$pred,
         data_modelling_test$Target_altdef1,
         ptitle = "priorglmm_birth_30",
         psubtitle = "External Testset")

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_30External.png")



######priorglmm_birth_31######

model.formula <- formula(Target_altdef1 ~
                           damlikebirth_cat *
                           damntestsbirth +
                           granddamstatusbirth_cat +
                           prophorizontalrelsstatus2birth_recat +
                           (1 | Farm))


priorglmm_birth_31 <- glmer(model.formula,
                            family = "binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               ptitle = "priorglmm_birth_31",
               psubtitle = "10:10 Cross Validation")


ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_31.png")

data_modelling_test$pred <- predict(priorglmm_birth_31,
                                    newdata = data_modelling_test,
                                    type = "response",
                                    re.form = ~0)

CaliPlot(data_modelling_test$pred,
         data_modelling_test$Target_altdef1,
         ptitle = "priorglmm_birth_31",
         psubtitle = "External Testset")

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_31External.png")


######priorglmm_birth_32######

model.formula <- formula(Target_altdef1 ~
                           damlikebirth_recat +
                           granddamstatusbirth_cat +
                           prophorizontalrelsstatus2birth_recat +
                           (1 | Farm))


priorglmm_birth_32 <- glmer(model.formula,
                            family = "binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               ptitle = "priorglmm_birth_32",
               psubtitle = "10:10 Cross Validation")


ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_32.png")

data_modelling_test$pred <- predict(priorglmm_birth_32,
                                    newdata = data_modelling_test,
                                    type = "response",
                                    re.form = ~0)

CaliPlot(data_modelling_test$pred,
         data_modelling_test$Target_altdef1,
         ptitle = "priorglmm_birth_32",
         psubtitle = "External Testset")

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_32External.png")


######priorglmm_birth_33######

model.formula <- formula(Target_altdef1 ~
                           damlikebirth_recat +
                           granddamstatusbirth_cat +
                           prophorizontalrelsstatus2birth_recat +
                           prophorizontalrelsstatus3birth_recat +
                           (1 | Farm))


priorglmm_birth_33 <- glmer(model.formula,
                            family = "binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               ptitle = "priorglmm_birth_33",
               psubtitle = "10:10 Cross Validation")


ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_33.png")


data_modelling_test$pred <- predict(priorglmm_birth_33,
                                    newdata = data_modelling_test,
                                    type = "response",
                                    re.form = ~0)

CaliPlot(data_modelling_test$pred,
         data_modelling_test$Target_altdef1,
         ptitle = "priorglmm_birth_33",
         psubtitle = "External Testset")

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_33External.png")


######priorglmm_birth_34######

model.formula <- formula(Target_altdef1 ~
                           damlikebirth_recat +
                           granddamstatusbirth_cat +
                           prophorizontalrelsstatus2birth_recat +
                           prophorizontalrelsstatus3birth_recat +
                           propvaguelyproximaldamsstatus2birth +
                           (1 | Farm))


priorglmm_birth_34 <- glmer(model.formula,
                            family = "binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               ptitle = "priorglmm_birth_34",
               psubtitle = "10:10 Cross Validation")


ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_34.png")

data_modelling_test$pred <- predict(priorglmm_birth_34,
                                    newdata = data_modelling_test,
                                    type = "response",
                                    re.form = ~0)

CaliPlot(data_modelling_test$pred,
         data_modelling_test$Target_altdef1,
         ptitle = "priorglmm_birth_34",
         psubtitle = "External Testset")

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_34External.png")

######priorglmm_birth_35######

model.formula <- formula(Target_altdef1 ~
                           damlikebirth_recat +
                           granddamstatusbirth_cat +
                           prophorizontalrelsstatus2birth_recat +
                           prophorizontalrelsstatus3birth_recat +
                           propvaguelyproximaldamsstatus2birth +
                           propvaguelyproximaldamsstatus3birth +
                           (1 | Farm))


priorglmm_birth_35 <- glmer(model.formula,
                            family = "binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               ptitle = "priorglmm_birth_35",
               psubtitle = "10:10 Cross Validation")


ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_35.png")

data_modelling_test$pred <- predict(priorglmm_birth_35,
                                    newdata = data_modelling_test,
                                    type = "response",
                                    re.form = ~0)

CaliPlot(data_modelling_test$pred,
         data_modelling_test$Target_altdef1,
         ptitle = "priorglmm_birth_35",
         psubtitle = "External Testset")

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_35External.png")


######priorglmm_birth_36######

model.formula <- formula(Target_altdef1 ~
                           damlikebirth_cat +
                           granddamstatusbirth_cat +
                           prophorizontalrelsstatus2birth_recat +
                           propposavg_cat +
                           (1 | Farm))


priorglmm_birth_36 <- glmer(model.formula,
                            family = "binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               ptitle = "priorglmm_birth_36",
               psubtitle = "10:10 Cross Validation")


ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_36.png")

data_modelling_test$pred <- predict(priorglmm_birth_36,
                                    newdata = data_modelling_test,
                                    type = "response",
                                    re.form = ~0)

CaliPlot(data_modelling_test$pred,
         data_modelling_test$Target_altdef1,
         ptitle = "priorglmm_birth_36",
         psubtitle = "External Testset")

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_36External.png")


######priorglmm_birth_37######

model.formula <- formula(Target_altdef1 ~
                           damlikebirth_cat +
                           granddamstatusbirth_cat +
                           prophorizontalrelsstatus2birth_recat +
                          meantitreavg_cat +
                           (1 | Farm))


priorglmm_birth_37 <- glmer(model.formula,
                            family = "binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               ptitle = "priorglmm_birth_37",
               psubtitle = "10:10 Cross Validation")


ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_37.png")

data_modelling_test$pred <- predict(priorglmm_birth_37,
                                    newdata = data_modelling_test,
                                    type = "response",
                                    re.form = ~0)

CaliPlot(data_modelling_test$pred,
         data_modelling_test$Target_altdef1,
         ptitle = "priorglmm_birth_37",
         psubtitle = "External Testset")

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_37External.png")

######priorglmm_birth_38######

model.formula <- formula(Target_altdef1 ~
                           damlikebirth_cat +
                           granddamstatusbirth_cat +
                           prophorizontalrelsstatus2birth_recat +
                           meantitrenegcowsavg_recat +
                           (1 | Farm))


priorglmm_birth_38 <- glmer(model.formula,
                            family = "binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               ptitle = "priorglmm_birth_38",
               psubtitle = "10:10 Cross Validation")


ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_birth_38.png")



aicmods <- c("priorglmm_birth_1",
             "priorglmm_birth_2",
             "priorglmm_birth_3",
             "priorglmm_birth_4",
             "priorglmm_birth_5",
             "priorglmm_birth_6",
             "priorglmm_birth_7",
             "priorglmm_birth_8",
             "priorglmm_birth_9",
             "priorglmm_birth_10",
             "priorglmm_birth_11",
             "priorglmm_birth_12",
             "priorglmm_birth_13",
             "priorglmm_birth_14",
             "priorglmm_birth_15",
             "priorglmm_birth_16",
             "priorglmm_birth_17",
             "priorglmm_birth_18",
             "priorglmm_birth_19",
             "priorglmm_birth_20",
             "priorglmm_birth_21")

glmmaictable <- data.frame(Model = aicmods)
glmmaictable$AIC <- 0

for (i in 1:nrow(glmmaictable)){
  glmmaictable$AIC[i] <- AIC(get(glmmaictable$Model[i]))
  glmmaictable$features[i] <- toString(terms(get(glmmaictable$Model[i]))[[3]])
}

glmmaictable <- glmmaictable[order(glmmaictable$AIC),]

View(glmmaictable)




ggplot(data_modelling_train, aes(x = propvaguelyproximaldamsstatus3birth, y = as.numeric(Target_altdef1))) +
  geom_point() +
  geom_smooth()


#####Models 12m Old#####

######priorglmm_12mold_1######

priorglmm_12mold_1 <- glmer(Target_altdef1 ~ damstatus12mold_cat +
                              (1|Farm),
                            family ="binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl = list(maxfun = 100000)))

######priorglmm_12mold_2######

priorglmm_12mold_2 <- glmer(Target_altdef1 ~ damstatus12mold_cat * damntests12mold +
                              (1|Farm),
                            family ="binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl = list(maxfun = 100000)))

plot(ggpredict(priorglmm_12mold_2, terms = c("damntests12mold", "damstatus12mold_cat")))

######priorglmm_12mold_3######

priorglmm_12mold_3 <- glmer(Target_altdef1 ~ damstatus12mold_cat * damntests12mold +
                              granddamstatus12mold_cat +
                              (1|Farm),
                            family ="binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl = list(maxfun = 100000)))

######priorglmm_12mold_4######

priorglmm_12mold_4 <- glmer(Target_altdef1 ~ damstatus12mold_cat * damntests12mold +
                              granddamstatus12mold_cat * granddamntests12mold +
                              (1|Farm),
                            family ="binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl = list(maxfun = 100000)))

######priorglmm_12mold_5######

priorglmm_12mold_5 <- glmer(Target_altdef1 ~ damstatus12mold_cat * damntests12mold +
                              granddamstatus12mold_cat * granddamntests12mold +
                              prophorizontalrelsstatus212mold_cat +
                              prophorizontalrelsstatus312mold_cat +
                              (1|Farm),
                            family ="binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl = list(maxfun = 100000)))


######priorglmm_12mold_6######

model.formula <- formula(Target_altdef1 ~ damstatus12mold_cat * damntests12mold +
                           granddamstatus12mold_cat * granddamntests12mold +
                           prophorizontalrelsstatus212mold_cat +
                           (1|Farm))

priorglmm_12mold_6 <- glmer(model.formula,
                            family ="binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl = list(maxfun = 100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               ptitle = "priorglmm_12mold_6",
               psubtitle = "10:10 Cross Validation")


ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_12mold_6.png")



######priorglmm_12mold_6a######

model.formula = Target_altdef1 ~ damstatus12mold_cat * damntests12mold +
  granddamstatus12mold_cat * granddamntests12mold +
  prophorizontalrelsstatus212mold_cat +
  damtitre12mold_cat +
  (1|Farm)

priorglmm_12mold_6a <- glmer(model.formula,
                            family ="binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl = list(maxfun = 100000)))




parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               ptitle = "priorglmm_12mold_6a",
               psubtitle = "10:10 Cross Validation")


ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_12mold_6a.png")


######priorglmm_12mold_6b######

model.formula = Target_altdef1 ~ damstatus12mold_cat * damntests12mold +
  granddamstatus12mold_cat * granddamntests12mold +
  prophorizontalrelsstatus212mold_cat +
  damlike12mold_cat +
  (1|Farm)

priorglmm_12mold_6b <- glmer(model.formula,
                             family ="binomial",
                             data = data_modelling_train,
                             control = glmerControl(optimizer = "bobyqa",
                                                    optCtrl = list(maxfun = 100000)))




parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               ptitle = "priorglmm_12mold_6b",
               psubtitle = "10:10 Cross Validation")


ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_12mold_6b.png")



######priorglmm_12mold_6c######

model.formula = Target_altdef1 ~ damstatus12mold_cat * damntests12mold +
  granddamstatus12mold_cat * granddamntests12mold +
  prophorizontalrelsstatus212mold_cat +
  dampp12mold_cat +
  (1|Farm)

priorglmm_12mold_6c <- glmer(model.formula,
                             family ="binomial",
                             data = data_modelling_train,
                             control = glmerControl(optimizer = "bobyqa",
                                                    optCtrl = list(maxfun = 100000)))




parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               ptitle = "priorglmm_12mold_6c",
               psubtitle = "10:10 Cross Validation")


ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/Target_altdef1/priorglmm_12mold_6c.png")

data_modelling_test$pred <- predict(priorglmm_12mold_6c,
                                    newdata = data_modelling_test,
                                    type = "response",
                                    re.form = ~0)

CaliPlot(data_modelling_test$pred,
         data_modelling_test$Target_altdef1,
         ptitle = "priorglmm_12mold_6c",
         psubtitle = "External Testing Data")

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/GLMM/Target_altdef1/priorglmm_12mold_6cExternal.png")

######priorglmm_12mold_7######

ggplot(data_modelling_train, aes(x = propsiblingsstatus212mold, y = as.numeric(Target_altdef1))) +
  geom_point() +
  geom_smooth(method="gam", formula = y ~ s(x, bs = "cs", k=5))

priorglmm_12mold_7 <- glmer(Target_altdef1 ~ damstatus12mold_cat * damntests12mold +
                              granddamstatus12mold_cat * granddamntests12mold +
                              propsiblingsstatus212mold +
                              (1|Farm),
                            family ="binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl = list(maxfun = 100000)))

######priorglmm_12mold_8######

priorglmm_12mold_8 <- glmer(Target_altdef1 ~ damstatus12mold_cat * damntests12mold +
                              granddamstatus12mold_cat * granddamntests12mold +
                              propsiblingsstatus212mold +
                              propsiblingsstatus312mold +
                              (1|Farm),
                            family ="binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl = list(maxfun = 100000)))

######priorglmm_12mold_9######

priorglmm_12mold_9 <- glmer(Target_altdef1 ~ damstatus12mold_cat * damntests12mold +
                              granddamstatus12mold_cat * granddamntests12mold +
                              propsiblingsstatus212mold +
                              propauntsstatus212mold +
                              (1|Farm),
                            family ="binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl = list(maxfun = 100000)))

######priorglmm_12mold_10######

priorglmm_12mold_10 <- glmer(Target_altdef1 ~ damstatus12mold_cat * damntests12mold +
                              granddamstatus12mold_cat * granddamntests12mold +
                              propsiblingsstatus212mold +
                               propvaguelyproximaldamsstatus212mold +
                              (1|Farm),
                            family ="binomial",
                            data = data_modelling_train,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl = list(maxfun = 100000)))


######priorglmm_12mold_11######

priorglmm_12mold_11 <- glmer(Target_altdef1 ~ damstatus12mold_cat * damntests12mold +
                               granddamstatus12mold_cat * granddamntests12mold +
                               propsiblingsstatus212mold +
                               propproximaldamsstatus212mold +
                               (1|Farm),
                             family ="binomial",
                             data = data_modelling_train,
                             control = glmerControl(optimizer = "bobyqa",
                                                    optCtrl = list(maxfun = 100000)))

######priorglmm_12mold_12######

priorglmm_12mold_12 <- glmer(Target_altdef1 ~ damstatus12mold_cat * damntests12mold +
                               granddamstatus12mold_cat * granddamntests12mold +
                               propsiblingsstatus212mold +
                               propvaguelyproximaldamsstatus212mold +
                               propvaguelyproximaldamsstatus312mold +
                               (1|Farm),
                             family ="binomial",
                             data = data_modelling_train,
                             control = glmerControl(optimizer = "bobyqa",
                                                    optCtrl = list(maxfun = 100000)))

######priorglmm_12mold_13######

priorglmm_12mold_13 <- glmer(Target_altdef1 ~ damstatus12mold_cat * damntests12mold +
                               granddamstatus12mold_cat * granddamntests12mold +
                               propsiblingsstatus212mold +
                               propvaguelyproximaldamsstatus212mold +
                               propvaguelyproximaldamsstatus312mold +
                               meantitrenegcowsavg_recat +
                               (1|Farm),
                             family ="binomial",
                             data = data_modelling_train,
                             control = glmerControl(optimizer = "bobyqa",
                                                    optCtrl = list(maxfun = 100000)))

######priorglmm_12mold_14######

priorglmm_12mold_14 <- glmer(Target_altdef1 ~ damstatus12mold_cat * damntests12mold +
                               granddamstatus12mold_cat * granddamntests12mold +
                               prophorizontalrelsstatus212mold +
                               propvaguelyproximaldamsstatus212mold +
                               propvaguelyproximaldamsstatus312mold +
                               meantitrenegcowsavg_recat +
                               (1|Farm),
                             family ="binomial",
                             data = data_modelling_train,
                             control = glmerControl(optimizer = "bobyqa",
                                                    optCtrl = list(maxfun = 100000)))

######priorglmm_12mold_15######

priorglmm_12mold_15 <- glmer(Target_altdef1 ~ damstatus12mold_cat * damntests12mold +
                               granddamstatus12mold_cat * granddamntests12mold +
                               prophorizontalrelsstatus212mold +
                               prophorizontalrelsstatus312mold +
                               propvaguelyproximaldamsstatus212mold +
                               propvaguelyproximaldamsstatus312mold +
                               meantitrenegcowsavg_recat +
                               (1|Farm),
                             family ="binomial",
                             data = data_modelling_train,
                             control = glmerControl(optimizer = "bobyqa",
                                                    optCtrl = list(maxfun = 100000)))


aicmods <- c("priorglmm_12mold_1",
             "priorglmm_12mold_2",
             "priorglmm_12mold_3",
             "priorglmm_12mold_4",
             "priorglmm_12mold_5",
             "priorglmm_12mold_6",
             "priorglmm_12mold_7",
             "priorglmm_12mold_8",
             "priorglmm_12mold_9",
             "priorglmm_12mold_10",
             "priorglmm_12mold_11",
             "priorglmm_12mold_12",
             "priorglmm_12mold_13",
             "priorglmm_12mold_14",
             "priorglmm_12mold_15")

glmmaictable <- data.frame(Model = aicmods)
glmmaictable$AIC <- 0

for (i in 1:nrow(glmmaictable)){
  glmmaictable$AIC[i] <- AIC(get(glmmaictable$Model[i]))
  glmmaictable$features[i] <- toString(terms(get(glmmaictable$Model[i]))[[3]])
}

glmmaictable <- glmmaictable[order(glmmaictable$AIC),]

View(glmmaictable)


#####Models Current#####

######Random effect only models######

priorglmm_crt_reonly_none <- glm(Target_altdef1 ~
                                     1,
                                   family ="binomial",
                                   data = data_modelling_train)

priorglmm_crt_reonly_farm <- glmer(Target_altdef1 ~
                                     (1|Farm),
                                   family ="binomial",
                                   data = data_modelling_train,
                                   control = glmerControl(optimizer = "bobyqa",
                                                          optCtrl = list(maxfun = 100000)))


######priorglmm_crt_1######

model.formula <- formula(Target_altdef1 ~ 
                           damcrtstatus_cat * damntestscrt + 
                           (1|Farm))

priorglmm_crt_1 <- glmer(model.formula,
                         family ="binomial",
                         data = data_modelling_train,
                         control = glmerControl(optimizer = "bobyqa",
                                                optCtrl = list(maxfun = 100000)))


parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               stratifier = "Farm",
               ptitle = "priorglmm_crt_1",
               psubtitle = "10:10 CV",
               coresadjust = -1)

ggsave('Y:/Ian/JohnesThresholds/JohnesProper/Data/BirthPriorModel/GLMM/Target_altdef1/priorglmm_crt_1_CV.png')

######priorglmm_crt_2######

model.formula <- formula(Target_altdef1 ~ damcrtstatus_cat * damntestscrt +
                           granddamcrtstatus_cat * granddamntestscrt +
                           (1|Farm))

priorglmm_crt_2 <- glmer(model.formula,
                         family ="binomial",
                         data = data_modelling_train,
                         control = glmerControl(optimizer = "bobyqa",
                                                optCtrl = list(maxfun = 100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               stratifier = "Farm",
               ptitle = "priorglmm_crt_2",
               psubtitle = "10:10 CV",
               coresadjust = -1)

ggsave('Y:/Ian/JohnesThresholds/JohnesProper/Data/BirthPriorModel/GLMM/Target_altdef1/priorglmm_crt_2_CV.png')


######priorglmm_crt_3######

model.formula <- formula(Target_altdef1 ~ damcrtstatus_cat * damntestscrt +
                           granddamcrtstatus_cat * granddamntestscrt +
                           prophorizontalrelsstatus2crt_cat +
                           (1|Farm))

priorglmm_crt_3 <- glmer(model.formula,
                         family ="binomial",
                         data = data_modelling_train,
                         control = glmerControl(optimizer = "bobyqa",
                                                optCtrl = list(maxfun = 100000)))


parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               stratifier = "Farm",
               ptitle = "priorglmm_crt_3",
               psubtitle = "10:10 CV",
               coresadjust = -1)

ggsave('Y:/Ian/JohnesThresholds/JohnesProper/Data/BirthPriorModel/GLMM/Target_altdef1/priorglmm_crt_3_CV.png')


######priorglmm_crt_4######

model.formula <- formula(Target_altdef1 ~ damcrtstatus_cat * damntestscrt +
                           granddamcrtstatus_cat * granddamntestscrt +
                           prophorizontalrelsstatus2crt_recat +
                           (1|Farm))

#priorglmm_crt_4 <- glmer(model.formula,
#                         family ="binomial",
#                         data = data_modelling_train,
#                         control = glmerControl(optimizer = "bobyqa",
#                                                optCtrl = list(maxfun = 100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               stratifier = "Farm",
               ptitle = "priorglmm_crt_4",
               psubtitle = "10:10 CV",
               coresadjust = -1)

ggsave('Y:/Ian/JohnesThresholds/JohnesProper/Data/BirthPriorModel/GLMM/Target_altdef1/priorglmm_crt_4_CV.png')

######priorglmm_crt_5######

model.formula <- formula(Target_altdef1 ~ damcrtstatus_cat * damntestscrt +
                           granddamcrtstatus_cat * granddamntestscrt +
                           prophorizontalrelsstatus2crt +
                           (1|Farm))

#priorglmm_crt_5 <- glmer(model.formula,
                         #family ="binomial",
                         #data = data_modelling_train,
                         #control = glmerControl(optimizer = "bobyqa",
                        #                        optCtrl = list(maxfun = 100000)))


parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               stratifier = "Farm",
               ptitle = "priorglmm_crt_5",
               psubtitle = "10:10 CV",
               coresadjust = -1)

ggsave('Y:/Ian/JohnesThresholds/JohnesProper/Data/BirthPriorModel/GLMM/Target_altdef1/priorglmm_crt_5_CV.png')

######priorglmm_crt_6######

model.formula <- formula(Target_altdef1 ~ damcrtstatus_cat * damntestscrt +
                           granddamcrtstatus_cat * granddamntestscrt +
                           prophorizontalrelsstatus2crt_recat +
                           prophorizontalrelsstatus3crt_cat +
                           (1|Farm))

#priorglmm_crt_6 <- glmer(model.formula,
#                         family ="binomial",
#                         data = data_modelling_train,
#                         control = glmerControl(optimizer = "bobyqa",
#                                                optCtrl = list(maxfun = 100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               stratifier = "Farm",
               ptitle = "priorglmm_crt_6",
               psubtitle = "10:10 CV",
               coresadjust = -1)

ggsave('Y:/Ian/JohnesThresholds/JohnesProper/Data/BirthPriorModel/GLMM/Target_altdef1/priorglmm_crt_6_CV.png')


######priorglmm_crt_7######

model.formula <- formula(Target_altdef1 ~ damcrtstatus_cat * damntestscrt +
                           granddamcrtstatus_cat * granddamntestscrt +
                           propsiblingsstatus2crt +
                           (1|Farm))

#priorglmm_crt_7 <- glmer(model.formula,
#                         family ="binomial",
#                         data = data_modelling_train,
#                         control = glmerControl(optimizer = "bobyqa",
#                                                optCtrl = list(maxfun = 100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               stratifier = "Farm",
               ptitle = "priorglmm_crt_7",
               psubtitle = "10:10 CV",
               coresadjust = -1)

ggsave('Y:/Ian/JohnesThresholds/JohnesProper/Data/BirthPriorModel/GLMM/Target_altdef1/priorglmm_crt_7_CV.png')


######priorglmm_crt_8######

model.formula <- formula(Target_altdef1 ~ damcrtstatus_cat * damntestscrt +
                           granddamcrtstatus_cat * granddamntestscrt +
                           propsiblingsstatus2crt_cat +
                           (1|Farm))

#priorglmm_crt_8 <- glmer(model.formula,
#                         family ="binomial",
#                         data = data_modelling_train,
#                         control = glmerControl(optimizer = "bobyqa",
#                                                optCtrl = list(maxfun = 100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               stratifier = "Farm",
               ptitle = "priorglmm_crt_8",
               psubtitle = "10:10 CV",
               coresadjust = -1)

ggsave('Y:/Ian/JohnesThresholds/JohnesProper/Data/BirthPriorModel/GLMM/Target_altdef1/priorglmm_crt_8_CV.png')


######priorglmm_crt_9######

model.formula <- formula(Target_altdef1 ~ damcrtstatus_cat * damntestscrt +
                           granddamcrtstatus_cat * granddamntestscrt +
                           prophorizontalrelsstatus2crt_recat +
                           prophorizontalrelsstatus3crt_recat +
                           (1|Farm))

#priorglmm_crt_9 <- glmer(model.formula,
#                         family ="binomial",
#                         data = data_modelling_train,
#                         control = glmerControl(optimizer = "bobyqa",
#                                                optCtrl = list(maxfun = 100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               stratifier = "Farm",
               ptitle = "priorglmm_crt_9",
               psubtitle = "10:10 CV",
               coresadjust = -1)

ggsave('Y:/Ian/JohnesThresholds/JohnesProper/Data/BirthPriorModel/GLMM/Target_altdef1/priorglmm_crt_9_CV.png')



######priorglmm_crt_10######

model.formula <- formula(Target_altdef1 ~ damcrtstatus_cat * damntestscrt +
                           granddamcrtstatus_cat * granddamntestscrt +
                           prophorizontalrelsstatus2crt_recat +
                           prophorizontalrelsstatus3crt_recat +
                           propproximaldamsstatus2crt +
                           (1|Farm))

#priorglmm_crt_10 <- glmer(formula,
#                         family ="binomial",
#                         data = data_modelling_train,
                         #control = glmerControl(optimizer = "bobyqa",
                      #                          optCtrl = list(maxfun = 100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               stratifier = "Farm",
               ptitle = "priorglmm_crt_10",
               psubtitle = "10:10 CV",
               coresadjust = -1)

ggsave('Y:/Ian/JohnesThresholds/JohnesProper/Data/BirthPriorModel/GLMM/Target_altdef1/priorglmm_crt_10_CV.png')



######priorglmm_crt_11######

model.formula <- formula(Target_altdef1 ~ damcrtstatus_cat * damntestscrt +
                           granddamcrtstatus_cat * granddamntestscrt +
                           prophorizontalrelsstatus2crt_recat +
                           prophorizontalrelsstatus3crt_recat +
                           propvaguelyproximaldamsstatus2crt +
                           (1|Farm))

#priorglmm_crt_11 <- glmer(model.formula,
#                         family ="binomial",
#                         data = data_modelling_train,
#                         control = glmerControl(optimizer = "bobyqa",
#                                                optCtrl = list(maxfun = 100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               stratifier = "Farm",
               ptitle = "priorglmm_crt_11",
               psubtitle = "10:10 CV",
               coresadjust = -1)

ggsave('Y:/Ian/JohnesThresholds/JohnesProper/Data/BirthPriorModel/GLMM/Target_altdef1/priorglmm_crt_11_CV.png')



######priorglmm_crt_12######

model.formula <- formula(Target_altdef1 ~ damcrtstatus_cat * damntestscrt +
                           granddamcrtstatus_cat * granddamntestscrt +
                           prophorizontalrelsstatus2crt_recat +
                           prophorizontalrelsstatus3crt_recat +
                           propvaguelyproximaldamsstatus2crt +
                           propvaguelyproximaldamsstatus3crt +
                           (1|Farm))

#priorglmm_crt_12 <- glmer(model.formula,
#                          family ="binomial",
#                          data = data_modelling_train,
#                          control = glmerControl(optimizer = "bobyqa",
#                                                 optCtrl = list(maxfun = 100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               stratifier = "Farm",
               ptitle = "priorglmm_crt_12",
               psubtitle = "10:10 CV",
               coresadjust = -1)

ggsave('Y:/Ian/JohnesThresholds/JohnesProper/Data/BirthPriorModel/GLMM/Target_altdef1/priorglmm_crt_12_CV.png')


######priorglmm_crt_13######

model.formula <- formula(Target_altdef1 ~ damcrtstatus_cat * damntestscrt +
                           granddamcrtstatus_cat * granddamntestscrt +
                           prophorizontalrelsstatus2crt_recat +
                           prophorizontalrelsstatus3crt_recat +
                           propvaguelyproximaldamsstatus2crt +
                           propvaguelyproximaldamsstatus3crt +
                           propproximalcalvesstatus2crt +
                           (1|Farm))

#priorglmm_crt_13 <- glmer(model.formula,
#                          family ="binomial",
#                          data = data_modelling_train,
#                          control = glmerControl(optimizer = "bobyqa",
#                                                 optCtrl = list(maxfun = 100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               stratifier = "Farm",
               ptitle = "priorglmm_crt_13",
               psubtitle = "10:10 CV",
               coresadjust = -1)

ggsave('Y:/Ian/JohnesThresholds/JohnesProper/Data/BirthPriorModel/GLMM/Target_altdef1/priorglmm_crt_13_CV.png')


######priorglmm_crt_14######

model.formula <- formula(Target_altdef1 ~ damcrtstatus_cat * damntestscrt +
                           granddamcrtstatus_cat * granddamntestscrt +
                           prophorizontalrelsstatus2crt_recat +
                           prophorizontalrelsstatus3crt_recat +
                           propvaguelyproximaldamsstatus2crt +
                           propvaguelyproximaldamsstatus3crt +
                           propvaguelyproximalcalvesstatus2crt +
                           (1|Farm))

#priorglmm_crt_14 <- glmer(model.formula,
#                          family ="binomial",
#                          data = data_modelling_train,
#                          control = glmerControl(optimizer = "bobyqa",
#                                                 optCtrl = list(maxfun = 100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               stratifier = "Farm",
               ptitle = "priorglmm_crt_14",
               psubtitle = "10:10 CV",
               coresadjust = -1)

ggsave('Y:/Ian/JohnesThresholds/JohnesProper/Data/BirthPriorModel/GLMM/Target_altdef1/priorglmm_crt_14_CV.png')


######priorglmm_crt_15######

model.formula <- formula(Target_altdef1 ~ damcrtstatus_cat * damntestscrt +
                           granddamcrtstatus_cat * granddamntestscrt +
                           prophorizontalrelsstatus2crt_recat +
                           prophorizontalrelsstatus3crt_recat +
                           propvaguelyproximaldamsstatus2crt +
                           propvaguelyproximaldamsstatus3crt +
                           propvaguelyproximalcalvesstatus2crt +
                           propproximalcalvesstatus3crt +
                           (1|Farm))

#priorglmm_crt_15 <- glmer(model.formula,
#                          family ="binomial",
#                          data = data_modelling_train,
#                          control = glmerControl(optimizer = "bobyqa",
#                                                 optCtrl = list(maxfun = 100000)))

parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               ptitle = "priorglmm_crt_15",
               psubtitle = "10:10 CV")

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/GLMM/Target_altdef1/priorglmm_crt_15.png")

######priorglmm_crt_15a######

model.formula <- formula(Target_altdef1 ~ damcrtstatus_cat * damntestscrt +
                           granddamcrtstatus_cat * granddamntestscrt +
                           prophorizontalrelsstatus2crt_recat +
                           prophorizontalrelsstatus3crt_recat +
                           propvaguelyproximaldamsstatus2crt +
                           propvaguelyproximaldamsstatus3crt +
                           propvaguelyproximalcalvesstatus2crt +
                           propproximalcalvesstatus3crt +
                           damtitrecrt_cat +
                           (1|Farm))

priorglmm_crt_15a <- glmer(model.formula,
                          family ="binomial",
                          data = data_modelling_train,
                          control = glmerControl(optimizer = "bobyqa",
                                                 optCtrl = list(maxfun = 100000)))



parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               ptitle = "priorglmm_crt_15a",
               psubtitle = "10:10 Cross Validation")

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/GLMM/Target_altdef1/priorglmm_crt_15a.png")

data_modelling_test$pred <- predict(priorglmm_crt_15a,
                                    newdata = data_modelling_test,
                                    type = "response",
                                    re.form = ~ 0)

CaliPlot(data_modelling_test$pred,
         data_modelling_test$Target_altdef1,
         ptitle = "priorglmm_crt_15a",
         psubtitle = "External Testing Data")

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/GLMM/Target_altdef1/priorglmm_crt_15aExternal.png")

######priorglmm_crt_15b######

model.formula <- formula(Target_altdef1 ~ damcrtstatus_cat * damntestscrt +
                           granddamcrtstatus_cat * granddamntestscrt +
                           prophorizontalrelsstatus2crt_recat +
                           prophorizontalrelsstatus3crt_recat +
                           propvaguelyproximaldamsstatus2crt +
                           propvaguelyproximaldamsstatus3crt +
                           propvaguelyproximalcalvesstatus2crt +
                           propproximalcalvesstatus3crt +
                           damlikecrt_cat +
                           (1|Farm))

priorglmm_crt_15b <- glmer(model.formula,
                           family ="binomial",
                           data = data_modelling_train,
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl = list(maxfun = 100000)))



parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               ptitle = "priorglmm_crt_15b",
               psubtitle = "10:10 Cross Validation")

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/GLMM/Target_altdef1/priorglmm_crt_15b.png")


data_modelling_test$pred <- predict(priorglmm_crt_15b,
                                    newdata = data_modelling_test,
                                    type = "response",
                                    re.form = ~0)


CaliPlot(data_modelling_test$pred,
         data_modelling_test$Target_altdef1,
         ptitle = "priorglmm_crt_15b",
         psubtitle = "External Testing Data")

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/GLMM/Target_altdef1/priorglmm_crt_15bExternal.png")

######priorglmm_crt_15b_2######

model.formula <- formula(Target_altdef1 ~ granddamcrtstatus_cat * granddamntestscrt +
                           prophorizontalrelsstatus2crt_recat +
                           prophorizontalrelsstatus3crt_recat +
                           propvaguelyproximaldamsstatus2crt +
                           propvaguelyproximaldamsstatus3crt +
                           propvaguelyproximalcalvesstatus2crt +
                           propproximalcalvesstatus3crt +
                           damlikecrt_cat +
                           (1|Farm))

priorglmm_crt_15b_2 <- glmer(model.formula,
                           family ="binomial",
                           data = data_modelling_train,
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl = list(maxfun = 100000)))



parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               ptitle = "priorglmm_crt_15b_2",
               psubtitle = "10:10 Cross Validation")

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/GLMM/Target_altdef1/priorglmm_crt_15b_2.png")


data_modelling_test$pred <- predict(priorglmm_crt_15b_2,
                                    newdata = data_modelling_test,
                                    type = "response",
                                    re.form = ~0)


CaliPlot(data_modelling_test$pred,
         data_modelling_test$Target_altdef1,
         ptitle = "priorglmm_crt_15b_2",
         psubtitle = "External Testing Data")

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/GLMM/Target_altdef1/priorglmm_crt_15b_2External.png")


######priorglmm_crt_15c######

model.formula <- formula(Target_altdef1 ~ damcrtstatus_cat * damntestscrt +
                           granddamcrtstatus_cat * granddamntestscrt +
                           prophorizontalrelsstatus2crt_recat +
                           prophorizontalrelsstatus3crt_recat +
                           propvaguelyproximaldamsstatus2crt +
                           propvaguelyproximaldamsstatus3crt +
                           propvaguelyproximalcalvesstatus2crt +
                           propproximalcalvesstatus3crt +
                           damppcrt_cat +
                           (1|Farm))

priorglmm_crt_15c <- glmer(model.formula,
                           family ="binomial",
                           data = data_modelling_train,
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl = list(maxfun = 100000)))



parallelCVglmm(data_modelling_train,
               model.formula,
               nfolds = 10,
               nrepeats = 10,
               ptitle = "priorglmm_crt_15c",
               psubtitle = "10:10 Cross Validation")

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/GLMM/Target_altdef1/priorglmm_crt_15c.png")


data_modelling_test$pred <- predict(priorglmm_crt_15c,
                                    newdata = data_modelling_test,
                                    type = "response",
                                    re.form = ~0)


CaliPlot(data_modelling_test$pred,
         data_modelling_test$Target_altdef1,
         ptitle = "priorglmm_crt_15c",
         psubtitle = "External Testing Data")

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/GLMM/Target_altdef1/priorglmm_crt_15cExternal.png")


######priorglmm_crt_16######

priorglmm_crt_16 <- glmer(Target_altdef1 ~ damcrtstatus_cat * damntestscrt +
                            granddamcrtstatus_cat * granddamntestscrt +
                            prophorizontalrelsstatus2crt_recat +
                            prophorizontalrelsstatus3crt_recat +
                            propvaguelyproximaldamsstatus2crt +
                            propvaguelyproximaldamsstatus3crt +
                            propvaguelyproximalcalvesstatus2crt +
                            propvaguelyproximalcalvesstatus3crt +
                            (1|Farm),
                          family ="binomial",
                          data = data_modelling_train,
                          control = glmerControl(optimizer = "bobyqa",
                                                 optCtrl = list(maxfun = 100000)))


######priorglmm_crt_17######

priorglmm_crt_17 <- glmer(Target_altdef1 ~ damcrtstatus_cat * damntestscrt +
                            granddamcrtstatus_cat * granddamntestscrt +
                            prophorizontalrelsstatus2crt_recat +
                            prophorizontalrelsstatus3crt_recat +
                            propvaguelyproximaldamsstatus2crt +
                            propvaguelyproximaldamsstatus3crt +
                            propvaguelyproximalcalvesstatus2crt +
                            propvaguelyproximalcalvesstatus3crt +
                            meantitrenegcowsavg_recat +
                            (1|Farm),
                          family ="binomial",
                          data = data_modelling_train,
                          control = glmerControl(optimizer = "bobyqa",
                                                 optCtrl = list(maxfun = 100000)))


######priorglmm_crt_18######

priorglmm_crt_18 <- glmer(Target_altdef1 ~ damcrtstatus_cat * damntestscrt +
                            prophorizontalrelsstatus2crt_recat +
                            prophorizontalrelsstatus3crt_recat +
                            propvaguelyproximaldamsstatus2crt +
                            propvaguelyproximaldamsstatus3crt +
                            propvaguelyproximalcalvesstatus2crt +
                            propvaguelyproximalcalvesstatus3crt +
                            meantitrenegcowsavg_recat +
                            (1|Farm),
                          family ="binomial",
                          data = data_modelling_train,
                          control = glmerControl(optimizer = "bobyqa",
                                                 optCtrl = list(maxfun = 100000)))



aicmods <- c("priorglmm_crt_1",
             "priorglmm_crt_2",
             "priorglmm_crt_3",
             "priorglmm_crt_4",
             "priorglmm_crt_5",
             "priorglmm_crt_6",
             "priorglmm_crt_7",
             "priorglmm_crt_8",
             "priorglmm_crt_9",
             "priorglmm_crt_10",
             "priorglmm_crt_11",
             "priorglmm_crt_12",
             "priorglmm_crt_13",
             "priorglmm_crt_14",
             "priorglmm_crt_15",
             "priorglmm_crt_16",
             "priorglmm_crt_17",
             "priorglmm_crt_18")

glmmaictable <- data.frame(Model = aicmods)
glmmaictable$AIC <- 0

for (i in 1:nrow(glmmaictable)){
  glmmaictable$AIC[i] <- AIC(get(glmmaictable$Model[i]))
  glmmaictable$features[i] <- toString(terms(get(glmmaictable$Model[i]))[[3]])
}

glmmaictable <- glmmaictable[order(glmmaictable$AIC),]

View(glmmaictable)

data_modelling_train$prophorizontalrelsstatus2crt_cat_collapsed <- data_modelling_train$prophorizontalrelsstatus2crt_cat
levels(data_modelling_train$prophorizontalrelsstatus2crt_cat_collapsed) <- c(levels(data_modelling_train$prophorizontalrelsstatus2crt_cat_collapsed), "(0.333,1]")
data_modelling_train$prophorizontalrelsstatus2crt_cat_collapsed[data_modelling_train$prophorizontalrelsstatus2crt_cat_collapsed == "(0.333,0.667]" |
                                                                  data_modelling_train$prophorizontalrelsstatus2crt_cat_collapsed == "(0.667,1]"] <- "(0.333,1]"
data_modelling_train$prophorizontalrelsstatus2crt_cat_collapsed <- droplevels(data_modelling_train$prophorizontalrelsstatus2crt_cat_collapsed, c("(0.333,0.667]", "(0.667,1]"))

data_modelling_train$propposavg_recat <- cut(data_modelling_train$propposavg, breaks = c(-0.00001, 0.02,0.04,0.5))
levels(data_modelling_train$propposavg_recat) <- c(levels(data_modelling_train$propposavg_recat), "Missing")
data_modelling_train$propposavg_recat[is.na(data_modelling_train$propposavg_recat)] <- "Missing"

#data_modelling_train$prophorizontalrelsstatus2crt_recat <- cut(data_modelling_train$prophorizontalrelsstatus2crt, breaks = c(-0.00001, 0.15,0.3,1))
#levels(data_modelling_train$prophorizontalrelsstatus2crt_recat) <- c(levels(data_modelling_train$prophorizontalrelsstatus2crt_recat), "Missing")
#data_modelling_train$prophorizontalrelsstatus2crt_recat[is.na(data_modelling_train$prophorizontalrelsstatus2crt_recat)] <- "Missing"

####CROSS VALIDATE MODELS####

CVmods <- c("priorglmm_birth_18",
            "priorglmm_birth_20",
            "priorglmm_birth_21",
            "priorglmm_12mold_10",
            "priorglmm_12mold_11",
            "priorglmm_12mold_12",
            "priorglmm_12mold_13",
            "priorglmm_12mold_14",
            "priorglmm_crt_13",
            "priorglmm_crt_14",
            "priorglmm_crt_15",
            "priorglmm_crt_16",
            "priorglmm_crt_17",
            "priorglmm_crt_18"
)
               
time1 <- Sys.time()

CVResults <- data.frame()

for (eta in 1:length(CVmods)){
  
  modeltorun <- CVmods[eta]
  
  for (k in 1:5){
    set.seed(sample(1:10000,1))
    folds <- createFolds(data_modelling_train$Target_QMMS, k = 5)
    for (j in 1:5) {
      names(folds) <- c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")
      names(folds)[j] <- "test"
      trainset <- data_modelling_train[-folds$test,]
      testset <- data_modelling_train[folds$test,]
      print("-------------------------------------")
      print(paste0("Model ",eta,"/",length(CVmods)))
      print(modeltorun)
      print(paste("Testing on Fold",j,"Repetition",k))
      print(paste0("Progess: ",(eta-1)*5*5 + (((k-1)*5) + j)   ,"/",length(CVmods) * 5 * 5, " (",round((((eta-1)*5*5) + ((k-1)*5) + j) / (length(CVmods) * 5 * 5) * 100, 1),"%)"))
      timeelapsed <- difftime(Sys.time(), time1, units = "mins")
      totalmods <- 5*5*length(CVmods)
      modsdone <- (eta-1)*5*5 + (((k-1)*5) + j) -1
      modsremaining = totalmods - modsdone
      timeremaining <- modsremaining /modsdone * timeelapsed
      #if((modsdone/totalmods) >= 0.25){print(paste("Estimated time remaining:",round(timeremaining,1),"mins.\nEstimated finish time:",Sys.time + timeremaining))}
      #if((modsdone/totalmods) < 0.25){print("Calculating time remaining...")}
      
      if(modeltorun == "priorglmm_birth_18"){
        
        mod <- glmer(Target_QMMS ~ damstatusbirth_cat * damntestsbirth +
                                      propsiblingsstatus2birth +
                                      propsiblingsstatus3birth +
                                      propvaguelyproximaldamsstatus2birth +
                                      propvaguelyproximaldamsstatus3birth +
                                      meantitrenegcowsavg_recat +
                                      (1|Farm),
                                    family = "binomial",
                                    data = trainset,
                                    control = glmerControl(optimizer = "bobyqa",
                                                           optCtrl=list(maxfun=100000)))
        
      }
      
      if(modeltorun == "priorglmm_birth_20"){
      
      mod <- glmer(Target_QMMS ~ damstatusbirth_cat * damntestsbirth +
                                    prophorizontalrelsstatus2birth +
                                    propvaguelyproximaldamsstatus2birth +
                                    propvaguelyproximaldamsstatus3birth +
                                    meantitrenegcowsavg_recat +
                                    (1|Farm),
                                  family = "binomial",
                                  data = trainset,
                                  control = glmerControl(optimizer = "bobyqa",
                                                         optCtrl=list(maxfun=100000)))
      
      }
      
      if(modeltorun == "priorglmm_birth_21"){
      mod <- glmer(Target_QMMS ~ damstatusbirth_cat * damntestsbirth +
                                    prophorizontalrelsstatus2birth_cat +
                                    propvaguelyproximaldamsstatus2birth +
                                    propvaguelyproximaldamsstatus3birth +
                                    meantitrenegcowsavg_recat +
                                    (1|Farm),
                                  family = "binomial",
                                  data = trainset,
                                  control = glmerControl(optimizer = "bobyqa",
                                                         optCtrl=list(maxfun=100000)))
      
      }
      
      
      if(modeltorun == "priorglmm_12mold_10"){
      
      mod <- glmer(Target_QMMS ~ damstatus12mold_cat * damntests12mold +
                                     granddamstatus12mold_cat * granddamntests12mold +
                                     propsiblingsstatus212mold +
                                     propvaguelyproximaldamsstatus212mold +
                                     (1|Farm),
                                   family ="binomial",
                                   data = trainset,
                                   control = glmerControl(optimizer = "bobyqa",
                                                          optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_12mold_11"){
      
      
      mod <- glmer(Target_QMMS ~ damstatus12mold_cat * damntests12mold +
                                     granddamstatus12mold_cat * granddamntests12mold +
                                     propsiblingsstatus212mold +
                                     propproximaldamsstatus212mold +
                                     (1|Farm),
                                   family ="binomial",
                                   data = trainset,
                                   control = glmerControl(optimizer = "bobyqa",
                                                          optCtrl = list(maxfun = 100000)))
      }
      
      if(modeltorun == "priorglmm_12mold_12"){
      
      mod <- glmer(Target_QMMS ~ damstatus12mold_cat * damntests12mold +
                                     granddamstatus12mold_cat * granddamntests12mold +
                                     propsiblingsstatus212mold +
                                     propvaguelyproximaldamsstatus212mold +
                                     propvaguelyproximaldamsstatus312mold +
                                     (1|Farm),
                                   family ="binomial",
                                   data = trainset,
                                   control = glmerControl(optimizer = "bobyqa",
                                                          optCtrl = list(maxfun = 100000)))
      
      }
      
      if(modeltorun == "priorglmm_12mold_13"){
      
      mod <- glmer(Target_QMMS ~ damstatus12mold_cat * damntests12mold +
                                     granddamstatus12mold_cat * granddamntests12mold +
                                     propsiblingsstatus212mold +
                                     propvaguelyproximaldamsstatus212mold +
                                     propvaguelyproximaldamsstatus312mold +
                                     meantitrenegcowsavg_recat +
                                     (1|Farm),
                                   family ="binomial",
                                   data = trainset,
                                   control = glmerControl(optimizer = "bobyqa",
                                                          optCtrl = list(maxfun = 100000)))
      
      }
      
      if(modeltorun == "priorglmm_12mold_14"){
      
      mod <- glmer(Target_QMMS ~ damstatus12mold_cat * damntests12mold +
                                     granddamstatus12mold_cat * granddamntests12mold +
                                     prophorizontalrelsstatus212mold +
                                     propvaguelyproximaldamsstatus212mold +
                                     propvaguelyproximaldamsstatus312mold +
                                     meantitrenegcowsavg_recat +
                                     (1|Farm),
                                   family ="binomial",
                                   data = trainset,
                                   control = glmerControl(optimizer = "bobyqa",
                                                          optCtrl = list(maxfun = 100000)))
      
      
      }

        
      if(modeltorun == "priorglmm_crt_13"){
      
      mod <- glmer(Target_QMMS ~ damcrtstatus_cat * damntestscrt +
                                  granddamcrtstatus_cat * granddamntestscrt +
                                  prophorizontalrelsstatus2crt_recat +
                                  prophorizontalrelsstatus3crt_recat +
                                  propvaguelyproximaldamsstatus2crt +
                                  propvaguelyproximaldamsstatus3crt +
                                  propproximalcalvesstatus2crt +
                                  (1|Farm),
                                family ="binomial",
                                data = trainset,
                                control = glmerControl(optimizer = "bobyqa",
                                                       optCtrl = list(maxfun = 100000)))
      
      }
      
      if(modeltorun == "priorglmm_crt_14"){
      
      mod <- glmer(Target_QMMS ~ damcrtstatus_cat * damntestscrt +
                                  granddamcrtstatus_cat * granddamntestscrt +
                                  prophorizontalrelsstatus2crt_recat +
                                  prophorizontalrelsstatus3crt_recat +
                                  propvaguelyproximaldamsstatus2crt +
                                  propvaguelyproximaldamsstatus3crt +
                                  propvaguelyproximalcalvesstatus2crt +
                                  (1|Farm),
                                family ="binomial",
                                data = trainset,
                                control = glmerControl(optimizer = "bobyqa",
                                                       optCtrl = list(maxfun = 100000)))
      
      }
      
      if(modeltorun == "priorglmm_crt_15"){
      
      mod <- glmer(Target_QMMS ~ damcrtstatus_cat * damntestscrt +
                                  granddamcrtstatus_cat * granddamntestscrt +
                                  prophorizontalrelsstatus2crt_recat +
                                  prophorizontalrelsstatus3crt_recat +
                                  propvaguelyproximaldamsstatus2crt +
                                  propvaguelyproximaldamsstatus3crt +
                                  propvaguelyproximalcalvesstatus2crt +
                                  propproximalcalvesstatus3crt +
                                  (1|Farm),
                                family ="binomial",
                                data = trainset,
                                control = glmerControl(optimizer = "bobyqa",
                                                       optCtrl = list(maxfun = 100000)))
      
      }
      
      if(modeltorun == "priorglmm_crt_16"){
        
      mod <- glmer(Target_QMMS ~ damcrtstatus_cat * damntestscrt +
                                  granddamcrtstatus_cat * granddamntestscrt +
                                  prophorizontalrelsstatus2crt_recat +
                                  prophorizontalrelsstatus3crt_recat +
                                  propvaguelyproximaldamsstatus2crt +
                                  propvaguelyproximaldamsstatus3crt +
                                  propvaguelyproximalcalvesstatus2crt +
                                  propvaguelyproximalcalvesstatus3crt +
                                  (1|Farm),
                                family ="binomial",
                                data = trainset,
                                control = glmerControl(optimizer = "bobyqa",
                                                       optCtrl = list(maxfun = 100000)))
      
      }
      
      if(modeltorun == "priorglmm_crt_17"){
      
      mod <- glmer(Target_QMMS ~ damcrtstatus_cat * damntestscrt +
                                  granddamcrtstatus_cat * granddamntestscrt +
                                  prophorizontalrelsstatus2crt_recat +
                                  prophorizontalrelsstatus3crt_recat +
                                  propvaguelyproximaldamsstatus2crt +
                                  propvaguelyproximaldamsstatus3crt +
                                  propvaguelyproximalcalvesstatus2crt +
                                  propvaguelyproximalcalvesstatus3crt +
                                  meantitrenegcowsavg_recat +
                                  (1|Farm),
                                family ="binomial",
                                data = trainset,
                                control = glmerControl(optimizer = "bobyqa",
                                                       optCtrl = list(maxfun = 100000)))
      
      }
      
      if(modeltorun == "priorglmm_crt_18"){
      
      
      mod <- glmer(Target_QMMS ~ damcrtstatus_cat * damntestscrt +
                                  prophorizontalrelsstatus2crt_recat +
                                  prophorizontalrelsstatus3crt_recat +
                                  propvaguelyproximaldamsstatus2crt +
                                  propvaguelyproximaldamsstatus3crt +
                                  propvaguelyproximalcalvesstatus2crt +
                                  propvaguelyproximalcalvesstatus3crt +
                                  meantitrenegcowsavg_recat +
                                  (1|Farm),
                                family ="binomial",
                                data = trainset,
                                control = glmerControl(optimizer = "bobyqa",
                                                       optCtrl = list(maxfun = 100000)))
      
      }
      
      
      
      
      if(modeltorun == "birthglmm2_TQ"){
        
        mod <- glmer(Target_QMMS ~ 
                                 damcrtstatus_cat * damntests +
                                 granddamcrtstatus_cat * granddamntests +
                                 (1|Farm),
                               family = "binomial",
                               data = trainset,
                               control=glmerControl(optimizer="bobyqa",
                                                    optCtrl=list(maxfun=100000)))
         
      }
      
      if(modeltorun == "birthglmm4_TQ"){
      
      mod <- glmer(Target_QMMS ~ 
                               damcrtstatus_cat * damntests +
                               granddamcrtstatus_cat * granddamntests +
                               propsiblingsstatus2crt_cat +
                               propsiblingsstatus3crt_cat_collapsed +
                               (1|Farm),
                             family = "binomial",
                             data = trainset,
                             control=glmerControl(optimizer="bobyqa",
                                                  optCtrl=list(maxfun=100000)))
      
      }
      
      if(modeltorun == "birthglmm5_TQ"){
      
      mod <- glmer(Target_QMMS ~ 
                               damcrtstatus_cat * damntests +
                               granddamcrtstatus_cat * granddamntests +
                               propsiblingsstatus2crt_cat +
                               (1|Farm),
                             family = "binomial",
                             data = trainset,
                             control=glmerControl(optimizer="bobyqa",
                                                  optCtrl=list(maxfun=100000)))
      
      }
      
      if(modeltorun == "birthglmm6_TQ"){
        
        mod <- glmer(Target_QMMS ~ 
                                 damcrtstatus_cat * damntests +
                                 granddamcrtstatus_cat * granddamntests +
                                 prophorizontalrelsstatus2crt_cat +
                                 prophorizontalrelsstatus3crt_cat +
                                 (1|Farm),
                               family = "binomial",
                               data = trainset,
                               control=glmerControl(optimizer="bobyqa",
                                                    optCtrl=list(maxfun=100000)))
      }
      
      if(modeltorun == "birthglmm7_TQ"){
      mod <- glmer(Target_QMMS ~ 
                               damcrtstatus_cat * damntests +
                               granddamcrtstatus_cat * granddamntests +
                               prophorizontalrelsstatus2crt_cat * totalhorizontalrelsntests +
                               prophorizontalrelsstatus3crt_cat * totalhorizontalrelsntests +
                               (1|Farm),
                             family = "binomial",
                             data = trainset,
                             control=glmerControl(optimizer="bobyqa",
                                                  optCtrl=list(maxfun=100000)))
      
      }
      
      if(modeltorun == "birthglmm8_TQ"){
        
        mod <- glmer(Target_QMMS ~ 
                                 damcrtstatus_cat * damntests +
                                 granddamcrtstatus_cat * granddamntests +
                                 prophorizontalrelsstatus2crt_cat +
                                 prophorizontalrelsstatus3crt_cat +
                                 propvaguelyproximaldamsstatus2crt +
                                 propvaguelyproximaldamsstatus3crt +
                                 (1|Farm),
                               family = "binomial",
                               data = trainset,
                               control=glmerControl(optimizer="bobyqa",
                                                    optCtrl=list(maxfun=100000)))
        
        
        
      }
      
      if(modeltorun == "birthglmm9_TQ"){
      
      mod <- glmer(Target_QMMS ~ 
                               damcrtstatus_cat * damntests +
                               granddamcrtstatus_cat * granddamntests +
                               prophorizontalrelsstatus2crt_cat +
                               prophorizontalrelsstatus3crt_cat +
                               propproximaldamsstatus2crt +
                               propproximaldamsstatus3crt +
                               (1|Farm),
                             family = "binomial",
                             data = trainset,
                             control=glmerControl(optimizer="bobyqa",
                                                  optCtrl=list(maxfun=100000)))
      
      }
      if(modeltorun == "birthglmm10_TQ"){
      
      mod <- glmer(Target_QMMS ~ 
                                damcrtstatus_cat * damntests +
                                granddamcrtstatus_cat * granddamntests +
                                prophorizontalrelsstatus2crt_cat +
                                prophorizontalrelsstatus3crt_cat +
                                propvaguelyproximaldamsstatus2crt +
                                propvaguelyproximaldamsstatus3crt +
                                propvaguelyproximalcalvesstatus2crt +
                                propvaguelyproximalcalvesstatus3crt +
                                
                                (1|Farm),
                              family = "binomial",
                              data = trainset,
                              control=glmerControl(optimizer="bobyqa",
                                                   optCtrl=list(maxfun=100000)))
      
      }
      
      if(modeltorun == "birthglmm11_TQ"){
        
        mod <- glmer(Target_QMMS ~ 
                                  damcrtstatus_cat * damntests +
                                  granddamcrtstatus_cat * granddamntests +
                                  prophorizontalrelsstatus2crt_cat +
                                  prophorizontalrelsstatus3crt_cat +
                                  propvaguelyproximaldamsstatus2crt +
                                  propvaguelyproximaldamsstatus3crt +
                                  propproximalcalvesstatus2crt +
                                  propproximalcalvesstatus3crt +
                                  
                                  (1|Farm),
                                family = "binomial",
                                data = trainset,
                                control=glmerControl(optimizer="bobyqa",
                                                     optCtrl=list(maxfun=100000)))
        
      }
      
      if(modeltorun == "birthglmm12_TQ"){
        
        mod <- glmer(Target_QMMS ~ 
                                  damcrtstatus_cat * damntests +
                                  granddamcrtstatus_cat * granddamntests +
                                  prophorizontalrelsstatus2crt_cat +
                                  prophorizontalrelsstatus3crt_cat +
                                  propvaguelyproximaldamsstatus2crt +
                                  propvaguelyproximaldamsstatus3crt +
                                  propproximalcalvesstatus2crt +
                                  propproximalcalvesstatus3crt +
                                  propposavg_cat +
                                  (1|Farm),
                                family = "binomial",
                                data = trainset,
                                control=glmerControl(optimizer="bobyqa",
                                                     optCtrl=list(maxfun=100000)))
        
      }
      
      if(modeltorun == "birthglmm13_TQ"){
        
        mod <- glmer(Target_QMMS ~ 
                       damcrtstatus_cat * damntests +
                       granddamcrtstatus_cat * granddamntests +
                       prophorizontalrelsstatus2crt_cat +
                       prophorizontalrelsstatus3crt_cat +
                       propvaguelyproximaldamsstatus2crt +
                       propvaguelyproximaldamsstatus3crt +
                       propproximalcalvesstatus2crt +
                       propproximalcalvesstatus3crt +
                       meantitreavg_cat +
                       (1|Farm),
                     family = "binomial",
                     data = trainset,
                     control=glmerControl(optimizer="bobyqa",
                                          optCtrl=list(maxfun=100000)))
        
      }
      
      if(modeltorun == "birthglmm14_TQ"){
        
        mod <- glmer(Target_QMMS ~ 
                       damcrtstatus_cat * damntests +
                       granddamcrtstatus_cat * granddamntests +
                       prophorizontalrelsstatus2crt_cat +
                       prophorizontalrelsstatus3crt_cat +
                       propvaguelyproximaldamsstatus2crt +
                       propvaguelyproximaldamsstatus3crt +
                       propproximalcalvesstatus2crt +
                       propproximalcalvesstatus3crt +
                       meantitrenegcowsavg_cat +
                       (1|Farm),
                     family = "binomial",
                     data = trainset,
                     control=glmerControl(optimizer="bobyqa",
                                          optCtrl=list(maxfun=100000)))
          
      }
      
      if(modeltorun == "birthglmm15_TQ"){
        
        mod <- glmer(Target_QMMS ~ 
                                  damcrtstatus_cat * damntests +
                                  prophorizontalrelsstatus2crt_cat +
                                  prophorizontalrelsstatus3crt_cat +
                                  propvaguelyproximaldamsstatus2crt +
                                  propvaguelyproximaldamsstatus3crt +
                                  propproximalcalvesstatus2crt +
                                  propproximalcalvesstatus3crt +
                                  propposavg_cat +
                                  (1|Farm),
                                family = "binomial",
                                data = trainset,
                                control=glmerControl(optimizer="bobyqa",
                                                     optCtrl=list(maxfun=100000)))
        
      }
      
      if(modeltorun == "birthglmm17_TQ"){
        
        mod <- glmer(Target_QMMS ~ 
                                  damcrtstatus_cat * damntests +
                                  prophorizontalrelsstatus2crt_cat +
                                  prophorizontalrelsstatus3crt_cat +
                                  propvaguelyproximaldamsstatus2crt +
                                  propproximalcalvesstatus2crt +
                                  propproximalcalvesstatus3crt +
                                  propposavg_cat +
                                  (1|Farm),
                                family = "binomial",
                                data = trainset,
                                control=glmerControl(optimizer="bobyqa",
                                                     optCtrl=list(maxfun=100000)))
        
      }
      
      if(modeltorun == "birthglmm21_TQ"){
        
        mod <- glmer(Target_QMMS ~ 
                                  damcrtstatus_cat * damntests +
                                  prophorizontalrelsstatus2crt_cat_collapsed +
                                  prophorizontalrelsstatus3crt_cat +
                                  propvaguelyproximaldamsstatus2crt +
                                  propproximalcalvesstatus2crt +
                                  propproximalcalvesstatus3crt +
                                  propposavg_cat +
                                  (1|Farm),
                                family = "binomial",
                                data = trainset,
                                control=glmerControl(optimizer="bobyqa",
                                                     optCtrl=list(maxfun=100000)))
        
      }
      
      if(modeltorun == "birthglmm22_TQ"){
      
      mod <- glmer(Target_QMMS ~ 
                                damcrtstatus_cat * damntests +
                                prophorizontalrelsstatus2crt_cat +
                                prophorizontalrelsstatus3crt_cat +
                                propvaguelyproximaldamsstatus2crt +
                                propproximalcalvesstatus2crt +
                                propproximalcalvesstatus3crt +
                                meantitreavg_cat +
                                (1|Farm),
                              family = "binomial",
                              data = trainset,
                              control=glmerControl(optimizer="bobyqa",
                                                   optCtrl=list(maxfun=100000)))
      }
      
      if(modeltorun == "birthglmm23_TQ"){
      
      mod <- glmer(Target_QMMS ~ 
                                damcrtstatus_cat * damntests +
                                prophorizontalrelsstatus2crt_cat +
                                prophorizontalrelsstatus3crt_cat +
                                propvaguelyproximaldamsstatus2crt +
                                propproximalcalvesstatus2crt +
                                propproximalcalvesstatus3crt +
                                meantitrenegcowsavg_cat +
                                (1|Farm),
                              family = "binomial",
                              data = trainset,
                              control=glmerControl(optimizer="bobyqa",
                                                   optCtrl=list(maxfun=100000)))
      
      }
      
      if(modeltorun == "birthglmm24_TQ"){
      
      mod <- glmer(Target_QMMS ~ 
                                damcrtstatus_cat * damntests +
                                granddamcrtstatus_cat * granddamntests +
                                prophorizontalrelsstatus2crt_cat +
                                prophorizontalrelsstatus3crt_cat +
                                propvaguelyproximaldamsstatus2crt +
                                propvaguelyproximaldamsstatus3crt +
                                propproximalcalvesstatus2crt +
                                propproximalcalvesstatus3crt +
                                meantitrenegcowsavg_recat +
                                (1|Farm),
                              family = "binomial",
                              data = trainset,
                              control=glmerControl(optimizer="bobyqa",
                                                   optCtrl=list(maxfun=100000)))
      
      }
      
      
      if(modeltorun == "birthglmm27_TQ"){
        
        mod <- glmer(Target_QMMS ~ 
                                  damcrtstatus_cat * damntests +
                                  prophorizontalrelsstatus2crt_cat_collapsed +
                                  prophorizontalrelsstatus3crt_cat +
                                  propvaguelyproximaldamsstatus2crt +
                                  propvaguelyproximaldamsstatus3crt +
                                  propproximalcalvesstatus2crt +
                                  propproximalcalvesstatus3crt +
                                  meantitrenegcowsavg_recat +
                                  (1|Farm),
                                family = "binomial",
                                data = trainset,
                                control=glmerControl(optimizer="bobyqa",
                                                     optCtrl=list(maxfun=100000)))
        
      }
      
      if(modeltorun == "birthglmm28_TQ"){
        
        mod <- glmer(Target_QMMS ~ 
                                  damcrtstatus_cat * damntests +
                                  prophorizontalrelsstatus2crt_cat_collapsed +
                                  prophorizontalrelsstatus3crt_cat +
                                  propvaguelyproximaldamsstatus2crt +
                                  I(propvaguelyproximaldamsstatus2crt^2) +
                                  propvaguelyproximaldamsstatus3crt +
                                  propproximalcalvesstatus2crt +
                                  propproximalcalvesstatus3crt +
                                  meantitrenegcowsavg_recat +
                                  (1|Farm),
                                family = "binomial",
                                data = trainset,
                                control=glmerControl(optimizer="bobyqa",
                                                     optCtrl=list(maxfun=100000)))
      }
      
      if(modeltorun == "birthglmm29_TQ"){
      
      mod <- glmer(Target_QMMS ~ 
                                damcrtstatus_cat * damntests +
                                prophorizontalrelsstatus2crt_cat_collapsed +
                                prophorizontalrelsstatus3crt_cat +
                                propvaguelyproximaldamsstatus2crt +
                                I(propvaguelyproximaldamsstatus2crt^2) +
                                propvaguelyproximaldamsstatus3crt +
                                I(propvaguelyproximaldamsstatus3crt^2) +
                                propproximalcalvesstatus2crt +
                                propproximalcalvesstatus3crt +
                                meantitrenegcowsavg_recat +
                                (1|Farm),
                              family = "binomial",
                              data = trainset,
                              control=glmerControl(optimizer="bobyqa",
                                                   optCtrl=list(maxfun=100000)))
      
      }
      
      if(modeltorun == "birthglmm30_TQ"){
      
      mod <- glmer(Target_QMMS ~ 
                                damcrtstatus_cat * damntests +
                                prophorizontalrelsstatus2crt_cat_collapsed +
                                prophorizontalrelsstatus3crt_cat +
                                propvaguelyproximaldamsstatus2crt +
                                I(propvaguelyproximaldamsstatus2crt^2) +
                                propvaguelyproximaldamsstatus3crt +
                                propproximalcalvesstatus2crt +
                                I(propproximalcalvesstatus2crt^2) +
                                propproximalcalvesstatus3crt +
                                meantitrenegcowsavg_recat +
                                (1|Farm),
                              family = "binomial",
                              data = trainset,
                              control=glmerControl(optimizer="bobyqa",
                                                   optCtrl=list(maxfun=100000)))
      }
      
      if(modeltorun == "birthglmm31_TQ"){
      
      mod <- glmer(Target_QMMS ~ 
                                damcrtstatus_cat * damntests +
                                prophorizontalrelsstatus2crt_cat_collapsed +
                                prophorizontalrelsstatus3crt_cat +
                                propvaguelyproximaldamsstatus2crt +
                                I(propvaguelyproximaldamsstatus2crt^2) +
                                propvaguelyproximaldamsstatus3crt +
                                propproximalcalvesstatus2crt +
                                propproximalcalvesstatus3crt +
                                I(propproximalcalvesstatus3crt^2) +
                                meantitrenegcowsavg_recat +
                                (1|Farm),
                              family = "binomial",
                              data = trainset,
                              control=glmerControl(optimizer="bobyqa",
                                                   optCtrl=list(maxfun=100000)))
      
      }
      
      if(modeltorun == "birthglmm32_TQ"){
        
        mod <- glmer(Target_QMMS ~ damcrtstatus_cat * damntests +
                       granddamcrtstatus_cat * granddamntests +
                       prophorizontalrelsstatus2crt_cat_collapsed +
                       prophorizontalrelsstatus3crt_cat +
                       propvaguelyproximaldamsstatus2crt +
                       propvaguelyproximaldamsstatus3crt +
                       propproximalcalvesstatus2crt +
                       propproximalcalvesstatus3crt +
                       meantitrenegcowsavg_recat +
                       (1|Farm),
                     family = "binomial",
                     data = trainset,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 100000)))
        
      }
      
      if(modeltorun == "birthglmm33_TQ"){
        
        mod <- glmer(Target_QMMS ~ damcrtstatus_cat * damntests +
                                  granddamcrtstatus_cat * granddamntests +
                                  prophorizontalrelsstatus2crt_cat_collapsed +
                                  prophorizontalrelsstatus3crt_cat +
                                  propvaguelyproximaldamsstatus2crt +
                                  propvaguelyproximaldamsstatus3crt +
                                  propproximalcalvesstatus2crt +
                                  propproximalcalvesstatus3crt +
                                  (1|Farm),
                                family = "binomial",
                                data = trainset,
                                control = glmerControl(optimizer = "bobyqa",
                                                       optCtrl = list(maxfun = 100000)))
        
      }
      
      testset$pred <- tryCatch({predict(mod, newdata=testset, type="response", re.form=~0, allow.new.levels = T)},
                               error=function(cond) {
                                 print(paste0("Failed to predict model ", eta,", rep ",k,", fold ",j,"!"))
                                 return("FAILED")})
                                 
      
      foldresults <- data.frame(Model = modeltorun, Rep = k, Fold = j, Observed = testset$Target_QMMS)
      foldresults <- cbind(foldresults, testset$pred)
      
      
      
      CVResults <<- rbind(CVResults, foldresults)
      
    } 
    
  }
  
}



time2 <- Sys.time()

print(paste("Time taken for model cross validation:", round(difftime(time2, time1, units = "mins"),1), "mins"))

ans <- readline("Save to GLMM_TQ_CVResults.csv? ")

if(ans == "Y" |
   ans == "y"){

    print("Saving to GLMM_TQ_CVResults.csv....")
    write.csv(CVResults, "y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/GLMM_TQ_CVResults.csv")
    
  }

print("Generating CaliPlots and extracting calibration errors...")

time1 <- Sys.time()

####SAVE FINAL PRIOR MODELS####

finalpriormodel_birth <- priorglmm_birth_5

finalpriormodel_12mold <- priorglmm_12mold_6

finalpriormodel_crt <- priorglmm_crt_15

saveRDS(finalpriormodel_birth, file = "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/finalpriormodel_birth.Rds")
saveRDS(finalpriormodel_12mold, file = "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/finalpriormodel_12mold.Rds")
saveRDS(finalpriormodel_crt, file = "y:/ian/johnesthresholds/johnesproper/data/pickledmodels/finalpriormodel_crt.Rds")

####REMOVE OBJECTS####

rm(data_modelling)
rm(data_modelling_train)
rm(data_modelling_test)
rm(trainset)
rm(testset)
rm(data)
rm(CVResults)
rm(CVResults_nona)
rm(tempglmmpreds)
rm(CPData)
rm(calibration_data)
rm(calibration_plot_data)
rm(folds)

