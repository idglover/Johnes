####READ TVU RAIDER DATA####

time0 <- Sys.time()

print("Reading data...")

data <- read.csv("y:/ian/johnesthresholds/johnesproper/data/CombinedData/Combined_RSQL.csv")

####UNITE TIME-VARYING FEATURES####

print("Combining milk recording data...")

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

####CLEAN DATA####

print("Cleaning data...")

#####CONVERT DATES#####

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

####PRINT DATASET SIZE####

print(paste("Full dataset (after cleaning):",dim(data)[1],"rows"))

####CHECK ALL FEATURES ARE PRESENT IN DATA####

for (i in c("Farm",
            "calfeartag",
            "damparity",
            "damparity_cat",
            "damstatusbirth",
            "damstatusbirth_cat",
            "damntestsbirth",
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
            "covsandtitre37")){
  if((i %in% colnames(data)) == FALSE){
    print(paste0(i, " missing from data"))
  }
}

####CHANGE COLUMN ORDER####

data <- data[,c("Farm",
                "calfeartag",
                "damparity",
                "damparity_cat",
                "damstatusbirth",
                "damstatusbirth_cat",
                "damntestsbirth",
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
                "ntests",
                "ageatfirsttest",
                "ageatlasttest",
                "testingperiod",
                "nonehigh",
                "Hanypoint",
                "HHanypoint",
                "HLHanypoint",
                "HMHanypoint",
                "ageatfirstM",
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

####READ PICKLED MODELS####

finalpriormodel_birth <- readRDS(paste0("y:/ian/johnesthresholds/johnesproper/data/pickledmodels/priormodels/",finalpriormodel_birth,".rds"))
finalpriormodel_12mold <- readRDS(paste0("y:/ian/johnesthresholds/johnesproper/data/pickledmodels/priormodels/",finalpriormodel_12mold,".rds"))
finalpriormodel_crt <- readRDS(paste0("y:/ian/johnesthresholds/johnesproper/data/pickledmodels/priormodels/",finalpriormodel_crt,".rds"))

####CONVERT DATE WIDE TO LONG####

print("Converting data wide to long...")

data <- tidyr::gather(data, covsandtitre, value, -c(Farm,
                                                    calfeartag,
                                                    damparity,
                                                    damparity_cat,
                                                    damstatusbirth,
                                                    damstatusbirth_cat,
                                                    damntestsbirth,
                                                    granddamstatusbirth,
                                                    granddamstatusbirth_cat,
                                                    granddamntestsbirth,
                                                    nsiblingsbirth,
                                                    nsiblingsstatus2birth,
                                                    propsiblingsstatus2birth,
                                                    propsiblingsstatus2birth_cat,
                                                    nsiblingsstatus3birth,
                                                    propsiblingsstatus3birth,
                                                    propsiblingsstatus3birth_cat,
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
                                                    prophorizontalrelsstatus2birth,
                                                    prophorizontalrelsstatus2birth_cat,
                                                    prophorizontalrelsstatus2birth_recat,
                                                    prophorizontalrelsstatus3birth,
                                                    prophorizontalrelsstatus3birth_cat,
                                                    prophorizontalrelsstatus3birth_recat,
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
                                                    damstatus12mold,
                                                    damstatus12mold_cat,
                                                    damntests12mold,
                                                    granddamstatus12mold,
                                                    granddamstatus12mold_cat,
                                                    granddamntests12mold,
                                                    nsiblings12mold,
                                                    nsiblingsstatus212mold,
                                                    propsiblingsstatus212mold,
                                                    propsiblingsstatus212mold_cat,
                                                    nsiblingsstatus312mold,
                                                    propsiblingsstatus312mold,
                                                    propsiblingsstatus312mold_cat,
                                                    naunts12mold,
                                                    nauntsstatus212mold,
                                                    propauntsstatus212mold,
                                                    nauntsstatus312mold,
                                                    propauntsstatus312mold,
                                                    ngreataunts12mold,
                                                    ngreatauntsstatus212mold,
                                                    propgreatauntsstatus212mold,
                                                    ngreatauntsstatus312mold,
                                                    propgreatauntsstatus312mold,
                                                    prophorizontalrelsstatus212mold,
                                                    prophorizontalrelsstatus212mold_cat,
                                                    prophorizontalrelsstatus312mold,
                                                    prophorizontalrelsstatus312mold_cat,
                                                    nproximaldams.1,
                                                    nproximaldamsstatus212mold,
                                                    propproximaldamsstatus212mold,
                                                    nproximaldamsstatus312mold,
                                                    propproximaldamsstatus312mold,
                                                    nvaguelyproximaldams.1,
                                                    nvaguelyproximaldamsstatus212mold,
                                                    propvaguelyproximaldamsstatus212mold,
                                                    nvaguelyproximaldamsstatus312mold,
                                                    propvaguelyproximaldamsstatus312mold,
                                                    damcrtstatus,
                                                    damcrtstatus_cat,
                                                    damntestscrt,
                                                    granddamcrtstatus,
                                                    granddamcrtstatus_cat,
                                                    granddamntestscrt,
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
                                                    prophorizontalrelsstatus2crt_recat,
                                                    prophorizontalrelsstatus3crt,
                                                    prophorizontalrelsstatus3crt_cat,
                                                    prophorizontalrelsstatus3crt_recat,
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
                                                    ageatfirstM,
                                                    ageatfirstH,
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

####PREDICT PRIOR MODELS####

print("Predicting birth models on full dataset...")

if(target == "QMMS"){
  data$priorprob_birth <- predict(finalpriormodel_birth, newdata = data, type = "response", re.form=~0, allow.new.levels = T)
  data$priorprob_12mold <- predict(finalpriormodel_12mold, newdata = data, type = "response", re.form=~0, allow.new.levels = T)
  data$priorprob_crt <- predict(finalpriormodel_crt, newdata = data, type = "response", re.form=~0, allow.new.levels = T)
}

####HISTOGRAMS OF PRIOR PROBABILITIES####

print(ggplot(data, aes(x = priorprob_birth)) +
  geom_histogram() +
  labs(title = "Predicted Probabilities at Birth", subtitle = paste0("Target ", target), x = "Probability"))

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/priorhist_birth.png")

print(ggplot(data, aes(x = priorprob_12mold)) +
  geom_histogram() +
  labs(title = "Predicted Probabilities at 12m Old", subtitle = paste0("Target ", target), x = "Probability"))

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/priorhist_12mold.png")

print(ggplot(data, aes(x = priorprob_crt)) +
  geom_histogram() +
  labs(title = "Predicted Probabilities Current", subtitle = paste0("Target ", target), x = "Probability"))

ggsave("y:/ian/johnesthresholds/johnesproper/data/birthpriormodel/glmm/priorhist_crt.png")

####CONVERT PRIOR PROBABILITIES TO ODDS####

data$priorodds_birth <- data$priorprob_birth / (1 - data$priorprob_birth) #Prior ODDS at birth
data$priorodds_12mold <- data$priorprob_12mold / (1 - data$priorprob_12mold) #Prior ODDS at birth
data$priorodds_crt <- data$priorprob_crt / (1 - data$priorprob_crt) #Prior ODDS at birth

####SAVING DATA####

print("Saving data (data_birthpriors.csv)")

write.csv(data, "y:/ian/johnesthresholds/johnesproper/data/data_birthpriors.csv", row.names = FALSE)

time1 <- Sys.time()
print(paste("Time processing birth models:", round(difftime(time1, time0, units = "mins"),2), "mins"))

rm(tempdata)
rm(data)

