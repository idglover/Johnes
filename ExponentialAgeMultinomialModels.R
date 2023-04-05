data_combi <- read.csv("y:/ian/johnesthresholds/johnesproper/data/CombinedData/Combined_RSQL.csv")

data_combi$calfdob <- ymd(data_combi$calfdob)
data_combi$latesttestdate <- ymd(data_combi$latesttestdate)
data_combi$nexttestdate <- ymd(data_combi$nexttestdate)


data_combi <- data_combi[which(!is.na(data_combi$calfeartag)),]
data_combi <- data_combi[which(data_combi$calfdob != "1985-01-01"),]

data_combi$testdobinterval <- as.integer(data_combi$testdobinterval)
data_combi$ntestedlatest <- as.integer(data_combi$ntestedlatest)
data_combi$nrecordedlatest <- as.integer(data_combi$nrecordedlatest)


data_combi$profile <- as.character(data_combi$profile)

dim(data_combi)

data_combi$Target_Meyer <- ifelse(data_combi$ntests >= 9 & grepl("H", substr(data_combi$profile, nchar(data_combi$profile)-7, nchar(data_combi$profile))) == FALSE, "0", "U")
data_combi$Target_Meyer <- ifelse(data_combi$ntests >= 3 &  substr(data_combi$profile, nchar(data_combi$profile) - 1, nchar(data_combi$profile)) == "HH", "1", data_combi$Target_Meyer)
data_combi$Target_Meyer <- as.factor(data_combi$Target_Meyer)


summary(data_combi$Target_Meyer)

data_combi <- data_combi[which(!is.na(data_combi$Target_Meyer)),]

data_sens_age <- data_combi

dim(data_sens_age)

data_sens_age <- data_sens_age[,c(4,528,
                                  92:103,
                                  104:115,
                                  116:127,
                                  128:139,
                                  140:151,
                                  152:163,
                                  164:175,
                                  176:187,
                                  188:199,
                                  200:211,
                                  212:223,
                                  224:235,
                                  236:247,
                                  248:259,
                                  260:271,
                                  272:283,
                                  284:295,
                                  296:307,
                                  308:319,
                                  320:331,
                                  332:343,
                                  344:355,
                                  356:367,
                                  368:379,
                                  380:391,
                                  392:403,
                                  404:415,
                                  416:427,
                                  428:439,
                                  440:451,
                                  452:463,
                                  464:475,
                                  476:487,
                                  488:499,
                                  527)]

data_sens_age <- tidyr::unite(data_sens_age, covsandtitre1, c(3:13), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre2, c(4:14), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre3, c(5:15), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre4, c(6:16), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre5, c(7:17), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre6, c(8:18), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre7, c(9:19), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre8, c(10:20), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre9, c(11:21), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre10, c(12:22), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre11, c(13:23), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre12, c(14:24), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre13, c(15:25), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre14, c(16:26), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre15, c(17:27), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre16, c(18:28), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre17, c(19:29), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre18, c(20:30), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre19, c(21:31), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre20, c(22:32), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre21, c(23:33), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre22, c(24:34), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre23, c(25:35), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre24, c(26:36), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre25, c(27:37), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre26, c(28:38), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre27, c(29:39), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre28, c(30:40), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre29, c(31:41), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre30, c(32:42), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre31, c(33:43), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre32, c(34:44), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre33, c(35:45), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre34, c(36:46), remove = TRUE, sep = ":")
data_sens_age <- tidyr::unite(data_sens_age, covsandtitre35, c(37:47), remove = TRUE, sep = ":")

data_sens_age <- tidyr::gather(data_sens_age, covsandtitre, value, -c(calfeartag, Farm, Target_Meyer))

data_sens_age <- data_sens_age[order(data_sens_age$calfeartag),]

data_sens_age <- tidyr::separate(data_sens_age, value, c("date", "age", "parity", "dim", "yield", "butterfat", "protein", "lactose", "cellcount", "titre"), sep = ":")

data_sens_age <- data_sens_age[data_sens_age$age != "NA",]

data_sens_age$calfeartag <- as.character(data_sens_age$calfeartag)
data_sens_age$Farm <- as.character(data_sens_age$Farm)
data_sens_age$date <- as.character(data_sens_age$date)
data_sens_age$age <- as.numeric(data_sens_age$age)
data_sens_age$parity <- as.numeric(data_sens_age$parity)
data_sens_age$dim <- as.numeric(data_sens_age$dim)
data_sens_age$yield <- as.numeric(data_sens_age$yield)
data_sens_age$butterfat <- as.numeric(data_sens_age$butterfat)
data_sens_age$protein <- as.numeric(data_sens_age$protein)
data_sens_age$lactose <- as.numeric(data_sens_age$lactose)
data_sens_age$cellcount <- as.numeric(data_sens_age$cellcount)
data_sens_age$titre <- as.numeric(data_sens_age$titre)
data_sens_age$Target_Meyer <- as.factor(data_sens_age$Target_Meyer)


summary(data_sens_age$titre)

random_samples <- data.frame(calfeartag = character(0),
                             Farm = character(0),
                             Target_Meyer = character(0),
                             covsandtitre = character(0),
                             date = character(0),
                             age = numeric(0),
                             parity = numeric(0),
                             dim = numeric(0),
                             yield = numeric(0),
                             butterfat = numeric(0),
                             protein = numeric(0),
                             lactose = numeric(0),
                             cellcount = numeric(0),
                             titre = numeric(0))





random_samples$calfeartag <- as.character(random_samples$calfeartag)
random_samples$Farm <- as.character(random_samples$Farm)
random_samples$covsandtitre <- as.character(random_samples$covsandtitre)
random_samples$date <- as.character(random_samples$date)

for(i in 1:length(unique(data_sens_age$calfeartag))) {
  uniquetag <- unique(data_sens_age$calfeartag)[i]
  tempdata <- data_sens_age[data_sens_age$calfeartag == uniquetag,]
  samp <- tempdata[sample(nrow(tempdata), 1),]
  random_samples[i,] <- samp
}


random_samples$Target_Meyer <- as.factor(random_samples$Target_Meyer)

levels(random_samples$Target_Meyer) <- c("0","1","U")

summary(random_samples$Target_Meyer)

random_samples$age <- as.numeric(random_samples$age)


random_samples$dim <- as.numeric(random_samples$dim)

random_samples$titre <- as.numeric(random_samples$titre)

random_samples$class <- factor(ifelse(random_samples$titre >= 30, 1, 0))



set.seed(1981)
trainRowNumbers <- createDataPartition(random_samples$Target_Meyer, p = 0.8, list = FALSE)
data_train <- random_samples[trainRowNumbers,]
data_test <- random_samples[-trainRowNumbers,]

summary(as.factor(data_train$Farm))

summary(as.factor(data_test$Farm))

age <- as.vector(data_train$age)
dim <- as.vector(data_train$dim)
yield <- as.vector(data_train$yield)
butterfat <- as.vector(data_train$butterfat)
protein <- as.vector(data_train$protein)
cellcount <- as.vector(data_train$cellcount)
class <- as.vector(data_train$class)
n <- nrow(data_train)

AgeSpecSensModel <- function() {
  
  for (i in 1:n) {
    class[i] ~ dbern(p[i])
    logit(p[i]) <- alpha - (beta * exp(-gamma*age[i]))
    
  }
  ## Priors
  alpha ~ dnorm(0.0,1.0E-4)
  beta ~ dnorm(0.0,1.0E-4)
  gamma ~ dnorm(0.0,1.0E-4)
  
  
}

AgeSpecSensModel_dim <- function() {
  
  for (i in 1:n) {
    class[i] ~ dbern(p[i])
    logit(p[i]) <- alpha - (beta * exp(-gamma*age[i])) + delta * dim[i]
    
  }
  ## Priors
  alpha ~ dnorm(0.0,1.0E-4)
  beta ~ dnorm(0.0,1.0E-4)
  gamma ~ dnorm(0.0,1.0E-4)
  delta ~ dnorm(0.0,1.0E-4)
}