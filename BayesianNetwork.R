data <- read.csv("Y:/Ian/JohnesThresholds/JohnesProper/Data/data_birthpriors.csv")

print("Cleaning data...")

data$Target_QMMS <- as.factor(data$Target_QMMS)

data$titre <- as.numeric(data$titre)

data <- data[which(data$yield <= 70 &
                     data$protein <=7 &
                     data$cellcount < 10000 &
                     data$butterfat < 10 &
                     data$age >= 24 &
                     !is.na(data$age)),]




data$age_cat <- cut(as.numeric(data$age), breaks = agebreaks)

data$yield_cat <- cut(data$yield, breaks = c(0,10,20,30,40,50,70))
levels(data$yield_cat) <- c(levels(data$yield_cat), "Missing")
data$yield_cat[which(is.na(data$yield_cat))] <- "Missing"

data$protein_cat <- cut(data$protein, breaks = c(0,3,3.5,4,4.5,7))
levels(data$protein_cat) <- c(levels(data$protein_cat), "Missing")
data$protein_cat[which(is.na(data$protein_cat))] <- "Missing"

data$butterfat_cat <- cut(data$butterfat, breaks = c(0,2,4,6,10))
levels(data$butterfat_cat) <- c(levels(data$butterfat_cat), "Missing")
data$butterfat_cat[which(is.na(data$butterfat_cat))] <- "Missing"

data$cellcount_cat <- cut(data$cellcount, breaks = c(0,50,100,150,200,500,1000,10000))
levels(data$cellcount_cat) <- c(levels(data$cellcount_cat), "Missing")
data$cellcount_cat[which(is.na(data$cellcount_cat))] <- "Missing"




print("Creating titre cutpoints...")

data$cutpoint <- length(cutpoints)


time1 <- Sys.time()

source_python("Y:/Ian/JohnesThresholds/JohnesProper/Data/PythonScripts/PyCreateCutpoints.py")

data <- createcutpoints(data, cutpoints)




time2 <- Sys.time()

print(paste("Time taken assigning cutpoints:", round(difftime(time2, time1, units = "mins"),2), "mins"))

print("Titre cutpoints:")
print(head(cbind(data$titre, data$cutpoint),30))


data$cutpoint <- as.factor(data$cutpoint)



####Create data for multinomial model (NNET Package)

print("Creating data for multinomial likelihood models...")

if(target == "MEYER"){
  data_multinom <- data[data$Target_Meyer != "U",]
  data_multinom$Target_Meyer <- droplevels(data_multinom$Target_Meyer, "U")
  data_multinom$Target_Meyer <- as.factor(data_multinom$Target_Meyer)
}

if(target == "QMMS"){
  data_multinom <- data[data$Target_QMMS != "U",]
  data_multinom$Target_QMMS <- droplevels(data_multinom$Target_QMMS, "U")
  data_multinom$Target_QMMS <- as.factor(data_multinom$Target_QMMS)
  
}





data_multinom$age <- as.numeric(data_multinom$age)

data_multinom$yield <- as.numeric(data_multinom$yield)

data_multinom$cellcount <- as.numeric(data_multinom$cellcount)

data_multinom$dim <- as.numeric(data_multinom$dim)

data_multinom$meantitrenegcows <- as.numeric(data_multinom$meantitrenegcows)

data_multinom$butterfat <- as.numeric(data_multinom$butterfat)

data_multinom$protein <- as.numeric(data_multinom$protein)


data_multinom$agesq <- data_multinom$age^2
data_multinom$yieldsq <- data_multinom$yield^2
data_multinom$proteinsq <- data_multinom$protein^2
data_multinom$butterfatsq <- data_multinom$butterfat^2
data_multinom$meantitrenegcowssq <- data_multinom$meantitrenegcows^2
data_multinom$cellcountlog <- log(data_multinom$cellcount)


