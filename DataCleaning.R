


#####READ COMBINED FARMS COMBI DATA#####

data_combi <- read.csv("y:/ian/johnesthresholds/johnesproper/data/CombinedData/Combined_RSQL.csv")

data_combi$calfdob <- ymd(data_combi$calfdob)
data_combi$latesttestdate <- ymd(data_combi$latesttestdate)
data_combi$nexttestdate <- ymd(data_combi$nexttestdate)

countNA(data_combi)


#####OVERALL STRUCTURE######

#dataMaid

check(data_combi)
summarize(data_combi)


#DataExplorer

str(data_combi[,c(3:57)])

plot_str(data_combi[,c(3:57)])
introduce(data_combi[,c(3:57)])
plot_intro(data_combi[,c(3:57)])
plot_missing(data_combi[,c(3:57)])
plot_bar(data_combi[,c(3:57)])
plot_histogram(data_combi[,c(3:57)])



#####CLEAN AND FORMAT INDIVIDUAL PREDICTORS######




data_combi <- data_combi[!is.na(data_combi$calfeartag),]
data_combi <- data_combi[data_combi$calfdob != "1985-01-01",]

data_combi$testdobinterval <- as.integer(data_combi$testdobinterval)
data_combi$ntestedlatest <- as.integer(data_combi$ntestedlatest)
data_combi$nrecordedlatest <- as.integer(data_combi$nrecordedlatest)

data_combi <- data_combi[data_combi$nsiblings < 18,]
data_combi <- data_combi[data_combi$naunts < 9,]
data_combi <- data_combi[data_combi$ngreataunts <13,]








