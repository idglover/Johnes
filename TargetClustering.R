####GET DATA####

#####READ FILE#####

UML_data <- read_excel("Y:/LAMP/Data/OriginalDataNewMethod/OriginalDataClean.xlsx")

#####GET ONLY FEATURES NEEDED#####

UML_data <- UML_data[,c("Herd",
                        "Eartag",
                        "SCC1",
                        "SCC2",
                        "SCC3",
                        "SCC4",
                        "NoSubMR",
                        "NoSubCM3MR",
                        "SCCCombinedCureNoCMAnyQrt")]

UML_data <- UML_data[complete.cases(UML_data) == TRUE,]

#####KMEANS CLUSTERING#####

km <- stats::kmeans(UML_data[,c(3:8)], centers = 2, nstart = 100)

######Plot features vs clusters######

res <- data.frame(SCC1 = UML_data$SCC1, 
                  SCC2 = UML_data$SCC2, 
                  SCC3 = UML_data$SCC3,
                  SCC4 = UML_data$SCC4,
                  NoSubCM3MR = UML_data$NoSubCM3MR,
                  cluster = as.factor(km$cluster),
                  cure = as.factor(UML_data$SCCCombinedCureNoCMAnyQrt))

ggplot(res,
       aes(x = SCC1, y = SCC2, color = cluster)) +
  geom_point() +
  geom_jitter()


ggplot(res,
       aes(x = SCC3, y = SCC4, color = cluster)) +
  geom_point()

ggplot(res,
       aes(x = NoSubCM3MR, y = cluster)) +
  geom_boxplot()


ggplot(res,
       aes(x = SCC1)) +
  geom_histogram() +
  facet_wrap(~cluster)

ggplot(res,
       aes(x = SCC2)) +
  geom_histogram() +
  facet_wrap(~cluster)

ggplot(res,
       aes(x = SCC3)) +
  geom_histogram() +
  facet_wrap(~cluster)

ggplot(res,
       aes(x = SCC4)) +
  geom_histogram() +
  facet_wrap(~cluster)

ggplot(res,
       aes(x = NoSubCM3MR)) +
  geom_histogram() +
  facet_wrap(~cluster)

ggplot(res,
       aes(x = cure, y = as.numeric(cluster)-1)) +
  geom_bar(stat = "summary", fun = "mean")
