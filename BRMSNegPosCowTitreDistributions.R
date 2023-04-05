####READ DATA####

data_brms <- read.csv("y:/ian/johnesthresholds/johnesproper/data/data_brms.csv")

data_brms$Target_QMMS <- as.factor(data_brms$Target_QMMS)

####PLOTS####

for (f in unique(data_brms$Farm)) {
  print(ggplot(data_brms[data_brms$Target_QMMS == "0" &
                           data_brms$Farm == f,],
               aes(x = titre)) +
          geom_density() +
          labs(x = "Titre",
               title = f))
          
}

ggplot(data_brms[data_brms$Target_QMMS == "0",],
       aes(x = titre)) +
  geom_density() +
  labs(title = "Target_QMMS", x = "Titre") +
  facet_wrap(~ Farm)

####ALTERNATIVE NEG COW DEFINITIONS####

data_brms$Target_altdef1 <- "U"

data_brms$Target_altdef1 <- as.factor(ifelse(grepl("HH", data_brms$profile) == TRUE |
                             grepl("HMH", data_brms$profile) == TRUE |
                             grepl("HLH", data_brms$profile) == TRUE,
                           "1",
                           ifelse(data_brms$ntests >= 3 &
                                    substr(data_brms$profile, nchar(data_brms$profile) - 2, nchar(data_brms$profile)) == "LLL" &
                                    (grepl("M", data_brms$profile) == TRUE | grepl("H", data_brms$profile) == TRUE), "0",
                                  data_brms$Target_altdef1)))

ggplot(data_brms[data_brms$Target_altdef1 == "0",],
       aes(x = titre)) +
  geom_density() +
  labs(title = "Target_altdef1", x = "Titre") +
  facet_wrap(~ Farm)


ggplot(data_brms[data_brms$Target_altdef1 == "0",],
       aes(x = age,
           y = titre)) +
  geom_point() +
  labs(x = "Age (m)", y = "Titre", title = "Target_altdef1")

ggplot() +
  geom_density(data = data_brms[data_brms$Target_QMMS == "0",],
               aes(x = titre), color = "grey") +
  geom_density(data=data_brms[data_brms$Target_altdef1 == "0",],
               aes(x = titre), color = "darkred")

tmp <- data_brms[grepl("H", data_brms$profile) == TRUE |
                   grepl("M", data_brms$profile) == TRUE,]

ggplot(tmp[tmp$Target_altdef1 == "0",],
       aes(x = age,
           y = titre)) +
  geom_point() +
  labs(x = "Age (m)", y = "Titre", title = "Target_altdef1")

ggplot() +
  geom_density(data = tmp[tmp$Target_altdef1 == "1",],
               aes(x = titre), color = "red") +
  geom_density(data=tmp[tmp$Target_altdef1 == "0",],
               aes(x = titre), color = "green")
