#####AGE AT FIRST HH######







#####TESTING PROFILE OF HERDS######

HerdTestingCalendar <- read_excel("y:/ian/johnesthresholds/johnesproper/HerdTestingCalendar.xlsx")

HerdTestingCalendar_long <- melt(HerdTestingCalendar, id.vars = c("NAUserRef",
                                                                  "HDHerdNo",
                                                                  "NABusName",
                                                                  "HDProgType",
                                                                  "Total Of HDHerdName"), 
                                                                  variable.name = "Year")

HerdTestingCalendar_long <- HerdTestingCalendar_long[with(HerdTestingCalendar_long, order(NAUserRef, Year)),]

write.csv(HerdTestingCalendar_long, "y:/ian/johnesthresholds/johnesproper/data/HerdTestingCalendar_long.csv")  

HerdTestingCalendar_farm_summary <- read_excel("y:/ian/johnesthresholds/johnesproper/data/HerdTestingCalendar_long.xlsx", sheet = "HerdSummaries")

ggplot(HerdTestingCalendar_farm_summary, aes(x = maxnconsecyears3tests)) +
  geom_histogram() +
  labs(title = "n consecutive years 3+ tests")

ggplot(HerdTestingCalendar_farm_summary[HerdTestingCalendar_farm_summary$maxnconsecyears3tests >=4,], aes(x = maxnconsecyears3tests)) +
  geom_histogram() +
  labs(title = "n consecutive years 3+ tests (Minimum 4 years)")

summary(as.factor(HerdTestingCalendar_farm_summary$maxnconsecyears3tests[HerdTestingCalendar_farm_summary$maxnconsecyears3tests >= 5])) #n consecutive years of >= 3 tests/year

summary(as.factor(HerdTestingCalendar_farm_summary$NAUserRef[HerdTestingCalendar_farm_summary$maxnconsecyears3tests == 5])) # Show herds with 11 consecutive years of 3 tests/year

                  