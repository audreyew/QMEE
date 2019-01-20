library(readr)
library(tidyverse)
data <- read_csv("data/FO_TempPlasticitySelection_complete_April2018.csv")
View(data)
str(data)
summary(data)
(data
  %>% group_by(individual, temperature, trait, cage)
  %>% summarize(count=n()))
#plots
ggplot(data, aes(x=survival)) + geom_histogram()
#survival can only be of value 0-6. Graph shows that.
ggplot(data, aes(x = individual, y = Length, colour = trait)) + geom_point()
#some traits have lengths that are much too large. Flies would not have wings that are over 900mm.
data_2 <- mutate(data, length_fix = if_else(Length > 100, Length * 0.00186, Length))
ggplot(data_2, aes(x = individual, y = length_fix, colour = trait)) + geom_point()

save(data, data_2, file = "C:/Users/HP/Desktop/Stats 708/QMEE/QMEEdata.RData")
rm(data, data_2)
load(file = "QMEEdata.RData")
source('C:/Users/HP/Desktop/Stats 708/QMEE/Assignment2.R', echo=TRUE)
