#bring in data set
library(readr)
data <- read_csv("FO_TempPlasticitySelection_complete_April2018.csv")
View(data)
#load tidyverse
library(tidyverse)
#Find the average number of days individuals survived within each treatment
(data 
  %>% group_by(treatment)
  %>% summarise(avrg_days = mean(survival)))