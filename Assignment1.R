#bring in data set
download.file(url = "https://raw.githubusercontent.com/audreyew/QMEE/master/FO_TempPlasticitySelection_complete_April2018.csv",
              destfile = "data/FO_TempPlasticitySelection_complete_April2018.csv")
library(readr)
data <- read_csv("FO_TempPlasticitySelection_complete_April2018.csv")
View(data)
#load tidyverse
library(tidyverse)
#Find the average number of days individuals survived within each treatment
(data 
  %>% group_by(treatment)
  %>% summarise(avrg_days = mean(survival)))
