#bring in data set
library(readr)
data <- read_csv("FO_TempPlasticitySelection_complete_April2018.csv")

## JD: Usually better practice to comment out View, it's intrusive for a script

## View(data)
#load tidyverse
library(tidyverse)
#Find the average number of days individuals survived within each treatment
(data 
  %>% group_by(treatment)
  %>% summarise(avrg_days = mean(survival)))

## JD: Fairly unambitious calculation, but meets the standard
## JD: score 2. (1=poor, 2=fine, 3=excellent)

