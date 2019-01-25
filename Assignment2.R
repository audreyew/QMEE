library(readr)
library(tidyverse)
## BMB: had to edit -- this wasn't in a data/ subdirectory ...
data <- read_csv("FO_TempPlasticitySelection_complete_April2018.csv")

## BMB: see assignment checklist. Comment out View() etc;
##  don't call your data "data"
## View(data)
## str(data)
## summary(data)
(data
  %>% group_by(individual, temperature, trait, cage)
    %>% summarize(count=n()))

## BMB: or ...
with(data,table(individual, temperature, trait, cage))

#plots
ggplot(data, aes(x=survival)) + geom_histogram()
##survival can only be of value 0-6. Graph shows that.

stopifnot(all(data$survival %in% 1:6))

ggplot(data, aes(x = individual, y = Length, colour = trait)) + geom_point()
##some traits have lengths that are much too large. Flies would not have wings that are over 900mm.
## BBB ?? What's going here?  Why are you multiplying by 0.00186?
## were these measurements recorded in the wrong units, or what ??
## you may know what's going on, but I don't, and you might not
## remember 6 months from now ...

## if you're sure about this change, then you might just want
##  to replace the 'length' variable
data_2 <- mutate(data,
                 length_fix = if_else(Length > 100, Length * 0.00186, Length))
ggplot(data_2, aes(x = individual, y = length_fix, colour = trait)) + geom_point()

## BMB: no absolute path names please!!!
## save(data, data_2, file = "C:/Users/HP/Desktop/Stats 708/QMEE/QMEEdata.RData")
save(data, data_2, file = "QMEEdata.RData")
rm(data, data_2) 
load(file = "QMEEdata.RData")
## BMB: no absolute pathnames please!
## source('C:/Users/HP/Desktop/Stats 708/QMEE/Assignment2.R', echo=TRUE)
