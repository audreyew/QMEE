library(readr)
library(ggplot2)
library(tidyverse)
library(scales)
load(file = "data/QMEEdata.RData")
#data needed some more cleaning and a name change

## JD nice name and clear code.
## If it was me I would space it out for readability, but you probably have better eyes
FlyvsPreds <- (data_2 %>% mutate(temperature=as.factor(temperature), survival = as.factor(survival)))

#Plot1:This plot is trying to show the size differences of the flies that are reared at different temperatures
#I took out data that had no trait or sex identified as these are errors in data entry and including them makes the graph messy and hard to read
## JD: Maybe make this part of the cleaning? And maybe really follow up.

#Since my priority was to look at temperature and size, these are on the main axis 
#sex is separated by colour because this is also known to affect fruit fly size (males are typically smaller)
#for ease of reading, traits are separated and scales are adjusted
(FlyvsPreds
  %>%  filter(trait != "N", sex != "N")
  %>% ggplot(aes(temperature, length_fix, color = sex))
  + geom_boxplot()
  + facet_wrap(vars(trait), scales = "free")
  + xlab("Rearing Temperature")
  + ylab("Trait Length (mm)")
  + scale_color_brewer(palette = "Dark2")
)

#Plot2:How long did individuals survive between the predator and the non-predator treatments? 
#grouping the dataset first should avoid counting individuals more than once (some individuals have the same number id but are in different treatments)
## JD: I'm not exactly following the logic here. How would we count individuals more than once?
(FlyvsPreds
  %>% group_by(individual, cage, sex)
  %>%  ggplot(aes(treatment, survival))
  + geom_count(aes(size = stat(prop), group = treatment))
  + xlab("Treatment")
  + ylab("Survival (days)")
  )
#wanted to put the proportions on the graph so it was easier to tell the values of the points but I couldn't figure out how with my set up
## JD: It seems what you really want is to have survival vs. day
## (two super-important variables with many values)
## You could use color for treatment
## If you _are_ using area for something with a physical flavor (like prop)
## you should figure out how to make the areas proportional (not the default, unfortunately, we can help)

#Plot3:Do individuals that are larger survive for longer?
#filtered to remove data entry errors and grouped to avoid (or try to avoid) counting the same individual twice
#included both plot points and lines to get a better idea of what is going on with the data
## JD: Fixed an apparent typo (trait was here twice, and not sex)
## This is a great example of why you should clean once
(FlyvsPreds
  %>% filter(trait != "N", sex != "N")
  %>% filter(!(trait == "T" & length_fix>=1.5))
  %>% group_by(individual, cage)
  %>% ggplot(aes(length_fix, survival))
  +labs(x= "Length (mm)", y = "Survival (days)")
  + geom_point()
  + geom_smooth(aes(y = as.numeric(survival), color = treatment))
  + facet_wrap(vars(trait), scales="free")
  + scale_color_brewer(palette = "Set3")
  )
#would like to take out those two points on the "T" portion of graph
#tried changing to filter(trait != "N", trait == "T" & length_fix < 1.5) but couldn't figure out how to keep traits "L" and "W".
## JD: Tried that. Also added scales="free" (which I copied from you, although I guess I've seen it before).

## Grade: good (2/3)
