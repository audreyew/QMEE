library(dplyr)
library(tidyverse)
library(gtools)
library(lmPerm)

load(file = "data/QMEEdata.RData")

#Hypothesis one:
#The temperature at which the flies were reared has an effect on their adult size.
#Since this is a larger dataset, we will subset for thorax size and use that.
## JD: I think `filter` is "tidier" than subset
TvsTemp <- data_2 %>% subset(trait == "T")

#randomly assign the three temperature categories to thorax size
#for this I tried to use the same code for the second hypothesis below but knew I needed to change "res[i]" and the calculation for my observed values
## JD: Not really following

#I tried to calculate the F statistic for this but I couldn't figure out how to do it without just using the anova function. 
## JD: Nothing wrong with that -- but aov is kind of a weird function, might have been better with anova
summary(lmp(length_fix~temperature, data = TvsTemp))

anova <- aov(length_fix ~ temperature, data = TvsTemp)
summary(anova)

#Hypothesis two:
#The flies that were contained within the treatment that included a predator, survived for a shorter period.
#this time we will randomly assign survival times between the two survival treatments and compare this to our observed values. 
set.seed(500)
numsims <- 1000
res2 <- numeric(numsims)
for (i in 1:numsims) {
  perm <- sample(nrow(TvsTemp))
  pdat2 <- transform(TvsTemp, survival = survival[perm])
  res2[i] <- mean(pdat2[pdat2$treatment=="NP", "survival"]) - 
    mean(pdat2[pdat2$treatment=="P", "survival"])
}
TNP <- subset(TvsTemp, treatment == "NP", na.rm = TRUE)
TP <- subset(TvsTemp, treatment == "P", na.rm = TRUE)
observed2 <- mean(TNP$survival) - mean(TP$survival)
#I know that this is not a clean as one line of code but kept getting error message
#"argument is not numeric or logical: returning NA" even though survival is numeric when I examine the structure of the data
res2 <- c(res2,observed2)
hist(res2, col = "gray", las = 1, main="")
abline(v=observed2, col = "red")
pval2 <- 2*mean(res2 >= observed2)
pval2
ttest <- t.test(survival~treatment, data=TvsTemp, var.equal=TRUE)
ttest

## JD: Grade 2/3 (good)
