library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(lattice)
library(ordinal)
load(file = "data/QMEEdata.RData")
(Thorax <- filter(FlyvsPreds, trait == "T", sex != "N", length_fix < 1.5))

#trying to see if individual flies that are larger survive more days than smaller flies in the presence of a predator
#using glmer because linear models from previous assignments were no good
survivalmodel <- glmer(survival~length_fix*treatment*sex + (1|cage), data = Thorax, family = binomial)

#fixed effects
  #length_fix; expect that individuals that are larger to live longer (measured as thorax size in mm)
  #treatment; individuals that are in the no predator treatment should live longer than those in the predator treatment
  #sex; sex only has two levels and cannot be treated as a random effect, it is also related to the overall size of the flies (not independent from length_fix)
#random effects
  #cage; this is basically replicates, cages are nested within treatments, treatment with predators has cages 1-4 and treatment without predators has cages 1-4
  #used (1|cage) instead of (1|treatment/cage) because the latter produced a singluar fit
#Maximal Model:
#the maximal model would have been the same as above but used (1|treatment/cage)
plot(survivalmodel)
summary(survivalmodel)
#when looking at the random effects, cage does not account for much of the variance that isn't explained by the fixed effects
plot(survivalmodel, sqrt(abs(resid(.))) ~ fitted(.), 
     type = c("p", "smooth"), col.line = "red")
qqmath(survivalmodel)
#both of these diagnostic plots do not look great
survivalaa <- allFit(survivalmodel)
survivalss <- summary(survivalaa)
survivalss$fixef
#all optimizers that worked gave similar answers
isSingular(survivalmodel)
#plot
(ggplot(Thorax, aes(length_fix, survival, colour =treatment)) +
    facet_wrap(~sex) +
    geom_point() +
    geom_smooth(data = cbind(Thorax, pred = predict(survivalmodel)), aes(y = pred)))
#couldn't get the expand.grid function to work like in the example in class which I am assuming is why my no preditor fits are above the data
#this model still doesn't seem like a good representation of the data
#try doing a mixed model with an ordinal model
ordinalm <- clmm(survival ~ length_fix*treatment*sex + (1|temperature) + (1|cage), data = Thorax)
summary(ordinalm)
coef(summary(ordinalm))
#could not figure out how to do diagnostics