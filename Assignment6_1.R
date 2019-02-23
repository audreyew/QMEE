library(dplyr)
library(ggplot2)
L <- load(file = "data/QMEEdata.RData")
## BMB: thanks for save/load

## BMB: I had to reconstruct this
FlyvsPreds <- (data_2 %>% mutate(temperature=as.factor(temperature), survival = as.factor(survival)))
#Hypothesis 1: Individuals that are larger (measured as thorax length in this case) will survive more days.
(Thorax <- subset(FlyvsPreds, trait == "T")   ## BMB: maybe use filter() [stay in tidyverse]?
  %>% mutate(survival = as.numeric(survival)) 
  %>% filter(trait != "N", sex != "N"))
##Length of the thorax will be different between the sexes (not independent), also, whether the flies were kept with a predator or not should have an effect on their survival

## BMB: can you be more precise?  "has an effect" is usually trivial. (It's biology, we know already that everything is different from everything else and everything has an effect on everything -- we just don't know how *big* the differences/effects are and whether we can distinguish the sign of effects. Can you make a *directional* prediction?

## BMB: why did you decide to make treatment additive (i.e. not length_fix*sex*treatment) ?
SizeSurvivalLM<- lm(survival ~ length_fix*sex + treatment, data = Thorax)
summary(SizeSurvivalLM)
par(mfrow=c(2,2),mar=c(2,3,1.5,1),mgp=c(2,1,0))
plot(SizeSurvivalLM, id.n = 4)

#Residuals vs fitted - with this graph, we want to see no pattern with our values and an average around 0. This would indicate that our model fits the linearity assumption. We do not see that in this case.
#Normal Q-Q - if points are close to the dotted line, the residuals are normally distributed, does not look like this model meets that assumption.
#Scale location - should see a horizontal line to indicate equal variance for all values of the predictor variable. Not seen here.  
                                        #Residuals vs leverage - this graph shows which values have the greatest influence on the model. In this case, there are two points that stand out (282 and 284).

## BMB: yeah, this is pretty bad. Can you remind me what Thorax$survival is? Number of
## days survived?

## try some GLMs
m2 <- MASS::glm.nb(survival ~ length_fix*sex + treatment, data = Thorax)
m3 <- glm(survival ~ length_fix*sex + treatment, data = Thorax, family=poisson)
## still weird.

## ordinal model?
m4 <- MASS::polr(ordered(survival) ~ length_fix*sex + treatment, data = Thorax)
## but diagnostics are harder ...

(ggplot(data = Thorax, aes(length_fix, survival, color = treatment)) 
  + geom_point()
  + geom_smooth(method = "lm")
  + theme_bw()
  + facet_wrap(vars(sex)))
#from this, I would say that this does not conform to the assumptions of linearity, normally distributed residuals and homoscedasticity. 

#Seeing what the plots would look like if we removed those two points that have a length_fix > 1.5.
(Thorax_adjust <- subset(FlyvsPreds, trait == "T") 
  %>% mutate(survival = as.numeric(survival)) 
  %>% filter(trait != "N", length_fix < 1.5, sex != "N"))

## BMB: note that these plots correspond to the full interaction model,
##  since it fits a different linear reg for each combination of factors

SizeSurvivalLM2<- lm(survival ~ length_fix*sex + treatment, data = Thorax_adjust)
par(mfrow=c(2,2),mar=c(2,3,1.5,1),mgp=c(2,1,0))
plot(SizeSurvivalLM2, id.n = 4)

(ggplot(data = Thorax_adjust, aes(length_fix, survival, color = treatment)) 
  + geom_point()
  + geom_smooth(method = "lm")
  + theme_bw()
  + facet_wrap(~sex)
##still doesn't conform to assumptions after possible outliers are removed
## BMB: yes, but much more sensible!
## 

#Hypothesis 2: Flies that are raised at lower temperatures will be larger (measured again as thorax size).
    (wingvstemp <- subset(FlyvsPreds, trait == "W")
        %>% mutate(ftemp=temperature,  ## save factor version
                   ## BMB: need as.numeric(as.character(temperature)) to convert
                   temperature = as.numeric(as.character(temperature)))
        %>% filter(sex != "N"))
TempLM <- lm(length_fix ~ temperature*sex, data = wingvstemp)
summary(TempLM)
par(mfrow=c(2,2),mar=c(2,3,1.5,1),mgp=c(2,1,0))
plot(TempLM, id.n = 4)
    ## the diagnostic plots look better with this test, however for all plots other than the Q-Q plot, the values appear to be grouped along certain values of the x-axes and not randomly distributed.
    ## BMB: that's an inevitable consequence of the fact that there are only 6 (2*3) unique
    ## values of the predictor variables ... it's not a problem (although it means it might
    ## be better to use boxplots [which isn't trivial]
    with(wingvstemp,table(temperature,sex))
(ggplot(data = wingvstemp, aes(temperature, length_fix, color = sex)) 
  + geom_point() 
  + geom_smooth(method = "lm"))
    ##I had to change temperature to a numeric to get the model to work properly but now the x-axis values are off (should be 18, 21, and 24), how would I fix this? BMB: see above
    ## BMB: why do you say that temperature must be numeric for the model to work properly?
    ##  if you make temperature numeric, you're *assuming* exactly equal differences between successive temperature -- seems like a strong assumption to make
    ##Overall, I would say that this conforms to the assumptions of linearity, normally distributed residuals, and homoscedasticity.
(ggplot(data = wingvstemp, aes(ftemp, length_fix, color = sex)) 
    + geom_boxplot(notch=TRUE)
    + geom_smooth(method="lm",
                  aes(group=sex)))

    ##
## BMB: score 2.5
