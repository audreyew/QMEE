library(dplyr)
library(ggplot2)
load(file = "data/QMEEdata.RData")
#Hypothesis 1: Individuals that are larger (measured as thorax length in this case) will survive more days.
(Thorax <- subset(FlyvsPreds, trait == "T") 
  %>% mutate(survival = as.numeric(survival)) 
  %>% filter(trait != "N", sex != "N"))
#Length of the thorax will be different between the sexes (not independent), also, whether the flies were kept with a predator or not should have an effect on their survival
SizeSurvivalLM<- lm(survival ~ length_fix*sex + treatment, data = Thorax)
summary(SizeSurvivalLM)
par(mfrow=c(2,2),mar=c(2,3,1.5,1),mgp=c(2,1,0))
plot(SizeSurvivalLM, id.n = 4)
#Residuals vs fitted - with this graph, we want to see no pattern with our values and an average around 0. This would indicate that our model fits the linearity assumption. We do not see that in this case.
#Normal Q-Q - if points are close to the dotted line, the residuals are normally distributed, does not look like this model meets that assumption.
#Scale location - should see a horizontal line to indicate equal variance for all values of the predictor variable. Not seen here.  
#Residuals vs leverage - this graph shows which values have the greatest influence on the model. In this case, there are two points that stand out (282 and 284). 
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

SizeSurvivalLM2<- lm(survival ~ length_fix*sex + treatment, data = Thorax_adjust)
par(mfrow=c(2,2),mar=c(2,3,1.5,1),mgp=c(2,1,0))
plot(SizeSurvivalLM2, id.n = 4)

(ggplot(data = Thorax_adjust, aes(length_fix, survival, color = treatment)) 
  + geom_point()
  + geom_smooth(method = "lm")
  + theme_bw()
  + facet_wrap(vars(sex)))
#still doesn't conform to assumptions after possible outliers are removed

#Hypothesis 2: Flies that are raised at lower temperatures will be larger (measured again as thorax size).
(wingvstemp <- subset(FlyvsPreds, trait == "W")
  %>% mutate(temperature = as.numeric(temperature))
  %>% filter(sex != "N"))
TempLM <- lm(length_fix ~ temperature*sex, data = wingvstemp)
summary(TempLM)
par(mfrow=c(2,2),mar=c(2,3,1.5,1),mgp=c(2,1,0))
plot(TempLM, id.n = 4)
#the diagnostic plots look better with this test, however for all plots other than the Q-Q plot, the values appear to be grouped along certain values of the x-axes and not randomly distributed. 
(ggplot(data = wingvstemp, aes(temperature, length_fix, color = sex)) 
  + geom_point() 
  + geom_smooth(method = "lm"))
#I had to change temperature to a numeric to get the model to work properly but now the x-axis values are off (should be 18, 21, and 24), how would I fix this?
#Overall, I would say that this confroms to the assumptions of linearity, normally distributed residuals, and homoscedasticity.