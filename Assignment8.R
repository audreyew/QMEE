library(dplyr)
library(ggplot2)
library(MASS)
library(PResiduals)
load(file = "data/QMEEdata.RData")
#what we are looking for:
#Do flies that are larger live longer than smaller flies when exposed to a predator?
#back in assignment 6 when looking at this we removed the outliers and got somewhat better output, for this we will remove them again
(Thorax_adjust <- filter(FlyvsPreds, trait == "T", sex != "N", length_fix < 1.5))
ggplot(Thorax_adjust, aes(length_fix, survival, colour = treatment)) +
  geom_point(aes(alpha = 0.5)) +
  facet_wrap(~sex) + 
  scale_color_brewer(palette = "Dark2") +
    scale_x_log10()

## BMB: would boxplots be better? need ggstance for horiz boxplots
## (can't use coord_flip() + faceting)
library(ggstance)
ggplot(Thorax_adjust, aes(length_fix, as.numeric(as.character(survival)),
                          fill = treatment, colour = treatment)) +
  geom_boxploth(alpha = 0.5, aes(group=interaction(survival,treatment))) +
    facet_wrap(~sex) + 
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    geom_smooth() +
    scale_y_continuous(limits=c(0.5,6.5),oob=scales::squish)
## BMB: "oob" means "out-of-bounds", i.e. what to do with stuff
##  that falls outside of the specified limits.  Used it to squash
##  the confidence intervals for the smooths
##     scale_x_log10()
## also not really clear that log10 does much here
##  (lengths are far from zero and don't vary that much)
##  BMB: might want to look at a quadratic effect of length?
##  although maybe the Female/P-treatment curve is sensitive to the several
## very small individuals who died quickly?

#still not seeing anything interesting but let's explore more
#survival is measured as number of days that an individual survived to a maximum of 6 days. 
SizeSurvivalGLM <- glm(survival ~ length_fix*sex*treatment, data = Thorax_adjust, family = binomial(link = "logit"))

## BMB: this is interesting. A  binomial might not be the best model.
## (Not unreasonable of you to pick it though).  It doesn't really account for
## the fact that you can only die once ...
## what sets the 6-day maximum?  Is this just when the experiment ended?
## If so you probably are closest to having a *censored negative binomial*
## distribution (a NB is a useful basic discrete-time survival model)

par(mfrow=c(2,2),mar=c(2,3,1.5,1),mgp=c(2,1,0))
plot(SizeSurvivalGLM)
summary(SizeSurvivalGLM)
#residual vs fitted plot makes it look like there is something else going on with the individuals that are not living very long
(Thorax_adjust2 <- filter(FlyvsPreds, trait == "T", sex != "N", length_fix < 1.5) %>% mutate(survival = as.numeric(as.character(survival))))
(ggplot(Thorax_adjust2, aes(length_fix, survival)) + 
         geom_point() +
    facet_wrap(~sex) +
    geom_smooth(method = "glm", colour = "purple",
                method.args = list(family = poisson(link = "log"))) +
    scale_x_log10())
#so here I changed survival so I could get the line across all categories of survival and not a line within each group
#(ie. before this I was getting six trendlines for each graph)
##but now the binomial family does not work but the poisson does. I am not sure why this is.
## BMB: binomial would be a little tricky to squeeze in here ...


OrderedSizeSurvivalGLM <- polr(survival ~ length_fix*sex*treatment, data = Thorax_adjust)
summary(OrderedSizeSurvivalGLM)
table <- coef(summary(OrderedSizeSurvivalGLM))
p <- pnorm(abs(table[,"t value"]), lower.tail = FALSE) *2
fulltable <- cbind(table, "pvalue" = p)
fulltable
presiduals <- presid(OrderedSizeSurvivalGLM)
ResidFit <- ggplot(data.frame(x = Thorax_adjust$length_fix, y = presiduals), aes(x, y)) +
  geom_point() +
  geom_smooth(colour = "red", se = FALSE)
ResidFit #If I did this correctly this should be the residuals vs. fitted plot
## BMB: I think that's right ...
## I think you might have complete separation in this problem (because
## you have some very large parameter values - e.g. sexM is 27.8.
## definitely worth considering working further in this direction ...
#In assignment 6, Ben was playing with my data and used the polr function so I thought I would try it 
#I then went on some forums to try to figure out how to get p-values and diagnostics from this

## BMB: score 2.75
