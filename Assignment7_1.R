library(rjags)
library(R2jags)
library(dplyr)
library(coda)
library(tidyr)
library(arm)
library(emdbook)
library(lattice)
library(dotwhisker)
library(broom.mixed)

load(file = "data/QMEEdata.RData")
#look at how wing size changes with the temperature flies were raised at using Bayesian model
(wingtemp <- subset(FlyvsPreds, trait == "W")
  %>% filter(sex != "N")
  %>% mutate(temperature=factor(temperature,levels=c("18","21","24")))
  %>% mutate_if(is.character,factor)
)

named_list <- function (...) 
{
  L <- list(...)
  snm <- sapply(substitute(list(...)), deparse)[-1]
  if (is.null(nm <- names(L))) 
    nm <- snm
  if (any(nonames <- nm == "")) 
    nm[nonames] <- snm[nonames]
  setNames(L, nm)
}
wingdata <- with(wingtemp,
                 named_list(N = nrow(wingtemp),
                            ntemp = length(levels(temperature)),
                            temperature = as.numeric(temperature),
                            sex = as.numeric(sex),
                            length_fix))
tempsexmodel <- function(){
  for (i in 1:N) {
    logmean0[i] <- b_temp[temperature[i]]
    sexeff[i] <- b_sex*(sex[i] -1)
    pred[i] <- exp(logmean0[i] + sexeff[i]) #overall size of the flies has been shown to change with temperature and there is going to be a size difference depending on the sex
    length_fix[i] ~dnorm(pred[i], 0.001) #length_fix is a continuous variable of the length of the flies wings measured in mm
  }
  for (i in 1:ntemp) {
    b_temp[i] ~ dnorm(0,0.001) 
  }
  b_sex ~dnorm(0, 0.001) 
}
#leaving the priors as non-informative
j1 <- jags(data = wingdata,
           inits = NULL,
           n.chains = 4,
           #n.iter = 5000,
           #n.burnin = 1000,
           parameters = c("b_temp", "b_sex"),
           model.file = tempsexmodel)
tidy(j1, conf.int = TRUE, conf.method = "quantile")
#large confidence intervals but do not contain 0
bb <- j1$BUGSoutput
mm <- as.mcmc.bugs(bb)
plot(j1) 
xyplot(mm, layout = c(2,3))
densityplot(mm, layout=c(2,3))
#sex look alright, but the temperatures are a bit skewed
#tried using n.iter and n.burnin to see if it changed but did not change the output
print(dwplot(j1))

#how does this change if we give our priors a bit more info?
tempsexmodel2 <- function(){
  for (i in 1:N) {
    logmean0[i] <- b_temp[temperature[i]]
    sexeff[i] <- b_sex*(sex[i] -1)
    pred[i] <- exp(logmean0[i] + sexeff[i])
    length_fix[i] ~dnorm(pred[i], 0.001) 
  }
  for (i in 1:ntemp) {
    b_temp[i] ~ dnorm(1,0.001) 
  }
  b_sex ~dnorm(0, 0.001) 
}
j2 <- jags(data = wingdata,
           inits = NULL,
           n.chains = 4,
           parameters = c("b_temp", "b_sex"),
           model.file = tempsexmodel2)
tidy(j2, conf.int = TRUE, conf.method = "quantile")
#values are similar to what we saw before with less informative priors
bb2 <- j2$BUGSoutput
mm2 <- as.mcmc.bugs(bb2)
plot(j2) 
xyplot(mm2, layout = c(2,3))
densityplot(mm2, layout=c(2,3))

#compare to frequentist
freqtempsexmod <- lm(length_fix~temperature + sex, data=wingtemp)
par(mfrow=c(2,2),mar=c(2,3,1.5,1),mgp=c(2,1,0))
plot(freqtempsexmod)
summary(freqtempsexmod)
#models show quite different results