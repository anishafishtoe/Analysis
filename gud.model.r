#mixed effects models
library(lme4)
library(car)

#Are GUDS normal?
hist(gMean$meanGUD, main="", xlab="Mean GUD")
hist(log(gMean$meanGUD))
#main effects, no interaction

microhabitat=lmer(meanGUD~FeedingTrayPosition+(1|fStn), REML=FALSE, data=gMean, family="poisson")
habitat=lmer(meanGUD~habitat+(1|fStn), REML=FALSE, data=gMean)
moonPhase=lmer(meanGUD~moonPhase+(1|fStn), REML=FALSE, data=gMean)
month=lmer(meanGUD~month+(1|fStn), REML=FALSE, data=gMean)


#Full model and significance testing for fixed effects
full.model=lmer(meanGUD~feedingTrayPosition+habitat+moonPhase+month+feedingTrayPosition*habitat+feedingTrayPosition*moonPhase+moonPhase*habitat+moonPhase*month +habitat*month + feedingTrayPosition*month +feedingTrayPosition*habitat*month+habitat*moonPhase*month+feedingTrayPosition*moonPhase*habitat+(1|fStn), data=gMean, REML=FALSE)
Anova(full.model)
