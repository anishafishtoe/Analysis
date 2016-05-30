#mixed effects models
library(lme4)
library(car)

#habitat


#moon phase

#microhabitat

#month


#Full model and significance testing for fixed effects
full.model=lmer(meanGUD~feedingTrayPosition+habitat+moonPhase+month+feedingTrayPosition*habitat+feedingTrayPosition*moonPhase+moonPhase*habitat+moonPhase*month +habitat*month + feedingTrayPosition*month +feedingTrayPosition*habitat*month+habitat*moonPhase*month+feedingTrayPosition*moonPhase*habitat+(1|fStn), data=gMean, REML=FALSE)
Anova(full.model)
