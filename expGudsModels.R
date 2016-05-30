#REF models for GUDS of tree cutting experiment

library(lme4)
library(car)

full.model=lmer(data=expGuds, GUD~BefOnAft+NF+cutUncut+(1|fStn), REML=FALSE)
Anova(full.model)

dayofCutting=lmer(data=expGuds, GUD~BefOnAft +(1|fStn), REML=FALSE)
summary(dayofCutting)
Anova(dayofCutting)


a=aov(data=expGuds, GUD~BefOnAft)
TukeyHSD(a)

library(lmerTest)
