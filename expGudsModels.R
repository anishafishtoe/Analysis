expGudMean=read.csv("expGudsMean.csv")

library(lme4)
library(car)
library(multcomp)

full.model.cut=lmer(data=subset(expGuds, cutUncut=="Cut"), GUD~BefOnAft+NF+BefOnAft*NF+(1|fStn), REML=FALSE)
Anova(full.model.cut)
full.model.uncut=lmer(data=subset(expGuds, cutUncut=="Uncut"), GUD~BefOnAft+NF+BefOnAft*NF+(1|fStn), REML=FALSE)
Anova(full.model.uncut)

#Separate models for cut and uncut trees
full.model.cut=lmer(data=subset(expGuds, cutUncut=="Cut"), GUD~BefOnAft+NF+(1|fStn), REML=FALSE)
Anova(full.model.cut)
full.model.uncut=lmer(data=subset(expGuds, cutUncut=="Uncut"), GUD~BefOnAft+NF+(1|fStn), REML=FALSE)
Anova(full.model.uncut)

#Compare activity with time of tree cutting
summary(glht(full.model.cut, mcp(BefOnAft="Tukey")))
summary(glht(full.model.uncut, mcp(BefOnAft="Tukey")))


dayofCutting=lmer(data=expGuds, GUD~BefOnAft +(1|fStn), REML=FALSE)
summary(dayofCutting)
Anova(dayofCutting)

#--13-06---

#How is the response variable distributed?
hist(expGuds$GUD)

#Which distribution fits the data?
qqp(expGuds$GUD, "norm")
qqp(expGuds$GUD, "lnorm")
nbinom=fitdistr(expGuds$GUD,"Negative Binomial")
qqp(expGuds$GUD,"nbinom", size= nbinom$estimate[[1]], mu=nbinom$estimate[[2]])
poisson=fitdistr(expGuds$GUD, "Poisson")
qqp(expGuds$GUD, "pois", poisson$estimate)
gamma=fitdistr(expGuds$GUD, "gamma")
qqp(expGuds$GUD, "gamma", shape=gamma$estimate[[1]], rate=gamma$estimate[[2]])

#lmm
gud.full=lmer(data=expGuds, GUD~BefOnAft+NF+BefOnAft+cutUncut+BefOnAft*cutUncut*NF+(1|fStn))
Anova(gud.full)
gud.BefOnAft=lmer(data=expGuds, GUD~BefOnAft+(1|fStn), REML=FALSE)
gud.cutUncut=lmer(data=expGuds, GUD~cutUncut+(1|fStn), REML=FALSE)
gud.TimeCut=lmer(data=expGuds, GUD~BefOnAft*cutUncut+(1|fStn), REML=FALSE)
gud.cutNF=lmer(data=expGuds, GUD~cutUncut*NF+(1|fStn), REML=FALSE)
gud.3way=lmer(data=expGuds, GUD~cutUncut*NF+BefOnAft*NF*cutUncut+(1|fStn), REML=FALSE)
anova(gud.BefOnAft, gud.cutUncut, gud.TimeCut, gud.cutNF, gud.3way)

#post hoc test
require(lsmeans)
  #interaction between time of cutting and cut vs uncut
  lsm=lsmeans(gud.TimeCut, ~BefOnAft*cutUncut)
  summary(lsm, type="response")

  #3way
  lsm.3way=lsmeans(gud.3way, ~BefOnAft*cutUncut*NF)
  summary(lsm.3way, type="response")
  
#1806
gud.full=lmer(data=expGudMean, meanGUD~BefOnAft+cutUncut+FeedingTrayPosition+(1|fStn))
plot(gud.full)
qqnorm(residuals(gud.full))
qqline(residuals(gud.full))
Anova(gud.full)

gud.aov=aov(data=expGudMean, meanGUD~cutUncut+BefOnAft+FeedingTrayPosition+cutUncut:BefOnAft +
              cutUncut:FeedingTrayPosition+FeedingTrayPosition:BefOnAft+
              cutUncut:FeedingTrayPosition:BefOnAft+Error(fStn))


expG.full=lmer(data=expGudMean, meanGUD~cutUncut+BefOnAft+FeedingTrayPosition+cutUncut:BefOnAft +
                       cutUncut:FeedingTrayPosition+FeedingTrayPosition:BefOnAft+
                 FeedingTrayPosition:BefOnAft:cutUncut+
                     (1|fStn))
expG.additive=lmer(data=expGudMean, meanGUD~cutUncut+BefOnAft+FeedingTrayPosition+cutUncut:BefOnAft+(1|fStn), REML=FALSE)
expG.no3way=update(expG.full,.~.-cutUncut:FeedingTrayPosition:BefOnAft,REML=FALSE)
expG.noMicroTime=update(expG.no3way,.~.-FeedingTrayPosition:BefOnAft,REML=FALSE)
expG.noCutMicro=update(expG.noMicroTime,.~.-cutUncut:FeedingTrayPosition,REML=FALSE)
expG.noCutTime=update(expG.noCutMicro,.~.-cutUncut:BefOnAft,REML=FALSE)
expG.noFtray=update(expG.noCutTime,.~.-FeedingTrayPosition,REML=FALSE)
expG.noTime=update(expG.noFtray,.~.-BefOnAft,REML=FALSE)
expG.noCut=update(expG.noFtray,.~.-cutUncut,REML=FALSE)
summary(expG.noCut)

anova(expG.no3way, expG.full)
anova(expG.noMicroTime, expG.no3way)
anova(expG.noCutMicro, expG.noMicroTime)
anova(expG.noCutTime, expG.noCutMicro)
anova(expG.noTime, expG.noFtray)
anova(expG.noCut, expG.noFtray)

cutTime=lsmeans(expG.additive, c("BefOnAft","cutUncut"))
treatT.letters=cld(cutTime, Letters=letters)
lsmeans(expG.full, pairwise~cutUncut:BefOnAft) #pairwise results
  