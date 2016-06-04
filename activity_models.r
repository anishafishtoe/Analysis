#anova

#Is mean crossings normally distributed?
hist(mAct.df$meanCrossings) #--- Nay, it is not!
stripchart(mAct.df$meanCrossings)
mean(mAct.df$meanCrossings)
hist(log(mAct.df$meanCrossings))
shapiro.test(log(mAct.df$meanCrossings))
head(mAct.df)

#cast from long to wide
library(reshape2)
library(dplyr)
mAct.df$habitat=NULL
mAct.df$moonPhase=NULL
mAct.df=mAct.df %>% group_by(phaseRep, fStn) %>% mutate(id=row_number())
mAct.df.wide=dcast(mAct.df,fStn+id~phaseRep+FeedingTrayPosition , value.var = "meanCrossings")

#Create idataframe
df.struc= data.frame(phaseRep=rep(unique(mAct.df$phaseRep), each=2), FeedingTrayPosition=rep(c("F","N"),8))

#Rename column names of mAct.df.wide to match idataframe
colnames(mAct.df.wide)=gsub("_","",colnames(mAct.df.wide))

act.bind=with(mAct.df.wide,cbind(Full1F, Full1N, Full2F, Full2N, New1F, New1N, New2F, New2N, Wane1F, Wane1N, Wane2F, Wane2N, Wax1F, Wax1N, Wax2F, Wax2N))
act.model=lm(act.bind~1)

#23-05-2016: Mixed effect model

#How much variation exists within microhabitat?
boxplot(meanCrossings~FeedingTrayPosition, data=mAct.df)

#How much variation is there between feeding stations?
boxplot(meanCrossings~fStn, data=mAct.df)

#How much variation is there between phase replicates for each moon phase?
par(mfrow=c(1,2))
boxplot(meanCrossings~moonPhase, data=mAct.df[mAct.df$phaseRep %in% c("Wane1", "Wax1","Full1","New1"),], main="Month 1")
boxplot(meanCrossings~moonPhase, data=mAct.df[mAct.df$phaseRep %in% c("Wane2", "Wax2","Full2","New2"),], main="Month 2")
boxplot(meanCrossings~month, data=mAct.df)
boxplot(meanCrossings~month+moonPhase, data=mAct.df)

#full, null and partial mixed effects models without interaction
library(lme4)

act.me.null=lmer(meanCrossings~FeedingTrayPosition+(1|fStn)+(1|month), REML=FALSE, data=mAct.df)
act.me.mp=lmer(meanCrossings~FeedingTrayPosition+moonPhase+(1|fStn)+(1|month), REML=FALSE, data=mAct.df)
act.me.hab=lmer(meanCrossings~FeedingTrayPosition+habitat+(1|fStn)+(1|month), REML=FALSE, data=mAct.df)
act.me.full= lmer(meanCrossings~FeedingTrayPosition+moonPhase+habitat+(1|fStn)+(1|month), REML=FALSE, data=mAct.df)
summary(act.me.null)
summary(act.me.mp)
summary(act.me.hab)
summary(act.me.full)
anova(act.mod, act.me.null, act.me.hab, act.me.mp)

#mixed effects models with random slopes
act.nullr=lmer(meanCrossings~FeedingTrayPosition+(1+moonPhase|fStn)+(1|month), REML=FALSE, data=mAct.df)
act.mpr=lmer(meanCrossings~FeedingTrayPosition+moonPhase+(1+moonPhase|fStn)+(1|month), REML=FALSE, data=mAct.df)
act.habr=lmer(meanCrossings~FeedingTrayPosition+habitat+(1+moonPhase|fStn)+(1|month), REML=FALSE, data=mAct.df)
act.fullr=lmer(meanCrossings~moonPhase+habitat+FeedingTrayPosition + (1+moonPhase|fStn) + (1|phaseRep), REML=FALSE, data=mAct.df)
summary(act.nullr)
summary(act.mpr)
summary(act.habr)
summary(act.fullr)
anova( act.nullr, act.habr, act.mpr, act.fullr)


#mixed effects models with interaction
act.int.null=lmer(meanCrossings~FeedingTrayPosition+(1|fStn), REML=FALSE, data=mAct.df)
act.int.mp=lmer(meanCrossings~FeedingTrayPosition*moonPhase+(1|fStn), REML=FALSE, data=mAct.df)
act.int.hab=lmer(meanCrossings~FeedingTrayPosition*habitat+(1|fStn), REML=FALSE, data=mAct.df)
act.int.full=lmer(meanCrossings~moonPhase*habitat*FeedingTrayPosition + (1|fStn), REML=FALSE, data=mAct.df)
summary(act.int.null)
summary(act.int.mp)
summary(act.int.hab)
summary(act.int.full)
anova( act.int.null, act.int.hab, act.int.mp, act.int.full)

#multiple models with  random intercept 
act.int.nullr=lmer(meanCrossings~1+(1|fStn), REML=FALSE, data=mAct.df)
microhabitat=lmer(meanCrossings~FeedingTrayPosition+(1|fStn), REML=FALSE, data=mAct.df)
habitat=lmer(meanCrossings~habitat+(1|fStn), REML=FALSE, data=mAct.df)
moonPhase=lmer(meanCrossings~moonPhase+(1|fStn), REML=FALSE, data=mAct.df)
month=lmer(meanCrossings~month+(1|fStn), REML=FALSE, data=mAct.df)
#habitat.microhab=lmer(meanCrossings~FeedingTrayPosition+habitat+(1|fStn), REML=FALSE, data=mAct.df)
#microhab.moonph=lmer(meanCrossings~FeedingTrayPosition+moonPhase+(1|fStn), REML=FALSE, data=mAct.df)
#habitat.moonph=lmer(meanCrossings~habitat+moonPhase+(1|fStn), REML=FALSE, data=mAct.df)
#habitat.microhab.moonph=lmer(meanCrossings~FeedingTrayPosition+habitat+moonPhase+(1|fStn), REML=FALSE, data=mAct.df)
#habitat.microhab.moonph.month=lmer(meanCrossings~FeedingTrayPosition+habitat+moonPhase+month+(1|fStn), REML=FALSE, data=mAct.df)
#microhab.moonphase.int=lmer(meanCrossings~FeedingTrayPosition*moonPhase+(1|fStn), REML=FALSE, data=mAct.df)
moonPh.mon=lmer(meanCrossings~moonPhase*month+(1|fStn), REML=FALSE, data=mAct.df)
habitat.mon=lmer(meanCrossings~habitat*month+(1|fStn), REML=FALSE, data=mAct.df)
habitat.microhab.int=lmer(meanCrossings~FeedingTrayPosition*habitat+(1|fStn), REML=FALSE, data=mAct.df)
hab.microhab.mon.int=lmer(meanCrossings~FeedingTrayPosition*habitat*month+(1|fStn), REML=FALSE, data=mAct.df)
habitat.moonph.int=lmer(meanCrossings~moonPhase*habitat+(1|fStn), REML=FALSE, data=mAct.df)
hab.moonph.mon.int=lmer(meanCrossings~moonPhase*habitat*month+(1|fStn), REML=FALSE, data=mAct.df)
hab.moonph.microhab=lmer(meanCrossings~FeedingTrayPosition*habitat*moonPhase+(1|fStn), REML=FALSE, data=mAct.df)
full.int=lmer(meanCrossings~FeedingTrayPosition*habitat*moonPhase*month+(1|fStn), REML=FALSE, data=mAct.df)

anova(act.int.nullr, microhabitat,habitat, moonPhase, month, habitat.microhab.int,habitat.mon,
      hab.microhab.mon.int, habitat.moonph.int,  hab.moonph.mon.int, hab.moonph.microhab, int.full)

summary(act.int.nullr)
summary(microhabitat)
summary(habitat.microhab)
summary(microhab.moonph)
summary(habitat.microhab.moonph)
summary(habitat.microhab.moonph.month)
summary(int.full)

#p-values for fixed effects
library(car)
full.model=lmer(log(meanCrossings)~FeedingTrayPosition+habitat+moonPhase+month+
                  FeedingTrayPosition*habitat+ moonPhase*habitat+habitat*month+
                  habitat*moonPhase*month+
                  habitat*FeedingTrayPosition*month+
                  FeedingTrayPosition*moonPhase*habitat+
                    FeedingTrayPosition*moonPhase*habitat*month+  
                  (1|fStn), data=mAct.df, REML=FALSE)
Anova(full.model)


#GLMM
act.int.nullr=lmer(meanCrossings~1+(1|fStn), REML=FALSE, data=mAct.df, family="poisson")
microhabitat=lmer(meanCrossings~FeedingTrayPosition+(1|fStn), REML=FALSE, data=mAct.df)
habitat=lmer(meanCrossings~habitat+(1|fStn), REML=FALSE, data=mAct.df)
moonPhase=lmer(meanCrossings~moonPhase+(1|fStn), REML=FALSE, data=mAct.df)
month=lmer(meanCrossings~month+(1|fStn), REML=FALSE, data=mAct.df)
#habitat.microhab=lmer(meanCrossings~FeedingTrayPosition+habitat+(1|fStn), REML=FALSE, data=mAct.df)
#microhab.moonph=lmer(meanCrossings~FeedingTrayPosition+moonPhase+(1|fStn), REML=FALSE, data=mAct.df)
#habitat.moonph=lmer(meanCrossings~habitat+moonPhase+(1|fStn), REML=FALSE, data=mAct.df)
#habitat.microhab.moonph=lmer(meanCrossings~FeedingTrayPosition+habitat+moonPhase+(1|fStn), REML=FALSE, data=mAct.df)
#habitat.microhab.moonph.month=lmer(meanCrossings~FeedingTrayPosition+habitat+moonPhase+month+(1|fStn), REML=FALSE, data=mAct.df)
#microhab.moonphase.int=lmer(meanCrossings~FeedingTrayPosition*moonPhase+(1|fStn), REML=FALSE, data=mAct.df)
moonPh.mon=lmer(meanCrossings~moonPhase*month+(1|fStn), REML=FALSE, data=mAct.df)
habitat.mon=lmer(meanCrossings~habitat*month+(1|fStn), REML=FALSE, data=mAct.df)
habitat.microhab.int=lmer(meanCrossings~FeedingTrayPosition*habitat+(1|fStn), REML=FALSE, data=mAct.df)
hab.microhab.mon.int=lmer(meanCrossings~FeedingTrayPosition*habitat*month+(1|fStn), REML=FALSE, data=mAct.df)
habitat.moonph.int=lmer(meanCrossings~moonPhase*habitat+(1|fStn), REML=FALSE, data=mAct.df)
hab.moonph.mon.int=lmer(meanCrossings~moonPhase*habitat*month+(1|fStn), REML=FALSE, data=mAct.df)
hab.moonph.microhab=lmer(meanCrossings~FeedingTrayPosition*habitat*moonPhase+(1|fStn), REML=FALSE, data=mAct.df)
full.int=lmer(meanCrossings~FeedingTrayPosition*habitat*moonPhase*month+(1|fStn), REML=FALSE, data=mAct.df)

anova(act.int.nullr, microhabitat,habitat, moonPhase, month, habitat.microhab.int,habitat.mon,
      hab.microhab.mon.int, habitat.moonph.int,  hab.moonph.mon.int, hab.moonph.microhab, int.full)
