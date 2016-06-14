library(lme4)
library(car)


setwd("F:/NCBS/Thesis/Data/")
activity=read.csv("activityClean.csv")
mAct.df=read.csv("meanActivity.csv")

#which distribution fits the data? 
qqp(mAct.df$meanCrossings, "norm")
qqp(mAct.df$meanCrossings, "lnorm")
nbinom=fitdistr(mAct.df$meanCrossings,"Negative Binomial")
qqp(mAct.df$meanCrossings,"nbinom", size= nbinom$estimate[[1]], mu=nbinom$estimate[[2]])
poisson=fitdistr(mAct.df$meanCrossings, "Poisson")
qqp(mAct.df$meanCrossings, "pois", poisson$estimate)
gamma=fitdistr(mAct.df$meanCrossings, "gamma")
qqp(mAct.df$meanCrossings, "gamma", shape=gamma$estimate[[1]], rate=gamma$estimate[[2]])


#Is mean crossings normally distributed?
hist(mAct.df$meanCrossings) #--- Nay, it is not!
stripchart(mAct.df$meanCrossings)
mean(mAct.df$meanCrossings)
hist(log(mAct.df$meanCrossings))
shapiro.test(log(mAct.df$meanCrossings))
head(mAct.df)


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
full.model=lmer(meanCrossings~FeedingTrayPosition+habitat+moonPhase+month+
                  FeedingTrayPosition*habitat+ moonPhase*habitat+habitat*month+
                  habitat*moonPhase*month+
                  habitat*FeedingTrayPosition*month+
                  FeedingTrayPosition*moonPhase*habitat+
                    FeedingTrayPosition*moonPhase*habitat*month+  
                  (1|fStn), data=mAct.df, REML=FALSE)
Anova(full.model)
summary(full.model)

#summary statistics 
act.hab=with(mAct.df, describeBy(meanCrossings, habitat, mat=T, digits=4))
summary(act.hab)
act.hab$mean

act.microhab=with(mAct.df, describeBy(meanCrossings, FeedingTrayPosition, mat=T, digits=4))

act.moonphase=with(mAct.df, describeBy(meanCrossings, moonPhase, mat=T, digits=4))

#Tukey's test
library(multcomp)
model.additive=lmer(meanCrossings~FeedingTrayPosition+habitat+moonPhase+month+(1|fStn), data=mAct.df, REML=FALSE)
Anova(model.additive)

summary(glht(model.additive, mcp(moonPhase="Tukey")))

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



#09-06
library(nlme)

model.gls= gls(data= activity, log(Ncrossing)~MoonPhase*Habitat*NF*month)

full.model3=lme(data=activity, log(Ncrossing)~ MoonPhase*Habitat*NF*month , random= ~1|FStn, method="REML")
full.model4=lme(data=activity, log(Ncrossing)~ MoonPhase*Habitat*NF*month , random= ~1|MoonPhase/Night, , method="REML")
plot(full.model3)
#check gls vs lmer
anova(model.gls, full.model3, full.model4) #model with random intercept is better

res= resid(full.model3, type="normalized")
fit= fitted(full.model3)
op <- par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))
plot(x=fit, y=res, xlab="Fitted", ylab="Residuals")
boxplot(res~MoonPhase, data= activity, ylab="Residuals")
boxplot(res~Habitat, data= activity, ylab="Residuals")
boxplot(res~NF, data= activity, ylab="Residuals")
boxplot(res~month, data= activity, ylab="Residuals")

#GLMM

full.model.glmm=glmer(data=activity, Ncrossing~ MoonPhase*Habitat*NF*month +(1|FStn),
                     family="poisson")
#Test for over-dispersion
mq1= update(full.model.glmm, family="quasipoisson")
(phi=lme4:::sigma(mq1))


#-------------------------------13-06------------------------
#LMER full model 
lm.full=lmer(data=mAct.df, meanCrossings~ moonPhase*habitat*FeedingTrayPosition*month +(1|fStn))
plot(lm.full)
  #transform response
  lm.fullT=lmer(data=mAct.df, log(meanCrossings)~ moonPhase*habitat*FeedingTrayPosition*month +(1|fStn))
  plot(lm.fullT)  
  lm.fullT1=lmer(data=mAct.df, log10(meanCrossings+1)~ moonPhase*habitat*FeedingTrayPosition*month +(1|fStn))
  plot(lm.fullT1)  
  lm.fullT2=lmer(data=mAct.df, sqrt(meanCrossings)~ moonPhase*habitat*FeedingTrayPosition*month +(1|fStn))
  plot(lm.fullT2)  
  
lm.full.actnight=lmer(data=activity, Ncrossing~ MoonPhase*Habitat*NF*month +(1|FStn))
plot(lm.full.actnight)
  lm.full.actnight=lmer(data=activity, log10(Ncrossing+1)~ MoonPhase*Habitat*NF*month +(1|FStn))
  plot(lm.full.actnight)
Anova(lm.full.actnight)

#compare AIC and deviance of models
act.moon= lmer(data=activity, Ncrossing~ MoonPhase +(1|FStn))
act.habitat= lmer(data=activity, Ncrossing~ Habitat +(1|FStn))
act.microhab=  lmer(data=activity, Ncrossing~ NF +(1|FStn))
act.season=  lmer(data=activity, Ncrossing~ month +(1|FStn))
act.moonHab=lmer(data=activity, Ncrossing~ MoonPhase*Habitat +(1|FStn))
act.moonSeason=lmer(data=activity, Ncrossing~ MoonPhase*month +(1|FStn))
act.habMicro=lmer(data=activity, Ncrossing~ Habitat*NF +(1|FStn))
act.habSeason=lmer(data=activity, Ncrossing~ Habitat*month +(1|FStn))
act.moonHabMicrohab=lmer(data=activity, Ncrossing~ Habitat*MoonPhase*NF +(1|FStn))
act.habMicrohabSeason=lmer(data=activity, Ncrossing~ Habitat*NF*month +(1|FStn))
act.moonHabSeason=lmer(data=activity, Ncrossing~ Habitat*MoonPhase*month +(1|FStn))
anova(act.moon, act.habitat, act.microhab, act.season, act.moonHab, act.moonSeason ,act.habMicro, 
      act.habSeason, act.moonHabMicrohab, act.moonHabSeason)

#Using mean activity
act.moon1= lmer(data=mAct.df, meanCrossings~ moonPhase +(1|fStn))
act.habitat1= lmer(data=mAct.df, meanCrossings~ habitat +(1|fStn))
act.microhab1=  lmer(data=mAct.df, meanCrossings~ FeedingTrayPosition +(1|fStn))
act.season1=  lmer(data=mAct.df, meanCrossings~ month +(1|fStn))
act.moonHab1=lmer(data=mAct.df, meanCrossings~ moonPhase*habitat +(1|fStn))
act.moonSeason1=lmer(data=mAct.df, meanCrossings~ moonPhase*month +(1|fStn))
act.habMicro1=lmer(data=mAct.df, meanCrossings~ habitat*FeedingTrayPosition +(1|fStn))
act.habSeason1=lmer(data=mAct.df, meanCrossings~ habitat*month +(1|fStn))
act.moonHabMicrohab1=lmer(data=mAct.df, meanCrossings~ habitat*moonPhase*FeedingTrayPosition +(1|fStn))
act.habMicrohabSeason1=lmer(data=mAct.df, meanCrossings~ habitat*FeedingTrayPosition*month +(1|fStn))
act.moonHabSeason1=lmer(data=mAct.df, meanCrossings~ habitat*moonPhase*month +(1|fStn))
anova(act.moon1, act.habitat1, act.microhab1, act.season1, act.moonHab1, act.moonSeason1 ,act.habMicro1, 
      act.habSeason1, act.moonHabMicrohab1, act.moonHabSeason1)
