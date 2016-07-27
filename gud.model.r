#mixed effects models
library(lme4)
library(car)
library(MASS)
library(RVAideMemoire)
library(plyr)

setwd("F:/NCBS/Thesis/Data/")
gMean=read.csv("gudMean.csv")

############ ----- IMP-----------###########
#Check if Full moon phase is only recorded in Spring
gMean$season[gMean$phaseRep=="Full2"]="Winter"

#Drop full moon phase
gMean.drop=gMean[-which(gMean$moonPhase=="Full"),]
gMean.drop$moonPhase=factor(gMean.drop$moonPhase)

#Revalue seasons
gMean.drop$season=revalue(gMean.drop$season,c(Winter="Winter",Spring="Early Summer"))
levels(gMean.drop$season)

#Revalue Ftray position
gMean.drop$FeedingTrayPosition=revalue(gMean.drop$FeedingTrayPosition,c(N="Bush",F="Open"))
levels(gMean.drop$FeedingTrayPosition)
########################

#Create new data frame with no full moon phase
gMean.drop=gMean[-which(gMean$moonPhase=="Full"),]
with(gMean.drop,table(fStn, phaseRep))
gMean.drop$moonPhase=factor(gMean.drop$moonPhase)
gMean.drop$phaseRep=factor(gMean.drop$phaseRep)

#Drop feeding stations with unequal replicates
gMean.drop1=gMean.drop[-which(gMean.drop$fStn %in% c("A11","A31","B3","D11","D13","D31","D33")),]
gMean.drop1$fStn=factor(gMean.drop1$fStn)

#Are GUDS normal?
hist(gMean$meanGUD, main="", xlab="Mean GUD")
hist(log(gMean$meanGUD))
#main effects, no interaction

microhabitat=lmer(meanGUD~FeedingTrayPosition+(1|fStn), REML=FALSE, data=gMean, family="poisson")
habitat=lmer(meanGUD~habitat+(1|fStn), REML=FALSE, data=gMean)
moonPhase=lmer(meanGUD~moonPhase+(1|fStn), REML=FALSE, data=gMean)
month=lmer(meanGUD~month+(1|fStn), REML=FALSE, data=gMean)


#Full model and significance testing for fixed effects
full.model=lmer(meanGUD~FeedingTrayPosition+habitat+moonPhase+month+FeedingTrayPosition*habitat+FeedingTrayPosition*moonPhase+moonPhase*habitat +habitat*month +FeedingTrayPosition*habitat*month+habitat*moonPhase*month+FeedingTrayPosition*moonPhase*habitat+(1|fStn), data=gMean, REML=FALSE)
Anova(full.model)

#GLMM with Laplace
summary(glmer(meanGUD~FeedingTrayPosition+habitat+moonPhase+month+FeedingTrayPosition*habitat+
                FeedingTrayPosition*moonPhase+moonPhase*habitat +habitat*month +
                FeedingTrayPosition*habitat*month+habitat*moonPhase*month+
                FeedingTrayPosition*moonPhase*habitat+(1|fStn), data= gMean, family= Gamma(link="log")))

#0906
gud.lmer.fullmodel=lmer(data=gMean, log10(meanGUD+1)~ moonPhase*habitat*FeedingTrayPosition*month +(1|fStn), REML=FALSE)
plot(gud.lmer.fullmodel)

res= residuals(gud.lmer.fullmodel)
fit= fitted(gud.lmer.fullmodel)
op <- par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))
plot(x=fit, y=res, xlab="Fitted", ylab="Residuals")
boxplot(res~moonPhase, data= gMean, ylab="Residuals")
boxplot(res~habitat, data= gMean, ylab="Residuals")
boxplot(res~FeedingTrayPosition, data= gMean, ylab="Residuals")
boxplot(res~month, data= gMean, ylab="Residuals")

Anova(gud.lmer.fullmodel)

gud.sigmodel=lmer(data=gMean, log10(meanGUD+1)~month+habitat*month+(1|fStn), REML=FALSE)
anova(gud.lmer.fullmodel, gud.sigmodel)

gud.lmer.full.month=lmer(data=subset(gMean, month =="Month2"), meanGUD~ moonPhase*habitat*FeedingTrayPosition +(1|fStn), REML=FALSE)
plot(gud.lmer.fullmodel)
Anova(gud.lmer.full.month)

#16-06

gMean.subs=gMean[-which(is.na(gMean$meanGUD)),]

gud.nonaT2=lmer(data=gMean, meanGUD~ moonPhase+habitat+FeedingTrayPosition+season+
                moonPhase:habitat + moonPhase:FeedingTrayPosition + moonPhase:season+
                  habitat:FeedingTrayPosition+
                  habitat:season+  moonPhase:habitat:FeedingTrayPosition +
                  habitat:moonPhase:season+ (1|fStn))
plot(gud.nonaT2)
qqnorm(residuals(gud.nonaT2))
qqline(residuals(gud.nonaT2))
hist(residuals(gud.visit))
summary(gud.full)
Anova(gud.nonaT2)


#AOV with no transformation
gud.aov=aov(data=gMean, meanGUD~ moonPhase+habitat+FeedingTrayPosition+season+
  moonPhase:habitat + moonPhase:FeedingTrayPosition + moonPhase:season+
  habitat:FeedingTrayPosition+
  habitat:season+  moonPhase:habitat:FeedingTrayPosition +
  habitat:moonPhase:season+ Error(fStn))
summary(gud.aov)


hist(gMeanVisit.trans$meanGUD)

plot(gMean.trans$habitat, residuals(gud.nonaT2))

#post-hoc for habitat-season
model.habseas=lmer(data=gMean, meanGUD~ habitat+ season+habitat:season + 
                     + (1|fStn))
plot(model.habseas)
qqnorm(residuals(model.habseas))
qqline(residuals(model.habseas))
act.ref=ref.grid(model.habseas)
act.contrast=contrast(act.ref, "pairwise")



Anova(gud.nonaT2)
gud.red=lmer(meanGUD~habitat+season+habitat:season+(1|fStn), data=gMean)
plot(gud.red)

plot(gud.full, fStn~resid(.), abline=0)
plot(gud.full, fStn~resid(., type="p")~fitted(.)|habitat,adj=-0.3)



#transform response
gMean.trans=gMean.subs
gMean.trans$meanGUD=asin(gMean$meanGUD/3)
gMean.noFull=gMean[-which(gMean$moonPhase=="Full"),]

#LMM for no full moon phase
gud.noFull=rlmer(data=gMean, meanGUD~ moonPhase+habitat+FeedingTrayPosition+season+
                  moonPhase:habitat + moonPhase:FeedingTrayPosition + moonPhase:season+
                  habitat:FeedingTrayPosition+
                  habitat:season+  moonPhase:habitat:FeedingTrayPosition +
                  habitat:moonPhase:season+ (1|fStn))
aov.noFull=aov(data=gMean.drop1, meanGUD~ moonPhase+habitat+FeedingTrayPosition+season+
              moonPhase:habitat + moonPhase:FeedingTrayPosition + moonPhase:season+
              habitat:FeedingTrayPosition+
              habitat:season+  moonPhase:habitat:FeedingTrayPosition +
              habitat:moonPhase:season+ Error(fStn))
summary(aov.noFull)
plot(gud.noFull)
qqnorm(residuals(gud.noFull))
qqline(residuals(gud.noFull))
Anova(gud.noFull)

#ANOVAs with asine/log and with/without Error term
gud.aov.log=aov(data=gMean.noFull, log(meanGUD)~ moonPhase+habitat+FeedingTrayPosition+season+
              moonPhase:habitat + moonPhase:season+moonPhase:FeedingTrayPosition + habitat:FeedingTrayPosition+
              habitat:season+  moonPhase:habitat:FeedingTrayPosition +
              habitat:moonPhase:season)
summary(gud.aov.log)
gud.aov.arcsin=aov(data=gMean.noFull, asin(meanGUD/3)~ moonPhase+habitat+FeedingTrayPosition+season+
                  moonPhase:habitat + moonPhase:season+moonPhase:FeedingTrayPosition + habitat:FeedingTrayPosition+
                  habitat:season+  moonPhase:habitat:FeedingTrayPosition +
                  habitat:moonPhase:season)
summary(gud.aov.arcsin)
gud.aov.log.e=aov(data=gMean.noFull, log(meanGUD)~ moonPhase+habitat+FeedingTrayPosition+season+
                  moonPhase:habitat +moonPhase:season+ moonPhase:FeedingTrayPosition + habitat:FeedingTrayPosition+
                  habitat:season+  moonPhase:habitat:FeedingTrayPosition +
                  habitat:moonPhase:season + Error(fStn))
summary(gud.aov.log.e)
gud.aov.arcsin.e=aov(data=gMean.noFull, asin(meanGUD/3)~ moonPhase+habitat+FeedingTrayPosition+season+
                     moonPhase:habitat +moonPhase:season+ moonPhase:FeedingTrayPosition + habitat:FeedingTrayPosition+
                     habitat:season+  moonPhase:habitat:FeedingTrayPosition +
                     habitat:moonPhase:season+Error(fStn))
summary(gud.aov.arcsin.e)


summary(gud.aov.arcsin.e)
summary(gud.aov.arcsin)
qqnorm(residuals(gud.aov.arcsin))
qqline(residuals(gud.aov.arcsin))
plot(residuals(gud.aov.log), fitted(gud.aov.log))

#with visited trays
gMeanV.subs=gMeanVisited[-which(is.na(gMeanVisited$meanGUD)),]
hist(asin(gMeanV.subs$meanGUD/3))

gud.subs=lmer(data=gMean.subs, asin(meanGUD/3)~ moonPhase+habitat+FeedingTrayPosition+season+
                  moonPhase:habitat + moonPhase:FeedingTrayPosition + habitat:FeedingTrayPosition+
                moonPhase:season+  habitat:season+  moonPhase:habitat:FeedingTrayPosition +
                  habitat:moonPhase:season)
plot(gud.subs)
qqnorm(residuals(gud.subs))
qqline(residuals(gud.subs))

gMean.subs=gMean[-which(is.na(gMean$meanGUD)),]


#2606
#Likelihood ratio tests
#first, the full model and Anova
with(gMean.drop,table(fStn,phaseRep, FeedingTrayPosition))
gMean.drop$phaseRep=factor(gMean.drop$phaseRep)
gud.model=lmer(data=gMean.drop, meanGUD^2~ moonPhase+habitat+FeedingTrayPosition+season+
                   moonPhase:habitat + moonPhase:FeedingTrayPosition + moonPhase:season+
                   habitat:FeedingTrayPosition+
                   habitat:season+  moonPhase:habitat:FeedingTrayPosition +
                   habitat:moonPhase:season+habitat:FeedingTrayPosition:season+ (1|fStn))
plot(gud.additive)
qqnorm(residuals(gud.season1))
qqline(residuals(gud.season1))
Anova(gud.model)

#LRT
gud.null=lmer(data=gMean.drop, meanGUD~(1|fStn))
gud.moon1= lmer(data=gMean.drop, meanGUD~ moonPhase +(1|fStn))
gud.habitat1= lmer(data=gMean.drop, meanGUD~ habitat +(1|fStn))
gud.microhab1=  lmer(data=gMean.drop, meanGUD~ FeedingTrayPosition +(1|fStn))
gud.season1=  lmer(data=gMean.drop, meanGUD~ season +(1|fStn))
gud.additive=lmer(data=gMean.drop, meanGUD~moonPhase+habitat+FeedingTrayPosition+season+(1|fStn))

gud.moonHab1=lmer(data=gMean.drop, meanGUD~ moonPhase*habitat +(1|fStn))
gud.moonSeason1=lmer(data=gMean.drop, meanGUD~ moonPhase*season +(1|fStn))
gud.habMicro1=lmer(data=gMean.drop, meanGUD~ habitat*FeedingTrayPosition +(1|fStn))
gud.habSeason1=lmer(data=gMean.drop, meanGUD~ habitat*season +(1|fStn))
gud.moonHabMicrohab1=lmer(data=gMean.drop, meanGUD~ habitat*moonPhase*FeedingTrayPosition +(1|fStn))
gud.habMicrohabSeason1=lmer(data=gMean.drop, meanGUD~ habitat*FeedingTrayPosition*season +(1|fStn))
gud.moonHabSeason1=lmer(data=gMean.drop, meanGUD~ habitat*moonPhase*season +(1|fStn))

#fixed effects
anova(gud.moon1, gud.null)
anova(gud.season1, gud.null)
anova(gud.habitat1, gud.null)
anova(gud.microhab1, gud.null)
gud.additive=lmer(data=gMean.drop, meanGUD~season+habitat+FeedingTrayPosition+(1|fStn))
#interaction
anova(gud.moonHab1, gud.additive)
anova(gud.moonSeason1, gud.additive)
anova(gud.habMicro1, gud.additive)
anova(gud.habSeason1, gud.additive)
anova(gud.moonHabMicrohab1, gud.additive)
anova(gud.habMicrohabSeason1, gud.additive)
anova(gud.moonHabSeason1, gud.additive)

#post hoc
gud.fullDrop=lmer(data=gMean.drop, (meanGUD)~ habitat+FeedingTrayPosition+season+moonPhase+
                    habitat:FeedingTrayPosition+habitat:moonPhase+
                    habitat:season+ moonPhase:season+habitat:season:FeedingTrayPosition+(1|fStn))
summary(gud.fullDrop)
HabSF=lsmeans(gud.fullDrop, c("habitat","season","FeedingTrayPosition"))

HabSF.letters=cld(HabSF, Letters=letters)

act.ref=ref.grid(gud.fullDrop)
HabSF=lsmeans(gud.fullDrop,c("habitat","season","FeedingTrayPosition"))
HabSF.letters=lsmeans::cld(HabSF, Letters=letters,alpha=0.05)

Hab.SF=lsmeans(gud.fullDrop, pairwise~habitat:season:FeedingTrayPosition) #pairwise results





#2706 backward selection
gud.full=lmer(data=gMeanVisited, meanGUD~moonPhase+habitat+FeedingTrayPosition+ season+ 
                moonPhase:habitat + moonPhase:season +moonPhase:FeedingTrayPosition+ habitat:FeedingTrayPosition + 
                habitat:season + moonPhase:habitat:FeedingTrayPosition +
                moonPhase:habitat:season + habitat:FeedingTrayPosition:season+(1|fStn))
gud.noHabFS=update(gud.full,.~.-habitat:FeedingTrayPosition:season)
gud.noMoHabS=update(gud.noHabFS,.~.-moonPhase:habitat:season)
gud.noMoHabF=update(gud.noMoHabS,.~.-moonPhase:habitat:FeedingTrayPosition)
gud.2way=lmer(data=gMean.drop, meanGUD~moonPhase+habitat+FeedingTrayPosition+ season+ 
                moonPhase:habitat + moonPhase:season + habitat:FeedingTrayPosition + 
                habitat:season +(1|fStn))
gud.nohabS=update(gud.noMoHabF, .~.-habitat:season)
gud.nohabF=update(gud.noMoHabF,.~.-habitat:FeedingTrayPosition)
gud.noMoS=update(gud.nohabF,.~.-moonPhase:season)
gud.noMoH=update(gud.noMoS, .~.-moonPhase:habitat)
gud.additive=lmer(data=gMeanVisited, meanGUD~moonPhase+habitat+FeedingTrayPosition+ season+(1|fStn))
gud.noF=update(gud.noMoH,.~.-FeedingTrayPosition)
gud.noHab=update(gud.noMoH,.~.-habitat)
gud.noMo=update(gud.noHab,.~.-moonPhase)

anova(gud.noMoHabF,gud.noF)

summary(gud.noMoS)
