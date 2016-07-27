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
