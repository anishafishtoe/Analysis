library(lme4)
library(car)
library(MASS)
library(lsmeans)

setwd("F:/NCBS/Thesis/Data/")
actJoin=read.csv("activityJoin.csv")
mAct.df=read.csv("meanActivity.csv")

############ ----- IMP-----------###########
#Check if Full moon phase is only recorded in Spring
mAct.df$season[mAct.df$phaseRep=="Full2"]="Winter"

#Drop full moon phase
mAct.drop=mAct.df[-which(mAct.df$moonPhase=="Full"),]
mAct.drop$moonPhase=factor(mAct.drop$moonPhase)

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

#ANOVA

full.aov=aov(data=mAct.df, meanCrossings~moonPhase+habitat+FeedingTrayPosition+ season+ 
  moonPhase:habitat + moonPhase:season + habitat:FeedingTrayPosition + 
  moonPhase:FeedingTrayPosition+
  habitat:season + moonPhase:habitat:FeedingTrayPosition +
  moonPhase:habitat:season + Error(fStn))
summary(full.aov)
single=lmer(data=mAct.df, meanCrossings~moonPhase:habitat:season +(1|fStn))
plot(single)
plot(full.model)
qqnorm(residuals(full.model))
qqline(residuals(full.model))
hist(log10(mAct.df$meanCrossings+1))
summary(full.model)
Anova(full.model)
help("pvalues")
confint(full.model, method="Wald")

plot(full.model, fStn~resid(.), abline=0)

mp.sea=lmer(data=mAct.df, log10(meanCrossings+1)~moonPhase+season+moonPhase:season+(1|fStn))
#post-hoc
act.ref=ref.grid(mp.sea)
act.contrast=contrast(act.ref, "pairwise")
lsmeans(full.model, pairwise~moonPhase|season)
summary(glht(full.model, mcp(moonPhase*season="Tukey")))
class(act.contrast)

#19-06 
#Post hoc- moon phase not dropped

model.moonphaseSeas1=lmer(data=mAct.df, log(meanCrossings+1)~ moonPhase+ season+moonPhase:season + 
                           + (1|fStn))
plot(model.moonphaseHab)
qqnorm(residuals(model.moonphaseSeas))

act.ref=ref.grid(full.model)
act.contrast=contrast(act.ref, "pairwise")
lsmeans(full.model, ~habitat|moonPhase)

#moonphase*habitat
model.moonphaseHab=lmer(data=mAct.df, meanCrossings~ moonPhase+ habitat+moonPhase:habitat + 
                            + (1|fStn))

#moonphase-season
model.moonphaseSeason=lmer(data=mAct.df, meanCrossings~ moonPhase+ habitat+moonPhase:habitat + 
                          + (1|fStn))

#hab-micro
model.habmicro1=lmer(data=mAct.df, meanCrossings~ habitat+ FeedingTrayPosition+habitat:FeedingTrayPosition + 
                      + (1|fStn))
Anova(model.habmicro1)
#hab-season
model.habseas=lmer(data=mAct.df, log(meanCrossings+1)~ habitat+ season+habitat:season + 
                       + (1|fStn))

#moonphase-habitat-season
model.3way=lmer(data=mAct.df, (meanCrossings)~ habitat+ moonPhase+season + habitat:season:moonPhase + 
                     + (1|fStn))


#--Drop moon phase
#################27-06: mp=lsmeans
mAct.df.drop=mAct.df[-which(mAct.df$moonPhase=="Full"),]
full.modelDrop=lmer(data=mAct.df, meanCrossings~moonPhase+habitat+FeedingTrayPosition+ season+ 
        moonPhase:season + habitat:FeedingTrayPosition + habitat:season+
         habitat:FeedingTrayPosition:season+(1|fStn))
print(lsmeans(full.modelDrop, pairwise~habitat:season:FeedingTrayPosition:habitat))
plot(full.modelDrop)
qqnorm(residuals(full.modelDrop))
qqline(residuals(full.modelDrop))
Anova(full.modelDrop)

model.habmicro=lmer(data=mAct.df.drop, (meanCrossings)~ habitat+ FeedingTrayPosition+habitat:FeedingTrayPosition + 
                      + (1|fStn))
model.moonphaseSeas=lmer(data=mAct.df.drop, (meanCrossings)~ moonPhase+ season+moonPhase:season + 
                           + (1|fStn))
act.ref=ref.grid(full.modelDrop)
act.contrast=contrast(act.ref, "pairwise")
contrast(full.modelDrop, )



full.model1= lmer(data=mAct.df.drop, meanCrossings~habitat+FeedingTrayPosition+season+moonPhase+
                         moonPhase:season+moonPhase:habitat+moonPhase:FeedingTrayPosition+habitat:season+
                         habitat:FeedingTrayPosition+habitat:season:FeedingTrayPosition+
                       + (1|fStn))
plot(full.model1)
qqnorm(residuals(full.model1))
qqline(residuals(full.model1))
lsmeans(full.model1, pairwise~habitat:season:FeedingTrayPosition) #pairwise results
library(multcompView)
HabSF=lsmeans(full.model1, c("habitat","season","FeedingTrayPosition" ))# to get lsmean +- SE for each effect
lsmeans(full.modelDrop, "habitat")
HabSF.letters=cld(HabSF, Letters=letters)


#2706 backward selection
act.full=lmer(data=mAct.drop, meanCrossings~moonPhase+habitat+FeedingTrayPosition+ season+ 
                moonPhase:habitat + moonPhase:season +moonPhase:FeedingTrayPosition+ habitat:FeedingTrayPosition + 
                habitat:season + moonPhase:habitat:FeedingTrayPosition +
                moonPhase:habitat:season + habitat:FeedingTrayPosition:season+(1|fStn))
act.noHabFS=update(act.full,.~.-habitat:FeedingTrayPosition:season)
act.noMoHabS=update(act.full,.~.-moonPhase:habitat:season)
act.noMoHabF=update(act.noMoHabS,.~.- moonPhase:habitat:FeedingTrayPosition)
act.noMoHabF1=update(act.noMoHabF,.~.-moonPhase:FeedingTrayPosition)
act.no3way=update(act.noMoHabF1,.~.-habitat:FeedingTrayPosition:season)
act.noHabS=update(act.no3way,.~.-habitat:season)
act.noHabFtray=update(act.noHabS,.~.-habitat:FeedingTrayPosition)
act.noMoS=update(act.noHabS,.~.-moonPhase:season)
act.noMoH=update(act.noHabS,.~.-moonPhase:habitat)
act.additive=lmer(data=mAct.drop, meanCrossings~FeedingTrayPosition+moonPhase+habitat+ season+(1|fStn))
act.noH=update(act.additive,.~.-moonPhase)

plot(act.full)
qqnorm(residuals(act.full))
qqline(residuals(act.full))
anova(act.noHabFS,act.full)





#21-06
#How many trays visited on each night, for each phase replicate
activityJoin=read.csv("activityJoin.csv")
#Remove records with Ncrossing=0
activityVisit=activityJoin[-which(activityJoin$Ncrossing==0),]
activityVisit=activityVisit[,-c(which(colnames(activityJoin)%in% c("FLength","FWidth","HLength","HWidth","HTLength","HTWidth","Digging","FTLength","Comments", "FTWidth","InOut")))]

#month as season
activityVisit$season=revalue(activityVisit$month, c(Month1="Winter", Month2="Spring"))
activityVisit$season[activityVisit$phaseRep=="Full2"]="Winter"


fStnVisited=ddply(activityVisit, c("MoonPhase","season","Habitat", "phaseRep", "NF"), summarise, Visited=length(unique(FStn)))
fStnVisited.drop=fStnVisited[-which(fStnVisited$MoonPhase=="Full" ),]
fStnVisited.drop$MoonPhase=factor(fStnVisited.drop$MoonPhase)
fStnVisited.drop$phaseRep=factor(fStnVisited.drop$phaseRep)

fStnVisited.Pool=ddply(fStnVisited.drop, c("MoonPhase","Habitat", "season"), summarise, sum(Visited))
fStnVisited.Season=ddply(fStnVisited.drop, c("NF","season", "Habitat"), summarise, sum(Visited))
  
fStnVisited$moonPhase=gsub("[1-2]","",fStnVisited$phaseRep)
with(subset(fStnVisited, Habitat=="Sparse"), boxplot(Visited~Habitat+moonPhase))
with(fStnVisited, boxplot(Visited~Habitat+season))
fStnVisited=arrange(fStnVisited,Habitat, phaseRep)

#Means
fStnVisitedMean=ddply(fStnVisited, c("season","Habitat","moonPhase"), summarise, MeanVisited=mean(Visited))


