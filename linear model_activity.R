setwd("F:/NCBS/Thesis/Data/")


hist(mAct.df$meanCrossings)
hist(log(mAct.df$meanCrossings)) #more or less normal
hist(sqrt(mAct.df$meanCrossings))

#glm additive

activity.lm=lm(log(mAct.df$meanCrossings)~mAct.df$moonPhase+mAct.df$NF+mAct.df$habitat)
par(mfrow=c(2,2))

summary(activity.lm)
plot(activity.lm)

hist(residuals(activity.lm))

#glm interaction
activity.lm.int=lm(log(mAct.df$meanCrossings)~mAct.df$moonPhase*mAct.df$NF*mAct.df$habitat)
summary(activity.lm.int)
plot(activity.lm.int)


#anova with all three factors
act.aov=aov(log(mAct.df$meanCrossings)~mAct.df$moonPhase+mAct.df$NF+mAct.df$habitat)
summary(act.aov)
act.aov.int=aov(log(mAct.df$meanCrossings)~mAct.df$moonPhase*mAct.df$NF*mAct.df$habitat) #interaction not significant
summary(act.aov.int)

#glm with moon phase
act.lm.mPhase=lm(log(mAct.df$meanCrossings)~mAct.df$moonPhase)
summary(act.lm.mPhase)

#glm with habitat

#step-wise aic
null=lm(log(mAct.df$meanCrossings)~1)
full= lm(log(mAct.df$meanCrossings)~mAct.df$moonPhase+mAct.df$NF+mAct.df$habitat)
step(null, scope=list(lower=null, upper=full), direction="forward")
#Model containing all 3 has lowest AIC


guds= read.csv("GUDs.csv")

guds$MoonPhase[which(grepl("B",guds$AntBirdN)) | which(grepl("B",guds$AntBirdF))]
