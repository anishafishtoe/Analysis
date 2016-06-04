#lm no interaction

activity.lm=lm(log(mAct.df$meanCrossings)~mAct.df$moonPhase+mAct.df$FeedingTrayPosition+mAct.df$habitat)
par(mfrow=c(2,2))

summary(activity.lm)
plot(activity.lm)
hist(residuals(activity.lm))


#lm interaction
activity.lm.int=lm(log(mAct.df$meanCrossings)~mAct.df$moonPhase*mAct.df$NF*mAct.df$habitat)
summary(activity.lm.int)
plot(activity.lm.int)

#generalized linear model
activity.glm=glm(mAct.df$meanCrossings~mAct.df$moonPhase+mAct.df$`Feeding tray position`+mAct.df$habitat)
par(mfrow=c(2,2))

summary(activity.glm)
plot(activity.glm)


#anova with all three factors
act.aov=aov(log(mAct.df$meanCrossings)~mAct.df$moonPhase+mAct.df$`Feeding tray position`+mAct.df$habitat)
summary(act.aov)
act.aov.int=aov(log(mAct.df$meanCrossings)~mAct.df$moonPhase*mAct.df$FeedingTrayPosition*mAct.df$habitat) #interaction not significant
summary(act.aov.int)
plot(act.aov.int)


#step-wise aic
null=lm(log(mAct.df$meanCrossings)~1)
full= lm(log(mAct.df$meanCrossings)~mAct.df$moonPhase+mAct.df$NF+mAct.df$habitat)
step(null, scope=list(lower=null, upper=full), direction="forward")
#Model containing all 3 has lowest AIC





