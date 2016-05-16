setwd("F:/NCBS/Thesis/Data/")


hist(mAct.df$meanCrossings)
hist(log(mAct.df$meanCrossings)) #more or less normal
hist(sqrt(mAct.df$meanCrossings))

#lm no interaction

activity.lm=lm(log(mAct.df$meanCrossings)~mAct.df$moonPhase+mAct.df$FeedingTrayPosition+mAct.df$habitat)
par(mfrow=c(2,2))

summary(activity.lm)
plot(activity.lm)
hist(residuals(activity.lm))

#plot no interaction
  #1. moon phase
act.mPhase=with(mAct.df, describeBy(sqrt(meanCrossings), moonPhase, mat=T, digits=4))
names(act.mPhase)[names(act.mPhase)=="group1"]="MoonPhase"
limits=with(act.mPhase,aes(ymax=mean+(1.96*se), ymin=mean-(1.96*se)))
dodge=position_dodge(width=0.9)
ggplot(act.mPhase, aes(x=MoonPhase, y=mean))+ 
  geom_point(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab('Mean Number of Crossings')+
  scale_fill_grey()

  #2. habitat

act.hab=with(mAct.df, describeBy(sqrt(meanCrossings), habitat, mat=T, digits=4))
names(act.hab)[names(act.hab)=="group1"]="Habitat"
limits=with(act.hab,aes(ymax=mean+(1.96*se), ymin=mean-(1.96*se)))
dodge=position_dodge(width=0.9)
ggplot(act.hab, aes(x=Habitat, y=mean))+ 
  geom_point(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab('Mean Number of Crossings')+
  scale_fill_grey()

  #3. microhabitat

act.mhab=with(mAct.df, describeBy(sqrt(meanCrossings),FeedingTrayPosition , mat=T, digits=4))
names(act.mhab)[names(act.mhab)=="group1"]="FeedingTrayPosition"
limits=with(act.mhab,aes(ymax=mean+(1.96*se), ymin=mean-(1.96*se)))
dodge=position_dodge(width=0.9)
ggplot(act.mhab, aes(x=FeedingTrayPosition, y=mean))+ 
  geom_point(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab('Mean Number of Crossings')+
  scale_fill_grey()


#plot interaction between moon phase and habitat

act.mPhase.hab.summary=with(mAct.df,describeBy(sqrt(meanCrossings), list(moonPhase, habitat), mat=T, digits=4))
names(act.mPhase.hab.summary)[names(act.mPhase.hab.summary)=="group1"]="MoonPhase"
names(act.mPhase.hab.summary)[names(act.mPhase.hab.summary)=="group2"]="Habitat"
limits=with(act.mPhase.hab.summary,aes(ymax=mean+(1.96*se), ymin=mean-(1.96*se)))
dodge=position_dodge(width=0.9)
ggplot(act.mPhase.hab.summary, aes(x=MoonPhase, y=mean, fill=Habitat))+ 
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab('Mean Number of Crossings')+
  scale_fill_grey()

#plot interaction between moon phase and microhabitat

act.mPhase.trayPos.summary=with(mAct.df,describeBy(sqrt(meanCrossings), list(moonPhase, FeedingTrayPosition), mat=T, digits=4))
names(act.mPhase.trayPos.summary)[names(act.mPhase.trayPos.summary)=="group1"]="MoonPhase"
names(act.mPhase.trayPos.summary)[names(act.mPhase.trayPos.summary)=="group2"]="FeedingTrayPosition"
limits=with(act.mPhase.trayPos.summary,aes(ymax=mean+(1.96*se), ymin=mean-(1.96*se)))
dodge=position_dodge(width=0.9)
ggplot(act.mPhase.trayPos.summary, aes(x=MoonPhase, y=mean, fill=FeedingTrayPosition))+ 
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab('Mean Number of Crossings')+
  scale_fill_grey()


#plot interaction between moon phase, habitat and microhabitat

act.mPhase.hab.muhab.summary=with(mAct.df,describeBy(sqrt(meanCrossings), list(moonPhase, habitat, FeedingTrayPosition), mat=T, digits=4))
names(act.mPhase.hab.muhab.summary)[names(act.mPhase.hab.muhab.summary)=="group1"]="MoonPhase"
names(act.mPhase.hab.muhab.summary)[names(act.mPhase.hab.muhab.summary)=="group2"]="Habitat"
names(act.mPhase.hab.muhab.summary)[names(act.mPhase.hab.muhab.summary)=="group3"]="FeedingTrayPosition"
limits=with(act.mPhase.hab.muhab.summary,aes(ymax=mean+(1.96*se), ymin=mean-(1.96*se)))
dodge=position_dodge(width=0.9)

ggplot(data=act.mPhase.hab.muhab.summary, aes(x=Habitat, y=mean, fill=FeedingTrayPosition))+
  geom_bar(stat="identity", position="dodge")+
  facet_wrap(~MoonPhase, switch="x", scales="free_x")+
  theme(panel.margin=unit(0,"lines"), strip.background=element_blank())+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab("Mean Number of Crossings")+
  scale_fill_grey()



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





