#linear models and graphs

library(ggplot2)
library(psych)

#Are GUDs distributed normally?
hist(gMean$meanGUD, xlab="Mean GUD", main="")
hist(log(gMean$meanGUD), xlab="Mean GUD Transformed", main="")
hist(sqrt(gMean$meanGUD))
hist((gMean$meanGUD)^2)

#How much did ants and birds eat up the seeds

guds.ants=c(guds$GUDN[which(guds$AntBirdN=="A" & is.na(guds$PrintsInside))],guds$GUDF[which(guds$AntBirdF=="A" & is.na(guds$PrintsInside))] )
boxplot(guds.ants, xlab="Ants", ylab="Number of seeds harvested")

guds.birds=c(guds$GUDN[which(guds$AntBirdN=="B" & is.na(guds$PrintsInside))],guds$GUDF[which(guds$AntBirdF=="B" & is.na(guds$PrintsInside))] )
boxplot(guds.birds, xlab="Birds", ylab="Number of seeds harvested")

guds.antbird=c(guds$GUDN[which(guds$AntBirdN=="AB" & is.na(guds$PrintsInside))],guds$GUDF[which(guds$AntBirdF=="AB" & is.na(guds$PrintsInside))] )
boxplot(guds.antbird, xlab="Ants and birds", ylab="Number of seeds harvested")


#no interaction
gud.lm=lm(sqrt(gMean$meanGUD)~gMean$moonPhase+gMean$`Feeding tray position`+gMean$habitat)
summary(gud.lm)#very low r square
par(mfrow=c(2,2))
plot(gud.lm) #errors not normally distributed
hist(residuals(gud.lm))#not normal!
colnames(gMean)[which(colnames(gMean)=="nf")]="feedingTrayPosition"


plot(sqrt(meanGUD)~moonPhase+feedingTrayPosition+habitat, data=gMean, ylab="Mean GUD")


#lm interaction
gud.lm.int=lm(sqrt(gMean$meanGUD)~gMean$moonPhase*gMean$feedingTrayPosition*gMean$habitat)
summary(gud.lm.int)
plot(gud.lm.int)

#effect of moon phase

gud.mPhase=with(gMean, describeBy(sqrt(meanGUD), moonPhase, mat=T, digits=4))
names(gud.mPhase)[names(gud.mPhase)=="group1"]="MoonPhase"
limits=with(gud.mPhase,aes(ymax=mean+(1.96*se), ymin=mean-(1.96*se)))
dodge=position_dodge(width=0.9)
ggplot(gud.mPhase, aes(x=MoonPhase, y=mean))+ 
  geom_point(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab('Mean GUD')+
  scale_fill_grey()

#effect of habitat

gud.hab=with(gMean, describeBy(sqrt(meanGUD), habitat, mat=T, digits=4))
names(gud.hab)[names(gud.hab)=="group1"]="Habitat"
limits=with(gud.hab,aes(ymax=mean+(1.96*se), ymin=mean-(1.96*se)))
dodge=position_dodge(width=0.9)
ggplot(gud.hab, aes(x=Habitat, y=mean))+ 
  geom_point(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab('Mean GUD')+
  scale_fill_grey()

#effect of microhabitat

gud.mhab=with(gMean, describeBy(sqrt(meanGUD),feedingTrayPosition , mat=T, digits=4))
names(gud.mhab)[names(gud.mhab)=="group1"]="FeedingTrayPosition"
limits=with(gud.mhab,aes(ymax=mean+(1.96*se), ymin=mean-(1.96*se)))
dodge=position_dodge(width=0.9)
ggplot(gud.mhab, aes(x=FeedingTrayPosition, y=mean))+ 
  geom_point(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab('Mean GUD')+
  scale_fill_grey()

#effect of month

gud.month=with(gMean, describeBy(sqrt(meanGUD),month , mat=T, digits=4))
names(gud.month)[names(gud.month)=="group1"]="Month"
limits=with(gud.month,aes(ymax=mean+(1.96*se), ymin=mean-(1.96*se)))
dodge=position_dodge(width=0.9)
ggplot(gud.month, aes(x=Month, y=mean))+ 
  geom_point(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab('Mean GUD')+
  scale_fill_grey()


#plot interaction between moon phase and feeding tray position

par(mfrow=c(1,1))
mPhase.trayPos.summary=with(gMean,describeBy(sqrt(meanGUD), list(moonPhase, feedingTrayPosition), mat=T, digits=2))
names(mPhase.trayPos.summary)[names(mPhase.trayPos.summary)=="group1"]="MoonPhase"
names(mPhase.trayPos.summary)[names(mPhase.trayPos.summary)=="group2"]="FeedingTrayPosition"
limits=with(mPhase.trayPos.summary,aes(ymax=mean+(1.96*se), ymin=mean-(1.96*se)))
dodge=position_dodge(width=0.9)
ggplot(mPhase.trayPos.summary, aes(x=MoonPhase, y=mean, fill=FeedingTrayPosition))+ 
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab('Mean GUD')+
  theme_set(theme_gray(base_size = 20))+
  scale_fill_grey()

#plot interaction between moon phase and habitat

mPhase.hab.summary=with(gMean,describeBy(sqrt(meanGUD), list(moonPhase, habitat), mat=T, digits=4))
names(mPhase.hab.summary)[names(mPhase.hab.summary)=="group1"]="MoonPhase"
names(mPhase.hab.summary)[names(mPhase.hab.summary)=="group2"]="Habitat"
limits=with(mPhase.hab.summary,aes(ymax=mean+(1.96*se), ymin=mean-(1.96*se)))
dodge=position_dodge(width=0.9)
ggplot(mPhase.hab.summary, aes(x=MoonPhase, y=mean, fill=Habitat))+ 
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab('Mean GUD')+
  theme_set(theme_gray(base_size = 20))+
  scale_fill_grey()

#interaction between habitat and microhabitat

hab.micro.summary=with(gMean,describeBy(sqrt(meanGUD), list(habitat, feedingTrayPosition), mat=T, digits=4))
names(hab.micro.summary)[names(hab.micro.summary)=="group1"]="Habitat"
names(hab.micro.summary)[names(hab.micro.summary)=="group2"]="FeedingTrayPosition"
limits=with(hab.micro.summary,aes(ymax=mean+(1.96*se), ymin=mean-(1.96*se)))
dodge=position_dodge(width=0.9)
ggplot(hab.micro.summary, aes(x=Habitat, y=mean, fill=FeedingTrayPosition))+ 
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab('Mean GUD')+
  theme_set(theme_gray(base_size = 20))+
  scale_fill_grey()

#interaction between month and moon phase

mPhase.mon.summary=with(gMean,describeBy(sqrt(meanGUD), list(moonPhase, month), mat=T, digits=4))
names(mPhase.mon.summary)[names(mPhase.mon.summary)=="group1"]="MoonPhase"
names(mPhase.mon.summary)[names(mPhase.mon.summary)=="group2"]="Month"
limits=with(mPhase.mon.summary,aes(ymax=mean+(1.96*se), ymin=mean-(1.96*se)))
dodge=position_dodge(width=0.9)
ggplot(mPhase.mon.summary, aes(x=Month, y=mean, fill=MoonPhase))+ 
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab('Mean GUD')+
  theme_set(theme_gray(base_size = 20))+
  scale_fill_grey()

#interaction between habitat and month

hab.mon.summary=with(gMean,describeBy(sqrt(meanGUD), list(habitat, month), mat=T, digits=4))
names(hab.mon.summary)[names(hab.mon.summary)=="group1"]="Habitat"
names(hab.mon.summary)[names(hab.mon.summary)=="group2"]="Month"
limits=with(hab.mon.summary,aes(ymax=mean+(1.96*se), ymin=mean-(1.96*se)))
dodge=position_dodge(width=0.9)
ggplot(hab.mon.summary, aes(x=Month, y=mean, fill=Habitat))+ 
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab('Mean GUD')+
  theme_set(theme_gray(base_size = 20))+
  scale_fill_grey()

#interaction between microhabitat and month

micro.mon.summary=with(gMean,describeBy(sqrt(meanGUD), list(feedingTrayPosition, month), mat=T, digits=4))
names(micro.mon.summary)[names(micro.mon.summary)=="group1"]="FeedingTrayPosition"
names(micro.mon.summary)[names(micro.mon.summary)=="group2"]="Month"
limits=with(micro.mon.summary,aes(ymax=mean+(1.96*se), ymin=mean-(1.96*se)))
dodge=position_dodge(width=0.9)
ggplot(micro.mon.summary, aes(x=Month, y=mean, fill=FeedingTrayPosition))+ 
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab('Mean GUD')+
  theme_set(theme_gray(base_size = 20))+
  scale_fill_grey()

#interaction between habitat, microhabitat and month
mon.hab.muhab.summary=with(gMean,describeBy(sqrt(meanGUD), list(month, habitat, feedingTrayPosition), mat=T, digits=4))
names(mon.hab.muhab.summary)[names(mon.hab.muhab.summary)=="group1"]="Month"
names(mon.hab.muhab.summary)[names(mon.hab.muhab.summary)=="group2"]="Habitat"
names(mon.hab.muhab.summary)[names(mon.hab.muhab.summary)=="group3"]="FeedingTrayPosition"
limits=with(mon.hab.muhab.summary,aes(ymax=mean+(1.96*se), ymin=mean-(1.96*se)))
dodge=position_dodge(width=0.9)

ggplot(data=mon.hab.muhab.summary, aes(x=Habitat, y=mean, fill=FeedingTrayPosition))+
  geom_bar(stat="identity", position="dodge")+
  facet_wrap(~Month, switch="x", scales="free_x")+
  theme(panel.margin=unit(0,"lines"), strip.background=element_blank())+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab("Mean GUD")+
  theme_set(theme_gray(base_size = 20))+
  scale_fill_grey()

#plot interaction between moon phase, habitat and microhabitat
mPhase.hab.muhab.summary=with(gMean,describeBy(sqrt(meanGUD), list(moonPhase, habitat, feedingTrayPosition), mat=T, digits=4))
names(mPhase.hab.muhab.summary)[names(mPhase.hab.muhab.summary)=="group1"]="MoonPhase"
names(mPhase.hab.muhab.summary)[names(mPhase.hab.muhab.summary)=="group2"]="Habitat"
names(mPhase.hab.muhab.summary)[names(mPhase.hab.muhab.summary)=="group3"]="FeedingTrayPosition"
limits=with(mPhase.hab.muhab.summary,aes(ymax=mean+(1.96*se), ymin=mean-(1.96*se)))
dodge=position_dodge(width=0.9)

ggplot(data=mPhase.hab.muhab.summary, aes(x=Habitat, y=mean, fill=FeedingTrayPosition))+
  geom_bar(stat="identity", position="dodge")+
  facet_wrap(~MoonPhase, switch="x", scales="free_x")+
  theme(panel.margin=unit(0,"lines"), strip.background=element_blank())+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab("Mean GUD")+
  theme_set(theme_gray(base_size = 20))+
  scale_fill_grey()


#anova
gud.anova=aov(gMean$meanGUD~gMean$moonPhase*gMean$habitat*gMean$feedingTrayPosition)
summary(gud.anova)
