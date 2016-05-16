hist(gMean$meanGUD, xlab="Mean GUD", main="")
hist(log(gMean$meanGUD), xlab="Mean GUD Transformed", main="")
hist(sqrt(gMean$meanGUD))
hist((gMean$meanGUD)^2)



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
  scale_fill_grey()


#anova
gud.anova=aov(gMean$meanGUD~gMean$moonPhase*gMean$habitat*gMean$feedingTrayPosition)
summary(gud.anova)
