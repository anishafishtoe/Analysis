#linear models and graphs

library(ggplot2)
library(psych)

#read gMean
gMean=read.csv("gMean2106.csv")

#drop full moon phase
gMean.drop=gMean[-which(gMean$moonPhase=="Full"),]
gMean.drop$moonPhase=factor(gMean.drop$moonPhase)

#Are GUDs distributed normally?
hist(gMean$meanGUD, xlab="Mean GUD", main="")
hist(log(gMean$meanGUD), xlab="Mean GUD Transformed", main="")
hist(sqrt(gMean$meanGUD))
hist((gMean$meanGUD)^2)

#How are GUDs distributed for each habitat
with(guds, hist(GUD[Habitat=="Sparse"],xlab="GUD", main="", col="#A4A4A4"))
with(guds, hist(GUD[Habitat=="Dense"], col="#04B4AE", add=T))
legend("topleft", c("Sparse", "Dense"), col=c("#A4A4A4", "#04B4AE") ,pch=16)

#How much did ants and birds eat up the seeds


guds.ants=c(guds$GUDN[which(guds$AntBirdN=="A" & is.na(guds$PrintsInside))],guds$GUDF[which(guds$AntBirdF=="A" & is.na(guds$PrintsInside))] )
boxplot(guds.ants, xlab="Ants", ylab="Number of seeds harvested")

guds.birds=c(guds$GUDN[which(guds$AntBirdN=="B" & is.na(guds$PrintsInside))],guds$GUDF[which(guds$AntBirdF=="B" & is.na(guds$PrintsInside))] )
boxplot(guds.birds, xlab="Birds", ylab="Number of seeds harvested")

guds.antbird=c(guds$GUDN[which(guds$AntBirdN=="AB" & is.na(guds$PrintsInside))],guds$GUDF[which(guds$AntBirdF=="AB" & is.na(guds$PrintsInside))] )
boxplot(guds.antbird, xlab="Ants and birds", ylab="Number of seeds harvested")

#Subset gMean to remove NAs
gMean.subs=gMean[-which(is.na(gMean$meanGUD)),]
which(is.na(gMean.subs$meanGUD))

#effect of moon phase

gud.mPhase=with(gMeanVisited, describeBy(meanGUD, moonPhase, mat=T, digits=4))
names(gud.mPhase)[names(gud.mPhase)=="group1"]="MoonPhase"
limits=with(gud.mPhase,aes(ymax=mean+se, ymin=mean-se))
dodge=position_dodge(width=0.9)
ggplot(gud.mPhase, aes(x=MoonPhase, y=mean))+ 
  geom_point(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab('Mean GUD')+
  theme_set(theme_gray(base_size = 20))+
  scale_fill_grey()

#effect of habitat

gud.hab=with(gMeanVisited, describeBy(meanGUD, habitat, mat=T, digits=4))
names(gud.hab)[names(gud.hab)=="group1"]="Habitat"
limits=with(gud.hab,aes(ymax=mean+se, ymin=mean-se))
dodge=position_dodge(width=0.9)
ggplot(gud.hab, aes(x=Habitat, y=mean))+ 
  geom_point(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab('Mean GUD')+
  theme_set(theme_gray(base_size = 20))+
  scale_fill_grey()



#effect of microhabitat

gud.mhab=with(gMeanVisited, describeBy(meanGUD,FeedingTrayPosition , mat=T, digits=4))
names(gud.mhab)[names(gud.mhab)=="group1"]="FeedingTrayPosition"
limits=with(gud.mhab,aes(ymax=mean+se, ymin=mean-se))
dodge=position_dodge(width=0.9)
ggplot(gud.mhab, aes(x=FeedingTrayPosition, y=mean))+ 
  geom_point(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab('Mean GUD')+
  theme_set(theme_gray(base_size = 20))+
  scale_fill_grey()

#effect of season

gud.month=with(gMeanVisited, describeBy(meanGUD,season , mat=T, digits=4))
names(gud.month)[names(gud.month)=="group1"]="Season"
limits=with(gud.month,aes(ymax=mean+se, ymin=mean-se))
dodge=position_dodge(width=0.9)
ggplot(gud.month, aes(x=Season, y=mean))+ 
  geom_point(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  theme_set(theme_gray(base_size = 20))+
  ylab('Mean GUD')+
  scale_fill_grey()


#plot interaction between moon phase and feeding tray position
gMean.drop$moonPhase=factor(gMean.drop$moonPhase)
mPhase.trayPos.summary=with(gMeanVisited,describeBy(meanGUD, list(moonPhase, FeedingTrayPosition), mat=T, digits=2))
names(mPhase.trayPos.summary)[names(mPhase.trayPos.summary)=="group1"]="MoonPhase"
names(mPhase.trayPos.summary)[names(mPhase.trayPos.summary)=="group2"]="FeedingTrayPosition"
limits=with(mPhase.trayPos.summary,aes(ymax=mean+se, ymin=mean-se))
dodge=position_dodge(width=0.9)
ggplot(mPhase.trayPos.summary, aes(x=MoonPhase, y=mean, fill=FeedingTrayPosition))+ 
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab('Mean GUD')+
  theme_set(theme_gray(base_size = 20))+
  scale_fill_grey()

#plot interaction between moon phase and habitat

mPhase.hab.summary=with(gMeanVisited,describeBy(meanGUD, list(moonPhase, habitat), mat=T, digits=4))
names(mPhase.hab.summary)[names(mPhase.hab.summary)=="group1"]="MoonPhase"
names(mPhase.hab.summary)[names(mPhase.hab.summary)=="group2"]="Habitat"
limits=with(mPhase.hab.summary,aes(ymax=mean+se, ymin=mean-se))
dodge=position_dodge(width=0.9)
ggplot(mPhase.hab.summary, aes(x=MoonPhase, y=mean, fill=Habitat))+ 
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab('Mean GUD')+
  theme_set(theme_gray(base_size = 20))+
  scale_fill_grey()

#plot activity across night
ggplot(data=activity, aes(x=MoonPhase, y=Ncrossing, fill=Night))+geom_boxplot()+
  theme_set(theme_gray(base_size = 20))+scale_fill_brewer()

#ggplot activity across night for each month
ggplot(data=activity, aes(x=MoonPhase, y=Ncrossing, fill=Night))+geom_boxplot()+
  facet_wrap(~month)+
  theme_set(theme_gray(base_size = 20))+scale_fill_brewer()


#interaction between habitat and microhabitat

hab.micro.summary=with(gMeanVisited,describeBy(meanGUD, list(habitat, FeedingTrayPosition), mat=T, digits=4))
names(hab.micro.summary)[names(hab.micro.summary)=="group1"]="Habitat"
names(hab.micro.summary)[names(hab.micro.summary)=="group2"]="FeedingTrayPosition"
limits=with(hab.micro.summary,aes(ymax=mean+se, ymin=mean-se))
dodge=position_dodge(width=0.9)
ggplot(hab.micro.summary, aes(x=Habitat, y=mean, fill=FeedingTrayPosition))+ 
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab('Mean GUD')+
  theme_set(theme_gray(base_size = 20))+
  scale_fill_grey()

#interaction between month and moon phase

mPhase.mon.summary=with(gMeanVisited,describeBy(meanGUD, list(moonPhase, season), mat=T, digits=4))
names(mPhase.mon.summary)[names(mPhase.mon.summary)=="group1"]="MoonPhase"
names(mPhase.mon.summary)[names(mPhase.mon.summary)=="group2"]="Season"
limits=with(mPhase.mon.summary,aes(ymax=mean+se, ymin=mean-se))
dodge=position_dodge(width=0.9)
ggplot(mPhase.mon.summary, aes(x=Season, y=mean, fill=MoonPhase))+ 
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab('Mean GUD')+
  theme_set(theme_gray(base_size = 20))+
  scale_fill_grey()

#interaction between habitat and season

hab.mon.summary=with(gMeanVisited,describeBy(meanGUD, list(habitat, season), mat=T, digits=4))
names(hab.mon.summary)[names(hab.mon.summary)=="group1"]="Habitat"
names(hab.mon.summary)[names(hab.mon.summary)=="group2"]="Season"
limits=with(hab.mon.summary,aes(ymax=mean+se, ymin=mean-se))

colnames(HabS.letters)[which(colnames(HabS.letters)=="habitat")]="Habitat"
colnames(HabS.letters)[which(colnames(HabS.letters)=="season")]="Season"
limits=with(HabS.letters,aes(ymax=lsmean+SE, ymin=lsmean-SE))

dodge=position_dodge(width=0.9)
ggplot(HabS.letters, aes(x=Season, y=lsmean, fill=Habitat))+ 
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab('Mean GUD (g)')+
  coord_cartesian(ylim=c(2,3.5))+
  theme_classic(base_size=18)+
  geom_text(data=HabS.letters, aes(x=Season, y=lsmean+SE+0.1, label=.group),position = position_dodge(0.9),size=6)+
  scale_fill_grey()+
  theme(axis.line.x = element_line(color="black", size = 0.2),
        axis.line.y = element_line(color="black", size = 0.2))

#interaction between microhabitat and month

micro.mon.summary=with(gMeanVisited,describeBy(meanGUD, list(FeedingTrayPosition, season), mat=T, digits=4))
names(micro.mon.summary)[names(micro.mon.summary)=="group1"]="FeedingTrayPosition"
names(micro.mon.summary)[names(micro.mon.summary)=="group2"]="Season"
limits=with(micro.mon.summary,aes(ymax=mean+se, ymin=mean-se))
dodge=position_dodge(width=0.9)
ggplot(micro.mon.summary, aes(x=Season, y=mean, fill=FeedingTrayPosition))+ 
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab('Mean GUD')+
  theme_set(theme_gray(base_size = 20))+
  scale_fill_grey()

#interaction between habitat, microhabitat and season
mon.hab.muhab.summary=with(gMeanVisited,describeBy(meanGUD, list(season, habitat, FeedingTrayPosition), mat=T, digits=4))
names(mon.hab.muhab.summary)[names(mon.hab.muhab.summary)=="group1"]="Season"
names(mon.hab.muhab.summary)[names(mon.hab.muhab.summary)=="group2"]="Habitat"
names(mon.hab.muhab.summary)[names(mon.hab.muhab.summary)=="group3"]="FeedingTrayPosition"
limits=with(mon.hab.muhab.summary,aes(ymax=mean+se, ymin=mean-se))

colnames(HabSF.letters)[which(colnames(HabSF.letters)=="habitat")]="Habitat"
colnames(HabSF.letters)[which(colnames(HabSF.letters)=="season")]="Season"
colnames(HabSF.letters)[which(colnames(HabSF.letters)=="FeedingTrayPosition")]="Microhabitat"

limits=with(HabSF.letters,aes(ymax=lsmean+SE, ymin=lsmean-SE))

dodge=position_dodge(width=0.9)

ggplot(data=HabSF.letters, aes(x=Habitat, y=lsmean, fill=Microhabitat))+
  geom_bar(stat="identity", position="dodge")+
  facet_wrap(~Season, switch="x", scales="free_x")+
  theme(panel.margin=unit(0,"lines"), strip.background=element_blank())+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab("Mean GUD (g)")+
  xlab("")+
  theme_classic(base_size=18)+
  coord_cartesian(ylim=c(2,3.5))+
  geom_text(data=HabSF.letters, aes(x=Habitat, y=lsmean+SE+0.1, label=.group),position = position_dodge(0.9),size=6)+
  scale_fill_grey()+
  theme(axis.line.x = element_line(color="black", size = 0.2),
        axis.line.y = element_line(color="black", size = 0.2),
        strip.background = element_rect(colour="white"))

#plot interaction between moon phase, habitat and microhabitat
mPhase.hab.muhab.summary=with(gMeanVisited,describeBy(meanGUD, list(moonPhase, habitat, FeedingTrayPosition), mat=T, digits=4))
names(mPhase.hab.muhab.summary)[names(mPhase.hab.muhab.summary)=="group1"]="MoonPhase"
names(mPhase.hab.muhab.summary)[names(mPhase.hab.muhab.summary)=="group2"]="Habitat"
names(mPhase.hab.muhab.summary)[names(mPhase.hab.muhab.summary)=="group3"]="FeedingTrayPosition"
limits=with(mPhase.hab.muhab.summary,aes(ymax=mean+se, ymin=mean-se))
dodge=position_dodge(width=0.9)

ggplot(data=mPhase.hab.muhab.summary, aes(x=Habitat, y=mean, fill=FeedingTrayPosition))+
  geom_bar(stat="identity", position="dodge")+
  facet_wrap(~MoonPhase, switch="x", scales="free_x")+
  theme(panel.margin=unit(0,"lines"), strip.background=element_blank())+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab("Mean GUD")+
  theme_set(theme_gray(base_size = 20))+
  scale_fill_grey()

#plot interaction between moon phase, habitat and season
mPhase.hab.muhab.summary=with(gMeanVisited,describeBy(meanGUD, list(moonPhase, habitat, season), mat=T, digits=4))
names(mPhase.hab.muhab.summary)[names(mPhase.hab.muhab.summary)=="group1"]="MoonPhase"
names(mPhase.hab.muhab.summary)[names(mPhase.hab.muhab.summary)=="group2"]="Habitat"
names(mPhase.hab.muhab.summary)[names(mPhase.hab.muhab.summary)=="group3"]="Season"
limits=with(mPhase.hab.muhab.summary,aes(ymax=mean+se, ymin=mean-se))
dodge=position_dodge(width=0.9)

ggplot(data=mPhase.hab.muhab.summary, aes(x=Habitat, y=mean, fill=Season))+
  geom_bar(stat="identity", position="dodge")+
  facet_wrap(~MoonPhase, switch="x", scales="free_x")+
  theme(panel.margin=unit(0,"lines"), strip.background=element_blank())+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab("Mean GUD")+
  theme_set(theme_gray(base_size = 20))+
  scale_fill_grey()



#plot activity across night
ggplot(data=subset(guds, !is.na(GUD)), aes(x=MoonPhase, y=GUD, fill=Night))+geom_boxplot()+
  theme_set(theme_gray(base_size = 20))+scale_fill_brewer()

#ggplot activity across night for each month
ggplot(data=subset(guds, !is.na(GUD)), aes(x=MoonPhase, y=GUD, fill=Night))+geom_boxplot()+
  facet_wrap(~month)+
  theme_set(theme_gray(base_size = 20))+scale_fill_brewer()


