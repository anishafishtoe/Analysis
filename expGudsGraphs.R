library(ggplot2)
library(psych)

expGuds= read.csv("expGudsClean.csv")

#Rename time of cutting levels
expGuds$BefOnAft=unlist(lapply(expGuds$BefOnAft, function(x){if (x=="B") "Before" else if (x=="O") "On" else if (x=="A")"After"}))
expGuds$BefOnAft=factor(expGuds$BefOnAft,levels = c("Before","On","After"))

#effect of day of cutting and tree cut/uncut
gud.cut.summary=with(expGuds,describeBy(GUD, list(BefOnAft, cutUncut), mat=T, digits=2))
names(gud.cut.summary)[names(gud.cut.summary)=="group1"]="TimeOfCutting"
gud.cut.summary$TimeOfCutting=factor(gud.cut.summary$TimeOfCutting,levels = c("Before","On","After"))
names(gud.cut.summary)[names(gud.cut.summary)=="group2"]="CutUncut"
limits=with(gud.cut.summary,aes(ymax=mean+(1.96*se), ymin=mean-(1.96*se)))
dodge=position_dodge(width=0.9)
ggplot(gud.cut.summary, aes(x=TimeOfCutting, y=mean, fill=CutUncut))+ 
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab('GUD')+
  theme_set(theme_gray(base_size = 20))+
  scale_fill_grey()

#effect of day of cutting and feeding tray position and tree cut/uncut
gud.cut.micro.summary=with(expGuds,describeBy(GUD, list(BefOnAft, cutUncut, NF), mat=T, digits=4))
names(gud.cut.micro.summary)[names(gud.cut.micro.summary)=="group1"]="TimeOfCutting"
gud.cut.micro.summary$TimeOfCutting=factor(gud.cut.micro.summary$TimeOfCutting,levels = c("Before","On","After"))
names(gud.cut.micro.summary)[names(gud.cut.micro.summary)=="group2"]="CutUncut"
names(gud.cut.micro.summary)[names(gud.cut.micro.summary)=="group3"]="FeedingTrayPosition"
limits=with(gud.cut.micro.summary,aes(ymax=mean+(1.96*se), ymin=mean-(1.96*se)))
dodge=position_dodge(width=0.9)

ggplot(data=gud.cut.micro.summary, aes(x=TimeOfCutting, y=mean, fill=FeedingTrayPosition))+
  geom_bar(stat="identity", position="dodge")+
  facet_wrap(~CutUncut, switch="x", scales="free_x")+
  theme(panel.margin=unit(0,"lines"), strip.background=element_blank())+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab("Number of Crossings")+
  theme_set(theme_gray(base_size = 20))+
  scale_fill_grey()

