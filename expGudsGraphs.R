library(ggplot2)
library(psych)

expGuds= read.csv("expGudsClean.csv")
head(expGuds)

#Rename time of cutting levels
expGud$BefOnAft= revalue(expGuds$BefOnAft,c(B="Before" ,O= "On",A="After"))


#Does GUD decrease with time? 
with(expGudsRaw, plot(GUD~Date))
with(subset(expGudsRaw, cutUncut=="Cut"), plot(GUD~Date, main="Cut before"))
with(subset(expGudsRaw, cutUncut=="Uncut"), plot(GUD~Date, main="Uncut before"))


#effect of tree cutting
gud.cut=with(expGudMean, describeBy(meanGUD, BefOnAft, mat=T, digits=4))
names(gud.cut)[names(gud.cut)=="group1"]="TimeOfCutting"
gud.cut$TimeOfCutting=factor(gud.cut$TimeOfCutting,levels = c("Before","After"))
limits=with(gud.cut,aes(ymax=mean+se, ymin=mean-se))
dodge=position_dodge(width=0.9)
ggplot(gud.cut, aes(x=TimeOfCutting, y=mean))+ 
  geom_bar(stat='identity', position=dodge, width=0.5, fill="#332F2E")+
  geom_errorbar(limits, position=dodge, width=0.15)+
  ylab('Mean GUD (g)')+
  xlab("Time of Cutting")+
  coord_cartesian(ylim=c(2.83,2.91))+
  theme_classic(base_size=20)+
  theme(axis.line.x = element_line(color="black", size = 0.2),
        axis.line.y = element_line(color="black", size = 0.2),
        strip.background = element_rect(colour="white"))

#effect of day of cutting and tree cut/uncut
gud.cut.summary=with(expGuds,describeBy(GUD, list(BefOnAft, cutUncut), mat=T, digits=2))
names(gud.cut.summary)[names(gud.cut.summary)=="group1"]="TimeOfCutting"
gud.cut.summary$TimeOfCutting=factor(gud.cut.summary$TimeOfCutting,levels = c("Before","After"))
names(gud.cut.summary)[names(gud.cut.summary)=="group2"]="CutUncut"
limits=with(gud.cut.summary,aes(ymax=mean+se, ymin=mean-se))

colnames(treatT.letters)[which(colnames(treatT.letters)=="cutUncut")]="Treatment"
colnames(treatT.letters)[which(colnames(treatT.letters)=="BefOnAft")]="TimeOfCutting"
limits=with(treatT.letters,aes(ymax=lsmean+SE, ymin=lsmean-SE))

dodge=position_dodge(width=0.9)
ggplot(treatT.letters, aes(x=Treatment, y=lsmean, fill=TimeOfCutting))+ 
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab('Mean GUD (g)')+
  xlab('Treatment')+
  coord_cartesian(ylim=c(2.8,2.95))+
  theme_classic(base_size=18)+
  geom_text(data=treatT.letters, aes(x=Treatment, y=lsmean+SE+0.01, label=.group),position = position_dodge(0.9),size=6)+
  scale_fill_grey()+
  theme(axis.line.x = element_line(color="black", size = 0.2),
        axis.line.y = element_line(color="black", size = 0.2))

#effect of day of cutting and feeding tray position and tree cut/uncut
gud.cut.micro.summary=with(expGudMean,describeBy(meanGUD, list(BefOnAft, cutUncut, FeedingTrayPosition), mat=T, digits=4))
names(gud.cut.micro.summary)[names(gud.cut.micro.summary)=="group1"]="TimeOfCutting"
gud.cut.micro.summary$TimeOfCutting=factor(gud.cut.micro.summary$TimeOfCutting,levels = c("Before","After"))
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

