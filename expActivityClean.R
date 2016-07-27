#Clean activity- tree cutting exp
library(dplyr)
library(plyr)
library(reshape2)

setwd("F:/NCBS/Thesis/Data/")
exp.act=read.csv("Tree cutting exp_Activity2904.csv")

#Delete foot measurement data
exp.act=exp.act[-which(colnames(exp.act) %in% c("FLength","FWidth", "HLength", "HWidth", "HTLength","HTWidth", "Digging", "Comments", "InOut"))]

#delete duplicates
exp.act=exp.act[!duplicated(exp.act[,c(1,2,3,4)]),]
write.csv(exp.act, "expActDupRM.csv")

#Create a new data frame with 0s for fstns with no activity
expActNew= data.frame(Date=rep(unique(exp.act$Date), each=length(levels(exp.act$FStn))*2),
                        FStn=rep(levels(exp.act$FStn), each=2, times=length(unique(exp.act$Date))),
                        NF=rep(c("N","F"), length(levels(exp.act$FStn))*length(unique(exp.act$Date))), 
                        Ncrossing=0)
expActJoin=join(exp.act, expActNew,by=c("Date","FStn", "NF"), type="full")
expActJoin=arrange(expActJoin, Date,  FStn)

#Fill in BefOnAft
for(i in c("B","O","A")){
  expActJoin$BefOnAft[which(expActJoin$Date %in% unique(exp.act$Date[exp.act$BefOnAft==i]))]=i
}


#Separate column for cut and uncut trees
expActJoin$cutUncut= ifelse(substring(expActJoin$FStn,1,1)=="T", "Cut", "Uncut")
exp.act$cutUncut= ifelse(substring(exp.act$FStn,1,1)=="T", "Cut", "Uncut")

#Rename time of cutting levels
levels(expActJoin$BefOnAft)=unlist(lapply(levels(expActJoin$BefOnAft), function(x){
  if (x=="B") "Before" else if (x=="O") "On" else if (x=="A")"After"}))
expActJoin$BefOnAft=factor(expActJoin$BefOnAft,levels = c("Before","On","After"))

######### Don't do this if not converting to some format. Need separate FStns for random effect--
#Replace NT names with T
levels(exp.act$FStn)=gsub("NT", "T", levels(exp.act$FStn))
#Create separate columns for cut and uncut, n and f
expActJoin=arrange(expActJoin, Date, BefOnAft)
expActJoin=expActJoin %>% group_by(Date, BefOnAft, FStn) %>% mutate(id=row_number())
exp.act.wide=dcast(expActJoin, formula=Date+BefOnAft+FStn~cutUncut+NF, value.var = "Ncrossing")
#Create columns containing differences between cut and uncut trees
exp.act.wide$DiffN=with(exp.act.wide,ifelse(!is.na(Cut_N) & !is.na(Uncut_N),Cut_N-Uncut_N,NA))
exp.act.wide$DiffF=with(exp.act.wide,ifelse(!is.na(Cut_F) & !is.na(Uncut_F),Cut_F-Uncut_F,NA))
#differences between N and F 
exp.act.wide$CutDiff=with(exp.act.wide,ifelse(!is.na(Cut_N) & !is.na(Cut_F),Cut_N-Cut_F,NA))
exp.act.wide$UncutDiff=with(exp.act.wide,ifelse(!is.na(Uncut_N) & !is.na(Uncut_F),Uncut_N-Uncut_F,NA))
#######################

#Omit first day before cutting
expActJoin= expActJoin[-which(expActJoin$Date %in% "15-03-2016"),]

#Omit data on the day tree was cut
expActJoin= expActJoin[-which(expActJoin$BefOnAft %in% "On"),]
expActJoin$Date= factor(expActJoin$Date)
expActJoin$BefOnAft=factor(expActJoin$BefOnAft)

exp.act.wide= exp.act.wide[-which(exp.act.wide$Date %in% "15-03-2016"),]
exp.act.wide= exp.act.wide[-which(exp.act.wide$BefOnAft %in% "On"),]
exp.act.wide$Date= factor(exp.act.wide$Date)
exp.act.wide$BefOnAft=factor(exp.act.wide$BefOnAft)

#Take means for each feeding station before and after tree is cut
expActMean= aggregate(expActJoin$Ncrossing, by=list(BefOnAft=expActJoin$BefOnAft, fStn=expActJoin$FStn, FeedingTrayPosition=expActJoin$NF, cutUncut=expActJoin$cutUncut), mean)
colnames(expActMean)[5]="meanCrossings"



#Write files
write.csv(expActJoin, "expActClean.csv")
write.csv(exp.act.wide, "expActClean_Wide.csv")
write.csv(expActMean, "expActMean.csv")
