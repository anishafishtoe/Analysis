#Clean activity- tree cutting exp
library(dplyr)
library(reshape2)

setwd("F:/NCBS/Thesis/Data/")
exp.act=read.csv("Tree cutting exp_Activity2904.csv")

#Delete foot measurement data
exp.act=exp.act[-which(colnames(exp.act) %in% c("FLength","FWidth", "HLength", "HWidth", "HTLength","HTWidth", "Digging", "Comments", "InOut"))]

#Separate column for cut and uncut trees
exp.act$cutUncut= ifelse(substring(exp.act$FStn,1,1)=="T", "Cut", "Uncut")

#Rename time of cutting levels
levels(exp.act$BefOnAft)=unlist(lapply(levels(exp.act$BefOnAft), function(x){
  if (x=="B") "Before" else if (x=="O") "On" else if (x=="A")"After"}))
exp.act$BefOnAft=factor(exp.act$BefOnAft,levels = c("Before","On","After"))

#Replace NT names with T
levels(exp.act$FStn)=gsub("NT", "T", levels(exp.act$FStn))

#delete duplicates
exp.act=exp.act[!duplicated(exp.act[,c(1,2,3,4)]),]

#Create separate columns for cut and uncut, n and f
exp.act=arrange(exp.act, Date, BefOnAft)
exp.act=exp.act %>% group_by(Date, BefOnAft, FStn) %>% mutate(id=row_number())
exp.act.wide=dcast(exp.act, formula=Date+BefOnAft+FStn~cutUncut+NF, value.var = "Ncrossing")

#Create columns containing differences between cut and uncut trees
exp.act.wide$DiffN=with(exp.act.wide,ifelse(!is.na(Cut_N) & !is.na(Uncut_N),Cut_N-Uncut_N,NA))
exp.act.wide$DiffF=with(exp.act.wide,ifelse(!is.na(Cut_F) & !is.na(Uncut_F),Cut_F-Uncut_F,NA))


#differences between N and F 
exp.act.wide$CutDiff=with(exp.act.wide,ifelse(!is.na(Cut_N) & !is.na(Cut_F),Cut_N-Cut_F,NA))
exp.act.wide$UncutDiff=with(exp.act.wide,ifelse(!is.na(Uncut_N) & !is.na(Uncut_F),Uncut_N-Uncut_F,NA))

#Omit first day before cutting
exp.act= exp.act[-which(exp.act$Date %in% "15-03-2016"),]
exp.act.wide= exp.act.wide[-which(exp.act.wide$Date %in% "15-03-2016"),]
  #Omit data on the day tree was cut
  exp.act= exp.act[-which(exp.act$BefOnAft %in% "On"),]
  exp.act.wide= exp.act.wide[-which(exp.act.wide$BefOnAft %in% "On"),]
exp.act$Date= factor(exp.act$Date)
exp.act.wide$Date= factor(exp.act.wide$Date)
exp.act$BefOnAft=factor(exp.act$BefOnAft)
exp.act.wide$BefOnAft=factor(exp.act.wide$BefOnAft)

#Write files
write.csv(exp.act, "expActClean.csv")
write.csv(exp.act.wide, "expActClean_Wide.csv")
