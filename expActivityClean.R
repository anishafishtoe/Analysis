#Clean activity- tree cutting exp

setwd("F:/NCBS/Thesis/Data/")
exp.act=read.csv("Tree cutting exp_Activity2904.csv")

#Delete foot measurement data
exp.act=exp.act[-which(colnames(exp.act) %in% c("FLength","FWidth", "HLength", "HWidth", "HTLength","HTWidth", "Digging", "Comments"))]

#Separate column for cut and uncut trees
exp.act$cutUncut= ifelse(substring(exp.act$FStn,1,1)=="T", "Cut", "Uncut")

#Rename time of cutting levels
exp.act$BefOnAft=factor(exp.act$BefOnAft,levels = c("B","O","A"))
levels(exp.act$BefOnAft)=c("Before","On","After")

