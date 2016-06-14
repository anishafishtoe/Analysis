#Tree cutting experiment- GUDS- clean up

setwd("F:/NCBS/Thesis/Data/")
expGuds=read.csv("Tree cutting exp2904.csv")

library(tidyr)
library(plyr)

#Change FStn column name
colnames(expGuds)[which(colnames(expGuds)=="FStn")]="fStn"

#Correct feeding station name
expGuds$fStn[which(expGuds$fStn=="TI")]="T1"

#Set destroyed trays to NA
expGuds$GUDF[expGuds$Date=="15-03-2016" & expGuds$fStn=="T3"]=NA

#Separate column for cut and uncut trees
expGuds$cutUncut= ifelse(substring(expGuds$fStn,1,1)=="T", "Cut", "Uncut")

#Delete columns
expGuds=expGuds[-which(colnames(expGuds) %in% c("PrintsAround","PrintsInside","Digging","Destroyed","Comments"))]

#Re-arrange factor levels of time of cutting
expGuds$BefOnAft=revalue(expGuds$BefOnAft, c("B"= "Before", "A"="After", "O"="On"))
expGuds$BefOnAft=factor(expGuds$BefOnAft, c("Before","On","After"))

#convert GUDS from wide to long
expGuds=gather(expGuds, NF, GUD, GUDN, GUDF)
expGuds$NF= ifelse(expGuds$NF=="GUDN","N","F")

#Omit first day before cutting
expGuds= expGuds[-which(expGuds$Date %in% "15-03-2016"),]
expGuds$Date= factor(expGuds$Date)

#Omit data on the day tree was cut
expGuds= expGuds[-which(expGuds$BefOnAft %in% "On"),]
expGuds$BefOnAft=factor(expGuds$BefOnAft)
expGuds$Date= factor(expGuds$Date)

#write into file
write.csv(expGuds,"expGudsClean.csv")
