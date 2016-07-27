#Tree cutting experiment- GUDS- clean up

setwd("F:/NCBS/Thesis/Data/")
expGudsRaw=read.csv("Tree cutting exp1806.csv.")

library(tidyr)
library(plyr)

#Change FStn column name
colnames(expGuds)[which(colnames(expGuds)=="FStn")]="fStn"

#Correct feeding station name
expGuds$fStn[which(expGuds$fStn=="TI")]="T1"

#Set destroyed trays to NA
expGuds$GUDF[expGuds$Date=="15-03-2016" & expGuds$fStn=="T3"]=NA
#Delete columns
expGuds=expGuds[-which(colnames(expGuds) %in% c("Digging","Destroyed","Comments"))]

#Re-arrange factor levels of time of cutting
expGuds$BefOnAft=revalue(expGuds$BefOnAft, c("B"= "Before", "A"="After", "O"="On"))
expGuds$BefOnAft=factor(expGuds$BefOnAft, c("Before","On","After"))


#Separate column for cut and uncut trees
expGuds$cutUncut= ifelse(substring(expGuds$fStn,1,1)=="T", "Cut", "Uncut")


#convert GUDS from wide to long
expGuds=gather(expGuds, NF, GUD, GUDN, GUDF)
expGuds$NF= ifelse(expGuds$NF=="GUDN","N","F")

#Omit first day before cutting
expGuds= expGuds[-which(expGuds$Date %in% "15-03-2016"),]

#Omit data on the day tree was cut
expGuds= expGuds[-which(expGuds$BefOnAft %in% "On"),]
expGuds$BefOnAft=factor(expGuds$BefOnAft)
expGuds$Date= factor(expGuds$Date)

#arrange according to Time of Cutting and Date
expGuds=arrange(expGuds, BefOnAft, Date, fStn)

#Mean for before and after

expGuds.narm=expGudsRaw[-which(is.na(expGudsRaw$GUD)),]
expGudMean= with(expGuds.narm,aggregate(GUD, by=list(BefOnAft=BefOnAft, fStn=FStn, FeedingTrayPosition=NF, cutUncut=cutUncut), mean))
colnames(expGudMean)[5]="meanGUD"


#write into file
write.csv(expGuds,"expGudsClean.csv")
write.csv(expGudMean, "expGudsMean0507.csv")
