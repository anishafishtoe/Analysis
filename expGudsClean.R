#Tree cutting experiment- GUDS- clean up

setwd("F:/NCBS/Thesis/Data/")
expGuds=read.csv("Tree cutting exp2904.csv")

library(tidyr)
library(plyr)

#Change FStn column name
colnames(expGuds)[which(colnames(expGuds)=="FStn")]="fStn"

#Correct feeding station name
expGuds$FStn[which(expGuds$FStn=="TI")]="T1"

#Set destroyed trays to NA
expGuds$GUDF[expGuds$Date=="15-03-2016" & expGuds$FStn=="T3"]=NA

#Separate column for cut and uncut trees
expGuds$cutUncut= ifelse(substring(expGuds$FStn,1,1)=="T", "Cut", "Uncut")

#Delete columns
expGuds=expGuds[-which(colnames(expGuds) %in% c("PrintsAround","PrintsInside","Digging","Destroyed","Comments"))]

#Re-arrange factor levels of time of cutting
gudMean$BefOnAft=factor(gMeans$BefOnAft, c("Before","On","After"))

#convert GUDS from wide to long
expGuds=gather(expGuds, NF, GUD, GUDN, GUDF)
expGuds$NF= ifelse(expGuds$NF=="GUDN","N","F")
expGuds1=arrange(expGuds, Date, FStn)
head(expGuds)