#guds

setwd("F:/NCBS/Thesis/Data/")
guds=read.csv("GUDs.csv")
str(guds)

sort(levels(guds$FStn))

#Delete columns X, X.1, Husksf, HusksN
guds=guds[,-(14:17)]

#Correct feeding station names
guds$FStn[which(guds$FStn=="D1")]="D11"
guds$FStn[which(guds$FStn=="D31 ")]="D31"

guds$FStn=factor(guds$FStn)

#Set PrintsInside=NA for row 136
guds$PrintsInside[which(guds$PrintsInside=="")]=NA

#Set N as B for prints inside
guds$PrintsInside[which(guds$PrintsInside=="N")]="B"
guds$PrintsAround[which(guds$PrintsAround=="N")]="B"
guds$PrintsAround=factor(guds$PrintsAround)
guds$PrintsInside=factor(guds$PrintsInside)

#Correct ant and bird entries
guds$AntBirdN[which(guds$AntBirdN=="")]=NA
guds$AntBirdN[which(guds$AntBirdN=="A ")]="A"
guds$AntBirdN[which(guds$AntBirdN=="I")]=NA
guds$AntBirdF[which(guds$AntBirdF=="I")]=NA

guds$AntBirdN=factor(guds$AntBirdN)
guds$AntBirdF=factor(guds$AntBirdF)

#Delete Sumit's data
guds=guds[-(which(guds$Date %in% c("02-01-2016","03-01-2016", "04-01-2016", "05-01-2016" ))),]
guds$Date=factor(guds$Date)


#create a new dataframe with gud=3g if gud more that 3g
guds$GUDN[-which(is.na(guds$GUDN))]= unlist(lapply(guds$GUDN[-which(is.na(guds$GUDN))], function(x){if(x>3) 3 else x}))
guds$GUDF[-which(is.na(guds$GUDF))]=unlist(lapply(guds$GUDF[-which(is.na(guds$GUDF))], function(x){if(x>3) 3 else x}))


#Column for phase replicates
date.df=data.frame(f1=c("24-12-2015", "25-12-2015", "26-12-2015"), f2=c("24-01-2016", "25-01-2016", "26-01-2016"),
                   n1=c("11-01-2016", "12-01-2016", "13-01-2016"), n2=c("08-02-2016","09-02-2016", "10-02-2016"),
                   wn1=c("31-01-2016", "01-02-2016","02-02-2016"), wn2=c("01-03-2016", "02-03-2016", "03-03-2016"),
                   wx1=c("16-01-2016","17-01-2016","18-01-2016"), wx2=c("16-02-2016","17-02-2016","18-02-2016"))

guds$phaseRep[guds$Date %in% date.df[["f1"]]]="Full1"
guds$phaseRep[guds$Date %in% date.df[["f2"]]]="Full2"
guds$phaseRep[guds$Date %in% date.df[["n1"]]]="New1"
guds$phaseRep[guds$Date %in% date.df[["n2"]]]="New2"
guds$phaseRep[guds$Date %in% date.df[["wn1"]]]="Wane1"
guds$phaseRep[guds$Date %in% date.df[["wn2"]]]="Wane2"
guds$phaseRep[guds$Date %in% date.df[["wx1"]]]="Wax1"
guds$phaseRep[guds$Date %in% date.df[["wx2"]]]="Wax2"

#How many birds and ants and during which moon phase
length(which(guds$AntBirdN=="B"|guds$AntBirdN=="AB"))
length(which(guds$AntBirdF=="B"|guds$AntBirdF=="AB"))

table(guds$phaseRep[which(guds$AntBirdN=="B"|guds$AntBirdN=="AB")])

#which phase rep had birds and gerbils feeding

table(guds$phaseRep[which(guds$AntBirdN=="B"|guds$AntBirdN=="AB" & !is.na(guds$PrintsInside))])
table(guds$phaseRep[which(guds$AntBirdF=="B"|guds$AntBirdF=="AB" & !is.na(guds$PrintsInside))])


#How much did ants and birds eat up the seeds

guds.ants=c(guds$GUDN[which(guds$AntBirdN=="A" & is.na(guds$PrintsInside))],guds$GUDF[which(guds$AntBirdF=="A" & is.na(guds$PrintsInside))] )
boxplot(guds.ants, xlab="Ants", ylab="Number of seeds harvested")

guds.birds=c(guds$GUDN[which(guds$AntBirdN=="B" & is.na(guds$PrintsInside))],guds$GUDF[which(guds$AntBirdF=="B" & is.na(guds$PrintsInside))] )
boxplot(guds.birds, xlab="Birds", ylab="Number of seeds harvested")

guds.antbird=c(guds$GUDN[which(guds$AntBirdN=="AB" & is.na(guds$PrintsInside))],guds$GUDF[which(guds$AntBirdF=="AB" & is.na(guds$PrintsInside))] )
boxplot(guds.antbird, xlab="Ants and birds", ylab="Number of seeds harvested")

#If only ants/birds fed from the stations, set GUD as 3.

guds$GUDN[!is.na(guds$AntBirdN) & is.na(guds$PrintsInside)]=3
guds$GUDF[!is.na(guds$AntBirdF) & is.na(guds$PrintsInside)]=3

#If gerbils and birds/ants fed from feeding station, set GUD as NA

guds$GUDN[which(guds$AntBirdN %in% c("AB","B") & !is.na(guds$PrintsInside))]=NA
guds$GUDF[which(guds$AntBirdF %in% c("AB","B") & !is.na(guds$PrintsInside))]=NA

#mean guds per feeding station

mGudN.df=aggregate(guds$GUDN[-which(is.na(guds$GUDN))], by=list(phaseRep=guds$phaseRep[-which(is.na(guds$GUDN))], fStn=guds$FStn[-which(is.na(guds$GUDN))], habitat=guds$Habitat[-which(is.na(guds$GUDN))], moonPhase=guds$MoonPhase[-which(is.na(guds$GUDN))]), mean)
colnames(mGudN.df)[5]="meanGUD"
mGudN.df$nf=rep("N",nrow(mGudN.df))

mGudF.df=aggregate(guds$GUDF[-which(is.na(guds$GUDF))], by=list(phaseRep=guds$phaseRep[-which(is.na(guds$GUDF))], fStn=guds$FStn[-which(is.na(guds$GUDF))], habitat=guds$Habitat[-which(is.na(guds$GUDF))], moonPhase=guds$MoonPhase[-which(is.na(guds$GUDF))]), mean)
colnames(mGudF.df)[5]="meanGUD"
mGudF.df$nf=rep("F",nrow(mGudF.df))

gMean=rbind(mGudN.df, mGudF.df)
colnames(gMean)[which(colnames(gMean)=="nf")]="feedingTrayPosition"
gMean$feedingTrayPosition=factor(gMean$feedingTrayPosition)
