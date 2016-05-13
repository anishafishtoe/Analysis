#ANOVA activity

setwd("F:/NCBS/Thesis/Data/")
activity= read.csv("Footprints.csv", as.is=T)
nrow(activity)
str(activity)

#Convert moon-phase, habitat, date, fst, NF to factors

activity$MoonPhase=as.factor(activity$MoonPhase)
activity$Habitat=as.factor(activity$Habitat)
activity$FStn=as.factor(activity$FStn)
activity$NF=as.factor(activity$NF)

#Correct feeding station names
levels(activity$FStn)
activity$FStn[which(activity$FStn=="B2")]="B3"
activity$FStn[which(activity$FStn=="D1")]="D11"
activity$FStn[which(activity$FStn=="B31")]="D31"
activity$FStn[which(activity$FStn %in% paste("D",seq(from=34, to=42, by=1), sep=""))]="D33"
activity$FStn=factor(activity$FStn)


#NAs
activity$Ncrossing[which(is.na(activity$Ncrossing))]=8


#Correct dates

activity$Date[which(activity$Date=="11-01-2015")]="11-01-2016"
activity$Date=as.factor(activity$Date)


#Remove Sumit's data
activity=activity[-(which(activity$Date %in% c("02-01-2016","03-01-2016", "04-01-2016", "05-01-2016" ))),]
activity$Date=factor(activity$Date)

levels(activity$Date)


#Remove duplicate entries for each feeding station

activity=activity[!duplicated(activity[,c(1,4,5)]),]

#Column for phase replicates
date.df=data.frame(f1=c("24-12-2015", "25-12-2015", "26-12-2015"), f2=c("24-01-2016", "25-01-2016", "26-01-2016"),
                   n1=c("11-01-2016", "12-01-2016", "13-01-2016"), n2=c("08-02-2016","09-02-2016", "10-02-2016"),
                   wn1=c("31-01-2016", "01-02-2016","02-02-2016"), wn2=c("01-03-2016", "02-03-2016", "03-03-2016"),
                   wx1=c("16-01-2016","17-01-2016","18-01-2016"), wx2=c("16-02-2016","17-02-2016","18-02-2016"))

activity$phaseRep[activity$Date %in% date.df[["f1"]]]="Full1"
activity$phaseRep[activity$Date %in% date.df[["f2"]]]="Full2"
activity$phaseRep[activity$Date %in% date.df[["n1"]]]="New1"
activity$phaseRep[activity$Date %in% date.df[["n2"]]]="New2"
activity$phaseRep[activity$Date %in% date.df[["wn1"]]]="Wane1"
activity$phaseRep[activity$Date %in% date.df[["wn2"]]]="Wane2"
activity$phaseRep[activity$Date %in% date.df[["wx1"]]]="Wax1"
activity$phaseRep[activity$Date %in% date.df[["wx2"]]]="Wax2"


#mean activity per feeding station

mAct.df=aggregate(activity$Ncrossing, by=list(phaseRep=activity$phaseRep, fStn=activity$FStn, NF=activity$NF, habitat=activity$Habitat, moonPhase=activity$MoonPhase), mean)
colnames(mAct.df)[6]="meanCrossings"
mAct.df$moonPhase
