library(plyr)
library(dplyr)
setwd("F:/NCBS/Thesis/Data/")
activity= read.csv("Footprints.csv", as.is=T)


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

#Remove duplicate entries for each feeding station
activity=activity[!duplicated(activity[,c(1,4,5)]),]

#Create a new dataframe containing zeroes for feeding stations around which there is no activity
fStnFull=c( "A11", "A13", "A31", "A33", "B1",  "B13", "B3",  "B33", "D11", "D13", "D31", "D33", "L11", "L13", "L31", "L33")
activityNew= data.frame(Date=rep(Dates, each=length(fStnFull)*2),
                        FStn=rep(fStnFull, each=2, times=length(Dates)),
                        NF=rep(c("N","F"), length(fStnFull)*length(Dates)), 
                        Ncrossing=0)
activityJoin=join(activity, activityNew,by=c("Date","FStn", "NF"), type="full")
activityJoin=arrange(activityJoin, Date,  FStn)

#Fill moon phases
for(i in c("Full","New","Wane","Wax")){
  activityJoin$MoonPhase[which(activityJoin$Date %in% unique(activity$Date[activity$MoonPhase==i]))]=i
}

#Fill habitats
activityJoin$Habitat=ifelse(substring(activityJoin$FStn,1,1) %in% c("A","L"), "Dense","Sparse")


#Column for phase replicates
date.df=data.frame(f1=c("24-12-2015", "25-12-2015", "26-12-2015"), f2=c("24-01-2016", "25-01-2016", "26-01-2016"),
                   n1=c("11-01-2016", "12-01-2016", "13-01-2016"), n2=c("08-02-2016","09-02-2016", "10-02-2016"),
                   wn1=c("31-01-2016", "01-02-2016","02-02-2016"), wn2=c("01-03-2016", "02-03-2016", "03-03-2016"),
                   wx1=c("16-01-2016","17-01-2016","18-01-2016"), wx2=c("16-02-2016","17-02-2016","18-02-2016"))

activityJoin$phaseRep[activityJoin$Date %in% date.df[["f1"]]]="Full1"
activityJoin$phaseRep[activityJoin$Date %in% date.df[["f2"]]]="Full2"
activityJoin$phaseRep[activityJoin$Date %in% date.df[["n1"]]]="New1"
activityJoin$phaseRep[activityJoin$Date %in% date.df[["n2"]]]="New2"
activityJoin$phaseRep[activityJoin$Date %in% date.df[["wn1"]]]="Wane1"
activityJoin$phaseRep[activityJoin$Date %in% date.df[["wn2"]]]="Wane2"
activityJoin$phaseRep[activityJoin$Date %in% date.df[["wx1"]]]="Wax1"
activityJoin$phaseRep[activityJoin$Date %in% date.df[["wx2"]]]="Wax2"


#Drop foot measurement columns
activity=activity[-c(which(colnames(activity)%in% c("FLength","FWidth","HLength","HWidth","HTLength","HTWidth","Digging","FTLength","Comments", "FTWidth","InOut")))]

#Create another column in activity for month
activityJoin$month=ifelse(activityJoin$phaseRep %in% grep("1$",activityJoin$phaseRep, value = T), "Month1", "Month2")

#Create column Night within activity numbering each night of sampling
library(dplyr)
library(plyr)
Dates=c("24-12-2015", "25-12-2015", "26-12-2015",
  "11-01-2016", "12-01-2016", "13-01-2016",
  "16-01-2016","17-01-2016","18-01-2016",
  "24-01-2016", "25-01-2016", "26-01-2016",
  "31-01-2016", "01-02-2016","02-02-2016",
  "08-02-2016","09-02-2016", "10-02-2016",
  "16-02-2016","17-02-2016","18-02-2016",
  "01-03-2016", "02-03-2016", "03-03-2016")
activityJoin$Date=factor(activityJoin$Date, c("24-12-2015", "25-12-2015", "26-12-2015",
                                      "11-01-2016", "12-01-2016", "13-01-2016",
                                      "16-01-2016","17-01-2016","18-01-2016",
                                      "24-01-2016", "25-01-2016", "26-01-2016",
                                      "31-01-2016", "01-02-2016","02-02-2016",
                                      "08-02-2016","09-02-2016", "10-02-2016",
                                      "16-02-2016","17-02-2016","18-02-2016",
                                      "01-03-2016", "02-03-2016", "03-03-2016"))
activityJoin=activityJoin %>% group_by(phaseRep) %>% mutate(Night=paste("N",as.numeric(factor(Date)), sep=""))



#Create a new dataframe containing mean activity per feeding station
mAct.df=aggregate(activityJoin$Ncrossing, by=list(phaseRep=activityJoin$phaseRep, fStn=activityJoin$FStn, NF=activityJoin$NF, habitat=activityJoin$Habitat, moonPhase=activityJoin$MoonPhase), mean)
colnames(mAct.df)[6]="meanCrossings"
colnames(mAct.df)[colnames(mAct.df)=="NF"]="FeedingTrayPosition"

#Create another column phaseRep1 for month
mAct.df$season=ifelse(mAct.df$phaseRep %in% grep("1$",mAct.df$phaseRep, value = T), "Winter", "Spring")
mAct.df$season=factor(mAct.df$season)


#write dataframes into file
write.csv(mAct.df, "meanActivity.csv")
write.csv(activity, "activityClean.csv")
write.csv(activityJoin, "activityJoin.csv")
