#guds

setwd("F:/NCBS/Thesis/Data/")
guds=read.csv("GUDs.csv")


#IMP- correct Moon phase of gMean
gMean$moonPhase=gMean$moonPhase[gMean$phaseRep=="Wane1"]="Wane"

#Delete columns X, X.1, Husksf, HusksN
guds=guds[,-(14:17)]

#Correct feeding station names
guds$FStn[which(guds$FStn=="D1")]="D11"
guds$FStn[which(guds$FStn=="D31 ")]="D31"

guds$FStn=factor(guds$FStn)

#Set PrintsInside=NA for row 136
guds$PrintsInside[which(guds$PrintsInside=="")]=NA

#set GUDN/F=NA if destroyed
guds$GUDN[which(guds$Destroyed=="B")]=NA
guds$GUDN[which(guds$Destroyed=="B partly destroyed")]=NA
guds$GUDN[which(guds$Destroyed=="BF")]=NA
guds$GUDN[which(guds$Destroyed=="N")]=NA
guds$GUDN[which(guds$Destroyed=="N destroyed")]=NA
guds$GUDN[which(guds$Destroyed=="NF")]=NA

guds$GUDF[which(guds$Destroyed=="BF")]=NA
guds$GUDF[which(guds$Destroyed=="F")]=NA
guds$GUDF[which(guds$Destroyed=="FP")]=NA
guds$GUDF[which(guds$Destroyed=="NF")]=NA

#Set N as NA for prints inside/around
guds$PrintsInside[which(guds$PrintsInside=="N")]=NA
guds$PrintsAround[which(guds$PrintsAround=="N")]=NA
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


#gud=3g if gud more that 3g
guds$GUDN[-which(is.na(guds$GUDN))]= unlist(lapply(guds$GUDN[-which(is.na(guds$GUDN))], function(x){if(x>3) 3 else x}))
guds$GUDF[-which(is.na(guds$GUDF))]=unlist(lapply(guds$GUDF[-which(is.na(guds$GUDF))], function(x){if(x>3) 3 else x}))


#If only ants/birds fed from the stations, set GUD as 3.
guds$GUDN[!is.na(guds$AntBirdN) & is.na(guds$PrintsInside)]=3
guds$GUDF[!is.na(guds$AntBirdF) & is.na(guds$PrintsInside)]=3

#If gerbils and birds/ants fed from feeding station, set GUD as NA
guds$GUDN[which(guds$AntBirdN %in% c("AB","B") & !is.na(guds$PrintsInside))]=NA
guds$GUDF[which(guds$AntBirdF %in% c("AB","B") & !is.na(guds$PrintsInside))]=NA

#Delete unnecessary columns
guds= guds[-which(colnames(guds) %in% c( "Destroyed","Digging", "Comments"))]

#Create a dataframe containing only rows with FStns that have prints around
gudVisited=subset(guds, PrintsInside %in% c("B","BF","F")| PrintsAround %in% c("B","BF","F"))

#Create a new dataframe containing 3g for feeding stations around which there is no activity--these were probably omitted fromfull: all rows in x with matching columns in y, then the rows of y that don't match x.
#full join: all rows in x with matching columns in y, then the rows of y that don't match x.
fStnFull=c( "A11", "A13", "A31", "A33", "B1",  "B13", "B3",  "B33", "D11", "D13", "D31", "D33", "L11", "L13", "L31", "L33")
gudNew= data.frame(Date=rep(Dates, each=length(fStnFull)),
                        FStn=rep(fStnFull, length(Dates)),
                        GUDN=3, GUDF=3, 
                        AntBirdN=NA, AntBirdF=NA)
gudJoin=join(guds, gudNew,by=c("Date","FStn"), type="full")
gudJoin=arrange(gudJoin, Date,  FStn)

#Fill moon phases
for(i in c("Full","New","Wane","Wax")){
  gudJoin$MoonPhase[which(gudJoin$Date %in% unique(guds$Date[activity$MoonPhase==i]))]=i
}

#Fill habitats
gudJoin$Habitat=ifelse(substring(gudJoin$FStn,1,1) %in% c("A","L"), "Dense","Sparse")

#Column for phase replicates
date.df=data.frame(f1=c("24-12-2015", "25-12-2015", "26-12-2015"), f2=c("24-01-2016", "25-01-2016", "26-01-2016"),
                   n1=c("11-01-2016", "12-01-2016", "13-01-2016"), n2=c("08-02-2016","09-02-2016", "10-02-2016"),
                   wn1=c("31-01-2016", "01-02-2016","02-02-2016"), wn2=c("01-03-2016", "02-03-2016", "03-03-2016"),
                   wx1=c("16-01-2016","17-01-2016","18-01-2016"), wx2=c("16-02-2016","17-02-2016","18-02-2016"))

gudJoin$phaseRep[gudJoin$Date %in% date.df[["f1"]]]="Full1"
gudJoin$phaseRep[gudJoin$Date %in% date.df[["f2"]]]="Full2"
gudJoin$phaseRep[gudJoin$Date %in% date.df[["n1"]]]="New1"
gudJoin$phaseRep[gudJoin$Date %in% date.df[["n2"]]]="New2"
gudJoin$phaseRep[gudJoin$Date %in% date.df[["wn1"]]]="Wane1"
gudJoin$phaseRep[gudJoin$Date %in% date.df[["wn2"]]]="Wane2"
gudJoin$phaseRep[gudJoin$Date %in% date.df[["wx1"]]]="Wax1"
gudJoin$phaseRep[gudJoin$Date %in% date.df[["wx2"]]]="Wax2"

#Column for phase replicates in GUD visited

gudVisited$phaseRep[gudVisited$Date %in% date.df[["f1"]]]="Full1"
gudVisited$phaseRep[gudVisited$Date %in% date.df[["f2"]]]="Full2"
gudVisited$phaseRep[gudVisited$Date %in% date.df[["n1"]]]="New1"
gudVisited$phaseRep[gudVisited$Date %in% date.df[["n2"]]]="New2"
gudVisited$phaseRep[gudVisited$Date %in% date.df[["wn1"]]]="Wane1"
gudVisited$phaseRep[gudVisited$Date %in% date.df[["wn2"]]]="Wane2"
gudVisited$phaseRep[gudVisited$Date %in% date.df[["wx1"]]]="Wax1"
gudVisited$phaseRep[gudVisited$Date %in% date.df[["wx2"]]]="Wax2"

#drop full moon phase
gudVisited.drop=gudVisited[-which(gudVisited$MoonPhase=="Full"),]
gudVisited.drop$MoonPhase=factor(gudVisited.drop$MoonPhase)
gudVisited.drop$phaseRep=factor(gudVisited.drop$phaseRep)

#How many birds and ants and during which moon phase
length(which(gudJoin$AntBirdN=="B"|gudJoin$AntBirdN=="AB"))
length(which(gudJoin$AntBirdF=="B"|gudJoin$AntBirdF=="AB"))

table(gudJoin$phaseRep[which(gudJoin$AntBirdN=="B"|gudJoin$AntBirdN=="AB")])

#which phase rep had birds and gerbils feeding
table(gudJoin$phaseRep[which(gudJoin$AntBirdN=="B"|gudJoin$AntBirdN=="AB" & !is.na(gudJoin$PrintsInside))])
table(gudJoin$phaseRep[which(gudJoin$AntBirdF=="B"|gudJoin$AntBirdF=="AB" & !is.na(gudJoin$PrintsInside))])


#Create a new column in gudJoin for night
library(dplyr)
gudJoin$Date=factor(gudJoin$Date, c("24-12-2015", "25-12-2015", "26-12-2015",
                                      "11-01-2016", "12-01-2016", "13-01-2016",
                                      "16-01-2016","17-01-2016","18-01-2016",
                                      "24-01-2016", "25-01-2016", "26-01-2016",
                                      "31-01-2016", "01-02-2016","02-02-2016",
                                      "08-02-2016","09-02-2016", "10-02-2016",
                                      "16-02-2016","17-02-2016","18-02-2016",
                                      "01-03-2016", "02-03-2016", "03-03-2016"))
gudJoin=gudJoin %>% group_by(phaseRep) %>% mutate(Night=paste("N",as.numeric(factor(Date)), sep=""))


#Create a new column in gudJoin and gudVisited for season
gudJoin$season=ifelse(gudJoin$phaseRep %in% grep("1$",gudJoin$phaseRep, value = T), "Winter", "Spring")
gMeanVisited$season=ifelse(gMeanVisited$phaseRep %in% grep("1$",gMeanVisited$phaseRep, value = T), "Winter", "Spring")


#Convert gudJoin from wide to long
library(tidyr)
gudJoin=gather(gudJoin, NF, GUD, GUDN, GUDF)
gudJoin$NF= ifelse(gudJoin$NF=="GUDN","N","F")
gudJoin=arrange(gudJoin, Date, FStn)

#convert gudVisited from wide to long
gudVisitL=gather(gudVisited.drop, NF, GUD, GUDN, GUDF)
gudVisitL$NF= ifelse(gudVisitL$NF=="GUDN","N","F")
gudVisitL=arrange(gudVisitL, phaseRep, Date)

write.csv(gudVisitL,"gudVisited1806.csv")


#mean gudJoin per feeding station
gMean.Narm=with(gudJoinNA.RM,aggregate(GUD, by=list(phaseRep=phaseRep, fStn=FStn, NF=NF, habitat=Habitat, moonPhase=MoonPhase), mean))
colnames(gMean.Narm)[6]="meanGUD"
colnames(gMean.Narm)[colnames(gMean.Narm)=="NF"]="FeedingTrayPosition"

gMean=arrange(gMean,phaseRep, fStn)

#mean gudJoin per feeding station for gudVisited
gudVisitL.narm=gudVisitL[-which(is.na(gudVisitL$GUD)),]
gMeanVisited=with(gudVisitL.narm,aggregate(GUD, by=list(phaseRep=phaseRep, fStn=FStn, NF=NF, habitat=Habitat, moonPhase=MoonPhase), mean))
colnames(gMeanVisited)[6]="meanGUD"
colnames(gMeanVisited)[colnames(gMeanVisited)=="NF"]="FeedingTrayPosition"

gMeanVisited=arrange(gMeanVisited,phaseRep, fStn)



#Create another column in gMean for season
gMean.Narm$season=ifelse(gMean.Narm$phaseRep %in% grep("1$",gMean.Narm$phaseRep, value = T), "Winter", "Spring")
gMean.Narm$season[gMean.Narm$phaseRep %in% "Full2"]="Winter"
gMean.Narm$season=factor(gMean.Narm$season)


gMeanVisited$season=ifelse(gMeanVisited$phaseRep %in% grep("1$",gMeanVisited$phaseRep, value = T), "Winter", "Spring")
gMeanVisited$season=factor(gMeanVisited$season)

gMeanVisited$season[gMeanVisited$phaseRep=="Full2"]="Winter"

#write into file
gMean=arrange(gMean, phaseRep, fStn)
write.csv(gMean, "gudMean.csv")
write.csv(gudJoin, "GUDClean.csv")
write.csv(gMeanVisited, "GMeanVisit0107.csv")
write.csv(gMean.Narm,"gMean2106.csv") #This is same as gudMean.csv

cleanGuds=read.csv("GUDCleanWithNAs.csv")
cleanGuds$MoonPhase[cleanGuds$phaseRep %in% c("Full1", "Full2")]="Full"
cleanGuds$MoonPhase[cleanGuds$phaseRep %in% c("New1", "New2")]="New"
cleanGuds$MoonPhase[cleanGuds$phaseRep %in% c("Wane1", "Wane2")]="Wane"
cleanGuds$MoonPhase[cleanGuds$phaseRep %in% c("Wax1", "Wax2")]="Wax"
cleanGuds=arrange(cleanGuds, MoonPhase)
write.csv(cleanGuds, "GUDClean1706.csv")

cleanGuds$season=ifelse(cleanGuds$phaseRep %in% grep("1$",cleanGuds$phaseRep, value = T), "Winter", "Spring")
cleanGuds$season[cleanGuds$season=="Full2"]="Winter"


#write gudJoin1- clean file with means 
