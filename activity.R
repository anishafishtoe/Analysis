setwd("F:/NCBS/Thesis/Data/")
activity= read.csv("Footprints.csv", as.is=T)
str(activity)
length(activity$Ncrossings)

unique(activity$FStn)

mPhase=unique(activity$MoonPhase)
habitat=unique(activity$Habitat)
position=unique(activity$NF)


#

#Does activity differ for different moon phases?
boxplot(activity$Ncrossing[activity$Habitat=="Sparse"]~activity$MoonPhase[activity$Habitat=="Sparse"], main="Activity patterns in Sparse habitat", ylab="Number of crossings")
boxplot(activity$Ncrossing[activity$Habitat=="Dense"]~activity$MoonPhase[activity$Habitat=="Dense"], main="Activity patterns in Dense habitat", ylab="Number of crossings")
par(mfrow=c(1,1))

#Does activity differ in the different habitats? 

boxplot(activity$Ncrossing[activity$Habitat=="Sparse"], activity$Ncrossing[activity$Habitat=="Dense"], xlab=c("Sparse", "Dense"), ylab="Number of crossings")

t.test(activity$Ncrossing[activity$Habitat=="Sparse"], activity$Ncrossing[activity$Habitat=="Dense"])



#How does activity change between near and far trays in different habitats and moon phases?
NFactivity=table(SN=activity$Ncrossing[activity$Habitat=="Sparse"&activity$NF=="N"], SN=activity$Ncrossing[activity$Habitat=="Sparse"&activity$NF=="F"], activity$Ncrossing[activity$Habitat=="Dense" & activity$NF=="N"], SF=activity$Ncrossing[activity$Habitat=="Sparse"&activity$NF=="F"])
par(mfrow=c(1,2))

boxplot(activity$Ncrossing[activity$Habitat=="Sparse"&activity$MoonPhase=="New"]~activity$NF[activity$Habitat=="Sparse"&activity$MoonPhase=="New"], main="New moon activity rates in Sparse habitat", ylab="Number of crossings")
boxplot(activity$Ncrossing[activity$Habitat=="Dense"&activity$MoonPhase=="New"]~activity$NF[activity$Habitat=="Dense"&activity$MoonPhase=="New"], main="New moon activity rates in Dense habitat", ylab="Number of crossings")

boxplot(activity$Ncrossing[activity$Habitat=="Sparse"&activity$MoonPhase=="Wax"]~activity$NF[activity$Habitat=="Sparse"&activity$MoonPhase=="Wax"], main="Waxing moon activity rates in Sparse", ylab="Number of crossings")
boxplot(activity$Ncrossing[activity$Habitat=="Dense"&activity$MoonPhase=="Wax"]~activity$NF[activity$Habitat=="Dense"&activity$MoonPhase=="Wax"], main="Waxing moon activity rates in Dense", ylab="Number of crossings")

boxplot(activity$Ncrossing[activity$Habitat=="Sparse"&activity$MoonPhase=="Full"]~activity$NF[activity$Habitat=="Sparse"&activity$MoonPhase=="Full"], main="Full moon activity rates in Sparse", ylab="Number of crossings")
boxplot(activity$Ncrossing[activity$Habitat=="Dense"&activity$MoonPhase=="Full"]~activity$NF[activity$Habitat=="Dense"&activity$MoonPhase=="Full"], main="Full moon activity rates in Dense", ylab="Number of crossings")

boxplot(activity$Ncrossing[activity$Habitat=="Sparse"&activity$MoonPhase=="Wane"]~activity$NF[activity$Habitat=="Sparse"&activity$MoonPhase=="Wane"], main="Waning moon activity rates in Sparse", ylab="Number of crossings")
boxplot(activity$Ncrossing[activity$Habitat=="Dense"&activity$MoonPhase=="Wane"]~activity$NF[activity$Habitat=="Dense"&activity$MoonPhase=="Wane"], main="Full moon activity rates in Dense", ylab="Number of crossings")


boxplot(activity$Ncrossing[activity$Habitat=="Dense"]~activity$NF[activity$Habitat=="Dense"])

boxplot(activity$Ncrossing~interaction( activity$NF, activity$Habitat ))
library(lattice)
bwplot(Ncrossing~MoonPhase|Habitat+NF, data=activity)

boxplot(Ncrossing~Habitat*MoonPhase*NF, data=activity)


#23-04-2016
#Averages of crossings

x="Full"
y="25-12-2015"

#Activity contains repeating NCrossings for each feeding station
sapply(unique(activity$MoonPhase), function(x){
  sapply(unique(activity$Date[activity$MoonPhase=="Full"]), function(y){
    print(paste("moon p",y))
    sapply(unique(activity$FStn[activity$MoonPhase==x & activity$Date==y]), function(z){
      print(paste("fstn",z))
      mean(activity$Ncrossing[activity$MoonPhase==x & activity$Date==y & activity$FStn==z & activity$NF=="N"])
    })
  })
})


#Row 198 has NA for ncrossing



#Create a new data frame with non-repeating feeding stations
tempAct=vector("list")
activityNew=vector("list")
tempAct=activity[activity$Date=="26-12-2015",]
tempAct[!duplicated(tempAct$FStn),]

activityNew=c(activityNew, lapply(unique(activity$Date), function(x){
    tempAct=vector("list")
    tempAct=activity[activity$Date==x,]
    tempAct[!duplicated(tempAct$FStn),]
}))

#But this does not take N, F into account
activityNew=c(activityNew, lapply(unique(activity$Date), function(x){
  tempAct=vector("list")
  tempAct=activity[activity$Date==x,]
  tempAct[!duplicated(tempAct$FStn),]
}))


#extract crossings for each moon phase

activityDF=do.call(rbind.data.frame,activityNew)

#Plot crossings for each moon phase

par(mfrow=c(2,4))
plot(1,1)

for (i in mPhase){boxplot(activity$Ncrossing[activity$MoonPhase==i], xlab=i, ylab="Number of crossings", ylim=c(0,20))}

#Plot crossings for each moon phase according to habitat

for (i in mPhase){boxplot(activity$Ncrossing[activity$MoonPhase==i & activity$Habitat=="Dense"], xlab=i, ylab="Number of crossings", ylim=c(0,20))}

#Plot crossings for each moon phase according to habitat and feeding tray position

lapply(habitat, function(x){
  lapply(mPhase, function(y){
    lapply(position, function(z){
      boxplot(activityDF$Ncrossing[activityDF$Habitat==x & activityDF$MoonPhase==y & activityDF$NF==z], xlab=paste(x[1],y,z))
    })    
  })
})

#Ditto as above, with averages taken for each day. 

activityAvg=matrix(rep(list(),8),nrow=4, ncol=2, byrow = T)
dimnames(activityAvg)= list(c("Full", "Wane", "New", "Wax"), c("Sparse", "Dense"))

for(i in habitat){
  for(j in mPhase){
    for (k in unique(activityDF$Date[activityDF$MoonPhase==j])){
      activityAvg[[j,i]]=c(activityAvg[[j,i]], mean(activityDF$Ncrossing[activityDF$Habitat==i & activityDF$MoonPhase==j & activityDF$Date==k]))
    }
  }
}

par(mfrow=c(2,4))

for (i in mPhase){
  for(j in habitat){
    boxplot(activityAvg[i,j], xlab=paste(j," ",i), ylab="Number of crossings")
  }
}

#Plot activity for each feeding station
actAvgNF=aggregate(activityDF$Ncrossing,by=list(habitat=activityDF$Habitat, moonPhase=activityDF$MoonPhase, NF=activityDF$NF, date=activityDF$Date ), FUN=mean)
colnames(actAvgNF)[5]="meanActivity"
actAvgNF$NF=as.factor(actAvgNF$NF)

for(i in mPhase){
  for (j in habitat){
    boxplot(actAvgNF$meanActivity[actAvgNF$moonPhase==i & actAvgNF$habitat==j]~actAvgNF$NF[actAvgNF$moonPhase==i & actAvgNF$habitat==j] , ylab="Number of crossings", xlab=paste(j," ",i))
  }
}

#30-04-2016

#Remove duplicates
activity=activity[!duplicated(activity[,c(1,4,5)]),]

meanAct=aggregate(activity$Ncrossing, by=list(date=activity$Date, moonPhase=activity$MoonPhase, habitat=activity$Habitat, nf=activity$NF), FUN=mean)
colnames(meanAct)[5]="mAct"

par(mfrow=c(2,2))

for(i in mPhase){
  for (j in habitat){
    boxplot(meanAct$mAct[meanAct$moonPhase==i & meanAct$habitat==j]~meanAct$nf[meanAct$moonPhase==i & meanAct$habitat==j] , ylab="Number of crossings", xlab=paste(j," ",i))
  }
}

for(i in mPhase){
  boxplot(meanAct$mAct[meanAct$moonPhase==i]~meanAct$habitat[meanAct$moonPhase==i]+meanAct$nf[meanAct$moonPhase==i], ylab="Number of crossings", main=i)
}

#boxplot using means for feeding stations instead of day
for(i in mPhase){
  boxplot(mAct.df$meanCrossings[mAct.df$moonPhase==i]~mAct.df$habitat[mAct.df$moonPhase==i]+mAct.df$NF[mAct.df$moonPhase==i], ylab="Number of crossings", main=i)
}

