setwd("F:/NCBS/Thesis/Data/")
expActivity=read.csv("Tree cutting exp_Activity2904.csv")

str(expActivity)



#encode B,O,A as full forms

expActivity$BefOnAft=sapply(expActivity$BefOnAft, function(x){if (x=="B") "Before" else if (x=="O") "On" else if (x=="A")"After"})

#Create a new data frame with non-repeating feeding stations -- don't use
activityNew=vector("list")
activityNew=c(activityNew, lapply(unique(activity$Date), function(x){

  tempAct=vector("list")
  tempAct=activity[activity$Date==x,]
  tempAct[!duplicated(tempAct$FStn),]
  
}))

activityDF=do.call(rbind.data.frame,activityNew)


#30-04-2016

par(mfrow=c(1,1))

expActivity=expActivity[!duplicated(expActivity[,c(1,2,3,4)]),]
expActivity$BefOnAft=factor(expActivity$BefOnAft, c("B","O","A"))

#Create a dataframe containing mean activity for cut and uncut trees

expActDF_c=aggregate(expActivity$Ncrossing[substring(expActivity$FStn,1,1)=="T"], by=list(date=expActivity$Date[substring(expActivity$FStn,1,1)=="T"], befOnAft=expActivity$BefOnAft[substring(expActivity$FStn,1,1)=="T"], nf=expActivity$NF[substring(expActivity$FStn,1,1)=="T"]), FUN=mean)
expActDF_c$cutUncut= rep("Cut", length(expActDF_c))
expActDF_uc=aggregate(expActivity$Ncrossing[substring(expActivity$FStn,1,1)=="N"], by=list(date=expActivity$Date[substring(expActivity$FStn,1,1)=="N"], befOnAft=expActivity$BefOnAft[substring(expActivity$FStn,1,1)=="N"], nf=expActivity$NF[substring(expActivity$FStn,1,1)=="N"]), FUN=mean)
expActDF_uc$cutUncut= rep("Uncut", length(expActDF_uc))

expActDF=rbind(expActDF_c, expActDF_uc)
colnames(expActDF)[4]="meanActivity"

#de-code B O A as full forms
expActDF$befOnAft=sapply(expActDF$befOnAft, function(x){if (x=="B") "Before" else if (x=="O") "On" else if (x=="A")"After"})
expActDF$befOnAft=factor(expActDF$befOnAft, c("Before","On","After"))

par(mfrow=c(1,1))
for (i in c("Before", "On", "After")){
  boxplot(expActDF$meanActivity[expActDF$befOnAft==i]~expActDF$nf[expActDF$befOnAft==i]+expActDF$cutUncut[expActDF$befOnAft==i], ylab="Number of crossings", main=i)
}