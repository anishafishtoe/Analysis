#tree cutting experiment

setwd("F:/NCBS/Thesis/Data/")

expGuds=read.csv("Tree cutting exp2904.csv")

str(expGuds)
length(which(is.na(expGuds$GUDN)))
length(which(is.na(expGuds$GUDF)))

length(which(expGuds$GUDN>3))
length(which(expGuds$GUDF>3))

stripchart(expGuds$GUDN)
stripchart(expGuds$GUDF)

unique(expGuds$FStn)
expGuds$FStn[which(expGuds$FStn=="TI")]="T1"

treesUncut= unique(grep("nt+",expGuds$FStn , ignore.case=T, value=T, perl=T))
treesCut=setdiff(unique(expGuds$FStn), treesCut)

#Mean guds for cut vs un-cut trees.


#mean guds

gudN.mean_c=aggregate(expGuds$GUDN[substring(expGuds$FStn,1,1)=="T"& !(is.na(expGuds$GUDN))], by=list(BefOnAft=expGuds$BefOnAft[substring(expGuds$FStn,1,1)=="T"& !(is.na(expGuds$GUDN))],Date=expGuds$Date[substring(expGuds$FStn,1,1)=="T"& !(is.na(expGuds$GUDN))]), FUN=mean)
colnames(gudN.mean_c)[3]="gudMean"
gudN.mean_c$nf=rep("N", nrow(gudN.mean_c))
gudN.mean_c$cutUncut=rep("Cut", nrow(gudN.mean_c))

gudN.mean_uc=aggregate(expGuds$GUDN[substring(expGuds$FStn,1,1)=="N"& !(is.na(expGuds$GUDN))], by=list(BefOnAft=expGuds$BefOnAft[substring(expGuds$FStn,1,1)=="N"& !(is.na(expGuds$GUDN))],Date=expGuds$Date[substring(expGuds$FStn,1,1)=="N"& !(is.na(expGuds$GUDN))]), FUN=mean)
colnames(gudN.mean_uc)[3]="gudMean"
gudN.mean_uc$nf=rep("N", nrow(gudN.mean_uc))
gudN.mean_uc$cutUncut=rep("Uncut", nrow(gudN.mean_uc))


gudF.mean_c=aggregate(expGuds$GUDF[substring(expGuds$FStn,1,1)=="T"& !(is.na(expGuds$GUDF))], by=list(BefOnAft=expGuds$BefOnAft[substring(expGuds$FStn,1,1)=="T"& !(is.na(expGuds$GUDF))],Date=expGuds$Date[substring(expGuds$FStn,1,1)=="T"& !(is.na(expGuds$GUDF))]), FUN=mean)
colnames(gudF.mean_c)[3]="gudMean"
gudF.mean_c$nf=rep("F", nrow(gudF.mean_c))
gudF.mean_c$cutUncut=rep("Cut", nrow(gudN.mean_c))

gudF.mean_uc=aggregate(expGuds$GUDF[substring(expGuds$FStn,1,1)=="N"& !(is.na(expGuds$GUDF))], by=list(BefOnAft=expGuds$BefOnAft[substring(expGuds$FStn,1,1)=="N"& !(is.na(expGuds$GUDF))],Date=expGuds$Date[substring(expGuds$FStn,1,1)=="N"& !(is.na(expGuds$GUDF))]), FUN=mean)
colnames(gudF.mean_uc)[3]="gudMean"
gudF.mean_uc$nf=rep("F", nrow(gudF.mean_uc))
gudF.mean_uc$cutUncut=rep("Uncut", nrow(gudN.mean_uc))

colnames(gudF.mean_c)
gudMean=rbind(gudN.mean_c, gudN.mean_uc, gudF.mean_c, gudF.mean_uc)

gudMean$BefOnAft=sapply(gudMean$BefOnAft, function(x){if (x=="B") "Before" else if (x=="O") "On" else if (x=="A")"After"})

class(gudMeans$BefOnAft)


#Plot Guds

boxplot(gudMeans$gudNMean~gudMeans$BefOnAft, ylab="GUD-Bush")
boxplot(gudMeans$gudFMean~gudMeans$BefOnAft, ylab="GUD-Open")

par(mfrow=c(1,2))




#plots

gudMean$BefOnAft=factor(gMeans$BefOnAft, c("Before","On","After"))


par(mfrow=c(2,2))
boxplot(gMeans$gudM_n_c~gMeans$BefOnAft, ylab="GUD", main="Bush- Cut")
boxplot(gMeans$gudM__n_uc~gMeans$BefOnAft, ylab="GUD", main="Bush- Uncut")
boxplot(gMeans$gudM_f_c~gMeans$BefOnAft, ylab="GUD", main="Open- Cut")
boxplot(gMeans$gudM_f_uc~gMeans$BefOnAft, ylab="GUD", main="Open- Uncut")



#30-04-2015

#Plot using dataframe containing separate entries for cut, uncut trees and near and fear stations
par(mfrow=c(2,2))
for (i in c("Before","On","After")){
  boxplot(gudMean$gudMean~gudMean$nf+gudMean$cutUncut, ylab="GUD", main=i)
}