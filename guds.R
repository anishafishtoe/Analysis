setwd("F:/NCBS/Thesis/Data/")
guds=read.csv("GUDs.csv")

str(guds)

unique(guds$Habitat)
unique(guds$FStn)

length(which(is.na(gudsN)))
length(which(is.na(guds$GUDN)))

stripchart(guds$GUDN)
stripchart(guds$GUDF)

mPhase=unique(guds$MoonPhase)
habitat=unique(guds$Habitat)


#Plot guds

par(mfrow=c(1,2))

for(i in mPhase){
  for(j in habitat){
    boxplot(gudNMean$gudMean[gudNMean$habitat=="Sparse"]~gudNMean$moonPhase[gudNMean$habitat=="Sparse"], main="Sparse Prosopis", ylab="GUD N")
  }
}
boxplot(gudFMean$gudMean[gudFMean$habitat=="Sparse"]~gudFMean$moonPhase[gudFMean$habitat=="Sparse"], main="Sparse Prosopis", ylab="GUD F")

boxplot(gudNMean$gudMean[gudNMean$habitat=="Dense"]~gudNMean$moonPhase[gudNMean$habitat=="Dense"], main="Dense Prosopis", ylab="GUD N")
boxplot(gudFMean$gudMean[gudFMean$habitat=="Dense"]~gudFMean$moonPhase[gudFMean$habitat=="Dense"], main="Dense Prosopis", ylab="GUD F")


#30-04-2016

#gudMeans
gudNMean=aggregate(guds$GUDN[-which(is.na(guds$GUDN))], by=list(habitat=guds$Habitat[-which(is.na(guds$GUDN))], moonPhase=guds$MoonPhase[-which(is.na(guds$GUDN))], date=guds$Date[-which(is.na(guds$GUDN))]), FUN=mean)
colnames(gudNMean)[4]="gudMean"
gudNMean$nf=rep("N",nrow(gudNMean))
gudFMean=aggregate(guds$GUDF[-which(is.na(guds$GUDF))], by=list(habitat=guds$Habitat[-which(is.na(guds$GUDF))], moonPhase=guds$MoonPhase[-which(is.na(guds$GUDF))], date=guds$Date[-which(is.na(guds$GUDF))]), FUN=mean)
colnames(gudFMean)[4]="gudMean"
gudFMean$nf=rep("F",nrow(gudFMean))
str(gudFMean)

#dataframe with separate columns for near and far
gMean=data.frame(date=vector(), habitat=vector(), moonPhase=vector(), gudMean=vector(), nf=vector())
gMean=rbind(gudNMean,gudFMean)


for(i in mPhase){
  boxplot(gMean$gudMean[gMean$moonPhase==i]~gMean$habitat[gMean$moonPhase==i]+gMean$nf[gMean$moonPhase==i], ylab="GUD", main=i)
}
