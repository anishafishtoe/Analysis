#tree cutting experiment

setwd("F:/NCBS/Thesis/Data/")

expGuds=read.csv("Tree cutting exp.csv")

str(expGuds)
length(which(is.na(expGuds$GUDN)))
length(which(is.na(expGuds$GUDF)))

stripchart(expGuds$GUDN)
stripchart(expGuds$GUDF)

#mean guds
gudN.mean=aggregate(expGuds$GUDN[-which(is.na(expGuds$GUDN))], by=list(BefOnAft=expGuds$BefOnAft[-which(is.na(expGuds$GUDN))],Date=expGuds$Date[-which(is.na(expGuds$GUDN))]), FUN=mean)
colnames(gudN.mean)[3]="gudNMean"
gudF.mean=aggregate(expGuds$GUDF[-which(is.na(expGuds$GUDF))], by=list(BefOnAft=expGuds$BefOnAft[-which(is.na(expGuds$GUDF))],Date=expGuds$Date[-which(is.na(expGuds$GUDF))]), FUN=mean)
colnames(gudF.mean)[3]="gudFMean"
nrow(gudF.mean)

gudMeans=gudN.mean
gudMeans$gudFMean=gudF.mean$gudFMean

gudMeans$BefOnAft=sapply(gudMeans$BefOnAft, function(x){if (x=="B") "Before" else if (x=="O") "On" else if (x=="A")"After"})

class(gudMeans$BefOnAft)
gudMeans$BefOnAft= as.factor(gudMeans$BefOnAft)

#Plot Guds

boxplot(gudMeans$gudNMean~gudMeans$BefOnAft, ylab="GUD-Bush")
boxplot(gudMeans$gudFMean~gudMeans$BefOnAft, ylab="GUD-Open")

par(mfrow=c(1,2))

#Mean guds for cut vs un-cut trees.
nRows=length(unique(expGuds$Date))
gMeans_c_uc= data.frame(BefOnAft=vector(length=nRows), gudM_n_c=vector(length=nRows),gudM__n_uc=vector(length=nRows), gudM_f_c= vector(length=nRows), gudM_f_uc=vector(length=nRows)  )

