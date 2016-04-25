setwd("F:/NCBS/Thesis/Data/")
activity= read.csv("Footprints.csv", as.is=T)
str(activity)
length(activity$Ncrossings)

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


  #delete duplicate entries for Ncrossings
  sapply(unique(activity$Date), function(x){
    crossings=c(crossings, )
  })

x="Full"
y="25-12-2015"


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