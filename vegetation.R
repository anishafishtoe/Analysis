#Vegetation sampling
#181215

setwd("F:/NCBS/Thesis/Data/")
veg=read.csv("veg_clean_2002.csv")
head(veg)
str(veg)
colnames(veg)


card_veg=data.frame(Colony=vector(length=40), VegN1=vector(length=40),VegN2=vector(length=40), VegN3=vector(length=40), VegN4=vector(length=40), VegS1= vector(length=40),VegS2= vector(length=40), VegS3= vector(length=40), VegS4= vector(length=40),  VegE1=vector(length=40),VegE2=vector(length=40), VegE3=vector(length=40), VegE4=vector(length=40),  VegW1=vector(length=40), VegW2=vector(length=40), VegW3=vector(length=40), VegW4=vector(length=40))

dir=c("N02", "N46", "N810", "N1214", "S02", "S46","S810", "S1214","E02", "E46","E810", "E1214", "W02", "W46","W810", "W1214"  )
j=1
for (i in levels(veg$Colony)){
  card_veg$Colony[j]=i
  card_veg[j]$VegN1=veg$N02[veg$Colony==i]
  print(i)
  print(j)
  j=j+1
}

lev=c("N02", "N46", "N810", "N1214", "S02", "S46","S810", "S1214","E02", "E46","E810", "E1214", "W02", "W46","W810", "W1214"  )
for (i in levels(veg$Colony)){
  for (j in lev){
    for(k in veg$j[veg$Colony==i])
    if(regexpr(",", k)){
      temp=split(k,",")
    }
    else
  }
}

veg1=data.frame(Colony=vector(length=nrow(veg)), Vegetation=vector(length(nrow(veg))))
for(i in levels(veg$Colony)){
  veg1$Vegetation[Colony=i]=paste(veg$lev)
}

#29-01-2016

#Create a nested list of colonies

vegL= list(Colony=length(unique(veg$Colony)), VegN1=vector(),VegN2=vector(), VegN3=vector(), VegN4=vector(), VegS1= vector(),VegS2= vector(), VegS3= vector(), VegS4= vector(),  VegE1=vector(),VegE2=vector(), VegE3=vector(), VegE4=vector(),  VegW1=vector(), VegW2=vector(), VegW3=vector(), VegW4=vector())
vegL= list(Colony=length(unique(veg$Colony)), VegN1=character(),VegN2=character(), VegN3=character(), VegN4=character(), VegS1= character(),VegS2= character(), VegS3= character(), VegS4= character(),  VegE1=character(),VegE2=character(), VegE3=character(), VegE4=character(),  VegW1=character(), VegW2=character(), VegW3=character(), VegW4=character())
for (i in levels(veg$Colony)){
  vegL$Colony=i
  j=2
  for(k in which(colnames(veg)=="N02"): which(colnames(veg)=="W1214")){
    vegL[j]=as.vector(subset(veg,veg$Colony==i)[k])
   
    while(length(which(grepl(",", vegL[j])))>0){
      for(m in which(grepl(",", vegL[j]))){
        print(m)
        tempStr=vegL[j][1:(m-1)]
        for(l in unlist(strsplit(as.vector(vegL[j][m]),",")) ){
          tempStr=c(tempStr, trimws(l))
        }
        tempStr=c(tempStr,vegL[j][(m+1):length(vegL[j])])
        vegL[k]=tempStr
      }
    }
    j=j+1
  }
 }

vegL$VegN1=as.vector(vegL$VegN1)

#05-02-2016
file.create("veg0602.csv")
tempDF=list()
for (i in levels(veg$Colony)){
  vegVec=vector()
  for(k in which(colnames(veg)=="N02"): which(colnames(veg)=="W1214")){
    vegVec=as.vector(subset(veg,veg$Colony==i)[,k], mode="character")
    while(length(which(grepl(",", vegVec)))>0){
      for(m in which(grepl(",", vegVec))){
        #print(m)
        tempStr=vegVec[1:(m-1)]
        for(l in unlist(strsplit(as.vector(vegVec[m]),",")) ){
          tempStr=c(tempStr, trimws(l))
        }
        tempStr=c(tempStr,vegVec[(m+1):length(vegVec)])
        vegVec=tempStr
      }
    }
    tempDF=rbind(tempDF, vegVec)
    
  }
  tempDF$Colony=rep(i, length(nrow(tempDF)))
  write.csv(tempDF, file="veg0602.csv", col.names=FALSE)
}