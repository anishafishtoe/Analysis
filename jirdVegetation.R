
veg= read.csv("F:/NCBS/Thesis/Data/veg_clean_2002.csv", as.is=T)
head(veg)
colnames(veg)


#Pool species according to distance class 
colonyVeg=subset(veg,veg$Colony=="K2")
v02=c(colonyVeg$N02,colonyVeg$S02,colonyVeg$E02,colonyVeg$W02)
v46=c(colonyVeg$N46,colonyVeg$S46,colonyVeg$E46,colonyVeg$W46)
v810=c(colonyVeg$N810,colonyVeg$S810,colonyVeg$E810,colonyVeg$W810)
v1214=c(colonyVeg$N1214,colonyVeg$S1214,colonyVeg$E1214,colonyVeg$W1214)

#How does species richness change with distance from colony

V=list(v02,v46,v810,v1214)
plot(NULL,xlim=c(0,20),ylim=c(0,20),xlab="Distance from colony",ylab="Number of species")
j=2
for (i in V){
  if ("B" %in% i) i=i[-which(i=="B")]
  points(j,length(unique(i)))
  j=j+4
}

cardDir=list(c("N02","S02","E02","W02"),c("N46","S46","E46","W46"), c("N810","S810","E810","W810"), c("N1214","S1214","E1214","W1214"))
vecSpec=vector("list",4)
numSpec=vector("list",4)

Uniqcolony= unique(veg$Colony)
colony=Uniqcolony[-grep(pattern="*P", x=Uniqcolony)]
#j=4 : 4 cardinal directions
#Each nested list in vecSpec contains species in a given distance bucket for all colonies.
for(j in 1:4){
  for (i in colony){
    
    vecSpec[[j]]=c(veg[[cardDir[[j]][1]]][veg$Colony==i],veg[[cardDir[[j]][2]]][veg$Colony==i],veg[[cardDir[[j]][3]]][veg$Colony==i],veg[[cardDir[[j]][4]]][veg$Colony==i])
    if("B" %in% vecSpec[[j]]) vecSpec[[j]]=vecSpec[[j]][-which(vecSpec[[j]]=="B")]
    vecSpec[[j]]=vecSpec[[j]][-which(vecSpec[[j]]=="")]
    numSpec[[j]]=c(numSpec[[j]],length(unique(vecSpec[[j]])))
    
  }
}

par(mfrow=c(1,4))
boxplot(numSpec[[1]], ylim=c(0,12), ylab="Species richness", xlab="0-2")
boxplot(numSpec[[2]], ylim=c(0,12), ylab="Species richness", xlab="4-6")
boxplot(numSpec[[3]], ylim=c(0,12), ylab="Species richness", xlab="8-10")
boxplot(numSpec[[4]], ylim=c(0,12), ylab="Species richness", xlab="12-14")

uniqColP= unique(veg$Colony)
colP=uniqColP[grep(pattern="*P", x=uniqColP)]


#How does the number of new species change with distance from colony 
#UPDATE with other directions

North=list(vegK2$N46[veg$Colony=="K2"], vegK2$N810[veg$Colony=="K2"],vegK2$N1214[veg$Colony=="K2"])
diffVec=vegK2$N02[veg$Colony=="K2"]
plot(2, xlab="Distance from colony", ylab="Number of new species", xlim=c(0,20),ylim=c(0,20))
i=6
lapply(North, function(x){
  print(unique(diffVec))
  if ("B" %in% x) x=x[-which(x=="B")]
  print(setdiff(unique(x),unique(diffVec)))
  points(i,length(setdiff(unique(x),unique(diffVec))))
  diffVec<<-x
  i<<-i+4
})


setdiff(unique(vegK2$N46[veg$Colony=="K2"]),unique(vegK2$N02[veg$Colony=="K2"]))
setdiff(unique(vegK2$N810[veg$Colony=="K2"]),unique(vegK2$N46[veg$Colony=="K2"]))
setdiff(unique(vegK2$N1214[veg$Colony=="K2"]),unique(vegK2$N810[veg$Colony=="K2"]))


#How does amount of bare ground change with distance from colony

#bar graph of species at each distance
