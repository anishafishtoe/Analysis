setwd("F:/NCBS/Thesis/Data/")
vegetation=read.csv("Vegetation density.csv")
str(vegetation)
library(tidyr)
library(plyr)
library(psych)
library(ggplot2)


#rename column name
colnames(vegetation)[which(colnames(vegetation)=="NumBushesLTm")]="NumBushesLT1m"

#gather different vegetation categories into one column 
vegetation.gather= gather(vegetation, vegType, abundance, NumProsopis, NumBushesLT1m, NumBushesGT1m)
vegetation.gather=arrange(vegetation.gather, Habitat, SubGrid)

#convert vegetation type into a factor
vegetation.gather$vegType=as.factor(vegetation.gather$vegType)
vegetation.gather$vegType=unlist(lapply(vegetation.gather$vegType, function(x){
  if (x=="NumProsopis") "Prosopis"
  else if (x=="NumBushesGT1m") "Bushes greater than 1m" else if (x=="NumBushesLT1m")"Bushes below 1m"}))

#boxplot of prosopis and combo of prosopis and bushes greater than 1m according to habitat
with(vegetation, boxplot(NumProsopis~Habitat))
with(vegetation, boxplot((NumProsopis+NumBushesGT1m)~Habitat))

#Are the abundances normal?
with(subset(vegetation.gather, Habitat=="Sparse"),hist(abundance[vegType=="Prosopis"], col="#A4A4A4"))
with(subset(vegetation.gather, Habitat=="Dense"),hist(abundance[vegType=="Prosopis"], col="#04B4AE", add=T ))
with(subset(vegetation.gather, Habitat=="Dense"),hist(abundance[vegType=="Bushes greater than 1m"], col="#04B4AE"))
with(subset(vegetation.gather, Habitat=="Sparse"),hist(abundance[vegType=="Bushes greater than 1m"], col="#A4A4A4", add=T))
with(subset(vegetation.gather, Habitat=="Dense"),hist(abundance[vegType=="Bushes below 1m"], col="#04B4AE"))
with(subset(vegetation.gather, Habitat=="Sparse"),hist(abundance[vegType=="Bushes below 1m"], col="#A4A4A4", add=T))

#Categorise prosopis as bushes greater than 1m
vegetation.gather$vegTypeNew=vegetation.gather$vegType
vegetation.gather$vegTypeNew[which(vegetation.gather$vegTypeNew=="Prosopis")]="Bushes greater than 1m"

#plot abundances of different vegetation according to habitat-- boxplots
ggplot(data=vegetation.gather, aes(x=Habitat, y=abundance, fill=vegType))+geom_boxplot()+
  theme_set(theme_gray(base_size = 20))+scale_fill_brewer()+ylab('Abundance')

#plot total abundance according to habitat
with(vegetation.gather, boxplot(abundance~Habitat))

with(subset(vegetation.gather,  Habitat=="Dense"),sd(abundance))

#convert to hectares
vegetation.gather$abundance=vegetation.gather$abundance/1000

#plot abundances of different vegetation according to habitat-- barplots
veg.summary=with(vegetation.gather,describeBy(abundance, list(Habitat, vegType), mat=T, digits=4))
names(veg.summary)[names(veg.summary)=="group1"]="Habitat"
names(veg.summary)[names(veg.summary)=="group2"]="VegetationType"
limits=with(veg.summary,aes(ymax=mean+(se), ymin=mean-(se)))
dodge=position_dodge(width=0.9)

ggplot(veg.summary, aes(x=Habitat, y=mean, fill=VegetationType))+ 
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab(bquote(Vegetation~density~(hectare^{-1})))+
  theme_classic(base_size=20)+
  scale_fill_grey()+
  theme(axis.line.x = element_line(color="black", size = 0.2),
        axis.line.y = element_line(color="black", size = 0.2))

  
  

#plot bar plot of total abundance
total.ab=with(vegetation.gather, describeBy(abundance, Habitat, mat=T, digits=4))
names(total.ab)[names(total.ab)=="group1"]="Habitat"
limits=with(total.ab,aes(ymax=mean+se, ymin=mean-se))
dodge=position_dodge(width=0.9)
ggplot(total.ab, aes(x=Habitat, y=mean))+ 
  geom_point(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  ylab('Total vegetation density')+
  theme_set(theme_gray(base_size = 20))+
  scale_fill_grey()

#is prosopis different between the 2 habitats?
with(vegetation.gather, kruskal.test(abundance~Habitat))

with(subset(vegetation.gather, vegType=="Prosopis"), kruskal.test(abundance~Habitat))
with(subset(vegetation.gather, vegType=="Bushes below 1m"), kruskal.test(abundance~Habitat))
with(subset(vegetation.gather, vegType=="Bushes greater than 1m"), kruskal.test(abundance~Habitat))
vegetation.gather=vegetation.gather[,-which(colnames(vegetation.gather) %in% c("Date","Grid","SubGrid","BushLTmID","BushGT1mID","PercCover.1m","Comments"))]

#post hoc
install.packages('FSA')
library(FSA)
install.packages('DescTools')
library(DescTools)
DunnTest(x = vegetation.gather$abundance,
         g = vegetation.gather$Habitat:vegetation.gather$vegType)
dunnTest(abundance~Habitat, data=vegetation.gather)
with(vegetation.gather, NemenyiTest(x=Habitat, g=abundance, dist="tukey"))
     
density=with(vegetation.gather, describeBy(abundance, list(Habitat, vegTypeNew), mat=T, digits=4))

