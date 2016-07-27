#Rodents
rodents=read.csv("Trapping.csv")
head(rodents)

class(rodents$Species)
rodents$Species=revalue(rodents$Species, c(Nanus="Millardia gleadowi", Tatera="Tatera indica", 'Bush rat? '="Mus booduga", 'Sand rat?'="Millardia meltada"))


rod.summary=with(rodentsdf,describeBy(abundance, list(Habitat, vegType), mat=T, digits=4))
names(veg.summary)[names(veg.summary)=="group1"]="Habitat"
names(veg.summary)[names(veg.summary)=="group2"]="VegetationType"
limits=with(veg.summary,aes(ymax=mean+(se), ymin=mean-(se)))
dodge=position_dodge(width=0.9)

ggplot(rodents.df, aes(x=Habitat, y=Freq, fill=Species))+ 
  geom_bar(stat='identity', position=dodge)+
  ylab("Number of individuals caught")+
  theme_classic(base_size=20)+
  scale_fill_grey()+
  theme(axis.line.x = element_line(color="black", size = 0.2),
        axis.line.y = element_line(color="black", size = 0.2))

rodents.df$Species=factor(rodents.df$Species,c("Millardia gleadowi","Tatera indica","Mus booduga","Millardia meltada"))
