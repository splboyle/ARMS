# ARMS

setwd("~/GitHub/ARMS")
workingARMS <- read.csv("data/workingARMS.csv", header = T, na.strings = c("", "NA"))
crabs <- read.csv("data/Brachyuran_ARMS.csv", header = T, na.strings = c("", "NA"))
genus.matrix <- read.csv("data/genus_matrix.csv", header = T) #Brachy genera
family.matrix <- read.csv("data/family_matrix.csv", header = T) 
taxon.matrix <- read.csv("data/lowtaxon_matrix.csv", header = T) 

library(reshape)
library(reshape2)
library(vegan)
library(ggplot2)
library(ggord)
library(plyr)

######################## GENERA ######################## 

# Call island out for legend later
uRel <- unique(genus.matrix$Region)
uIsl <- unique(genus.matrix$Island)


#rm2 <- ddply(genus.matrix, "ARMS_Unit", numcolwise(sum))
#write.csv(rm2, "data/matrix.csv")


# Specaccum for the Region 

CT <- specaccum(subset(genus.matrix, Region_ADJ == "Coral Triangle")[,5:ncol(genus.matrix)], method = "random")
NWHI <- specaccum(subset(genus.matrix, Region_ADJ == "NW Hawaiian Islands")[,5:ncol(genus.matrix)], method = "random")
MainHI <- specaccum(subset(genus.matrix, Region_ADJ == "Main Hawaiian Islands")[,5:ncol(genus.matrix)], method = "random")
Marianas <- specaccum(subset(genus.matrix, Region_ADJ == "Mariana Islands")[,5:ncol(genus.matrix)], method = "random")
Samoa <- specaccum(subset(genus.matrix, Region_ADJ == "American Samoa")[,5:ncol(genus.matrix)], method = "random")
Line <- specaccum(subset(genus.matrix, Region_ADJ == "Line Islands")[,5:ncol(genus.matrix)], method = "random")
Phoenix <- specaccum(subset(genus.matrix, Region_ADJ == "Phoenix Islands")[,5:ncol(genus.matrix)], method = "random")

# Create data frames for all the results, including island 
sp.ct <- data.frame(Sites=CT$sites, Richness=CT$richness, SD=CT$sd, Region = "Coral Triangle")
sp.nwhi <- data.frame(Sites=NWHI$sites, Richness=NWHI$richness, SD=NWHI$sd, Region = "Northwest Hawaiian Islands")
sp.MainHI <- data.frame(Sites=MainHI$sites, Richness=MainHI$richness, SD=MainHI$sd, Region = "Main Hawaiian Islands & Johnston")
sp.Mar <- data.frame(Sites=Marianas$sites, Richness=Marianas$richness, SD=Marianas$sd, Region = "Marianas Islands & Wake")
sp.line <- data.frame(Sites=Line$sites, Richness=Line$richness, SD=Line$sd, Region = "Line Islands")
sp.samoa <- data.frame(Sites=Samoa$sites, Richness=Samoa$richness, SD=Samoa$sd, Region = "American Samoa")
sp.ph <- data.frame(Sites=Phoenix$sites, Richness=Phoenix$richness, SD=Phoenix$sd, Region = "Phoenix Islands")

# Merge all data frames 
sp.all <- rbind(sp.ct, sp.nwhi, sp.MainHI, sp.Mar, sp.line, sp.samoa, sp.ph)

# Specaccum for RAMP Region

for (i in genus.matrix$Region_ADJ){
  
  s <- specaccum(subset(genus.matrix, Region_ADJ == i)[,5:ncol(genus.matrix)], method = "random")
  
  d <- data.frame(Sites=s$sites, Richness=s$richness, SD=s$sd, Region_ADJ = i)
  
  newname <- paste("richness", i, sep="_")
  
  assign(newname, d)
  
}


region2.all <- rbind(`richness_NW Hawaiian Islands`, `richness_Pacific Remote Islands`, `richness_Marianas Islands`, `richness_Main Hawaiian Islands`, `richness_Coral Triangle`, `richness_American Samoa`)

# Specaccum for the Islands
for (i in genus.matrix$Island){
  
  s <- specaccum(subset(genus.matrix, Island == i)[,5:ncol(genus.matrix)], method = "random")
  
  d <- data.frame(Sites=s$sites, Richness=s$richness, SD=s$sd, Island = i)
  
  newname <- paste("richness", i, sep="_")
  
  assign(newname, d)
  
}

genus.all <- rbind(richness_Baker, `richness_French Frigate Shoals`, richness_Guam, richness_Hawaii, richness_Howland, richness_Jarvis, richness_Johnston, richness_Kauai, richness_Kingman, richness_Kure, richness_Lisianski, richness_Maug, richness_Maui, richness_Oahu, `richness_Ofu/Olosega`, richness_Pagan, richness_Palmyra, `richness_Papua New Guinea`, `richness_Pearl and Hermes`, richness_Rose, richness_Saipan, richness_Swains, `richness_Timor Leste`, richness_Tutuila, richness_Wake)


# Plot all islands showing standard deviation - colors are hard to read here - need to change them at some point
ggplot(data = sp.all, aes(x = Sites, y = Richness, fill = Region)) +
  geom_line(aes(fill = Region)) + 
  geom_ribbon(data = sp.all, aes(x = Sites, ymin=(Richness-SD),ymax=(Richness+SD)),alpha=0.25) + 
  theme_bw() + 
  ggtitle("Randomized Genera Accumulation Curves \n By Region") + 
  scale_fill_discrete(name="Island") +
  theme(legend.position=c(.8,0.25)) +
  ylab("Number of Unique Genera") + 
  xlab("Number of ARMS Units") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.title=element_blank()) +
  scale_x_continuous(limits = c(0,40))
ggsave("graphs_tables/BrachyuranGenera_SpecAccumSD_ArchRegion.png")

ggplot(data = genus.all, aes(x = Sites, y = Richness, fill = Island)) +
  geom_line(aes(fill = Island)) + 
  geom_ribbon(data = genus.all, aes(x = Sites, ymin=(Richness-SD),ymax=(Richness+SD)),alpha=0.25) + 
  theme_bw() + 
  ggtitle("Randomized Genera Accumulation Curves \n By Island") + 
  scale_fill_discrete(name="Island") +
  theme(legend.position=c(.75,0.35)) +
  ylab("Number of Unique Genera") + 
  xlab("Number of ARMS Units") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.title=element_blank()) +
  scale_x_continuous(limits = c(0,30))
ggsave("graphs_tables/BrachyuranGenera_SpecAccumSD.png")

ggplot(data = region2.all, aes(x = Sites, y = Richness, fill = Region)) +
  geom_line(aes(fill = Region)) + 
  geom_ribbon(data = region2.all, aes(x = Sites, ymin=(Richness-SD),ymax=(Richness+SD)),alpha=0.25) + 
  theme_bw() + 
  ggtitle("Randomized Genera Accumulation Curves \n By RAMP Region") + 
  scale_fill_discrete(name="Island") +
  theme(legend.position=c(.75,0.35)) +
  ylab("Number of Unique Genera") + 
  xlab("Number of ARMS Units") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.title=element_blank()) +
  scale_x_continuous(limits = c(0,60))
ggsave("graphs_tables/BrachyuranGenera_SpecAccumSD.png")


jpeg(file = "graphs_tables/BrachyuranGenera_SpecAccumCI.jpg")
plot(CT, col = "purple", xlim=c(0,40), main="Randomized Genera Accumulation Curves\nby Region", ylim=c(0,40), ylab="Number of ARMS Units",xlab="Number of Unique Genera", lwd=0.1,ci.type='polygon', ci.lty=2,ci=1.96/max(sqrt(CT$sites)))
plot(NWHI,col="orange", add=TRUE,lwd=0.1,ci.type='polygon',ci.lty=2,ci=1.96/max(sqrt(NWHI$sites)))
plot(MainHI,col="green",add=TRUE,lwd=0.1,ci.type='polygon',ci.lty=2,ci=1.96/max(sqrt(MainHI$sites)))
plot(Marianas,col="blue",add=TRUE,lwd=0.1,ci.type='polygon',ci.lty=2,ci=1.96/max(sqrt(Marianas$sites)))
plot(Line,col="yellow",add=TRUE,lwd=0.1,ci.type='polygon',ci.lty=2,ci=1.96/max(sqrt(Line$sites)))
plot(Phoenix,col="red",add=TRUE,lwd=0.1,ci.type='polygon',ci.lty=2,ci=1.96/max(sqrt(Phoenix$sites)))
plot(Samoa,col="cyan",add=TRUE,lwd=0.1,ci.type='polygon',ci.lty=2,ci=1.96/max(sqrt(Samoa$sites)))
legsort=c(1,2,3,4,5,6,7)
legend(27,10,legend=(uRel[legsort]),
       fill = (c("red","orange","blue","green","yellow","purple", "cyan")[legsort]),
       cex = 0.75) 
dev.off()




ggplot(data = genus.all, aes(x = Island, y = Richness))+
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  theme(legend.position = "none")



ggplot(data = family.all, aes(x = Island, y = Richness))+
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  theme(legend.position = "none")








##########################
## FIGURE THIS SHIT OUT
xx <- ddply(genus.matrix, "Island", numcolwise(sum))
rownames(xx) <- xx[,1]
xx$Island <- NULL
Brachy.MDS <- metaMDS(xx[,2:ncol(xx)])

######################## FAMILY ######################## 


# Specaccum for the Islands
for (i in family.matrix$Island){
  
  s <- specaccum(subset(family.matrix, Island == i)[,5:ncol(family.matrix)], method = "random")
  
  d <- data.frame(Sites=s$sites, Richness=s$richness, SD=s$sd, Island = i)
  
  newname <- paste("f.richness", i, sep="_")
  
  assign(newname, d)
  
}

family.all <- rbind(f.richness_Baker, `f.richness_French Frigate Shoals`, f.richness_Guam, f.richness_Hawaii, f.richness_Howland, f.richness_Jarvis, f.richness_Johnston, f.richness_Kauai, f.richness_Kingman, f.richness_Kure, f.richness_Lisianski, f.richness_Maug, f.richness_Maui, f.richness_Oahu, `f.richness_Ofu/Olosega`, f.richness_Pagan, f.richness_Palmyra, `f.richness_Papua New Guinea`, `f.richness_Pearl and Hermes`, f.richness_Rose, f.richness_Saipan, f.richness_Swains, `f.richness_Timor Leste`, f.richness_Tutuila, f.richness_Wake)


for (i in taxon.matrix$Island){
  
  s <- specaccum(subset(taxon.matrix, Island == i)[,5:ncol(taxon.matrix)], method = "random")
  
  d <- data.frame(Sites=s$sites, Richness=s$richness, SD=s$sd, Island = i)
  
  newname <- paste("l.richness", i, sep="_")
  
  assign(newname, d)
  
}

taxon.all <- rbind(l.richness_Baker, `l.richness_French Frigate Shoals`, l.richness_Guam, l.richness_Hawaii, l.richness_Howland, l.richness_Jarvis, l.richness_Johnston, l.richness_Kauai, l.richness_Kingman, l.richness_Kure, l.richness_Lisianski, l.richness_Maug, l.richness_Maui, l.richness_Oahu, `l.richness_Ofu/Olosega`, l.richness_Pagan, l.richness_Palmyra, `l.richness_Papua New Guinea`, `l.richness_Pearl and Hermes`, l.richness_Rose, l.richness_Saipan, l.richness_Swains, `l.richness_Timor Leste`, l.richness_Tutuila, l.richness_Wake)













