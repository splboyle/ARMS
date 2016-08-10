# ARMS

setwd("~/GitHub/ARMS")
workingARMS <- read.csv("data/workingARMS.csv", header = T, na.strings = c("", "NA"))
crabs <- read.csv("data/Brachyuran_ARMS.csv", header = T, na.strings = c("", "NA"))
region.matrix <- read.csv("data/matrix.csv", header = T)

library(reshape)
library(reshape2)
library(vegan)
library(ggplot2)
library(ggord)
library(plyr)

# Call island out for legend later
uIsl <- unique(region.matrix$Region)


rm2 <- ddply(region.matrix, "ARMS_Unit", numcolwise(sum))
write.csv(rm2, "data/matrix.csv")
# Specaccum

CT <- specaccum(subset(region.matrix, Region == "Coral Triangle")[,5:ncol(region.matrix)], method = "random")
NWHI <- specaccum(subset(region.matrix, Region == "NW Hawaiian Islands")[,5:ncol(region.matrix)], method = "random")
MainHI <- specaccum(subset(region.matrix, Region == "Main Hawaiian Islands")[,5:ncol(region.matrix)], method = "random")
Marianas <- specaccum(subset(region.matrix, Region == "Marianas Islands")[,5:ncol(region.matrix)], method = "random")
Samoa <- specaccum(subset(region.matrix, Region == "American Samoa")[,5:ncol(region.matrix)], method = "random")
Line <- specaccum(subset(region.matrix, Region == "Line")[,5:ncol(region.matrix)], method = "random")
Phoenix <- specaccum(subset(region.matrix, Region == "Phoenix")[,5:ncol(region.matrix)], method = "random")

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
legend(27,10,legend=(uIsl[legsort]),
       fill = (c("red","orange","blue","green","yellow","purple", "cyan")[legsort]),
       cex = 0.75) 
dev.off()

ggplot(data = sp.all, aes(x = Region, y = Richness))+
  geom_boxplot(aes(fill = Region)) +
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  theme(legend.position = "none")

##########################
## FIGURE THIS SHIT OUT
xx <- ddply(region.matrix, "Island", numcolwise(sum))
rownames(xx) <- xx[,1]
xx$Island <- NULL
Brachy.MDS <- metaMDS(xx[,2:ncol(xx)])


