
############################## PLOT BARRES APIS-WILD
library(tidyr)
library(ggplot2)
library(dplyr)


d<-read.table("dades/Database3.txt", header=T)
filtered <- dplyr::select(d, Honeybees,Wild)

filtered <- filtered %>% 
  gather(species, abundance) 

filtered$Plot <- c(1:40)

filtered$species <-factor(filtered$species, 
                          levels = c("Honeybees", "Wild"))

filtered2 <- filtered %>% filter(., Plot == 40)

ggplot(data=filtered2, aes(x=species,y=abundance,fill=species)) +
  geom_bar(stat="identity") +
  ylim(c(0,190))+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position = "none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  scale_fill_manual(values=c("grey11","firebrick2"))


############################## PLOT BARRES FLORS
library(tidyr)
library(ggplot2)
library(dplyr)


d<-read.table("dades/Database3.txt", header=T)
filtered <- dplyr::select(d, ROF_Flowers, TVU_Flowers,other_flowers_abundance)

filtered <- filtered %>% 
  gather(species, abundance) 

filtered$Plot <- c(1:40)

filtered$species <-factor(filtered$species, 
                       levels = c("TVU_Flowers", "ROF_Flowers", "other_flowers_abundance"))

filtered2 <- filtered %>% filter(., Plot == 40)

ggplot(data=filtered2, aes(x=species,y=abundance,fill=species)) +
  geom_bar(stat="identity") +
  ylim(c(0,111280))+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position = "none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  scale_fill_manual(values=c("mediumblue","green2","red2"))

# 
# 
# ############################## PIE CHART PER PARCELA
# 
# database2<-read.table("dades/Database3.txt", header=T)
# plants<-read.table("dades/flors quantitatiu.txt", header=T)
# 
# library(sp)
# library(ape)
# library(raster)
# library(ggmap)
# library(mapr)
# library(rgbif)
# library(dismo)
# library(mapplots)
# library(tidyr)
# library(vegan)
# library(SpatialTools)
# library(ggplot2)
# library(dplyr)
# 
# filtered <- dplyr::select(database2, Wild, Honeybees)
# 
# 
# filtered <- filtered %>% 
#   gather(species, abundance) 
# 
# filtered$Plot <- c(1:40)
# filtered$X <- database2$X
# filtered$Y <- database2$Y
# 
# filtered$abundance <- as.numeric(filtered$abundance)
# 
# xlim <- c(1.82,1.95)
# ylim <- c(41.26,41.32)
# xyz <- make.xyz(filtered$X,filtered$Y,filtered$abundance,filtered$species)
# col <- c("grey11","indianred1")
# basemap(xlim, ylim,bg='white')
# draw.pie(xyz$x, xyz$y, xyz$z, radius = 0.005, col=col)
# legend.pie(1.93,41.26,labels=unique(filtered$species), radius=0.005, bty="n", col=col,cex=0.8, label.dist=1.3)
# legend.z <- round(max(rowSums(xyz$z,na.rm=TRUE)),0)
# legend.bubble(1.83,41.30,z=legend.z,round=1,maxradius=0.009,bty="n",txt.cex=0.7)
# text(1.83,41.32,"Abundance",cex=0.8)
# 
# # OJO!! ELS COLORS NO CORRESPONEN AMB L'ESPECIE QUE SE SUPOSA QUE CORRESPONEN




############################## BARPLOT PER PARCELA

require(devtools)
library(tidyverse)
library(ggplot2)

speciess<-read.table("dades/llista sp grafics2.txt",header=T)
bitxos<-read.table("dades/bitxos quantitatiu sense apis.txt",header=T)

filtered <- dplyr::select(bitxos, Rodanthidium_sticticum, Pseudophilotes_panoptes,Lobonyx_aeneus,Oxythyrea_funesta,Adela_aldrovandella,Lasioglossum_transitorium_planulum,Empididae,Andrena_angustior_impressa,Andrena_nigroaenea,Chrysotoxum_cautum,Bibio_sp,Andrena_djelfensis)

filtered2 <- filtered %>% 
  gather(species, abundance) 

filtered2$Plot <- c(1:40)
  
filtered2$species <- factor(filtered2$species,
                            levels = c("Rodanthidium_sticticum", "Pseudophilotes_panoptes","Lobonyx_aeneus","Bibio_sp","Oxythyrea_funesta","Adela_aldrovandella","Lasioglossum_transitorium_planulum","Empididae","Andrena_angustior_impressa","Andrena_nigroaenea","Chrysotoxum_cautum","Andrena_djelfensis"))

filtered3 <- filtered2 %>% filter(., Plot == 40)

ggplot(data=filtered3, aes(x=species,y=abundance,fill=species)) +
  geom_bar(stat="identity") +
  ylim(c(0,10))+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position = "none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  scale_fill_manual(values=c("grey11","darkolivegreen4","darkmagenta","green4","orangered3","navyblue","indianred4","yellow4","thistle4","dodgerblue","chocolate","magenta2"))
