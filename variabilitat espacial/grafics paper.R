

############################## PLOT APIS
library(rgdal)
library(ggmap)
library(gstat)


d<-read.table("dades/Database3.txt", header=T)

coords <- SpatialPoints(d[, c("X", "Y")], proj4string = CRS("+proj=longlat"))
plots <- SpatialPointsDataFrame(coords, d)
ddll <- spTransform(plots, CRS("+proj=longlat"))
pts <- as.data.frame(coordinates(ddll))
names(pts) <- c("lon", "lat")

print(bubble(plots, "Honeybees", maxsize = 11,key.entries = 10*2^(1:4),col="blue"))



############################## PIE CHART PER PARCELA

database2<-read.table("dades/Database3.txt", header=T)
plants<-read.table("dades/flors quantitatiu.txt", header=T)

library(sp)
library(ape)
library(raster)
library(ggmap)
library(mapr)
library(rgbif)
library(dismo)
library(mapplots)
library(tidyr)
library(vegan)
library(SpatialTools)
library(ggplot2)
library(dplyr)

plants <- bitxostt %>% 
  gather(species, abundance) 

plants$Plot <- c(1:40)
plants$X <- database2$X
plants$Y <- database2$Y

plants$abundance <- as.numeric(plants$abundance)

xlim <- c(1.82,1.95)
ylim <- c(41.26,41.32)
xyz <- make.xyz(plants$X,plants$Y,plants$abundance,plants$species)
col <- c("grey11","darkolivegreen4","darkorchid1","darkmagenta","green4","orangered3","navyblue","indianred4","yellow4","thistle4","dodgerblue","chocolate","magenta2","darkorange1","gold","green","mediumblue","red","springgreen4","firebrick4")
basemap(xlim, ylim,bg='white')
draw.pie(xyz$x, xyz$y, xyz$z, radius = 0.005, col=col)
legend.pie(1.93,41.26,labels=unique(plants$species), radius=0.005, bty="n", col=col,cex=0.8, label.dist=1.3)
legend.z <- round(max(rowSums(xyz$z,na.rm=TRUE)),0)
legend.bubble(1.83,41.30,z=legend.z,round=1,maxradius=0.015,bty="n",txt.cex=0.7)
text(1.83,41.32,"Abundance",cex=0.8)

# OJO!! ELS COLORS NO CORRESPONEN AMB L'ESPECIE QUE SE SUPOSA QUE CORRESPONEN




# ############################## BARPLOT PER PARCELA
# 
# require(devtools)
# library(tidyverse)
# library(ggplot2)
# 
# speciess<-read.table("dades/llista sp grafics2.txt",header=T)
# bitxos<-read.table("dades/bitxos quantitatiu sense apis.txt",header=T)
# 
# bitxost <- as.data.frame(t(bitxos))
# bitxost$Species <- colnames(bitxos)
# bitxosabundants <- bitxost %>% dplyr::inner_join(speciess, by=(c("Species")))
# bitxosraros <- bitxost %>% dplyr::anti_join(speciess, by=(c("Species")))
# bitxosraros <- colSums(bitxosraros[,1:40])
# bitxosraros$Species <- 1
# bitxosabundants[20,] <- bitxosraros
# 
# 
# bitxostt <- as.data.frame(t(bitxosabundants))
# names(bitxostt) <- c("Rodanthidium_sticticum","Pseudophilotes_panoptes","Microlepidoptera","Lasioglossum_mediterraneum","Chamaesyrphus","Lobonyx_aeneus","Oxythyrea_funesta","Adela_aldrovandella","Dasytes_nigroaeneus","Lasioglossum_transitorium_planulum","Empididae","Hylaeus_hyalinatus","Andrena_angustior_impressa","Andrena_nigroaenea","Bombus_terrestris","Anthophora_dispar","Chrysotoxum_cautum","Bibio_sp","Andrena_djelfensis","Other_species")
# bitxostt <- bitxostt[-41,]
# 
# barplot(bitxosabundants[,3])
# 
# 
# ggplot(data=bitxosabundants, aes(x=Species,y=V40,fill=Species)) +
#   geom_bar(stat="identity") +
#   ylim(c(0,23))+
#   theme(axis.line=element_blank(),axis.text.x=element_blank(),
#         axis.text.y=element_blank(),axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),legend.position = "none",
#         panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),plot.background=element_blank()) +
#   scale_fill_manual(values=c("grey11","darkolivegreen4","darkorchid1","darkmagenta","green4","orangered3","navyblue","indianred4","yellow4","thistle4","dodgerblue","chocolate","magenta2","darkorange1","gold","green","mediumblue","red","springgreen4","firebrick4"))


