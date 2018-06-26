
setwd("D:/Usuarios/s.reverte/OneDrive - CREAF/TESI/DADES/dades")

database2 <- read.table("Database3.txt",header=T)

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


plants <- database2[,2:3]
plants$others <- database2$other_flowers_abundance
plants$Plot <- c(1:40)
plants$X <- database2$X
plants$Y <- database2$Y

plants <- plants %>% 
  gather(species, abundance, c("ROF_Flowers","TVU_Flowers","others")) %>%
  arrange(Plot, X, Y, species)

xlim <- c(1.82,1.95)
ylim <- c(41.26,41.31)
xyz <- make.xyz(plants$X,plants$Y,plants$abundance,plants$species)
col <- c('red','green','blue')
basemap(xlim, ylim,bg='white')
draw.pie(xyz$x, xyz$y, xyz$z, radius = 0.005, col=col)
legend.pie(1.93,41.26,labels=unique(plants$species), radius=0.005, bty="n", col=col,cex=0.8, label.dist=1.3)
