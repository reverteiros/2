
######################## Introductory lines

# set the working directory, which is the folder containing the files
# in my case:
setwd("C:/Users/saret/Desktop/Tesi/dades")

# install the different packages that you will need
install.packages("rgdal")
install.packages("ape")
install.packages("vegan")
install.packages("SpatialTools")
install.packages("betapart")
install.packages("pvclust")
install.packages("MuMIn")
install.packages("caret")
install.packages("mapr")
install.packages("rgbif")
install.packages("dismo")
install.packages("mapplots")
install.packages("BiodiversityR")
install.packages("corrplot")
install.packages("MuMIn")
install.packages("caret")

library("rgdal")
library("ape")
library("vegan")
library("SpatialTools")
library("betapart")
library("pvclust")
library("MuMIn")
library("caret")
library("mapr")
library("rgbif")
library("dismo")
library("mapplots")
library("BiodiversityR")
library('corrplot')
library("MuMIn")
library("caret")

####################### Rank-abundance curves

library("BiodiversityR")

rankabundance(BCI, level, digits=1, t=qt(0.975, df=n-1))

rankabunplot(xr, addit=F, labels="", scale="abundance", scaledx=F, type="o",
             xlim=c(min(xpos), max(xpos)),
             ylim=c(0, max(x[,scale])),
             specnames=c(1:5), srt=0, ...)

rankabuncomp(x, y="", factor, return.data=T, specnames=c(1:3),
             scale="abundance", scaledx=F, type="o", rainbow=T,
             legend=T, xlim=c(1, max1), ylim=c(0, max2), ...)


######################## Rarefaction

library(vegan)

data(BCI)
S <- specnumber(BCI) # observed number of species
(raremax <- min(rowSums(BCI)))
Srare <- rarefy(BCI, raremax)
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(BCI, step = 20, sample = raremax, col = "blue", cex = 0.6)


# #################################### SPATIAL ANALYSIS
# 
# ######################## Moran's I
# # for simple variables, string of values: each site has one value, such as species richness
# # need to give the spatial coordinates, X and Y
# # Tells you if there is spatial autocorrelation in the variable of interest.
# library(ape)
# 
# database2 <- read.table("Database3.txt",header=T)
# 
# zone.dists <- as.matrix(dist(cbind(database2$X, database2$Y)))
# zone.dists.inv <- 1/zone.dists
# diag(zone.dists.inv) <- 0
# 
# Moran.I(database2$ROF_Flowers, zone.dists.inv)
# Moran.I(database2$flower_richness, zone.dists.inv)
# 
# 
# 
# ######################## Spatial representation variables
# 
# bubble plot

library(rgdal)
library(ggmap)
library(gstat)

d<-read.table("Database3.txt", header=T)

coords <- SpatialPoints(d[, c("X", "Y")], proj4string = CRS("+proj=longlat"))
plots <- SpatialPointsDataFrame(coords, d)
ddll <- spTransform(plots, CRS("+proj=longlat"))
pts <- as.data.frame(coordinates(ddll))
names(pts) <- c("lon", "lat")

print(bubble(plots, "Honeybees", maxsize = 5,key.entries = 40*(1:5),col="blue"))
# 
# 
# # spatial grid colours interpolation
# 
# library(sp)
# library(tidyverse)
# 
# d<-read.table("Database3.txt", header=T)
# 
# x.range <- range(d$X)
# y.range <- range(d$Y)
# x<-seq(x.range[1], x.range[2], length.out=20)
# y<-seq(y.range[1], y.range[2], length.out=20)
# grd<-expand.grid(x,y)
# coordinates(d) = ~X+Y
# coordinates(grd) <- ~ Var1+Var2
# gridded(grd) <- TRUE
# proj4string(d) <- CRS("+proj=longlat +datum=WGS84")
# proj4string(grd) <- CRS("+proj=longlat +datum=WGS84")
# plot(grd, cex=1.5)
# 
# dat.idw <- idw(formula=ROF_Flowers ~ 1, locations = d, newdata = grd, idp = 2.0)
# plot(dat.idw)
# 
# # bubble chart categories
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
# 
# plants <- database2[,2:3]
# plants$others <- database2$other_flowers_abundance
# plants$Plot <- c(1:40)
# plants$X <- database2$X
# plants$Y <- database2$Y
# 
# plants <- plants %>% 
#   gather(species, abundance, c("ROF_Flowers","TVU_Flowers","others")) %>%
#   arrange(Plot, X, Y, species)
# 
# xlim <- c(1.82,1.95)
# ylim <- c(41.25,41.32)
# xyz <- make.xyz(plants$X,plants$Y,plants$abundance,plants$species)
# col <- c('red','green','blue')
# basemap(xlim, ylim,bg='white')
# draw.pie(xyz$x, xyz$y, xyz$z, radius = 0.005, col=col)
# legend.pie(1.93,41.26,labels=unique(plants$species), radius=0.005, bty="n", col=col,cex=0.8, label.dist=1.3)
# 
# 
# legend.z <- round(max(rowSums(xyz$z,na.rm=TRUE)),0)
# legend.bubble(1.83,41.30,z=legend.z,round=1,maxradius=0.008,bty="n",txt.cex=0.7)
# text(1.83,41.32,"Abundance",cex=0.8)
# 
# 

