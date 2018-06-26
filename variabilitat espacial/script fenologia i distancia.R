
setwd("D:/Usuarios/s.reverte/Desktop/dades")

data<-read.table("fenologia.txt", header=T,colnames(1))

library(vegan)

fenologia<-dist(data$DATA)
  
mOS<-as.matrix(fenologia)

fenologiadist <- data.frame(t(combn(rownames(mOS),2)), as.numeric(fenologia))
names(fenologiadist) <- c("Plot1", "Plot2", "fenologia")


library(SpatialTools)

b <- read.table("coordinates.txt", header=T)

x <- as.matrix(b[,1-2])

c<-dist1(x)

d<-c*100

d.dist<-as.dist(d)

geographicdist <- data.frame(t(combn(rownames(mOS),2)), as.numeric(d.dist))
names(geographicdist) <- c("Plot1", "Plot2", "geographicdist")

