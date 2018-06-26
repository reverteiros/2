
##### QUANTITATIU

setwd("D:/Usuarios/s.reverte/OneDrive - CREAF/TESI/DADES/dades")

library(vegan)
library(betapart)
library(SpatialTools)

names(database2)

database2 <- read.table("Database3.txt",header=T)
a<-read.table("flors quantitatiu.txt", header=T)
b<-read.table("bitxos quantitatiu.txt", header=T)
c<-read.table("bitxos quantitatiu sense apis.txt", header=T)

x <- as.matrix(database2[,7:8])
c<-dist1(x)
d<-c*100
d.dist<-as.dist(d)
geographicdist <- as.matrix(d.dist)


quantitative.bitxos<-bray.part(b)
quantitative.senseapis<-bray.part(c)
quantitative.plants<-bray.part(a)

apisrate <- dist(database2$Honeybeerate)
tempdist <- dist(database2$T_Max)

mantel(quantitative.plants$bray, tempdist, method = "pearson", permutations = 9999, na.rm = FALSE)

mantel(quantitative.bitxos$bray, tempdist, method = "pearson", permutations = 9999, na.rm = FALSE)

mantel(quantitative.senseapis$bray, tempdist, method = "pearson", permutations = 9999, na.rm = FALSE)

mantel(quantitative.plants$bray, tempdist, method = "pearson", permutations = 9999, na.rm = FALSE)

mantel(quantitative.bitxos$bray, tempdist, method = "pearson", permutations = 9999, na.rm = FALSE)

mantel(quantitative.senseapis$bray, tempdist, method = "pearson", permutations = 9999, na.rm = FALSE)

mantel(quantitative.plants$bray, quantitative.bitxos$bray, method = "pearson", permutations = 9999, na.rm = FALSE)
mantel(quantitative.bitxos$bray, apisrate, method = "pearson", permutations = 9999, na.rm = FALSE)
mantel(quantitative.plants$bray, apisrate, method = "pearson", permutations = 9999, na.rm = FALSE)
mantel(quantitative.bitxos$bray, tempdist, method = "pearson", permutations = 9999, na.rm = FALSE)
mantel(tempdist, apisrate, method = "pearson", permutations = 9999, na.rm = FALSE)

mantel.partial(quantitative.senseapis$bray, quantitative.plants$bray, tempdist,method = "pearson", permutations = 9999, na.rm = FALSE)
mantel.partial(quantitative.bitxos$bray, tempdist, quantitative.plants$bray,method = "pearson", permutations = 9999, na.rm = FALSE)
mantel.partial(quantitative.bitxos$bray, apis, tempdist,method = "pearson", permutations = 9999, na.rm = FALSE)
mantel.partial(tempdist, apis, quantitative.plants$bray,method = "pearson", permutations = 9999, na.rm = FALSE)


