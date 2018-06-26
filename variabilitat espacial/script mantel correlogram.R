
setwd("D:/Usuarios/s.reverte/OneDrive - CREAF/TESI/DADES/dades")

library(vegan)
library(SpatialTools)

b<-read.table("bitxos quantitatiu.txt", header=T)
database2<-read.table("Database2.txt", header=T)

library(vegan)
library(betapart)

a<-read.table("flors quantitatiu.txt", header=T)
b<-read.table("bitxos quantitatiu.txt", header=T)
c<-read.table("bitxos quantitatiu sense apis.txt", header=T)


quantitative.plants<-bray.part(a)
quantitative.polls<-bray.part(b)
quantitative.senseapis<-bray.part(c)

d <- as.matrix(database2[,7:8])
c<-dist1(d)
e<-c*100
d.dist<-as.dist(e)

mite.correlog <- mantel.correlog(quantitative.polls$bray, D.geo=d.dist, nperm=999)
summary(mite.correlog)
mite.correlog  
plot(mite.correlog)

