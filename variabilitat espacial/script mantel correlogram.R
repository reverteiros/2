
library(vegan)
library(SpatialTools)
library(betapart)

database2<-read.table("dades/Database3.txt", header=T)
a<-read.table("dades/flors quantitatiu.txt", header=T)
b<-read.table("dades/bitxos quantitatiu.txt", header=T)
c<-read.table("dades/bitxos quantitatiu sense apis.txt", header=T)

quantitative.plants<-bray.part(a)
quantitative.polls<-bray.part(b)
quantitative.senseapis<-bray.part(c)

d <- as.matrix(database2[,7:8])
f<-dist1(d)
d.dist<-as.dist(f*100)

mite.correlog <- mantel.correlog(quantitative.polls$bray, D.geo=d.dist, nperm=999)
mite.correlog  
plot(mite.correlog)

