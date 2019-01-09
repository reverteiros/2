
##### QUANTITATIU

library(vegan)
library(betapart)
library(SpatialTools)

database2<-read.table("dades/Database3.txt", header=T)
database3<-read.table("dades/Databasealt.txt", header=T)
a<-read.table("dades/flors quantitatiu.txt", header=T)
b<-read.table("dades/bitxos quantitatiu.txt", header=T)
c<-read.table("dades/bitxos quantitatiu sense apis.txt", header=T)
database2$Altura <- database3$Altura

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
altdist <- dist(database3$Altura)


mantel(quantitative.senseapis$bray, tempdist, method = "pearson", permutations = 9999, na.rm = FALSE)
mantel(quantitative.plants$bray, altdist, method = "pearson", permutations = 9999, na.rm = FALSE)
mantel(quantitative.senseapis$bray, altdist, method = "pearson", permutations = 9999, na.rm = FALSE)
mantel(quantitative.plants$bray, quantitative.senseapis$bray, method = "pearson", permutations = 9999, na.rm = FALSE)
mantel(quantitative.senseapis$bray, apisrate, method = "pearson", permutations = 9999, na.rm = FALSE)
mantel(apisrate, tempdist, method = "pearson", permutations = 9999, na.rm = FALSE)
mantel(quantitative.plants$bray, apisrate, method = "pearson", permutations = 9999, na.rm = FALSE)
mantel(apisrate, altdist, method = "pearson", permutations = 9999, na.rm = FALSE)
mantel(quantitative.plants$bray, altdist, method = "pearson", permutations = 9999, na.rm = FALSE)

mantel.partial(quantitative.senseapis$bray, apisrate,quantitative.plants$bray,method = "pearson", permutations = 9999, na.rm = FALSE)
mantel.partial(quantitative.senseapis$bray, quantitative.plants$bray, apisrate,method = "pearson", permutations = 9999, na.rm = FALSE)
mantel.partial(quantitative.senseapis$bray, altdist,quantitative.plants$bray,method = "pearson", permutations = 9999, na.rm = FALSE)
mantel.partial(quantitative.senseapis$bray, quantitative.plants$bray,altdist,method = "pearson", permutations = 9999, na.rm = FALSE)
mantel.partial(quantitative.senseapis$bray, apisrate, altdist,method = "pearson", permutations = 9999, na.rm = FALSE)
mantel.partial(quantitative.senseapis$bray, altdist, apisrate,method = "pearson", permutations = 9999, na.rm = FALSE)
mantel.partial(quantitative.senseapis$bray, apisrate, tempdist,method = "pearson", permutations = 9999, na.rm = FALSE)
mantel.partial(quantitative.senseapis$bray, tempdist, apisrate,method = "pearson", permutations = 9999, na.rm = FALSE)

mantel.partial(quantitative.plants$bray, apisrate, altdist,method = "pearson", permutations = 9999, na.rm = FALSE)
mantel.partial(quantitative.plants$bray, altdist,apisrate,method = "pearson", permutations = 9999, na.rm = FALSE)
