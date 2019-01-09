

library(betapart)
library(SpatialTools)
library(ggplot2)

database2 <- read.table("dades/Database3.txt",header=T)

x <- as.matrix(database2[,7:8])
c<-dist1(x)
d<-c*100
d.dist<-as.dist(d)


pollinators<-read.table("dades/bitxos quantitatiu.txt",header=T)
senseapis<-read.table("dades/bitxos quantitatiu sense apis.txt",header=T)
plants<-read.table("dades/flors quantitatiu.txt", header=T)

quantitative.pollinators<-bray.part(pollinators)
quantitative.plants<-bray.part(plants)
quantitative.senseapis<-bray.part(senseapis)

polldist <- data.frame(t(combn(rownames(pollinators),2)), as.numeric(quantitative.pollinators$bray))
names(polldist) <- c("Plot", "Plot", "Pollinatorsdistance")

senseapisdist <- data.frame(t(combn(rownames(pollinators),2)), as.numeric(quantitative.senseapis$bray))
names(senseapisdist) <- c("Plot", "Plot", "Senseapisdistance")

plantdist <- data.frame(t(combn(rownames(pollinators),2)), as.numeric(quantitative.plants$bray))
names(plantdist) <- c("Plot", "Plot", "Plantdistance")

geographicdist <- data.frame(t(combn(rownames(pollinators),2)), as.numeric(d.dist))
names(geographicdist) <- c("Plot", "Plot", "Geographicaldistance")


quantitatiu<-data.frame(geographicdist$Geographicaldistance, polldist$Pollinatorsdistance,plantdist$Plantdistance,senseapisdist$Senseapisdistance)
zz<-c("geographicdist","polldist","plantdist","senseapis")
names(quantitatiu)<-zz

g <- ggplot(quantitatiu, aes(x=geographicdist))
g <- g + geom_smooth(aes(y=plantdist), colour="green")+coord_cartesian(ylim = c(0, 1))
g <- g + geom_smooth(aes(y=polldist), colour="brown")+coord_cartesian(ylim = c(0, 1))
g <- g + geom_smooth(aes(y=senseapis), colour="blue")+coord_cartesian(ylim = c(0, 1))
g <- g + theme_light()
g <- g +labs( x = "Geographical distance (km)", y = "Bray-Curtis dissimilarity")
g
