
library(rgdal)
library(ggmap)
library(gstat)

setwd("D:/Usuarios/s.reverte/OneDrive - CREAF/TESI/DADES/dades")

d<-read.table("Database3.txt", header=T)
f<-read.table("temperatures.txt", header=T)
names(d)
d$ROMA <- f$ROMA
d$Max <- f$Max
d$DATA <- f$DATA
d$dif <- f$Max-f$ROMA

dna <- d[8:40,]
names(dna)

coords <- SpatialPoints(dna[, c("X", "Y")], proj4string = CRS("+proj=longlat"))
plots <- SpatialPointsDataFrame(coords, dna)
ddll <- spTransform(plots, CRS("+proj=longlat"))
pts <- as.data.frame(coordinates(ddll))
names(pts) <- c("lon", "lat")

print(bubble(plots, "dif", maxsize = 5,key.entries = 2*(-1:4)))

print(bubble(plots, "Max", maxsize = 5,key.entries = 10*(2:3)))

plot(d$dif~d$Max)


hist(d$Max)
hist(dna$Max)


h<-0.0008
xrange<-diff(bbox(plots)[1,])
yrange<-diff(bbox(plots)[2,])
nx<-ceiling( (xrange/h) )
ny<-ceiling(yrange/h)
grdtop<-GridTopology(cellcentre.offset=bbox(plots)[,1],cellsize=c(h,h), cells.dim=c(nx,ny))
grd<-SpatialGrid(grdtop, proj4string=CRS("+proj=longlat") )
plot(grd)
plot(plots, add=TRUE, col="red")

library(deldir)
idw.out <- idw(dif ~ 1, plots, grd, idp = 2.5)

print(spplot(idw.out, "var1.pred"))
