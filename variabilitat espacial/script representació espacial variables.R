
library(rgdal)
library(ggmap)
library(gstat)


d<-read.table("dades/Database3.txt", header=T)

coords <- SpatialPoints(d[, c("X", "Y")], proj4string = CRS("+proj=longlat"))
plots <- SpatialPointsDataFrame(coords, d)
ddll <- spTransform(plots, CRS("+proj=longlat"))
pts <- as.data.frame(coordinates(ddll))
names(pts) <- c("lon", "lat")

print(bubble(plots, "Honeybees", maxsize = 5,key.entries = 40*(1:5),col="blue"))


