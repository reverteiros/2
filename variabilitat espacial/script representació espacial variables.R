
library(rgdal)
library(ggmap)
library(gstat)

setwd("D:/Usuarios/s.reverte/OneDrive - CREAF/TESI/DADES/dades")

## Leer tabla, debe contener coordenadas
d<-read.table("Database.txt", header=T)

d$ambapis<-ambapis
d$senseapis<-senseapis

## Le dices qué son las coordenadas y que son en latitud-longitud
coords <- SpatialPoints(d[, c("X", "Y")], proj4string = CRS("+proj=longlat"))
summary(coords)

## Generas un tipo de dataframe espacialmente explícito
plots <- SpatialPointsDataFrame(coords, d)
ddll <- spTransform(plots, CRS("+proj=longlat"))
pts <- as.data.frame(coordinates(ddll))
names(pts) <- c("lon", "lat")

names(d)
## Representar tus puntos sobre un mapa Google maps, en el primer paréntesis le pones el nombre del sitio que quieres, y modifica el zoom al gusto
#qmap("Rat-penat", zoom = 12) + geom_point(data = pts)

## Convertir las variables que quieras representar en numéricas
plots$senseapis1 <- as.numeric(plots$senseapis)

## Voilà! Te representa tus áreas en función de la variable que elijas
plot(plots, col = plots$senseapis, pch = 7)

## Otro tipo de gráfico, bubble plot. Puedes jugar con la leyenda, es el "key.entries", añadir o quitar particiones
library(lattice)
print(bubble(plots, "ambapis", maxsize = 9,key.entries = 0.2*(1:5)))


ambapis
senseapis

## Generar una malla espacial para hacer regresión espacial
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
idw.out <- idw(OVERALL_Flowers ~ 1, plots, grd, idp = 2.5)

print(spplot(idw.out, "var1.pred"))


mean(d$OVERALL_Flowers)
