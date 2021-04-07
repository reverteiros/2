

library(gstat)
library(sp)
library(tidyverse)


##### ROF

database3rof<-read.table("dades/Database3.txt", header=T) %>%
  filter(PLOT != 35) %>%
  filter(PLOT != 29)

meandataperplotROF$X <-  database3rof$X
meandataperplotROF$Y <-  database3rof$Y

x.range <- range(meandataperplotROF$X)
y.range <- range(meandataperplotROF$Y)
x<-seq(x.range[1], x.range[2], length.out=20)
y<-seq(y.range[1], y.range[2], length.out=20)
grd<-expand.grid(x,y)
coordinates(meandataperplotROF) = ~X+Y
coordinates(grd) <- ~ Var1+Var2
gridded(grd) <- TRUE
proj4string(meandataperplotROF) <- CRS("+proj=longlat +datum=WGS84")
proj4string(grd) <- CRS("+proj=longlat +datum=WGS84")
plot(grd, cex=1.5)

dat.idw <- idw(formula=Homospecific_presence ~ 1, locations = meandataperplotROF, newdata = grd, idp = 2.0)
plot(dat.idw)

dat.idw <- idw(formula=Mean_Homospecific ~ 1, locations = meandataperplotROF, newdata = grd, idp = 2.0)
plot(dat.idw)


#### TVUF

database3tvuf<-read.table("dades/Database3.txt", header=T) 

meandataperplotTVUF$X <-  database3tvuf$X
meandataperplotTVUF$Y <-  database3tvuf$Y

x.range <- range(meandataperplotTVUF$X)
y.range <- range(meandataperplotTVUF$Y)
x<-seq(x.range[1], x.range[2], length.out=20)
y<-seq(y.range[1], y.range[2], length.out=20)
grd<-expand.grid(x,y)
coordinates(meandataperplotTVUF) = ~X+Y
coordinates(grd) <- ~ Var1+Var2
gridded(grd) <- TRUE
proj4string(meandataperplotTVUF) <- CRS("+proj=longlat +datum=WGS84")
proj4string(grd) <- CRS("+proj=longlat +datum=WGS84")
plot(grd, cex=1.5)



dat.idw <- idw(formula=Homospecific_presence ~ 1, locations = meandataperplotTVUF, newdata = grd, idp = 2.0)
plot(dat.idw)

dat.idw <- idw(formula=Mean_Homospecific ~ 1, locations = meandataperplotTVUF, newdata = grd, idp = 2.0)
plot(dat.idw)

dat.idw <- idw(formula=Heterospecific_presence ~ 1, locations = meandataperplotTVUF, newdata = grd, idp = 2.0)
plot(dat.idw)

dat.idw <- idw(formula=Fruit_set ~ 1, locations = meandataperplotTVUF, newdata = grd, idp = 2.0)
plot(dat.idw)

dat.idw <- idw(formula=Seed_set ~ 1, locations = meandataperplotTVUF, newdata = grd, idp = 2.0)
plot(dat.idw)

