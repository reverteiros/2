

library(gstat)
library(sp)
library(tidyverse)

d<-read.table("dades/Database3.txt", header=T)

sample <- d %>%
  select(X,Y,T_Max) %>%
  mutate(Plot=c(1:40)) %>%
  left_join(ROF,by="Plot") %>%
  mutate(dTH = unlist(dROF)) %>%
  filter(Plot != 29)

x.range <- range(sample$X)
y.range <- range(sample$Y)
x<-seq(x.range[1], x.range[2], length.out=20)
y<-seq(y.range[1], y.range[2], length.out=20)
grd<-expand.grid(x,y)
coordinates(sample) = ~X+Y
coordinates(grd) <- ~ Var1+Var2
gridded(grd) <- TRUE
proj4string(sample) <- CRS("+proj=longlat +datum=WGS84")
proj4string(grd) <- CRS("+proj=longlat +datum=WGS84")
plot(grd, cex=1.5)

sample$dTH

dat.idw <- idw(formula=dTH ~ 1, locations = sample, newdata = grd, idp = 2.0)

plot(dat.idw)

dat.idw <- idw(formula=Mean_Homospecific ~ 1, locations = sample, newdata = grd, idp = 2.0)

plot(dat.idw)
dat.idw <- idw(formula=Fruit_set ~ 1, locations = sample, newdata = grd, idp = 2.0)

plot(dat.idw)
dat.idw <- idw(formula=Seed_set ~ 1, locations = sample, newdata = grd, idp = 2.0)

plot(dat.idw)
dat.idw <- idw(formula=Mean_Heterospecific ~ 1, locations = sample, newdata = grd, idp = 2.0)

plot(dat.idw)
dat.idw <- idw(formula=Flowers_with_pollen ~ 1, locations = sample, newdata = grd, idp = 2.0)

plot(dat.idw)
dat.idw <- idw(formula=Percent_pollination ~ 1, locations = sample, newdata = grd, idp = 2.0)

plot(dat.idw)
dat.idw <- idw(formula=Mean_weigth_viables ~ 1, locations = sample, newdata = grd, idp = 2.0)

plot(dat.idw)
dat.idw <- idw(formula=dTF ~ 1, locations = sample, newdata = grd, idp = 2.0)

plot(dat.idw)

sample$Mean_weigth_viables
