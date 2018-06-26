
library(ape)

setwd("D:/Usuarios/s.reverte/OneDrive - CREAF/TESI/DADES/dades")
database2 <- read.table("Database3.txt",header=T)

zone.dists <- as.matrix(dist(cbind(database2$X, database2$Y)))
zone.dists.inv <- 1/zone.dists
diag(zone.dists.inv) <- 0

Moran.I(DATA$OS, zone.dists.inv)
