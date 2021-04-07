

setwd("D:/Usuarios/s.reverte/OneDrive - CREAF/TESI/DADES/dades")

plants <- read.table("flors quantitatiu.txt", header=T)
bitxos <- read.table("bitxos quantitatiu.txt", header=T)
bitxossa <- read.table("bitxos quantitatiu sense apis.txt", header=T)
database2 <- read.table("database2.txt",header=T)

library(vegan)
library(ape)

Hplants <- diversity(plants)
Jplants <- Hplants/log(specnumber(plants))

Hbitxos <- diversity(bitxos)
Jbitxos <- H/log(specnumber(bitxos))

Hbitxossa <- diversity(bitxossa)
Jbitxossa <- H/log(specnumber(bitxossa))

zone.dists <- as.matrix(dist(cbind(database2$X, database2$Y)))
zone.dists.inv <- 1/zone.dists
diag(zone.dists.inv) <- 0

Moran.I(Jplants, zone.dists.inv)

mean(Jplants)
sd(Jplants)/mean(Jplants)*100
