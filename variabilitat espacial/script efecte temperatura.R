
setwd("D:/Usuarios/s.reverte/OneDrive - CREAF/TESI/DADES/dades")

database2 <- read.table("Database3.txt",header=T)
single <- read.table("sense singletons.txt",header=T)

database2$Poll_rate <- database2$Pollinator_count/database2$OVERALL_Flowers*1000
library(vegan)
mod1<-lm(database2$Pollinator_species~database2$T_Max)#si
mod2<-lm(database2$Wild~database2$T_Max)#no
mod3<-lm(database2$Wild_rate~database2$T_Max)#sisisi
mod4<-lm(database2$Honeybees~database2$T_Max)#no
mod5<-lm(database2$Honeybeerate~database2$T_Max)#sisisisis
mod6<-lm(database2$Poll_rate~database2$T_Max)#sisisisis
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)
summary(mod5)
summary(mod6)

library(ape)
zone.dists <- as.matrix(dist(cbind(database2$X, database2$Y)))
zone.dists.inv <- 1/zone.dists
diag(zone.dists.inv) <- 0

Moran.I(resid(mod1), zone.dists.inv)#richness
Moran.I(resid(mod2), zone.dists.inv)#wild
Moran.I(resid(mod3), zone.dists.inv)#wildrate
Moran.I(resid(mod4), zone.dists.inv)#honeybees
Moran.I(resid(mod5), zone.dists.inv)#honeybeerate
Moran.I(resid(mod6), zone.dists.inv)#pollrate
## COMPOSICIO

library(vegan)
library(betapart)

a<-read.table("flors quantitatiu.txt", header=T)
b<-read.table("bitxos quantitatiu.txt", header=T)
c<-read.table("bitxos quantitatiu sense apis.txt", header=T)


quantitative.plants<-bray.part(a)
quantitative.polls<-bray.part(b)
quantitative.senseapis<-bray.part(c)

tempdist <- dist(database2$T_Max)
apisrate <- dist(database2$Honeybeerate)


library(SpatialTools)
x <- as.matrix(database2[,7:8])
c<-dist1(x)
d<-c*100
d.dist<-as.dist(d)


mantel(apisrate, quantitative.plants$bray, method = "pearson", permutations = 9999, na.rm = FALSE)
mantel(apisrate, tempdist, method = "pearson", permutations = 9999, na.rm = FALSE)

mantel.partial(quantitative.senseapis$bray, tempdist, apisrate, method = "pearson", permutations = 9999, na.rm = FALSE)

mantel.partial(quantitative.senseapis$bray, quantitative.plants$bray, apisrate,  method = "pearson", permutations = 9999, na.rm = FALSE)

mantel.partial(quantitative.senseapis$bray, apisrate, tempdist,  method = "pearson", permutations = 9999, na.rm = FALSE)

mantel(apisrate, quantitative.plants$bray, method = "pearson", permutations = 9999, na.rm = FALSE)
mantel(apisrate, tempdist, method = "pearson", permutations = 9999, na.rm = FALSE)

mantel.partial(quantitative.groups$bray, tempdist, apisrate, method = "pearson", permutations = 9999, na.rm = FALSE)

mantel.partial(quantitative.groups$bray, quantitative.plants$bray, apisrate,  method = "pearson", permutations = 9999, na.rm = FALSE)

mantel.partial(quantitative.groups$bray, apisrate, tempdist,  method = "pearson", permutations = 9999, na.rm = FALSE)

## PROVA AMB SELECCIO DE MODELS

library(MuMIn)
options(na.action = "na.fail")
fm1 <- lm(resid(mod1) ~ resid(mod5)+ROF_Flowers+TVU_Flowers+other_flowers_abundance+flower_richness, data = database2)
dd <- dredge(fm1)
dd
subset(dd, delta < 4)
#'Best' model
summary(get.models(dd, 1)[[1]])


