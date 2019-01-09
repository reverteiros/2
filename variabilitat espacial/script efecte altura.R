
library(vegan)
library(SpatialTools)
library(betapart)
library(ape)

database2<-read.table("dades/Database3.txt", header=T)
database3<-read.table("dades/Databasealt.txt", header=T)
a<-read.table("dades/flors quantitatiu.txt", header=T)
b<-read.table("dades/bitxos quantitatiu.txt", header=T)
c<-read.table("dades/bitxos quantitatiu sense apis.txt", header=T)
database2$Altura <- database3$Altura
database2$Poll_rate <- database2$Pollinator_count/database2$OVERALL_Flowers*1000


mod1f <- lm(database2$OVERALL_Flowers~database2$Altura)
mod2f <- lm(database2$flower_richness~database2$Altura)
summary(mod1f)
summary(mod2f)

mod1<-lm(database2$Pollinator_species~database2$T_Max)#si
mod2<-lm(database2$Wild~database2$T_Max+database2$Altura)#no
mod3<-lm(database2$Wild_rate~database2$T_Max+database2$Altura)#sisisi
mod4<-lm(database2$Honeybees~database2$T_Max+database2$Altura)#no
mod5<-lm(database2$Honeybeerate~database2$T_Max+database2$Altura)#sisisisis
mod6<-lm(database2$Poll_rate~database2$T_Max+database2$Altura)#sisisisis
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)
summary(mod5)
summary(mod6)


zone.dists <- as.matrix(dist(cbind(database2$X, database2$Y)))
zone.dists.inv <- 1/zone.dists
diag(zone.dists.inv) <- 0

Moran.I(resid(mod1), zone.dists.inv)#richness
Moran.I(resid(mod2), zone.dists.inv)#wild
Moran.I(resid(mod3), zone.dists.inv)#wildrate
Moran.I(resid(mod4), zone.dists.inv)#honeybees
Moran.I(resid(mod5), zone.dists.inv)#honeybeerate
Moran.I(resid(mod6), zone.dists.inv)#pollrate
Moran.I(resid(mod1f), zone.dists.inv)#honeybeerate
Moran.I(resid(mod2f), zone.dists.inv)#pollrate


quantitative.plants<-bray.part(a)
quantitative.polls<-bray.part(b)
quantitative.senseapis<-bray.part(c)

d <- as.matrix(database2[,7:8])
f<-dist1(d)
d.dist<-as.dist(f*100)

altdist <- dist(database3$Altura)
tempdist <- dist(database2$T_Max)
apisrate <- dist(database2$Honeybeerate)

#### flors
mantel(quantitative.plants$bray, altdist, method = "pearson", permutations = 9999, na.rm = FALSE)



mantel.partial(quantitative.plants$bray, d.dist,altdist,method = "pearson", permutations = 9999, na.rm = FALSE)

#### bitxos amb apis
mantel(quantitative.polls$bray, altdist, method = "pearson", permutations = 9999, na.rm = FALSE)

mantel.partial(quantitative.polls$bray, d.dist,tempdist,method = "pearson", permutations = 9999, na.rm = FALSE)
mantel.partial(quantitative.polls$bray, d.dist,altdist,method = "pearson", permutations = 9999, na.rm = FALSE)


#### bitxos sense apis
mantel(quantitative.senseapis$bray, altdist, method = "pearson", permutations = 9999, na.rm = FALSE)

mantel.partial(quantitative.senseapis$bray,d.dist,tempdist,method = "pearson", permutations = 9999, na.rm = FALSE)
mantel.partial(quantitative.senseapis$bray,d.dist,altdist,method = "pearson", permutations = 9999, na.rm = FALSE)
