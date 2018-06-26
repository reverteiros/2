
setwd("D:/Usuarios/s.reverte/Desktop/dades")

temperatures2 <- read.table("flors quantitatiu.txt", header=T)

names(temperatures)

library(vegan)
diversity.bitxos.amb.apis<-diversity(temperatures, index = "shannon", MARGIN = 1, base = exp(1))
diversity.bitxos.sense.apis<-diversity(temperatures1, index = "shannon", MARGIN = 1, base = exp(1))
diversity.plants<-diversity(temperatures2, index = "shannon", MARGIN = 1, base = exp(1))


sqrt.distance<-sqrt(geographicdist$geographicdist)
sqrt.fenologia<-sqrt(fenologiadist$fenologia)

mod1<-lm(ambapis~diversity.plants)
mod2<-lm(tOSdist$tOSdist~sqrt.fenologia)
mod3<-lm(quant.plants$quant.plants~sqrt.fenologia+sqrt.distance)
mod4<-lm(quant.plants$quant.plants~sqrt.fenologia+sqrt.distance+sqrt.fenologia*sqrt.distance)


summary(mod1)
 summary(mod2)
summary(mod3)
summary(mod4)

anova(mod3,mod4)
plot1<-plot(tOSdist$tOSdist~sqrt.fenologia)
abline(mod2)

hist(beta.div.polls$beta.div.polls)

