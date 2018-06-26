
setwd("D:/Usuarios/s.reverte/OneDrive - CREAF/TESI/DADES/dades")
database2 <- read.table("Database3.txt",header=T)

names(database2)
database2$Wildrate <- database2$Pollinator_count/database2$OVERALL_Flowers*1000

hist(database2$Wildrate)
hist(sqrt(database2$Wildrate))
log10(database2$ROF_Flowers)
log10(database2$TVU_Flowers)
log10(database2$other_flowers_abundance)
log10(database2$OVERALL_Flowers)
sqrt(database2$Pollinator_species)
sqrt(database2$Pollinator_count)
sqrt(database2$Wild)
sqrt(database2$Honeybees)
sqrt(database2$Honeybeerate)
sqrt(database2$Honeybees_ROF_rate)
sqrt(database2$Honeybees_TVU_rate)
sqrt(database2$Wild_ROF_rate)
sqrt(database2$Wild_TVU_rate)
sqrt(database2$Wildrate)

fm1 <- lm(sqrt(Wildrate) ~ sqrt(Honeybeerate)*(log10(database2$ROF_Flowers)+log10(database2$TVU_Flowers)+log10(database2$other_flowers_abundance)+flower_richness), data = database2)

fm1 <- lm(sqrt(Wild) ~ sqrt(Pollinator_species)*(log10(OVERALL_Flowers)+flower_richness), data = database2)

fm1 <- lm(sqrt(Wildrate) ~ sqrt(Pollinator_species)*log10(OVERALL_Flowers), data = database2)

library(MuMIn)
options(na.action = "na.fail")
fm1 <- lm(sqrt(Honeybeerate) ~ log10(database2$ROF_Flowers)+log10(database2$TVU_Flowers)+log10(database2$other_flowers_abundance)+flower_richness, data = database2)
dd <- dredge(fm1)
dd
subset(dd, delta < 4)
#'Best' model
summary(get.models(dd, 1)[[1]])



library(ggplot2)

plot(database2$Honeybeerate~database2$flower_richness)

g <- ggplot(database2, aes(Honeybeerate))
g <- g + geom_point(aes(y=ROF_Flowers), colour="red")
g <- g + geom_point(aes(y=TVU_Flowers), colour="blue")
g <- g + geom_point(aes(y=other_flowers_abundance), colour="green")
g <- g + geom_smooth(aes(y=ROF_Flowers), colour="red")
g <- g + geom_smooth(aes(y=TVU_Flowers), colour="blue")
g <- g + geom_smooth(aes(y=other_flowers_abundance), colour="green")
g 

cor.test(database2$Wild,database2$Pollinator_species)
plot(database2$Wild,database2$Pollinator_species)
