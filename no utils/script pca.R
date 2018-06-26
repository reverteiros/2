
setwd("D:/Usuarios/s.reverte/OneDrive - CREAF/TESI/DADES/dades")

database2 <- read.table("database2.txt",header=T)

names(database2)

subdata <- data.frame(database2[,4:6])
subdata$other_flowers_abundance <- database2$other_flowers_abundance
subdata$flower_richness <- database2$flower_richness
subdata$Number.of.species.HL <- database2$Number.of.species.HL
subdata$Solitary <- database2$Solitary
subdata$Honeybees <- database2$Honeybees
subdata$Honeybeerate <- database2$Honeybeerate
subdata$solitaryrate <- database2$solitaryrate
#subdata$OVERALL_Flowers <- database2$OVERALL_Flowers

prcomp(subdata, scale = TRUE)
plot(prcomp(subdata))
summary(prcomp(subdata, scale = TRUE))
biplot(prcomp(subdata, scale = TRUE))

