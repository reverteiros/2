

setwd("D:/Usuarios/s.reverte/OneDrive - CREAF/TESI/DADES/dades")
muminn  <- read.table("mumin.txt",header=T)
database2 <- read.table("Database2.txt",header=T)
muminn$Flower_richness <- database2$flower_richness

names(database2)

d <- data.frame(database2$Honeybees,database2$ROF_Flowers,database2$TVU_Flowers,database2$flower_richness)


M <- cor(d) # get correlations

cor.test(database2$Honeybees,database2$OVERALL_Flowers)
library('corrplot')
corrplot(M, method = "circle")
