
setwd("D:/Usuarios/s.reverte/Desktop/TESI/DADES")

dades<-read.table("Database.txt", header=T,dec=",")
names(dades)

a<-lm(dades$TVU_Flowers~dades$TVU_Flowers_percent)
summary(a)
plot(dades$Links~dades$Number.of.pollinator.species)
abline(a,add=T)

# Variables transformades 
dades$ROF_Flowers_sqrt<-sqrt(dades$ROF_Flowers)
dades$ROF_abundance_sqrt<-sqrt(dades$ROF_abundance)
dades$OVERALL_sqrt<-sqrt(dades$OVERALL)
dades$Abundance_sqrt<-sqrt(dades$Abundance)
dades$TVUF_abundance_sqrt<-sqrt(dades$TVUF_abundance)
dades$TVUH_Seeds_sqrt<-sqrt(dades$TVUH_Seeds)
dades$TVUF_Seeds_sqrt<-sqrt(dades$TVUF_Seeds)
dades$TVUF_Seed_set_mean_sqrt<-sqrt(dades$TVUF_Seed_set_mean)
dades$TVUF_Homospecific_mean_sqrt<-sqrt(dades$TVUF_Homospecific_mean)
dades$TVUH_Homospecific_mean_sqrt<-sqrt(dades$TVUH_Homospecific_mean)
dades$TVUF_Total_pollen_sqrt<-sqrt(dades$TVUF_Total_pollen)
dades$TVUH_Total_pollen_sqrt<-sqrt(dades$TVUH_Total_pollen)


library(gplots)

fit <- aov(Fruit_set_mean~ Species, data=dades)
summary(fit)
plotmeans(dades$Fruit_set_mean~dades$Species)


par(mfrow=c(2, 2))
plot(a)
