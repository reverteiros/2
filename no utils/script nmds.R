
setwd("D:/Usuarios/s.reverte/OneDrive - CREAF/TESI/DADES/dades")

library(vegan)

## No volem que la funció ens transformi les dades, això és dolent.
## El nombre de dimensions (k) ha de ser baix, idealment 2 o 3. Volem
## que la funció arribi a una solució, si no arriba podem pujar el
## nombre de dimensions. En qualsevol cas, perquè ens en fiem de la 
## solució, ha de tenir un stress menor de 0.2

set.seed(2)

senseapis<-read.table("bitxos quantitatiu sense apis.txt",header=T)
senseapis_NMDS <- metaMDS(senseapis, k=3,trymax=100,autotransform = F)
senseapis_NMDS$stress

flors<-read.table("flors quantitatiu totes2.txt",header=T)
flors_NMDS <- metaMDS(flors, k=2,trymax=100,autotransform = F)
flors_NMDS$stress

florsscores <- as.data.frame(scores(flors_NMDS))
names(florsscores) <- c("Flors1","Flors2")
senseapisscores <- as.data.frame(scores(senseapis_NMDS))
florsscores$Bitx1 <- senseapisscores$NMDS1
florsscores$Bitx2 <- senseapisscores$NMDS2
florsscores$Bitx3 <- senseapisscores$NMDS3

ordiplot(senseapis_NMDS,type="n")
orditorp(flors_NMDS,display="sites",col="red",air=0.01)
orditorp(senseapis_NMDS,display="sites",cex=1.25,air=0.01)

library(MuMIn)
options(na.action = "na.fail")
fm1 <- lm(Bitx3 ~ Flors1+Flors2, data = florsscores)
dd <- dredge(fm1)
dd
summary(get.models(dd, 1)[[1]])
