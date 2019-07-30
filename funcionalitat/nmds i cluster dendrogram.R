
source("funcionalitat/netejar dades polinitzadors.R")

library(vegan)
library(pvclust)

## No volem que la funci? ens transformi les dades, aix? ?s dolent.
## El nombre de dimensions (k) ha de ser baix, idealment 2 o 3. Volem
## que la funci? arribi a una soluci?, si no arriba podem pujar el
## nombre de dimensions. En qualsevol cas, perqu? ens en fiem de la 
## soluci?, ha de tenir un stress menor de 0.2

set.seed(2)

taulacomposicioFG_ROF <- grupsfuncionals %>%
  filter(.,Codi_planta =="TVUF") %>%
  spread(Functional_group_Rocka, Abundance) %>%
  select(-c(Bugs,Others))
taulacomposicioFG_ROF[is.na(taulacomposicioFG_ROF)] <- 0
taulacomposicioFG_ROF <- as.data.frame(taulacomposicioFG_ROF)
taulacomposicioFG_ROF <- taulacomposicioFG_ROF[,-(1:2)]

senseapis_NMDS <- metaMDS(taulacomposicioFG_ROF, k=3,trymax=100,autotransform = F)
senseapis_NMDS$stress
senseapisscores <- scores(senseapis_NMDS, display=c("species"))


ordiplot(senseapis_NMDS,type="n")
orditorp(senseapis_NMDS,display="sites",cex=1.25,air=0.01)
orditorp(senseapis_NMDS,display="species",cex=1.25,air=0.01)



# Ward Hierarchical Clustering with Bootstrapped p values

tsenseapisscores<-t(senseapisscores)

fit <- pvclust(taulacomposicioFG_ROF, method.hclust="ward.D",method.dist="euclidean")
plot(fit) 
pvrect(fit, alpha=.95)



