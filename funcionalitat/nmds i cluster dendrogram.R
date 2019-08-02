
source("funcionalitat/netejar dades polinitzadors.R")

library(vegan)
library(pvclust)

## No volem que la funcio ens transformi les dades, aixo es dolent.
## El nombre de dimensions (k) ha de ser baix, idealment 2 o 3. Volem
## que la funcio arribi a una solucio, si no arriba podem pujar el
## nombre de dimensions. En qualsevol cas, perque ens en fiem de la 
## solucio, ha de tenir un stress menor de 0.2

set.seed(2)

## grups funcionals
flowerabundance <- read.table("dades/flors quantitatiu separant thymus morfs.txt",header=T) %>%
  select(., TVUH) %>%
  mutate(Plot=c(1:40)) %>%
  mutate(Flower_Abundance = TVUH)
taulacomposicioFG <- grupsfuncionals %>%
  filter(.,Species =="TVUH") %>%
  spread(Functional_group_Rocka, Abundance) %>%
  select(-c(Bugs,Others))%>%
  left_join(flowerabundance,by=c("Plot"))%>%
  mutate(Beeflies_VR = (Beeflies*1000/(3*Flower_Abundance))) %>%
  mutate(Beetles_VR = (Beetles*1000/(3*Flower_Abundance))) %>%
  mutate(Butterfly_VR = (Butterfly*1000/(3*Flower_Abundance))) %>%
  mutate(Hoverflies_VR = (Hoverflies*1000/(3*Flower_Abundance))) %>%
  mutate(Large_flies_VR = (Large_flies*1000/(3*Flower_Abundance))) %>%
  mutate(Large_wasps_VR = (Large_wasps*1000/(3*Flower_Abundance))) %>%
  mutate(Long_tongued_large_bees_VR = (Long_tongued_large_bees*1000/(3*Flower_Abundance))) %>%
  mutate(Moth_VR = (Moth*1000/(3*Flower_Abundance))) %>%
  mutate(Short_tongued_large_bees_VR = (Short_tongued_large_bees*1000/(3*Flower_Abundance))) %>%
  mutate(Short_tongued_medium_sized_bees_VR = (Short_tongued_medium_sized_bees*1000/(3*Flower_Abundance))) %>%
  mutate(Short_tongued_small_bees_VR = (Short_tongued_small_bees*1000/(3*Flower_Abundance))) %>%
  mutate(Small_flies_VR = (Small_flies*1000/(3*Flower_Abundance))) %>%
  mutate(Small_wasps_VR = (Small_wasps*1000/(3*Flower_Abundance)))  %>%
  select(-c(Beeflies,Beetles,Butterfly,Hoverflies,Large_flies,Large_wasps,Long_tongued_large_bees,Moth,Short_tongued_large_bees,Short_tongued_medium_sized_bees,Short_tongued_small_bees,Small_flies,Small_wasps,Flower_Abundance,TVUH))
  
taulacomposicioFG[is.na(taulacomposicioFG)] <- 0
taulacomposicioFG <- as.data.frame(taulacomposicioFG)
taulacomposicioFG <- taulacomposicioFG[-c(7,18,23,25),-(1:2)]

senseapis_NMDS <- metaMDS(taulacomposicioFG, k=3,trymax=100,autotransform = F)
senseapis_NMDS$stress
stressplot(senseapis_NMDS)
senseapisscores <- scores(senseapis_NMDS, display=c("species"))


ordiplot(senseapis_NMDS,type="n")
orditorp(senseapis_NMDS,display="sites",cex=1.25,air=0.01)
orditorp(senseapis_NMDS,display="species",cex=1.25,air=0.01)


## grups taxonomics
flowerabundance <- read.table("dades/flors quantitatiu separant thymus morfs.txt",header=T) %>%
  select(., TVUH) %>%
  mutate(Plot=c(1:40)) %>%
  mutate(Flower_Abundance = TVUH)
taulacomposicioFG <- grupstaxonomicsspread %>%
  filter(.,Species =="TVUH") %>%
  left_join(flowerabundance,by=c("Plot"))%>%
  mutate(Bee_VR = (Bee*1000/(3*Flower_Abundance))) %>%
  mutate(Coleoptera_VR = (Coleoptera*1000/(3*Flower_Abundance))) %>%
  mutate(Diptera_VR = (Diptera*1000/(3*Flower_Abundance))) %>%
  mutate(Lepidoptera_VR = (Lepidoptera*1000/(3*Flower_Abundance))) %>%
  mutate(Wasp_VR = (Wasp*1000/(3*Flower_Abundance))) %>%
  mutate(Honeybees_VR = (Honeybees*1000/(3*Flower_Abundance))) %>%
  select(-c(Bee,Coleoptera,Diptera,Honeybees,Lepidoptera,Wasp,TVUH,Flower_Abundance))
taulacomposicioFG[is.na(taulacomposicioFG)] <- 0
taulacomposicioFG <- as.data.frame(taulacomposicioFG)
taulacomposicioFG <- taulacomposicioFG[-c(7,18,23,25),-(1:2)]

senseapis_NMDS <- metaMDS(taulacomposicioFG, k=3,trymax=100,autotransform = F)
senseapis_NMDS$stress


ordiplot(senseapis_NMDS,type="n")
orditorp(senseapis_NMDS,display="sites",cex=1.25,air=0.01)
orditorp(senseapis_NMDS,display="species",cex=1.25,air=0.01)



# Ward Hierarchical Clustering with Bootstrapped p values

ttaulacomposicioFG<-t(taulacomposicioFG)

fit <- pvclust(ttaulacomposicioFG, method.hclust="ward.D",method.dist="euclidean")
plot(fit) 
pvrect(fit, alpha=.95)



