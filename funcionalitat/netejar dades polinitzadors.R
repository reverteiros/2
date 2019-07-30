

require(devtools)
library(tidyverse)
library(DataCombine)
library(vegan)
library(betapart)
source("funcionalitat/index xarxes.R")


censos <- read.table("dades/censos.txt",header=T)
names(censos) <- c("Plot","Pollinator","Species","Abundance")


############# flower abundance per plot
flors <- read.table("dades/flors quantitatiu separant thymus morfs.txt",header=T) %>%
  select(., ROF, TVUF, TVUH)%>%
  tidyr::gather(Species, "Flower_Abundance",1:3) 

flors$Plot = c(1:40)

# Pollinators 
pollinators <- droplevels(dplyr::filter(censos, Species == "ROF" | Species == "TVUF" | Species == "TVUH")) %>% 
  group_by(Plot, Species) %>% 
  summarise(Pollinator_abundance=sum(Abundance),Pollinator_richness=n_distinct(Pollinator))%>%
  complete(Species, Plot) %>%
  distinct() %>%
  left_join(flors, by = c("Plot","Species")) %>%
  mutate(Visitation_rate = Pollinator_abundance/Flower_Abundance*1000)



### grups taxonomics
grupstaxonomics <- read.table("dades/censos grups taxonomics.txt",header=T) %>%
  mutate(Taxonomic_group=Separing_honeybees) %>%
  select(Parcela,Taxonomic_group,Codi_planta,Frequencia) %>%
  group_by(Parcela,Taxonomic_group,Codi_planta) %>% 
  summarise(Abundance=sum(Frequencia)) %>%
  complete(Parcela,Codi_planta) %>%
  distinct() %>% 
  filter(.,Codi_planta =="ROF" | Codi_planta =="TVUF" | Codi_planta =="TVUH")


### grups funcionals rocka
grupsfuncionals <- read.table("dades/grups funcionals rocka.txt",header=T) %>%
  select(Parcela,Functional_group_Rocka,Codi_planta,Frequencia) %>%
  group_by(Parcela,Functional_group_Rocka,Codi_planta) %>% 
  summarise(Abundance=sum(Frequencia)) %>%
  complete(Parcela,Codi_planta) %>%
  distinct() %>% 
  filter(.,Codi_planta =="ROF" | Codi_planta =="TVUF" | Codi_planta =="TVUH")

## nombre grups funcionals
numerogrupsfuncionals <- read.table("dades/grups funcionals rocka.txt",header=T) %>%
  group_by(Parcela,Codi_planta) %>% 
  summarise(Functional_groups=n_distinct(Functional_group_Rocka)) %>%
  complete(Parcela,Codi_planta) %>%
  distinct() %>% 
  filter(.,Codi_planta =="ROF" | Codi_planta =="TVUF" | Codi_planta =="TVUH")
names(numerogrupsfuncionals) <- c("Plot","Species","Functional_group_Rocka")


###### join all datasets
datapollinatorsall <- pollinators %>%
  left_join(networkmetrics, by="Plot") %>%
  select(.,-Shannon_diversity) %>%
  left_join(numerogrupsfuncionals,by=c("Plot","Species"))


