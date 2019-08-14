

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
  tidyr::gather(Species, "Flower_Abundance",1:3) %>%
  mutate(Flower_Abundance = Flower_Abundance*3)

flors$Plot = c(1:40)

# Pollinators 
pollinators <- droplevels(dplyr::filter(censos, Species == "ROF" | Species == "TVUF" | Species == "TVUH")) %>% 
  group_by(Plot, Species) %>% 
  summarise(Pollinator_abundance=sum(Abundance),Pollinator_richness=n_distinct(Pollinator))%>%
  complete(Species, Plot) %>%
  distinct() %>%
  left_join(flors, by = c("Plot","Species")) %>%
  mutate(Visitation_rate = Pollinator_abundance*1000/Flower_Abundance)



### grups taxonomics
grupstaxonomics <- read.table("dades/censos grups taxonomics.txt",header=T) %>%
  select(Plot,Taxonomic_group,Species,Abundance) %>%
  group_by(Plot,Taxonomic_group,Species) %>% 
  summarise(Abundance=sum(Abundance)) %>%
  complete(Plot,Species) %>%
  distinct() %>% 
  filter(.,Species =="ROF" | Species =="TVUF" | Species =="TVUH")

grupstaxonomicsspread <- grupstaxonomics %>%
  spread(Taxonomic_group, Abundance) 
grupstaxonomicsspread[is.na(grupstaxonomicsspread)] <- 0


## database total
datapollinatorsall <- pollinators %>%
  left_join(networkmetrics, by="Plot") %>%
  mutate(logVisitation_rate = log(Visitation_rate))%>%
  mutate(logPollinator_richness = log(Pollinator_richness))%>%
  left_join(grupstaxonomicsspread,by=c("Species","Plot")) %>%
  mutate(Bee_VR = (Bee*1000/(Flower_Abundance))) %>%
  mutate(Coleoptera_VR = (Coleoptera*1000/(Flower_Abundance))) %>%
  mutate(Diptera_VR = (Diptera*1000/(Flower_Abundance))) %>%
  mutate(Lepidoptera_VR = (Lepidoptera*1000/(Flower_Abundance))) %>%
  mutate(Wasp_VR = (Wasp*1000/(Flower_Abundance))) %>%
  mutate(Honeybees_VR = (Honeybees*1000/(Flower_Abundance))) %>%
  mutate(Proportion_HB = Honeybees_VR/Visitation_rate) %>%
  mutate(Proportion_Bee = Bee_VR/Visitation_rate) %>%
  mutate(Proportion_Coleoptera = Coleoptera_VR/Visitation_rate) %>%
  mutate(Proportion_Diptera = Diptera_VR/Visitation_rate) %>%
  mutate(Proportion_Lepidoptera = Lepidoptera_VR/Visitation_rate) %>%
  mutate(Proportion_Wasp = Wasp_VR/Visitation_rate) 


