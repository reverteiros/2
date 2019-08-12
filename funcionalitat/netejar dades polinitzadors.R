

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


### grups funcionals rocka
grupsfuncionals <- read.table("dades/grups funcionals rocka.txt",header=T) %>%
  select(Plot,Functional_group_Rocka,Species,Abundance) %>%
  group_by(Plot,Functional_group_Rocka,Species) %>% 
  summarise(Abundance=sum(Abundance)) %>%
  complete(Plot,Species) %>%
  distinct() %>% 
  filter(.,Species =="ROF" | Species =="TVUF" | Species =="TVUH")

## nombre grups funcionals
numerogrupsfuncionals <- read.table("dades/grups funcionals rocka.txt",header=T) %>%
  group_by(Plot,Species) %>% 
  summarise(Functional_groups=n_distinct(Functional_group_Rocka)) %>%
  complete(Plot,Species) %>%
  distinct() %>% 
  filter(.,Species =="ROF" | Species =="TVUF" | Species =="TVUH")
names(numerogrupsfuncionals) <- c("Plot","Species","Functional_group_Rocka")


###### join all datasets
datapollinatorsall <- pollinators %>%
  left_join(networkmetrics, by="Plot") %>%
  select(.,-Shannon_diversity) %>%
  left_join(numerogrupsfuncionals,by=c("Plot","Species"))


