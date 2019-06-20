
require(devtools)
library(tidyverse)

florspertransecte <- read.table("dades/flors per transecte.txt",header=T) %>%
  group_by(Plot, Transect, Species) %>%
  summarise(Flower_abundance=sum(Flors))


censospertransecte <- read.table("dades/censos per transecte.txt",header=T) %>%
  filter(.,Species == "ROF" | Species == "TVUF" | Species == "TVUH") %>%
  group_by(Plot, Transect,Species) %>%
  summarise(Pollinator_abundance=sum(Abundance),Pollinator_richness=n_distinct(Pollinator)) %>%
  full_join(florspertransecte,by=c("Plot","Transect","Species")) %>%
  mutate(Visitation_rate = Pollinator_abundance/Flower_abundance*1000) %>%
  mutate(SP_1000_FL = Pollinator_richness/Flower_abundance*1000) %>%
  complete(Species,nesting(Transect,Plot), fill = list(Pollinator_abundance = 0)) %>%
  distinct() %>%
  filter(.,Flower_abundance > 1) 
censospertransecte[is.na(censospertransecte)] <- 0

ggplot(censospertransecte) +
  geom_point(aes(Plot,Visitation_rate,colour=Species)) +
  # theme_classic() +
  facet_grid(Species ~ .,scales = "free")

ggplot(censospertransecte) +
  geom_point(aes(Plot,SP_1000_FL,colour=Species)) +
  # theme_classic() +
  facet_grid(Species ~ .,scales = "free")







tvuf <- read.table("dades/censos per transecte.txt",header=T) %>%
  filter(.,Species == "TVUF") %>%
  group_by(Plot, Transect,Species) %>%
  summarise(Pollinator_abundance=sum(Abundance)) %>%
  left_join(florspertransecte,by=c("Plot","Transect","Species")) %>%
  mutate(Visitation_rate = Pollinator_abundance/Flower_abundance*1000) %>%
  complete(Species,nesting(Transect,Plot), fill = list(Pollinator_abundance = 0)) %>%
  distinct() %>%
  filter(.,Flower_abundance > 1) %>%
  group_by(Plot, Species) %>%
  summarise(Mean_VR = mean(Visitation_rate),Max_VR=max(Visitation_rate),Min_VR = min(Visitation_rate)) %>%
  mutate(Range = Max_VR/Min_VR)

rof <- read.table("dades/censos per transecte.txt",header=T) %>%
  filter(.,Species == "ROF") %>%
  group_by(Plot, Transect,Species) %>%
  summarise(Pollinator_abundance=sum(Abundance)) %>%
  left_join(florspertransecte,by=c("Plot","Transect","Species")) %>%
  mutate(Visitation_rate = Pollinator_abundance/Flower_abundance*1000) %>%
  complete(Species,nesting(Transect,Plot), fill = list(Pollinator_abundance = 0)) %>%
  distinct() %>%
  filter(.,Flower_abundance > 1) %>%
  group_by(Plot, Species) %>%
  summarise(Mean_VR = mean(Visitation_rate),Max_VR=max(Visitation_rate),Min_VR = min(Visitation_rate)) %>%
  mutate(Range = Max_VR/Min_VR)

tvuh <- read.table("dades/censos per transecte.txt",header=T) %>%
  filter(.,Species == "TVUH") %>%
  group_by(Plot, Transect,Species) %>%
  summarise(Pollinator_abundance=sum(Abundance)) %>%
  left_join(florspertransecte,by=c("Plot","Transect","Species")) %>%
  mutate(Visitation_rate = Pollinator_abundance/Flower_abundance*1000) %>%
  complete(Species,nesting(Transect,Plot), fill = list(Pollinator_abundance = 0)) %>%
  distinct() %>%
  filter(.,Flower_abundance > 1) %>%
  group_by(Plot, Species) %>%
  summarise(Mean_VR = mean(Visitation_rate),Max_VR=max(Visitation_rate),Min_VR = min(Visitation_rate),n=n()) %>%
  mutate(Range = Max_VR/Min_VR)
