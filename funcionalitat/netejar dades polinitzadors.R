

library(dplyr)
library(tidyr)


censos <- read.table("dades/censos.txt",header=T)
names(censos) <- c("Plot","Pollinator","Species","Abundance")


############# flower abundance per plot
flowers <- read.table("dades/flors quantitatiu separant thymus morfs.txt",header=T) %>%
  select(., TVUF, TVUH)%>%
  tidyr::gather(Species, "Flower_Abundance",1:2) %>%
  mutate(Flower_Abundance = Flower_Abundance*3)

flowers$Plot = c(1:40)


# Pollinators
pollinators <- censos %>% 
  group_by(Plot, Species) %>% 
  dplyr::summarize(Pollinator_abundance=sum(Abundance),Pollinator_richness=n_distinct(Pollinator))%>%
  complete(Species, Plot) %>%
  distinct() %>%
  left_join(flowers, by = c("Plot","Species")) %>%
  mutate(Visitation_rate = Pollinator_abundance*1000/Flower_Abundance) %>%
  filter(Species == "TVUF" | Species == "TVUH") 

pollinators[is.na(pollinators)] <- 0

### grups taxonomics
grupstaxonomics <- read.table("dades/censos grups taxonomics.txt",header=T) %>%
  select(Plot,Taxonomic_group,Species,Abundance) %>%
  group_by(Plot,Taxonomic_group,Species) %>% 
  dplyr::summarise(Abundance=sum(Abundance)) %>%
  complete(Plot,Species) %>%
  distinct() %>% 
  filter(., Species =="TVUF" | Species =="TVUH")

grupstaxonomicsspread <- grupstaxonomics %>%
  spread(Taxonomic_group, Abundance) 
grupstaxonomicsspread[is.na(grupstaxonomicsspread)] <- 0

## database total
datapollinatorsall <- pollinators %>%
  # left_join(networkmetricsTVU, by="Plot") %>%
  left_join(grupstaxonomicsspread,by=c("Species","Plot")) %>%
  mutate(Proportion_HB = Honeybees/Pollinator_abundance) %>%
  mutate(Proportion_Bee = Bee/Pollinator_abundance) %>%
  mutate(Proportion_Coleoptera = Coleoptera/Pollinator_abundance) %>%
  mutate(Proportion_Diptera = Diptera/Pollinator_abundance) %>%
  mutate(Proportion_Lepidoptera = Lepidoptera/Pollinator_abundance) %>%
  mutate(Proportion_Wasps = Wasp/Pollinator_abundance) %>%
  select(-c(Bee,Coleoptera,Diptera,Honeybees,Wasp,Lepidoptera,Heteroptera)) %>%
  filter(Flower_Abundance > 0) 



