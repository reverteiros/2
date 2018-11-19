
require(devtools)
library(tidyverse)

############# POllen
pollen<-read.table("dades/polen.txt",header=T)

names(pollen) <- c("Plot","Species","Plant","Flower","TVU","ROF","OTHERS","Total")

pollen <- pollen %>%
  select(., -Total) %>%
  mutate(Total = (ROF + TVU + OTHERS))

pollenwtNA <- droplevels(dplyr::filter(pollen, !is.na(TVU) & !is.na(ROF)& !is.na(OTHERS)& !is.na(Total)))

ROFpollen <- filter(pollenwtNA, Species == "ROF") %>%
  mutate(Homospecific = ROF) %>%
  mutate(Heterospecific = TVU+OTHERS)

TVUFpollen <- filter(pollenwtNA, Species == "TVUF") %>%
  mutate(Homospecific = TVU) %>%
  mutate(Heterospecific = ROF+OTHERS)
  
TVUHpollen <- filter(pollenwtNA, Species == "TVUH") %>%
  mutate(Homospecific = TVU) %>%
  mutate(Heterospecific = ROF+OTHERS)

pollentotal <- bind_rows(ROFpollen, TVUFpollen, TVUHpollen)

pollenperspecies <- group_by(pollentotal, Plot, Species) %>% 
  summarize(Samples_pollen=n(),Mean_pollen=mean(Total),SD_pollen=sd(Total),Max_pollen=max(Total),Mean_Homospecific=mean(Homospecific),SD_Homospecific=sd(Homospecific),Max_Homospecific=max(Homospecific),Mean_Heterospecific=mean(Heterospecific),SD_Heterospecific=sd(Heterospecific),Max_Heterospecific=max(Heterospecific))%>%
  complete(Species, Plot) %>%
  distinct() 


############## Seed viability and weight

# seedsmeasured <- read.table("dades/llavors pesades i no pesades.txt",header=T)
# names(seedsmeasured) <- c("Plot","Pesades","No_pesades","Species")
# seedsmeasured <- seedsmeasured %>% mutate(TOTAL_llavors=Pesades+No_pesades)

seedweight <- read.table("dades/pesos llavors.txt",header=T)

seedviabilityperplot <- seedweight %>%
  filter(!is.na(Embryo)) %>%
  mutate(Embryo_Numeric=if_else(Embryo=="morta",0,1)) %>%
  group_by(Species,Plot) %>% 
  summarize(Percent_embryo=mean(Embryo_Numeric)) %>%
  complete(Species, Plot) %>%
  distinct() # %>%
# dplyr::left_join(., seedsmeasured, by = c("Species","Plot")) %>%
# select(., -c(Pesades,No_pesades))

seedweightperplot <- seedweight %>%
  filter(Embryo == "viable") %>%
  group_by(Species,Plot) %>% 
  summarize(Mean_weigth=mean(Weight),SD_weight=sd(Weight)) %>%
  complete(Species, Plot) %>%
  distinct() %>%
  dplyr::left_join(., seedviabilityperplot, by = c("Species","Plot"))


############# Fruits and seeds

seedsraw<-read.table("dades/llavors i fruits.txt",header=T)

names(seedsraw) <- c("Plot","Species","Plant","Flower","Avorted","Ovule","Seed","Total")

seedset <- droplevels(dplyr::filter(seedsraw, !is.na(Avorted) & Total == 4)) %>% 
  mutate(Pollinated = Avorted + Seed) %>% 
  mutate(Fruits = if_else(Pollinated > 0, 1,0)) %>%
  mutate(Seed_set = (Seed/Fruits)) %>%
  filter(Seed_set > 0) %>%
  group_by(Plot, Species) %>% 
  summarize(Seed_set_raw=mean(Seed_set))

seeds <- droplevels(dplyr::filter(seedsraw, !is.na(Avorted) & Total == 4)) %>% 
  mutate(Pollinated = Avorted + Seed) %>% 
  mutate(Fruits = if_else(Pollinated > 0, 1,0)) %>%
  group_by(Plot, Species) %>% 
  summarize(Samples_seeds=n(),Fruits=sum(Fruits),Pollination=(mean(Pollinated)))%>%
  mutate(Fruit_set=(Fruits/Samples_seeds)) %>%
  select(., -c(Fruits)) %>%
  left_join(seedset, by = c("Plot","Species"))


 ############# JOIN ALL DATASETS

datafunction <- pollenperspecies %>%
  dplyr::left_join(., seedweightperplot, by = c("Species","Plot")) %>%
  dplyr::left_join(., seeds, by = c("Species","Plot")) %>%
  mutate(Seed_set = Seed_set_raw*Percent_embryo) %>%
  select(., -c(Seed_set_raw))

  