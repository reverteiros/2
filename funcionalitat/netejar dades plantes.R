
require(devtools)
library(tidyverse)
library(DataCombine)
library(vegan)

############# Pollen #######################################################################################
pollenraw<-read.table("dades/polen.txt",header=T)

names(pollenraw) <- c("Plot","Species","Plant","Flower","TVU","ROF","OTHERS","Total")

# recalculate total pollen column
pollenraw <- pollenraw %>%
  select(., -Total) %>%
  mutate(Total = (ROF + TVU + OTHERS))

# remove NAs in the dataset
pollenwtNA <- droplevels(dplyr::filter(pollenraw, !is.na(TVU) & !is.na(ROF)& !is.na(OTHERS)& !is.na(Total)))

# define homospecific and heterospecific pollen per species
ROFpollen <- filter(pollenwtNA, Species == "ROF") %>%
  mutate(Homospecific = ROF) %>%
  mutate(Heterospecific = TVU+OTHERS)

TVUFpollen <- filter(pollenwtNA, Species == "TVUF") %>%
  mutate(Homospecific = TVU) %>%
  mutate(Heterospecific = ROF+OTHERS)

TVUHpollen <- filter(pollenwtNA, Species == "TVUH") %>%
  mutate(Homospecific = TVU) %>%
  mutate(Heterospecific = ROF+OTHERS)

# pollentotal <- bind_rows(ROFpollen, TVUFpollen, TVUHpollen)


################### comunitat floral - quantitat de polen

granspolenrof<-read.table("dades/grans polen.txt",header=T) %>%
  gather(Species, "Pollen_disponible",2:25) %>%
  filter(Species == "ROF")

granspolentvuh<-read.table("dades/grans polen.txt",header=T) %>%
  gather(Species, "Pollen_disponible",2:25) %>%
  filter(Species == "TVUH")

granspolenaltresflors<-read.table("dades/grans polen.txt",header=T) %>%
  gather(Species, "Pollen_disponible",2:25) %>%
  filter(Species != "ROF" & Species != "TVUH") %>%
  group_by(Plot) %>%
  summarise(Grans_pollen = sum(Pollen_disponible))

granspollen <- diversitatTVUH %>%
  bind_rows(.,diversitatTVUF) %>%
  bind_rows(.,diversitatROF)


