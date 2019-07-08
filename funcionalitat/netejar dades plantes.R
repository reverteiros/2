
require(devtools)
library(tidyverse)
library(DataCombine)
library(vegan)

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
  summarise(Pollen_disponible = sum(Pollen_disponible)) %>%
  mutate(Species = "Others")

granspollen <- granspolenrof %>%
  bind_rows(.,granspolentvuh) %>%
  bind_rows(.,granspolenaltresflors)%>%
  spread(Species,Pollen_disponible) %>%
  mutate(ROF_pollen = ROF) %>%
  mutate(Other_pollen = Others) %>%
  mutate(TVU_pollen = TVUH) %>%
  select(-c(Others,ROF,TVUH))



############# Pollen estigmes ###############################################################################

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
  mutate(Heterospecific = TVU+OTHERS) %>%
  left_join(granspollen,by="Plot")

TVUFpollen <- filter(pollenwtNA, Species == "TVUF") %>%
  mutate(Homospecific = TVU) %>%
  mutate(Heterospecific = ROF+OTHERS)%>%
  left_join(granspollen,by="Plot")

TVUHpollen <- filter(pollenwtNA, Species == "TVUH") %>%
  mutate(Homospecific = TVU) %>%
  mutate(Heterospecific = ROF+OTHERS)%>%
  left_join(granspollen,by="Plot")

pollenclean <- bind_rows(ROFpollen,TVUFpollen,TVUHpollen)

