
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

