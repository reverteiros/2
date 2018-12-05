
require(devtools)
library(tidyverse)

############# POllen
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
  mutate(Pollen=if_else(Total>0,1,0)) %>%
  mutate(Homospecific = ROF) %>%
  mutate(Heterospecific = TVU+OTHERS)

TVUFpollen <- filter(pollenwtNA, Species == "TVUF") %>%
  mutate(Pollen=if_else(Total>0,1,0)) %>%
  mutate(Homospecific = TVU) %>%
  mutate(Heterospecific = ROF+OTHERS)
  
TVUHpollen <- filter(pollenwtNA, Species == "TVUH") %>%
  mutate(Pollen=if_else(Total>0,1,0)) %>%
  mutate(Homospecific = TVU) %>%
  mutate(Heterospecific = ROF+OTHERS)

pollentotal <- bind_rows(ROFpollen, TVUFpollen, TVUHpollen)

# generate final pollen dataset
pollen <- group_by(pollentotal, Plot, Species) %>% 
  summarize(Samples_pollen=n(),Percent_pollinated=mean(Pollen),Mean_pollen=mean(Total),
            SD_pollen=sd(Total),Max_pollen=max(Total),Mean_Homospecific=mean(Homospecific),
            SD_Homospecific=sd(Homospecific),Max_Homospecific=max(Homospecific),
            Mean_Heterospecific=mean(Heterospecific),SD_Heterospecific=sd(Heterospecific),
            Max_Heterospecific=max(Heterospecific))%>%
  complete(Species, Plot) %>%
  distinct() 


############## Seed viability and weight

# seedsmeasured <- read.table("dades/llavors pesades i no pesades.txt",header=T)
# names(seedsmeasured) <- c("Plot","Pesades","No_pesades","Species")
# seedsmeasured <- seedsmeasured %>% mutate(TOTAL_llavors=Pesades+No_pesades)

seedweightraw <- read.table("dades/pesos llavors.txt",header=T)

# mean and SD weight of seed weight
seedweight <- seedweightraw %>%
  filter(Embryo == "viable") %>%
  group_by(Species,Plot) %>% 
  summarize(Weighted_seeds=n(),Mean_weigth=mean(Weight),SD_weight=sd(Weight)) %>%
  complete(Species, Plot) %>%
  distinct() 

# from the weighted seeds, proportion of them that has viable embryo
seedweightandviability <- seedweightraw %>%
  filter(!is.na(Embryo)) %>%
  mutate(Embryo_Numeric=if_else(Embryo=="morta",0,1)) %>%
  group_by(Species,Plot) %>% 
  summarize(Percent_embryo=mean(Embryo_Numeric)) %>%
  complete(Species, Plot) %>%
  distinct() %>%
  dplyr::left_join(., seedweight, by = c("Species","Plot"))


############# Fruits and seeds

seedsraw<-read.table("dades/llavors i fruits.txt",header=T)

names(seedsraw) <- c("Plot","Species","Plant","Flower","Avorted","Ovule","Seed","Total")

# 
fruits <- droplevels(dplyr::filter(seedsraw, !is.na(Avorted) & Total == 4)) %>% 
  mutate(Pollinated = Avorted + Seed) %>% 
  mutate(Fruits = if_else(Seed > 0, 1,0)) %>%
  group_by(Plot, Species) %>% 
  summarize(Samples_seeds=n(),Fruits=sum(Fruits),Pollination=(mean(Pollinated)/4*100))%>%
  mutate(Fruit_set=(Fruits/Samples_seeds)) %>%
  select(., -c(Fruits)) 

fruitandseedset <- droplevels(dplyr::filter(seedsraw, !is.na(Avorted) & Total == 4)) %>% 
  mutate(Pollinated = Avorted + Seed) %>% 
  mutate(Fruits = if_else(Seed > 0, 1,0)) %>%
  filter(.,Fruits==1) %>%
  group_by(Plot, Species) %>% 
  summarize(Seed_set=mean(Seed))%>%
  left_join(fruits, by = c("Plot","Species"))


############# JOIN ALL DATASETS

datafunction <- pollen %>%
  dplyr::left_join(., seedweightandviability, by = c("Species","Plot")) %>%
  dplyr::left_join(., fruitandseedset, by = c("Species","Plot")) 

