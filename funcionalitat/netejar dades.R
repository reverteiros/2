
require(devtools)
library(tidyverse)

############# POllen
pollen<-read.table("dades/polen.txt",header=T)

names(pollen) <- c("Plot","Species","Plant","Flower","TVU","ROF","OTHERS","Total")

pollenwtNA <- droplevels(dplyr::filter(pollen, !is.na(TVU) & !is.na(ROF)& !is.na(OTHERS)& !is.na(Total)))

pollenperplot <- group_by(pollenwtNA, Plot) %>% 
  summarize(Samples=n(),Mean=mean(Total),SD=sd(Total),Max=max(Total))

pollenperspecies <- group_by(pollenwtNA, Plot, Species) %>% 
  summarize(Samples_pollen=n(),Mean_pollen=mean(Total),SD_pollen=sd(Total),Max_pollen=max(Total),Mean_TVU=mean(TVU),SD_TVU=sd(TVU),Max_TVU=max(TVU),Mean_ROF=mean(ROF),SD_ROF=sd(ROF),Max_ROF=max(ROF),Mean_OTHERS=mean(OTHERS),SD_OTHERS=sd(OTHERS),Max_OTHERS=max(OTHERS))%>%
  complete(Species, Plot) %>%
  distinct() 


############# Fruits and seeds

seedsraw<-read.table("dades/llavors i fruits.txt",header=T)

names(seedsraw) <- c("Plot","Species","Plant","Flower","Avorted","Ovule","Seed","Total")

seeds <- droplevels(dplyr::filter(seedsraw, !is.na(Avorted) & Total == 4)) %>% 
  mutate(Pollinated = Avorted + Seed) %>% 
  mutate(Fruits = if_else(Pollinated > 0, 1,0)) %>%
  group_by(Plot, Species) %>% 
  summarize(Samples_seeds=n(),Fruits=sum(Fruits),Pollination=(mean(Pollinated)))%>%
  mutate(Percent_Fruits=(Fruits/Samples_seeds)) %>%
  select(., -c(Fruits))


############## Seed viability and weight

seedsmeasured <- read.table("dades/llavors pesades i no pesades.txt",header=T)
names(seedsmeasured) <- c("Plot","Pesades","No_pesades","Species")
seedsmeasured <- seedsmeasured %>% mutate(TOTAL_llavors=Pesades+No_pesades)


seedweight <- read.table("dades/pesos llavors.txt",header=T)

seedweightperplot <- seedweight %>%
  filter(!is.na(Embryo)) %>%
  mutate(Embryo_Numeric=if_else(Embryo=="morta",0,1)) %>%
  group_by(Species,Plot) %>% 
  summarize(Mean_weigth=mean(Weight),SD_weight=sd(Weight),Percent_embryo=mean(Embryo_Numeric)) %>%
  complete(Species, Plot) %>%
  distinct() %>%
  dplyr::left_join(., seedsmeasured, by = c("Species","Plot")) %>%
  select(., -c(Pesades,No_pesades))


 ############# JOIN ALL DATASETS

datafunction <- pollenperspecies %>%
  dplyr::left_join(., seedweightperplot, by = c("Species","Plot")) %>%
  dplyr::left_join(., seeds, by = c("Species","Plot")) 
  