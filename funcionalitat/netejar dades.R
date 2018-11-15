
require(devtools)
library(tidyverse)

############# POllen
pollen<-read.table("dades/polen.txt",header=T)

pollenwtNA <- droplevels(dplyr::filter(pollen, !is.na(TVU) & !is.na(ROF)& !is.na(OTHERS)& !is.na(TOTAL)))

pollenperplot <- group_by(pollenwtNA, Plot) %>% 
  summarize(Samples=n(),Mean=mean(TOTAL),SD=sd(TOTAL),Max=max(TOTAL))

pollenperspecies <- group_by(pollenwtNA, Plot, Species) %>% 
  summarize(Samples=n(),Mean=mean(TOTAL),SD=sd(TOTAL),Max=max(TOTAL),Mean_TVU=mean(TVU),SD_TVU=sd(TVU),Max_TVU=max(TVU),Mean_ROF=mean(ROF),SD_ROF=sd(ROF),Max_ROF=max(ROF),Mean_OTHERS=mean(OTHERS),SD_OTHERS=sd(OTHERS),Max_OTHERS=max(OTHERS))


############# Fruits and seeds

seedsraw<-read.table("dades/llavors i fruits.txt",header=T)

seeds <- droplevels(dplyr::filter(seedsraw, !is.na(Avorted) & Total == 4)) %>% mutate(Pollinated = Avorted + Seed) %>% mutate(Fruits = if_else(Pollinated > 0, 1,0))

seedsperplot <- group_by(seeds, Plot, Sp) %>% 
  summarize(Samples=n(),Fruits=sum(Fruits),Pollination=mean(Pollinated))

