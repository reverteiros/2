
require(devtools)
library(tidyverse)

############# Read pollen database
pollen<-read.table("dades/polen.txt",header=T)

pollenwtNA <- droplevels(dplyr::filter(pollen, !is.na(TVU) & !is.na(ROF)& !is.na(OTHERS)& !is.na(TOTAL)))

pollenperplot <- group_by(pollenwtNA, Plot) %>% 
  summarize(Samples=n(),Mean=mean(TOTAL),SD=sd(TOTAL),Max=max(TOTAL))

pollenperspecies <- group_by(pollenwtNA, Plot, Species) %>% 
  summarize(Samples=n(),Mean=mean(TOTAL),SD=sd(TOTAL),Max=max(TOTAL),Mean_TVU=mean(TVU),SD_TVU=sd(TVU),Max_TVU=max(TVU),Mean_ROF=mean(ROF),SD_ROF=sd(ROF),Max_ROF=max(ROF),Mean_OTHERS=mean(OTHERS),SD_OTHERS=sd(OTHERS),Max_OTHERS=max(OTHERS))
