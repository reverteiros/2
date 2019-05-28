
require(devtools)
library(tidyverse)
library(DataCombine)
library(vegan)


eficients <- read.table("dades/bitxos eficients-no.txt",header=T) %>%
  select(Parcela,eficient,Codi_planta,Frequencia)%>%
  group_by(Parcela,eficient,Codi_planta) %>% 
  summarise(Abundance=sum(Frequencia)) %>%
  complete(Parcela,Codi_planta) %>%
  distinct() %>% 
  filter(.,Codi_planta =="ROF" | Codi_planta =="TVUF" | Codi_planta =="TVUH")%>%
  spread(eficient,Abundance)%>%
  left_join(flors, by = c("Plot","Species")) %>%
  mutate(Visitation_rate = Pollinator_abundance/Flower_Abundance*1000)

eficients[is.na(eficients)] <- 0
names(eficients) <- c("Plot","Species","Efficient","Non_Efficient")

