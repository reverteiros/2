

require(devtools)
library(tidyverse)


# flower abundance per plot
flors <- read.table("dades/flors quantitatiu separant thymus morfs.txt",header=T) %>%
  select(., ROF, TVUF, TVUH)%>%
  tidyr::gather(Species, "Flower_Abundance",1:3) 
flors$Plot = c(1:40)


datagroups <- read.table("dades/pollinator groups.txt",header=T) %>%
  select(Plot,Species,Pollinator_group,Abundance)%>%
  left_join(flors, by = c("Plot","Species")) %>%
  filter(.,Species =="ROF" | Species =="TVUF" | Species =="TVUH")%>%
  mutate(Visitation_rate = Abundance/Flower_Abundance*1000)%>%
  group_by(Species,Pollinator_group) %>% 
  summarise(Visitation_rate=mean(Visitation_rate)) %>%
  filter(.,Pollinator_group != "Mecoptera" & Pollinator_group != "Heteroptera")

ggplot(datagroups, aes(fill=Pollinator_group, y=Visitation_rate, x=Species)) + 
  geom_bar(position="stack", stat="identity")
