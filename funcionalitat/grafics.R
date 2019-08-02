
source("funcionalitat/analisis mitjana per parcela.R")
source("funcionalitat/netejar dades polinitzadors.R")

require(devtools)
library(tidyverse)
library(ggpubr)
library(cowplot)
library(gridExtra)


# barplot visitation rate including taxonomic groups
datagroups <- read.table("dades/pollinator groups.txt",header=T) %>%
  select(Plot,Species,Pollinator_group,Abundance)%>%
  left_join(flors, by = c("Plot","Species")) %>%
  filter(.,Species =="ROF" | Species =="TVUF" | Species =="TVUH")%>%
  mutate(Visitation_rate = Abundance/Flower_Abundance*1000)%>%
  group_by(Species,Pollinator_group) %>% 
  summarise(Visitation_rate=mean(Visitation_rate)) %>%
  filter(.,Pollinator_group != "Mecoptera" & Pollinator_group != "Heteroptera")

a1 <- ggplot(datagroups, aes(fill=Pollinator_group, y=Visitation_rate, x=Species)) + 
  geom_bar(position="stack", stat="identity")+
  theme_classic()+
  theme(legend.position = "top")+ 
  scale_fill_manual("legend", values = c("Bee" = "red", "Coleoptera" = "blue", "Diptera" = "black", "Honeybees" = "pink", "Lepidoptera" = "green", "Wasp" = "brown"))

# Visitation rate total
a2 <- ggplot(meandataperplot, aes(y=Visitation_rate, x=Species))+
  geom_boxplot(aes(fill="blue"))+
  scale_fill_manual(values=c("grey"))+
  theme_classic()+
  theme(legend.position = "none")

# pollinator richness
b1 <- ggplot(meandataperplot, aes(y=Pollinator_richness, x=Species))+
  geom_boxplot(aes(fill="blue"))+
  scale_fill_manual(values=c("grey"))+
  theme_classic()+
  theme(legend.position = "none")

b2 <- ggplot(meandataperplot, aes(y=Functional_group_Rocka, x=Species))+
  geom_boxplot(aes(fill="blue"))+
  scale_fill_manual(values=c("grey"))+
  theme_classic()+
  theme(legend.position = "none")

# flowers with pollen
pollenpergraph <- pollenclean %>%
  mutate(Pollen_presence=if_else(Total>0,1,0)) %>%
  group_by(Plot, Species) %>% 
  summarise(Mean_pollen=mean(Total),Flowers_with_pollen=mean(Pollen_presence),
            Mean_Homospecific=mean(Homospecific),Mean_Heterospecific=mean(Heterospecific),
            Proporcio_Heterosp=(Mean_Heterospecific/Mean_pollen))

d <- ggplot(pollenpergraph, aes(y=Flowers_with_pollen, x=Species))+
  geom_boxplot(aes(fill="blue"))+
  scale_fill_manual(values=c("grey"))+
  theme_classic()+
  coord_cartesian(ylim = c(0, 1))+
  theme(legend.position = "none")

# Homospecific i heterospecific pollen
pollenboxplot <- pollenpergraph %>%
  mutate(Homosp = Mean_Homospecific) %>%
  mutate(Heterosp = Mean_Heterospecific) %>%
  select(Plot,Species,Homosp,Heterosp) %>%
  gather(Pollen,Pollen_grains,-c(Plot,Species))

e <- ggplot(data = pollenboxplot, aes(x=Species, y=Pollen_grains)) + 
  geom_boxplot(aes(fill=Pollen))+
  theme_classic()+
  theme(legend.position = "top")


# fruit set
fruits <- meandataperplot %>%
  filter(Species != "ROF")

f <- ggplot(data = fruits, aes(x=Species, y=Fruit_set)) + 
  geom_boxplot(aes(fill="blue"))+
  scale_fill_manual(values=c("grey"))+
  theme_classic()+
  coord_cartesian(ylim = c(0, 1))+
  theme(legend.position = "none")

# fecundity and seed set
fruitsseeds <- meandataperplot %>%
  filter(Species != "ROF")%>%
  select(Plot,Species,Fecundity,Seed_set) %>%
  gather(Variable,Seeds,-c(Plot,Species)) 

g <- ggplot(data = fruitsseeds, aes(x=Species, y=Seeds)) + 
  geom_boxplot(aes(fill=Variable))+
  theme_classic()+
  coord_cartesian(ylim = c(0, 4))+
  theme(legend.position = "top")

# final graph tot junt
polls <- ggarrange(a1, a2, b1, b2, ncol = 4, nrow = 1)
pollen <- ggarrange(d,e,f,g, ncol = 4, nrow = 1)

final <- ggarrange(polls,pollen,nrow=2)
final


# taxa de visites i polen depositat
p <- ggplot(data = meandataperplot, aes(x=Visitation_rate, y=Mean_pollen,color=Species)) + 
  geom_point()+
  facet_wrap(.~Species)+
  theme_classic()+
  theme(legend.position = "none")

q <- ggplot(data = meandataperplot, aes(x=Pollinator_richness, y=Mean_pollen,color=Species)) + 
  geom_point()+
  facet_wrap(.~Species)+
  theme_classic()+
  theme(legend.position = "none")

r <- ggplot(data = meandataperplot, aes(x=Functional_group_Rocka, y=Mean_pollen,color=Species)) + 
  geom_point()+
  facet_wrap(.~Species)+
  theme_classic()+
  theme(legend.position = "none")
