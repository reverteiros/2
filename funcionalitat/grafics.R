
source("funcionalitat/netejar dades mitjana per parcela.R")

library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggplot2)
library(ggExtra)
library(corrplot)
library(Hmisc)


ggplot(meandataperplot, aes(x=Proportion_used)) +
  geom_histogram(alpha=0.3) +
  # geom_smooth()+
  theme_classic()+
  facet_grid(.~Species)


# barplot visitation rate including taxonomic groups
visitationrates <- pollinators %>%
  select(c(Species,Visitation_rate)) %>%
  group_by(Species) %>% 
  summarise(Visitation_rate=mean(Visitation_rate)) 

datapollinatorsall2 <- pollinators %>%
  left_join(networkmetrics, by="Plot") %>%
  left_join(grupstaxonomicsspread,by=c("Species","Plot")) %>%
  mutate(HB = Honeybees/Pollinator_abundance) %>%
  mutate(Bees = Bee/Pollinator_abundance) %>%
  mutate(Coleopt = Coleoptera/Pollinator_abundance) %>%
  mutate(Dipter = Diptera/Pollinator_abundance) %>%
  mutate(Lepidopt = Lepidoptera/Pollinator_abundance) %>%
  mutate(Wasps = Wasp/Pollinator_abundance) %>%
  select(c(Species,HB,Bees,Coleopt,Dipter,Lepidopt,Wasps)) %>%
  filter(Wasps > -1) %>%
  gather(Pollinator_group,"Proportion",3:8)%>%
  group_by(Species,Pollinator_group) %>% 
  summarise(Proportion=mean(Proportion)) %>%
  left_join(visitationrates,by="Species")%>%
  mutate(Proportion_rate = Proportion * Visitation_rate)


a <- ggplot(datapollinatorsall2, aes(fill=Pollinator_group, y=Proportion_rate, x=Species)) + 
  geom_bar(position="stack", stat="identity")+
  theme_classic()+
  theme(legend.position = "top")+ 
  scale_fill_manual("legend", values = c("Bees" = "red", "Coleopt" = "blue", "Dipter" = "black", "HB" = "pink", "Lepidopt" = "green", "Wasps" = "brown"))


colorthreespecies <- c("blue","green","yellow")
colortwospecies <- c("green","yellow")

# pollinator richness
b <- ggplot(meandataperplot, aes(y=Pollinator_richness, x=Species,fill=Species))+
  geom_boxplot()+
  # geom_point()+
  scale_fill_manual(values=colorthreespecies)+
  theme_classic()+
  theme(legend.position = "top")


# flowers with pollen and heterospecific
pollenboxplot <- meandataperplot %>%
  mutate(Total = Total_presence) %>%
  mutate(Heterospecific = Heterospecific_presence) %>%
  select(Plot,Species,Total,Heterospecific) %>%
  gather(Pollen,Pollen_presence,-c(Plot,Species))

c <- ggplot(data = pollenboxplot, aes(x=Pollen, y=Pollen_presence,fill=Species)) + 
  geom_boxplot(aes(fill=Species))+
  theme_classic()+
  coord_cartesian(ylim = c(0, 1))+
  scale_fill_manual(values=colorthreespecies)+
  theme(legend.position = "none")

# Mean pollen on stigmas
d <- ggplot(meandataperplot, aes(y=Mean_Total, x=Species,fill=Species))+
  geom_boxplot()+
  scale_fill_manual(values=colorthreespecies)+
  theme_classic()+
  # coord_cartesian(ylim = c(0, 1))+
  theme(legend.position = "none")

# fruit set
meandataperplotwtROF <- meandataperplot %>%
  filter(Species != "ROF")

e <- ggplot(data = meandataperplotwtROF, aes(x=Species, y=Fruit_set,fill=Species)) + 
  geom_boxplot()+
  scale_fill_manual(values=colortwospecies)+
  theme_classic()+
  coord_cartesian(ylim = c(0, 1))+
  theme(legend.position = "none")

# avorted and seed set
seedsboxplot <- meandataperplotwtROF %>%
  gather(Seed_type,Number,c(Seed_set,Avorted))

f <- ggplot(data = seedsboxplot, aes(x=Seed_type, y=Number, fill=Species)) + 
  geom_boxplot(aes(fill=Species))+
  theme_classic()+
  coord_cartesian(ylim = c(0, 4))+
  scale_fill_manual(values=colortwospecies)+
  theme(legend.position = "none")

# final graph tot junt
polls <- ggarrange(a, b, ncol = 2, nrow = 1)
pollen <- ggarrange(c,d, ncol = 2, nrow = 1)
seeds <- ggarrange(e,f, ncol = 2, nrow = 1)

final <- ggarrange(polls,pollen,seeds,nrow=3)
final









ROF <- meandataperplotROF %>%
  gather("Pollinator_variable","Pollinator_value",c(Proportion_HB,Proportion_Bee,Proportion_Diptera,Pollinator_richness,logVisitation_rate, Visitation_rate)) %>%
  gather("Fitness_variable","Fitness_value",c(Total_presence,Mean_Total))

ggplot(ROF, aes(x=Pollinator_value,y=Fitness_value)) +
  geom_point(alpha=0.3) +
  facet_grid(Fitness_variable~Pollinator_variable,scales = "free")


TVUF <- meandataperplotTVUF %>%
  gather("Pollinator_variable","Pollinator_value",c(Proportion_HB,Proportion_Bee,Proportion_Diptera,Pollinator_richness,Visitation_rate,ProporcioF)) %>%
  gather("Fitness_variable","Fitness_value",c(Total_presence,Mean_Total,Fruit_set,Seed_set))

ggplot(TVUF, aes(x=Pollinator_value,y=Fitness_value)) +
  geom_point(alpha=0.3) +
  facet_grid(Fitness_variable~Pollinator_variable,scales = "free")


Heterospecific <- meandataperplotTVUF %>%
  gather("Pollinator_variable","Pollinator_value",c(Proportion_HB,Proportion_Bee,Proportion_Diptera,Pollinator_richness,Visitation_rate,loggenerality,generality,Proportion_Heterosp_Community)) %>%
  gather("Fitness_variable","Fitness_value",c(Heterospecific_presence))

ggplot(Heterospecific, aes(x=Pollinator_value,y=Fitness_value)) +
  geom_point(alpha=0.3) +
  facet_grid(Fitness_variable~Pollinator_variable,scales = "free")


TVUH <- meandataperplotTVUH %>%
  gather("Pollinator_variable","Pollinator_value",c(Proportion_HB,Proportion_Bee,Proportion_Diptera,Pollinator_richness,logVisitation_rate,Visitation_rate,ProporcioF)) %>%
  gather("Fitness_variable","Fitness_value",c(Total_presence,Mean_Total,Fruit_set,Seed_set))

ggplot(TVUH, aes(x=Pollinator_value,y=Fitness_value)) +
  geom_point(alpha=0.3) +
  facet_grid(Fitness_variable~Pollinator_variable,scales = "free")







ggplot(meandataperplot, aes(x=Flower_Abundance,y=Pollinator_abundance)) +
  geom_point(alpha=0.3) +
  facet_grid(.~Species,scales = "free")




