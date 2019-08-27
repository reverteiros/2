
source("funcionalitat/netejar dades mitjana per parcela.R")

library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggplot2)
library(ggExtra)
library(corrplot)
library(Hmisc)


ROF <- meandataperplot %>%
  filter(Species=="ROF")%>%
  gather("Pollinator_variable","Pollinator_value",c(Proportion_HB,Proportion_Bee,Proportion_Diptera,Pollinator_richness,Visitation_rate)) %>%
  gather("Fitness_variable","Fitness_value",c(Total_presence,Mean_Total))

TVUF <- meandataperplot %>%
  filter(Species=="TVUF")%>%
  gather("Pollinator_variable","Pollinator_value",c(Proportion_HB,Proportion_Bee,Proportion_Diptera,Pollinator_richness,Visitation_rate,ProporcioF)) %>%
  gather("Fitness_variable","Fitness_value",c(Total_presence,Mean_Total,Fruit_set,Seed_set,Avorted))

Heterospecific <- meandataperplot %>%
  filter(Species=="TVUF")%>%
  gather("Pollinator_variable","Pollinator_value",c(Proportion_HB,Proportion_Bee,Proportion_Diptera,Pollinator_richness,Visitation_rate,ProporcioF,generality,Proportion_Heterosp_Community)) %>%
  gather("Fitness_variable","Fitness_value",c(Heterospecific_presence,Proportion_Heterosp_Stigma))

TVUH <- meandataperplot %>%
  filter(Species=="TVUH")%>%
  gather("Pollinator_variable","Pollinator_value",c(Proportion_HB,Proportion_Bee,Proportion_Diptera,Pollinator_richness,Visitation_rate,ProporcioF)) %>%
  gather("Fitness_variable","Fitness_value",c(Total_presence,Mean_Total,Fruit_set,Seed_set,Avorted))

ggplot(TVUH, aes(x=Pollinator_value,y=Fitness_value)) +
  geom_point(alpha=0.3) +
  # geom_smooth()+
  # theme_classic()+
  facet_grid(Fitness_variable~Pollinator_variable,scales = "free")








# barplot visitation rate including taxonomic groups
datagroups <- read.table("dades/pollinator groups.txt",header=T) %>%
  select(Plot,Species,Pollinator_group,Abundance)%>%
  left_join(flowers, by = c("Plot","Species")) %>%
  filter(.,Species =="ROF" | Species =="TVUF" | Species =="TVUH")%>%
  mutate(Visitation_rate = Abundance/Flower_Abundance*1000)%>%
  group_by(Species,Pollinator_group) %>% 
  summarise(Visitation_rate=mean(Visitation_rate)) %>%
  filter(.,Pollinator_group != "Mecoptera" & Pollinator_group != "Heteroptera")

a <- ggplot(datagroups, aes(fill=Pollinator_group, y=Visitation_rate, x=Species)) + 
  geom_bar(position="stack", stat="identity")+
  theme_classic()+
  theme(legend.position = "top")+ 
  scale_fill_manual("legend", values = c("Bee" = "red", "Coleoptera" = "blue", "Diptera" = "black", "Honeybees" = "pink", "Lepidoptera" = "green", "Wasp" = "brown"))


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




