
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



## logit regression presencia polen - taxa de visites

ggplot(data = TVUFpollenbitxos, aes(x=Visitation_rate, y=Homospecific_presence)) + 
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  theme_classic()


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



source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/analisis mitjana per parcela.R")

library(ggplot2)
library(ggExtra)
library(corrplot)
library(Hmisc)
library("PerformanceAnalytics")
library(vegan)
library(betapart)


## ggplots normals
ggplot(datanalysis) +
  geom_point(aes(Visitation_rate,Mean_pollen,colour=Species)) +
  geom_smooth(aes(Visitation_rate,Mean_pollen))+
  theme_classic() #+
# coord_cartesian(xlim = c(0, 32), ylim = c(0, 32))

qplot(TVUFpollenbitxos$Heterospecific,
      geom="histogram",
      binwidth = 0.5,  
      xlim=c(-1, 80))


## boxplots
ggplot(dataclean, aes(y=Ratio_Heterosp_Homosp, x=Species)) +
  geom_boxplot() +
  theme_classic()

pollenpergraph <- pollenclean %>%
  mutate(Pollen_presence=if_else(Total>0,1,0)) %>%
  group_by(Plot, Species) %>% 
  summarise(Flowers_with_pollen=mean(Pollen_presence))

## Corrplot
ROF <- meandataperplot %>%
  left_join(pollenpergraph,by=c("Plot","Species"))%>%
  filter(Species=="ROF") %>%
  select(-c(Fecundity,Fruit_set,Pollinated_ovules,Avorted_total,Seed_set,Avorted_per_fruit,ProporcioF,logVisitation_rate,logMean_pollen,logFunctional_group_Rocka,logPollinator_richness,Pollinator_abundance,Flower_Abundance,Mean_pollen))
ROF <- as.data.frame(ROF) %>%
  select(.,-c(Plot,Species))
ROF <- ROF[-29,]

res2<-rcorr(as.matrix(ROF))
corrplot(res2$r, type="upper", order="hclust",
         p.mat = res2$P, sig.level = 0.05, insig = "blank")


## Taula correlacions
chart.Correlation(ROF, histogram=TRUE, pch=19)


## Corrplot
TVUF <- meandataperplot %>%
  left_join(pollenpergraph,by=c("Plot","Species"))%>%
  filter(Species=="TVUF") %>%
  # select(-c(ProporcioF,logVisitation_rate,logMean_pollen,logFunctional_group_Rocka,logPollinator_richness,Pollinator_abundance,Flower_Abundance,Mean_pollen,Fecundity,Fruit_set,Pollinated_ovules,Avorted_total,Seed_set,Avorted_per_fruit))
  select(Species,Fecundity,Fruit_set,Pollinated_ovules,Avorted_total,Seed_set,Avorted_per_fruit,Mean_pollen,Mean_Homospecific,Flowers_with_pollen)

TVUF <- as.data.frame(TVUF) %>%
  select(.,-c(Plot,Species))

res2<-rcorr(as.matrix(TVUF))
corrplot(res2$r, type="upper", order="hclust",
         p.mat = res2$P, sig.level = 0.05, insig = "blank")


## Taula correlacions
chart.Correlation(TVUF, histogram=TRUE, pch=19)





## Corrplot
TVUH <- meandataperplot %>%
  filter(Species=="TVUH") %>%
  select(-c(ProporcioF,logVisitation_rate,logMean_pollen,logFunctional_group_Rocka,logPollinator_richness,Pollinator_abundance,Flower_Abundance,Mean_pollen,Fecundity,Fruit_set,Pollinated_ovules,Avorted_total,Seed_set,Avorted_per_fruit))
# select(Fecundity,Fruit_set,Pollinated_ovules,Avorted_total,Seed_set,Avorted_per_fruit,Mean_pollen,Mean_Homospecific,Mean_Heterospecific)

TVUH <- as.data.frame(TVUH) %>%
  select(.,-c(Plot,Species))
TVUH <- TVUH[-c(7,18,23,25),]

res2<-rcorr(as.matrix(TVUH))
corrplot(res2$r, type="upper", order="hclust",
         p.mat = res2$P, sig.level = 0.05, insig = "blank")


## Taula correlacions
chart.Correlation(TVUH, histogram=TRUE, pch=19)






## PCA
TVUFfinal <- TVUF[complete.cases(TVUF), ]
TVUF.pca <- prcomp(TVUFfinal, center = TRUE,scale. = TRUE)
summary(TVUF.pca)
biplot(TVUF.pca, scale = 0)


