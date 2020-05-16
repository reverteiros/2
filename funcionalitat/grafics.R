
source("funcionalitat/netejar dades mitjana per parcela.R")

library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggplot2)
library(ggExtra)
library(corrplot)
library(Hmisc)


##### Figura 1

meandataperplotwtROF <- meandataperplot %>%
  filter(Species!="ROF") %>%
  select(Homospecific_presence,Mean_Homospecific,Heterospecific_presence,Fruit_set,Seed_set,Species) 


m1 <- ggplot(meandataperplotwtROF, aes(y = Homospecific_presence, x =Species)) + 
  geom_boxplot() +
  theme_classic()+
  scale_x_discrete(labels=c("TVUF" = "Female", "TVUH" = "Hermaphrodite"))+
  theme(axis.title.x = element_blank())+
  labs( y = "Proportion of flowers with\n homospecific pollen")+
  ylim(0, 1) +
  geom_segment(aes(x = 0.8, y = 0.3, xend = 2.2, yend = 0.3))+
  annotate("text", x = 1.5, y = 0.25, label = "ns")

m2 <- ggplot(meandataperplotwtROF, aes(y = Mean_Homospecific, x =Species)) + 
  geom_boxplot() +
  theme_classic()+
  scale_x_discrete(labels=c("TVUF" = "Female", "TVUH" = "Hermaphrodite"))+
  theme(axis.title.x = element_blank())+
  labs( y = "\nStigmatic pollen loads")+
  ylim(0, 35)+
  geom_segment(aes(x = 0.8, y = 32, xend = 2.2, yend = 32))+
  annotate("text", x = 1.5, y = 33, label = "*", fontface ="bold")

m3 <- ggplot(meandataperplotwtROF, aes(y = Heterospecific_presence, x =Species)) + 
  geom_boxplot() +
  theme_classic()+
  scale_x_discrete(labels=c("TVUF" = "Female", "TVUH" = "Hermaphrodite"))+
  theme(axis.title.x = element_blank())+
  labs( y = "Proportion of flowers with\n heterospecific pollen")+
  ylim(0, 1)+
  geom_segment(aes(x = 0.8, y = 0.9, xend = 2.2, yend = 0.9))+
  annotate("text", x = 1.5, y = 0.92, label = "*", fontface ="bold")

m4 <- ggplot(meandataperplotwtROF, aes(y = Fruit_set, x =Species)) + 
  geom_boxplot() +
  theme_classic()+
  scale_x_discrete(labels=c("TVUF" = "Female", "TVUH" = "Hermaphrodite"))+
  theme(axis.title.x = element_blank())+
  labs( y = "\nFruit set")+
  ylim(0, 1)+
  geom_segment(aes(x = 0.8, y = 0.96, xend = 2.2, yend = 0.96))+
  annotate("text", x = 1.5, y = 0.98, label = "*", fontface ="bold")

m5 <- ggplot(meandataperplotwtROF, aes(y = Seed_set, x =Species)) + 
  geom_boxplot() +
  theme_classic()+
  scale_x_discrete(labels=c("TVUF" = "Female", "TVUH" = "Hermaphrodite"))+
  theme(axis.title.x = element_blank())+
  labs( y = "\nSeed set")+
  ylim(1, 4)+
  geom_segment(aes(x = 0.8, y = 3.8, xend = 2.2, yend = 3.8))+
  annotate("text", x = 1.5, y = 3.9, label = "*", fontface ="bold")


ggarrange(m1, m2, m3, m4, m5, ncol = 3, nrow = 2,labels = c("(a)", "(b)", "(c)", "(d)", "(e)"))
###• export en 5 x 8 inches



##### Figura 2

tvuf1 <- ggplot(meandataperplotTVUF, aes(x=Pollinator_richness, y=Homospecific_presence)) + 
  geom_point(alpha=0.4) +
  ggtitle("Female")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  labs( x = "Pollinator richness", y = "Proportion of flowers with\n homospecific pollen")+
  ylim(0.4, 1)

tvuf2 <- ggplot(meandataperplotTVUF, aes(x=Proportion_HB,y=Homospecific_presence)) +
  geom_point(alpha=0.4) +
  ggtitle("Female")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank())+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  labs( x = "Proportion of honey bees", y = "Proportion of flowers with\n homospecific pollen")+
  ylim(0.4, 1)

tvuf3 <- ggplot(meandataperplotTVUF, aes(x=Proportion_Bee,y=Homospecific_presence)) +
  geom_point(alpha=0.4) +
  ggtitle("Female")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  labs( x = "Proportion of wild bees", y = "Proportion of flowers with\n homospecific pollen")+
  ylim(0.4, 1)

tvuh1 <- ggplot(meandataperplotTVUH, aes(x=Visitation_rate,y=Homospecific_presence)) +
  geom_point(alpha=0.4) +
  ggtitle("Hermaphrodite")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank())+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  labs( x = "Visitation rate", y = "Proportion of flowers with\n homospecific pollen")+
  ylim(0.4, 1)


# final graph tot junt
ggarrange(tvuf1, tvuf2, tvuf3, tvuh1, ncol = 2, nrow = 2,labels = c("(a)", "(b)", "(c)", "(d)"))

# export 6 x 6.2 inches



##### Figura 2

tvuf1 <- ggplot(meandataperplotTVUF, aes(x=Pollinator_richness, y=Homospecific_presence)) + 
  geom_point(alpha=0.4) +
  ggtitle("Female")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  labs( x = "Pollinator richness", y = "Proportion of flowers with\n homospecific pollen")

tvuf2 <- ggplot(meandataperplotTVUF, aes(x=Proportion_HB,y=Homospecific_presence)) +
  geom_point(alpha=0.4) +
  ggtitle("Female")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank())+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  labs( x = "Proportion of honey bees", y = "Proportion of flowers with\n homospecific pollen")

tvuf3 <- ggplot(meandataperplotTVUF, aes(x=Proportion_Bee,y=Homospecific_presence)) +
  geom_point(alpha=0.4) +
  ggtitle("Female")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  labs( x = "Proportion of wild bees", y = "Proportion of flowers with\n homospecific pollen")

# tvuf4 <- ggplot(meandataperplotTVUF, aes(x=Proportion_Heterosp_Community,y=Heterospecific_presence)) +
#   geom_point(alpha=0.4) +
#   theme_classic()+
#   stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
#   labs( x = "Prop. Heterospecific in the Community", y = "Prop. flowers with heterospecific")

tvuh1 <- ggplot(meandataperplotTVUH, aes(x=Visitation_rate,y=Homospecific_presence)) +
  geom_point(alpha=0.4) +
  ggtitle("Hermaphrodite")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank())+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  labs( x = "Visitation rate", y = "Proportion of flowers with\n homospecific pollen")


# final graph tot junt
g1 <- ggarrange(tvuf1, tvuf2, ncol = 2, nrow = 1)
g2 <- ggarrange(tvuf3,tvuh1, ncol = 2, nrow = 1)


final <- ggarrange(g1,g2,nrow=2)
final
# annotate_figure(final,top = text_grob("Thymus vulgaris female"))






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
  mutate(Homospecific = Homospecific_presence) %>%
  mutate(Heterospecific = Heterospecific_presence) %>%
  select(Plot,Species,Homospecific,Heterospecific) %>%
  gather(Pollen,Flowers_with_pollen,-c(Plot,Species))

c <- ggplot(data = pollenboxplot, aes(x=Pollen, y=Flowers_with_pollen,fill=Species)) + 
  geom_boxplot(aes(fill=Species))+
  theme_classic()+
  coord_cartesian(ylim = c(0, 1))+
  scale_fill_manual(values=colorthreespecies)+
  theme(legend.position = "none")

# Mean pollen on stigmas
d <- ggplot(meandataperplot, aes(y=Mean_Homospecific, x=Species,fill=Species))+
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
f <- ggplot(data = meandataperplotwtROF, aes(x=Species, y=Seed_set, fill=Species)) + 
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
  gather("Fitness_variable","Fitness_value",c(Homospecific_presence,Mean_Homospecific))

ggplot(ROF, aes(x=Pollinator_value,y=Fitness_value)) +
  geom_point(alpha=0.3) +
  facet_grid(Fitness_variable~Pollinator_variable,scales = "free") +
  theme_classic()+
  theme(legend.position = "none")+
  geom_abline(mapping = NULL, data = NULL, ..., slope, intercept,
              na.rm = FALSE, show.legend = NA)





TVUF <- meandataperplotTVUF %>%
  gather("Pollinator_variable","Pollinator_value",c(Proportion_HB,Proportion_Bee,Proportion_Diptera,Pollinator_richness,Visitation_rate,ProporcioF)) %>%
  gather("Fitness_variable","Fitness_value",c(Homospecific_presence,Mean_Homospecific,Fruit_set,Seed_set))

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
  gather("Pollinator_variable","Pollinator_value",c(Proportion_HB,sqrtProportion_Bee,Proportion_Diptera,Pollinator_richness,logVisitation_rate,ProporcioF)) %>%
  gather("Fitness_variable","Fitness_value",c(Total_presence,Mean_Total,Fruit_set,Seed_set))

ggplot(TVUH, aes(x=Pollinator_value,y=Fitness_value)) +
  geom_point(alpha=0.3) +
  facet_grid(Fitness_variable~Pollinator_variable,scales = "free")







ggplot(meandataperplot, aes(x=Flower_Abundance,y=Pollinator_abundance)) +
  geom_point(alpha=0.3) +
  facet_grid(.~Species,scales = "free")




