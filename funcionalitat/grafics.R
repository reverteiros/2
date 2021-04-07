
source("funcionalitat/netejar dades mitjana per parcela.R")

library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggplot2)
library(ggExtra)
library(corrplot)
library(Hmisc)
library(grid)
library(plyr)


colortwospecies <- c("darkgreen","darkolivegreen1")


datapollinatorsall <- pollinators %>%
  left_join(grupstaxonomicsspread,by=c("Species","Plot")) %>%
  mutate(Proportion_HB = Honeybees/Pollinator_abundance) %>%
  mutate(Proportion_Bee = Bee/Pollinator_abundance) %>%
  mutate(Proportion_Coleoptera = Coleoptera/Pollinator_abundance) %>%
  mutate(Proportion_Diptera = Diptera/Pollinator_abundance) %>%
  mutate(Proportion_Lepidoptera = Lepidoptera/Pollinator_abundance) %>%
  mutate(Proportion_Wasps = Wasp/Pollinator_abundance) %>%
  select(-c(Bee,Coleoptera,Diptera,Honeybees,Wasp,Lepidoptera,Heteroptera)) %>%
  filter(Flower_Abundance > 0) %>%
  mutate(Proportion_used = Proportion_Diptera+Proportion_Bee+Proportion_HB)

datapollinatorsall$Species <- revalue(datapollinatorsall$Species, c("TVUF"="Female"))
datapollinatorsall$Species <- revalue(datapollinatorsall$Species, c("TVUH"="Hermaphrodite"))


########## gràfics comparació taxa de visites etc entre morfs
rich <- ggplot(datapollinatorsall, aes(y=Pollinator_richness, x=Species,fill=Species))+
  geom_boxplot(aes(fill=Species))+
  scale_fill_manual(values=colortwospecies)+
  theme_classic()+ 
  theme(legend.position = "none")+
  scale_y_continuous(name = "Pollinator richness")

vrate <- ggplot(datapollinatorsall, aes(y=Visitation_rate, x=Species,fill=Species))+
  geom_boxplot(aes(fill=Species))+
  scale_fill_manual(values=colortwospecies)+
  theme_classic()+ 
  theme(legend.position = "none")+ 
  scale_y_continuous(name = "Visitation rate\n(pollinators / 1000 flowers)")

pollenboxplot <- datapollinatorsall %>%
  mutate(Wild_bees=Proportion_Bee)%>%
  mutate(Honey_bees=Proportion_HB)%>%
  mutate(Wasps=Proportion_Wasps)%>%
  mutate(Dipterans=Proportion_Diptera)%>%
  mutate(Coleopterans=Proportion_Coleoptera)%>%
  mutate(Lepidopterans=Proportion_Lepidoptera)%>%
  select(Plot,Species,Wild_bees,Honey_bees,Wasps,Dipterans,Coleopterans,Lepidopterans) %>%
  gather(Pollinator_group,Proportion,-c(Plot,Species))

pollenboxplot$Pollinator_group <- factor(pollenboxplot$Pollinator_group,levels = c("Honey_bees", "Wild_bees", "Dipterans", "Lepidopterans","Coleopterans","Wasps"))

c <- ggplot(data = pollenboxplot, aes(x=Pollinator_group, y=Proportion,fill=Species)) + 
  geom_boxplot(aes(fill=Species))+
  theme_classic()+
  coord_cartesian(ylim = c(0, 1))+
  scale_fill_manual(values=colortwospecies)+
  theme(legend.position = "none")



##### Figura 1

meandataperplot$Species <- revalue(meandataperplot$Species, c("TVUF"="Female"))
meandataperplot$Species <- revalue(meandataperplot$Species, c("TVUH"="Hermaphrodite"))


m1 <- ggplot(meandataperplot, aes(y = Conspecific_presence, x =Species,fill=Species)) + 
  geom_boxplot(aes(fill=Species))+
  theme_classic()+
  scale_x_discrete(labels=c("TVUF" = "Female", "TVUH" = "Hermaphrodite"))+
  theme(axis.title.x = element_blank())+
  scale_fill_manual(values=colortwospecies)+
  labs( y = "Proportion of flowers with\n conspecific pollen")+
  ylim(0, 1) +
  theme(legend.position = "none")+ 
  geom_segment(aes(x = 0.8, y = 0.3, xend = 2.2, yend = 0.3))+
  annotate("text", x = 1.5, y = 0.25, label = "ns")

m2 <- ggplot(meandataperplot, aes(y = Mean_Conspecific, x =Species,fill=Species)) + 
  geom_boxplot(aes(fill=Species))+
  theme_classic()+
  scale_x_discrete(labels=c("TVUF" = "Female", "TVUH" = "Hermaphrodite"))+
  theme(axis.title.x = element_blank())+
  scale_fill_manual(values=colortwospecies)+
  labs( y = "\nStigmatic pollen loads")+
  ylim(0, 35)+
  theme(legend.position = "none")+ 
  geom_segment(aes(x = 0.8, y = 32, xend = 2.2, yend = 32))+
  annotate("text", x = 1.5, y = 33, label = "*", fontface ="bold")

m3 <- ggplot(meandataperplot, aes(y = Heterospecific_presence, x =Species, fill=Species)) + 
  geom_boxplot(aes(fill=Species))+
  theme_classic()+
  scale_x_discrete(labels=c("TVUF" = "Female", "TVUH" = "Hermaphrodite"))+
  theme(axis.title.x = element_blank())+
  scale_fill_manual(values=colortwospecies)+
  labs( y = "Proportion of flowers with\n heterospecific pollen")+
  ylim(0, 1)+
  theme(legend.position = "none")+ 
  geom_segment(aes(x = 0.8, y = 0.9, xend = 2.2, yend = 0.9))+
  annotate("text", x = 1.5, y = 0.92, label = "*", fontface ="bold")

m4 <- ggplot(meandataperplot, aes(y = Fruit_set, x =Species,fill=Species)) + 
  geom_boxplot(aes(fill=Species))+
  theme_classic()+
  scale_x_discrete(labels=c("TVUF" = "Female", "TVUH" = "Hermaphrodite"))+
  theme(axis.title.x = element_blank())+
  scale_fill_manual(values=colortwospecies)+
  labs( y = "\nFruit set")+
  ylim(0, 1)+
  theme(legend.position = "none")+ 
  geom_segment(aes(x = 0.8, y = 0.96, xend = 2.2, yend = 0.96))+
  annotate("text", x = 1.5, y = 0.98, label = "*", fontface ="bold")

m5 <- ggplot(meandataperplot, aes(y = Seed_set, x =Species,fill=Species)) + 
  geom_boxplot(aes(fill=Species))+
  theme_classic()+
  scale_x_discrete(labels=c("TVUF" = "Female", "TVUH" = "Hermaphrodite"))+
  theme(axis.title.x = element_blank())+
  scale_fill_manual(values=colortwospecies)+
  labs( y = "\nSeed set")+
  theme(legend.position = "none")+ 
  ylim(1, 4)+
  geom_segment(aes(x = 0.8, y = 3.8, xend = 2.2, yend = 3.8))+
  annotate("text", x = 1.5, y = 3.9, label = "*", fontface ="bold")


ggarrange(m1, m2, m3, m4, m5, ncol = 3, nrow = 2,labels = c("(a)", "(b)", "(c)", "(d)", "(e)"))
###• export en 5 x 8 inches



##### Figura 2

tvuf1 <- ggplot(meandataperplotTVUF, aes(x=Pollinator_richness, y=Conspecific_presence)) + 
  geom_point(alpha=0.4) +
  ggtitle("Female")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  labs( x = "Pollinator richness", y = "Proportion of flowers with\n conspecific pollen")

tvuf2 <- ggplot(meandataperplotTVUF, aes(x=Proportion_HB,y=Conspecific_presence)) +
  geom_point(alpha=0.4) +
  ggtitle("Female")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank())+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  labs( x = "Proportion of honey bees", y = "Proportion of flowers with\n conspecific pollen")

tvuf3 <- ggplot(meandataperplotTVUF, aes(x=Proportion_Bee,y=Conspecific_presence)) +
  geom_point(alpha=0.4) +
  ggtitle("Female")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  labs( x = "Proportion of wild bees", y = "Proportion of flowers with\n conspecific pollen")

tvuh1 <- ggplot(meandataperplotTVUH, aes(x=Visitation_rate,y=Conspecific_presence)) +
  geom_point(alpha=0.4) +
  ggtitle("Hermaphrodite")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank())+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  labs( x = "Visitation rate", y = "Proportion of flowers with\n conspecific pollen")

# final graph tot junt
ggarrange(tvuf1, tvuf2, tvuf3, tvuh1, ncol = 2, nrow = 2,labels = c("(a)", "(b)", "(c)", "(d)"))



