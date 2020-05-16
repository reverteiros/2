
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


## figure pollinators defensa
datapollinatorsall <- pollinators %>%
  filter(Species != "ROF") %>%
  # left_join(networkmetricsTVU, by="Plot") %>%
  left_join(grupstaxonomicsspread,by=c("Species","Plot")) %>%
  mutate(Proportion_HB = Honeybees/Pollinator_abundance) %>%
  mutate(Proportion_Bee = Bee/Pollinator_abundance) %>%
  mutate(Proportion_Coleoptera = Coleoptera/Pollinator_abundance) %>%
  mutate(Proportion_Diptera = Diptera/Pollinator_abundance) %>%
  mutate(Proportion_Lepidoptera = Lepidoptera/Pollinator_abundance) %>%
  mutate(Proportion_Wasps = Wasp/Pollinator_abundance) %>%
  select(-c(Bee,Coleoptera,Diptera,Honeybees,Wasp,Lepidoptera,Mecoptera,Heteroptera)) %>%
  filter(Flower_Abundance > 0) %>%
  mutate(Proportion_used = Proportion_Diptera+Proportion_Bee+Proportion_HB)%>%
  left_join(flowerrichness2, by="Plot")

datapollinatorsall$Species <- revalue(datapollinatorsall$Species, c("TVUF"="Female"))
datapollinatorsall$Species <- revalue(datapollinatorsall$Species, c("TVUH"="Hermaphrodite"))

rich <- ggplot(datapollinatorsall, aes(y=Pollinator_richness, x=Species,fill=Species))+
  geom_boxplot(aes(fill=Species))+
  scale_fill_manual(values=colortwospecies)+
  theme_classic()+ 
  # theme(legend.position = "none")+ 
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



a1 <- ggplot(data = meandataperplot, aes(x=Species, y=Homospecific_presence,fill=Species)) + 
  geom_boxplot(aes(fill=Species))+
  theme_classic()+
  theme(axis.title.x = element_blank())+
  scale_fill_manual(values=colortwospecies)+
  coord_cartesian(ylim = c(0, 1))+
  # scale_fill_manual(values=colorthreespecies)+
  theme(legend.position = "none")+ 
  scale_y_continuous(name = "Proportion of flowers with\nhomospecific pollen")

tvua1 <- arrangeGrob(a1, top = textGrob("a)", x = unit(0, "npc")
                                            , y   = unit(1, "npc"), just=c("left","top"),
                                            gp=gpar(col="black", fontsize=14)))


a2 <- ggplot(meandataperplot, aes(x=Species,y=Mean_Homospecific,fill=Species)) +
  geom_boxplot(aes(fill=Species))+
  theme_classic()+
  theme(axis.title.x = element_blank())+
  scale_fill_manual(values=colortwospecies)+
  # coord_cartesian(ylim = c(0, 1))+
  # scale_fill_manual(values=colorthreespecies)+
  theme(legend.position = "none")+ 
  scale_y_continuous(name = "Stigmatic pollen loads")

tvua2 <- arrangeGrob(a2, top = textGrob("b)", x = unit(0, "npc")
                                            , y   = unit(1, "npc"), just=c("left","top"),
                                            gp=gpar(col="black", fontsize=14)))


a3 <- ggplot(meandataperplot, aes(x=Species,y=Heterospecific_presence,fill=Species)) +
  geom_boxplot(aes(fill=Species))+
  theme_classic()+
  theme(axis.title.x = element_blank())+
  scale_fill_manual(values=colortwospecies)+
  coord_cartesian(ylim = c(0, 1))+
  # scale_fill_manual(values=colorthreespecies)+
  theme(legend.position = "none")+ 
  scale_y_continuous(name = "Proportion of flowers with\nheterospecific pollen")

tvua3 <- arrangeGrob(a3, top = textGrob("c)", x = unit(0, "npc")
                                            , y   = unit(1, "npc"), just=c("left","top"),
                                            gp=gpar(col="black", fontsize=14)))

a4 <- ggplot(meandataperplot, aes(x=Species,y=Fruit_set,fill=Species)) +
  geom_boxplot(aes(fill=Species))+
  theme_classic()+
  theme(axis.title.x = element_blank())+
  scale_fill_manual(values=colortwospecies)+
  coord_cartesian(ylim = c(0, 1))+
  # scale_fill_manual(values=colorthreespecies)+
  theme(legend.position = "none")+ 
  scale_y_continuous(name = "Fruit set")


tvua4 <- arrangeGrob(a4, top = textGrob("d)", x = unit(0, "npc")
                                            , y   = unit(1, "npc"), just=c("left","top"),
                                            gp=gpar(col="black", fontsize=14)))

a5 <- ggplot(meandataperplot, aes(x=Species,y=Seed_set,fill=Species)) +
  geom_boxplot(aes(fill=Species))+
  theme_classic()+
  theme(axis.title.x = element_blank())+
  scale_fill_manual(values=colortwospecies)+
  coord_cartesian(ylim = c(1, 4))+
  # scale_fill_manual(values=colorthreespecies)+
  theme(legend.position = "none")+ 
  scale_y_continuous(name = "Seed set")

tvua5 <- arrangeGrob(a5, top = textGrob("e)", x = unit(0, "npc")
                                            , y   = unit(1, "npc"), just=c("left","top"),
                                            gp=gpar(col="black", fontsize=14)))

grid.arrange(tvua1,tvua2,tvua3,tvua4,tvua5, ncol = 3)


##### Figura 2

meandataperplotwtROF <- meandataperplot %>%
  filter(Species != "ROF")

meandataperplotwtROF$Species <- revalue(meandataperplotwtROF$Species, c("TVUF"="Female"))
meandataperplotwtROF$Species <- revalue(meandataperplotwtROF$Species, c("TVUH"="Hermaphrodite"))



a1 <- ggplot(data = meandataperplotwtROF, aes(x=Species, y=Pollinator_richness)) + 
  geom_boxplot()+
  theme_classic()+
  theme(axis.title.x = element_blank())+
  # coord_cartesian(ylim = c(0, 1))+
  # scale_fill_manual(values=colorthreespecies)+
  theme(legend.position = "none")+ 
  scale_y_continuous(name = "Pollinator richness")

tvua1 <- arrangeGrob(a1, top = textGrob("a)", x = unit(0, "npc")
                                        , y   = unit(1, "npc"), just=c("left","top"),
                                        gp=gpar(col="black", fontsize=14)))


a2 <- ggplot(meandataperplotwtROF, aes(x=Species,y=Visitation_rate)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.title.x = element_blank())+
  # coord_cartesian(ylim = c(0, 1))+
  # scale_fill_manual(values=colorthreespecies)+
  theme(legend.position = "none")+ 
  scale_y_continuous(name = "Visitation rate")

tvua2 <- arrangeGrob(a2, top = textGrob("b)", x = unit(0, "npc")
                                        , y   = unit(1, "npc"), just=c("left","top"),
                                        gp=gpar(col="black", fontsize=14)))


a3 <- ggplot(meandataperplotwtROF, aes(x=Species,y=Proportion_HB)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.title.x = element_blank())+
  coord_cartesian(ylim = c(0, 1))+
  # scale_fill_manual(values=colorthreespecies)+
  theme(legend.position = "none")+ 
  scale_y_continuous(name = "Proportion of visits\nby honey bees")

tvua3 <- arrangeGrob(a3, top = textGrob("c)", x = unit(0, "npc")
                                        , y   = unit(1, "npc"), just=c("left","top"),
                                        gp=gpar(col="black", fontsize=14)))

a4 <- ggplot(meandataperplotwtROF, aes(x=Species,y=Proportion_Bee)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.title.x = element_blank())+
  coord_cartesian(ylim = c(0, 1))+
  # scale_fill_manual(values=colorthreespecies)+
  theme(legend.position = "none")+ 
  scale_y_continuous(name = "Proportion of visits\nby wild bees")


tvua4 <- arrangeGrob(a4, top = textGrob("d)", x = unit(0, "npc")
                                        , y   = unit(1, "npc"), just=c("left","top"),
                                        gp=gpar(col="black", fontsize=14)))

a5 <- ggplot(meandataperplotwtROF, aes(x=Species,y=Proportion_Diptera)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.title.x = element_blank())+
  coord_cartesian(ylim = c(0, 1))+
  # scale_fill_manual(values=colorthreespecies)+
  theme(legend.position = "none")+ 
  scale_y_continuous(name = "Proportion of visits\nby dipterans")

tvua5 <- arrangeGrob(a5, top = textGrob("e)", x = unit(0, "npc")
                                        , y   = unit(1, "npc"), just=c("left","top"),
                                        gp=gpar(col="black", fontsize=14)))

grid.arrange(tvua1,tvua2,tvua3,tvua4,tvua5, ncol = 3)



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
<<<<<<< HEAD
  theme(plot.title = element_text(hjust = 0.5))+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  labs( x = "Pollinator richness", y = "Proportion of flowers with\n homospecific pollen")+
  ylim(0.4, 1)
=======
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))+
  ggtitle("Female")+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  scale_y_continuous(name = "Proportion of flowers with\nhomospecific pollen") +
  scale_x_continuous(name = "Pollinator richness")

tvuf11 <- arrangeGrob(tvuf1, top = textGrob("a)", x = unit(0, "npc")
                                               , y   = unit(1, "npc"), just=c("left","top"),
                                               gp=gpar(col="black", fontsize=14)))
>>>>>>> 181b693be3211956233a7ef4f44a9e84d15198d4

tvuf2 <- ggplot(meandataperplotTVUF, aes(x=Proportion_HB,y=Homospecific_presence)) +
  geom_point(alpha=0.4) +
  ggtitle("Female")+
  theme_classic()+
<<<<<<< HEAD
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank())+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  labs( x = "Proportion of honey bees", y = "Proportion of flowers with\n homospecific pollen")+
  ylim(0.4, 1)
=======
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.title.y = element_blank())+
  ggtitle("Female")+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  scale_x_continuous(name = "Proportion of honey bees")


tvuf21 <- arrangeGrob(tvuf2, top = textGrob("b)", x = unit(0, "npc")
                                            , y   = unit(1, "npc"), just=c("left","top"),
                                            gp=gpar(col="black", fontsize=14)))

>>>>>>> 181b693be3211956233a7ef4f44a9e84d15198d4

tvuf3 <- ggplot(meandataperplotTVUF, aes(x=Proportion_Bee,y=Homospecific_presence)) +
  geom_point(alpha=0.4) +
  ggtitle("Female")+
  theme_classic()+
<<<<<<< HEAD
  theme(plot.title = element_text(hjust = 0.5))+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  labs( x = "Proportion of wild bees", y = "Proportion of flowers with\n homospecific pollen")+
  ylim(0.4, 1)
=======
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.title.y = element_blank())+
  ggtitle("Female")+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  scale_x_continuous(name = "Proportion of wild bees")

tvuf31 <- arrangeGrob(tvuf3, top = textGrob("c)", x = unit(0, "npc")
                                            , y   = unit(1, "npc"), just=c("left","top"),
                                            gp=gpar(col="black", fontsize=14)))

tvuf4 <- ggplot(meandataperplotTVUF, aes(x=Visitation_rate,y=Heterospecific_presence)) +
  geom_point(alpha=0.4) +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))+
  ggtitle("Female")+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  scale_y_continuous(name = "Proportion of flowers with\nheterospecific pollen") +
  scale_x_continuous(name = "Visitation rate")


tvuf41 <- arrangeGrob(tvuf4, top = textGrob("e)", x = unit(0, "npc")
                                             , y   = unit(1, "npc"), just=c("left","top"),
                                             gp=gpar(col="black", fontsize=14)))

tvuf5 <- ggplot(meandataperplotTVUF, aes(x=Pollinator_richness,y=Heterospecific_presence)) +
  geom_point(alpha=0.4) +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))+
  ggtitle("Female")+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  scale_y_continuous(name = "Proportion of flowers with\nheterospecific pollen") +
  scale_x_continuous(name = "Pollinator richness")


tvuf51 <- arrangeGrob(tvuf5, top = textGrob("e)", x = unit(0, "npc")
                                            , y   = unit(1, "npc"), just=c("left","top"),
                                            gp=gpar(col="black", fontsize=14)))
>>>>>>> 181b693be3211956233a7ef4f44a9e84d15198d4

tvuh1 <- ggplot(meandataperplotTVUH, aes(x=Visitation_rate,y=Homospecific_presence)) +
  geom_point(alpha=0.4) +
  ggtitle("Hermaphrodite")+
  theme_classic()+
<<<<<<< HEAD
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank())+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  labs( x = "Visitation rate", y = "Proportion of flowers with\n homospecific pollen")+
  ylim(0.4, 1)
=======
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))+
  ggtitle("Hermaphrodite")+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  scale_y_continuous(name = "Proportion of flowers with\nhomospecific pollen") +
  scale_x_continuous(name = "Visitation rate")
>>>>>>> 181b693be3211956233a7ef4f44a9e84d15198d4

tvuh11 <- arrangeGrob(tvuh1, top = textGrob("d)", x = unit(0, "npc")
                                            , y   = unit(1, "npc"), just=c("left","top"),
                                            gp=gpar(col="black", fontsize=14)))

<<<<<<< HEAD
# final graph tot junt
ggarrange(tvuf1, tvuf2, tvuf3, tvuh1, ncol = 2, nrow = 2,labels = c("(a)", "(b)", "(c)", "(d)"))

# export 6 x 6.2 inches
=======


grid.arrange(tvuf11,tvuf21,tvuf31,tvuh11,tvuf41,tvuf51, ncol = 3)
>>>>>>> 181b693be3211956233a7ef4f44a9e84d15198d4



##### Figura 2

<<<<<<< HEAD
tvuf1 <- ggplot(meandataperplotTVUF, aes(x=Pollinator_richness, y=Homospecific_presence)) + 
  geom_point(alpha=0.4) +
  ggtitle("Female")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  labs( x = "Pollinator richness", y = "Proportion of flowers with\n homospecific pollen")
=======
>>>>>>> 181b693be3211956233a7ef4f44a9e84d15198d4

tvuf2 <- ggplot(meandataperplotTVUF, aes(x=Proportion_HB,y=Homospecific_presence)) +
  geom_point(alpha=0.4) +
  ggtitle("Female")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank())+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  labs( x = "Proportion of honey bees", y = "Proportion of flowers with\n homospecific pollen")

<<<<<<< HEAD
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
=======

>>>>>>> 181b693be3211956233a7ef4f44a9e84d15198d4

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
b <- ggplot(meandataperplot, aes(y=Pollinator_richness, x=Species))+
  geom_boxplot()+
  # geom_point()+
  # scale_fill_manual(values=colorthreespecies)+
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

e <- ggplot(data = meandataperplotwtROF, aes(x=Species, y=Fruit_set)) + 
  geom_boxplot()+
  # scale_fill_manual(values=colortwospecies)+
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




