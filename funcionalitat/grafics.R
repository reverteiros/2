
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

##### Figura 1

meandataperplotwtROF <- meandataperplot %>%
  filter(Species != "ROF")

meandataperplotwtROF$Species <- revalue(meandataperplotwtROF$Species, c("TVUF"="Female"))
meandataperplotwtROF$Species <- revalue(meandataperplotwtROF$Species, c("TVUH"="Hermaphrodite"))



a1 <- ggplot(data = meandataperplotwtROF, aes(x=Species, y=Homospecific_presence)) + 
  geom_boxplot()+
  theme_classic()+
  theme(axis.title.x = element_blank())+
  coord_cartesian(ylim = c(0, 1))+
  # scale_fill_manual(values=colorthreespecies)+
  theme(legend.position = "none")+ 
  scale_y_continuous(name = "Proportion of flowers with\nhomospecific pollen")

tvua1 <- arrangeGrob(a1, top = textGrob("a)", x = unit(0, "npc")
                                            , y   = unit(1, "npc"), just=c("left","top"),
                                            gp=gpar(col="black", fontsize=14)))


a2 <- ggplot(meandataperplotwtROF, aes(x=Species,y=Mean_Homospecific)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.title.x = element_blank())+
  # coord_cartesian(ylim = c(0, 1))+
  # scale_fill_manual(values=colorthreespecies)+
  theme(legend.position = "none")+ 
  scale_y_continuous(name = "Stigmatic pollen loads")

tvua2 <- arrangeGrob(a2, top = textGrob("b)", x = unit(0, "npc")
                                            , y   = unit(1, "npc"), just=c("left","top"),
                                            gp=gpar(col="black", fontsize=14)))


a3 <- ggplot(meandataperplotwtROF, aes(x=Species,y=Heterospecific_presence)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.title.x = element_blank())+
  coord_cartesian(ylim = c(0, 1))+
  # scale_fill_manual(values=colorthreespecies)+
  theme(legend.position = "none")+ 
  scale_y_continuous(name = "Proportion of flowers with\nheterospecific pollen")

tvua3 <- arrangeGrob(a3, top = textGrob("c)", x = unit(0, "npc")
                                            , y   = unit(1, "npc"), just=c("left","top"),
                                            gp=gpar(col="black", fontsize=14)))

a4 <- ggplot(meandataperplotwtROF, aes(x=Species,y=Fruit_set)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.title.x = element_blank())+
  coord_cartesian(ylim = c(0, 1))+
  # scale_fill_manual(values=colorthreespecies)+
  theme(legend.position = "none")+ 
  scale_y_continuous(name = "Fruit set")


tvua4 <- arrangeGrob(a4, top = textGrob("d)", x = unit(0, "npc")
                                            , y   = unit(1, "npc"), just=c("left","top"),
                                            gp=gpar(col="black", fontsize=14)))

a5 <- ggplot(meandataperplotwtROF, aes(x=Species,y=Seed_set)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.title.x = element_blank())+
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



##### Figura 3

tvuf1 <- ggplot(meandataperplotTVUF, aes(x=Pollinator_richness, y=Homospecific_presence)) + 
  geom_point(alpha=0.4) +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))+
  ggtitle("Female")+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  scale_y_continuous(name = "Proportion of flowers with\nhomospecific pollen") +
  scale_x_continuous(name = "Pollinator richness")

tvuf11 <- arrangeGrob(tvuf1, top = textGrob("a)", x = unit(0, "npc")
                                               , y   = unit(1, "npc"), just=c("left","top"),
                                               gp=gpar(col="black", fontsize=14)))

tvuf2 <- ggplot(meandataperplotTVUF, aes(x=Proportion_HB,y=Homospecific_presence)) +
  geom_point(alpha=0.4) +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.title.y = element_blank())+
  ggtitle("Female")+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  scale_x_continuous(name = "Proportion of honey bees")


tvuf21 <- arrangeGrob(tvuf2, top = textGrob("b)", x = unit(0, "npc")
                                            , y   = unit(1, "npc"), just=c("left","top"),
                                            gp=gpar(col="black", fontsize=14)))


tvuf3 <- ggplot(meandataperplotTVUF, aes(x=Proportion_Bee,y=Homospecific_presence)) +
  geom_point(alpha=0.4) +
  theme_classic()+
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

tvuh1 <- ggplot(meandataperplotTVUH, aes(x=Visitation_rate,y=Homospecific_presence)) +
  geom_point(alpha=0.4) +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))+
  ggtitle("Hermaphrodite")+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+ 
  scale_y_continuous(name = "Proportion of flowers with\nhomospecific pollen") +
  scale_x_continuous(name = "Visitation rate")

tvuh11 <- arrangeGrob(tvuh1, top = textGrob("d)", x = unit(0, "npc")
                                            , y   = unit(1, "npc"), just=c("left","top"),
                                            gp=gpar(col="black", fontsize=14)))



grid.arrange(tvuf11,tvuf21,tvuf31,tvuh11,tvuf41,tvuf51, ncol = 3)















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




