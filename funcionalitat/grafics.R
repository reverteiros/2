
source("funcionalitat/analisis mitjana per parcela.R")

library(ggpubr)
library(cowplot)
library(gridExtra)

# grupstaxonomicsgather <- grupstaxonomics %>%
#   gather(.,"Parcela","Codi_planta",-c(Parcela,Codi_planta))
# 
# ## boxplots
# a <- ggplot(meandataperplot, aes(y=Ratio_Heterosp_Homosp, x=Species)) +
#   geom_boxplot() +
#   theme_classic()
# 
# s <- ggplot(grupstaxonomics, aes(Codi_planta, fill = Abundance)) +
#   geom_bar(position = "stack")
# s

pollenpergraph <- pollenclean %>%
  mutate(Pollen_presence=if_else(Total>0,1,0)) %>%
  group_by(Plot, Species) %>% 
  summarise(Mean_pollen=mean(Total),Flowers_with_pollen=mean(Pollen_presence),
            Mean_Homospecific=mean(Homospecific),Mean_Heterospecific=mean(Heterospecific),
            Proporcio_Heterosp=(Mean_Heterospecific/Mean_pollen))

pollenboxplot <- pollenpergraph %>%
  select(Plot,Species,Mean_Homospecific,Mean_Heterospecific) %>%
  gather(Pollen,Abundance,-c(Plot,Species))

c <- ggplot(meandataperplot, aes(y=Visitation_rate, x=Species))+
  geom_boxplot() +
  theme_classic()

d <- ggplot(meandataperplot, aes(y=Pollinator_richness, x=Species))+
  geom_boxplot() +
  theme_classic()

e <- ggplot(pollenpergraph, aes(y=Flowers_with_pollen, x=Species))+
  geom_boxplot() +
  theme_classic()+
  coord_cartesian(ylim = c(0, 1))

f <- ggplot(data = pollenboxplot, aes(x=Species, y=Abundance)) + 
  geom_boxplot(aes(fill=Pollen))+
  theme_classic()

g <- 

p1 <- ggarrange(sp, bp + font("x.text", size = 9),
                               ncol = 1, nrow = 2)


