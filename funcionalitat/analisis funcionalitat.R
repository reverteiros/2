
source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades.R")
source("funcionalitat/grups funcionals.R")

library(ggplot2)
library(ggExtra)
library(corrplot)
library(Hmisc)
library("PerformanceAnalytics")
library(vegan)
library(betapart)



ggplot(databaseglmROF) +
  geom_point(aes(Visitation_rate,Total),alpha=0.3) +
  geom_smooth(aes(Visitation_rate,Total))+
  theme_classic() #+
  # coord_cartesian(xlim = c(0, 32), ylim = c(0, 32))

datanalysis <- datapollinatorsall %>%
  left_join(meanpollenperplot,by=c("Species","Plot")) %>%
  filter(Species == "TVUH")

ggplot(datanalysis) +
  geom_point(aes(Visitation_rate,Mean_pollen,colour=Species)) +
  # geom_smooth(aes(Visitation_rate,Mean_pollen))+
  theme_classic() #+
# coord_cartesian(xlim = c(0, 32), ylim = c(0, 32))


ggplot(databaseglmTVUF) +
  geom_point(aes(Visitation_rate,Total),alpha=0.3) +
  geom_smooth(aes(Visitation_rate,Total))+
  theme_classic() #+
# coord_cartesian(xlim = c(0, 32), ylim = c(0, 32))


ggplot(databaseglmTVUH) +
  geom_point(aes(Visitation_rate,Total),alpha=0.3) +
  geom_smooth(aes(Visitation_rate,Total))+
  theme_classic() #+
# coord_cartesian(xlim = c(0, 32), ylim = c(0, 32))





dataclean <- dataanalysis %>%
  left_join(numerogrupsfuncionals,by=c("Plot","Species"))%>%
  mutate(Visitation_rate = HB_Visitation_rate+Wild_Visitation_rate) %>%
  select(Species, Plot, Pollinator_richness, Shannon_diversity, Functional_group_Rocka, Visitation_rate)


## boxplots
ggplot(dataclean, aes(y=Ratio_Heterosp_Homosp, x=Species)) +
  geom_boxplot() +
  theme_classic()

####################      ROF      #############################
ROF <- filter(dataclean, Species =="ROF")

# ROF <- ROF %>%
#   mutate(Homospecific_pollen = ROF_pollen) %>%
#   mutate(Heterospecific_pollen = TVU_pollen) %>%
#   mutate(Pollen_Heterosp_Total = Heterospecific_pollen/(ROF_pollen+TVU_pollen))

# Corrplot
ROF <- as.data.frame(ROF) %>%
  select(.,-c(Plot,Species))
ROF <- ROF[-29,]

res2<-rcorr(as.matrix(ROF))
corrplot(res2$r, type="upper", order="hclust",
         p.mat = res2$P, sig.level = 0.05, insig = "blank")

# # Taula correlacions
# ROFnetwork <- as.data.frame(ROF)
# ROFnetwork <- ROFnetwork[,-1]
# chart.Correlation(ROFnetwork, histogram=TRUE, pch=19)


#########################     TVUF     #############################
TVUF <- filter(dataclean, Species =="TVUF")

# TVUF <- TVUF %>%
#   mutate(Homospecific_pollen = TVU_pollen) %>%
#   mutate(Heterospecific_pollen = ROF_pollen) %>%
#   mutate(Pollen_Heterosp_Total = Heterospecific_pollen/(ROF_pollen+TVU_pollen))


# TVUF <- TVUF %>%
#   left_join(databaserecursos, by = "Plot") %>%
#   mutate(Homospecific_pollen = TVU_pollen) %>%
#   mutate(Heterospecific_pollen = ROF_pollen) %>%
#   mutate(Pollen_Heterosp_Total = Heterospecific_pollen/Pollen_total)

TVUF <- as.data.frame(TVUF) %>%
  select(.,-c(Plot,Species))



# Corrplot
res2<-rcorr(as.matrix(TVUF))
corrplot(res2$r, type="upper", order="hclust",
         p.mat = res2$P, sig.level = 0.05, insig = "blank")

#######################     TVUH      ##########################
TVUH <- filter(dataclean, Species =="ROF")

# TVUH <- TVUH %>%
#   left_join(databaserecursos, by = "Plot") %>%
#   mutate(Homospecific_pollen = TVU_pollen) %>%
#   mutate(Heterospecific_pollen = ROF_pollen) %>%
#   mutate(Pollen_Heterosp_Total = Heterospecific_pollen/Pollen_total)

TVUH <- as.data.frame(TVUH) %>%
  select(.,-c(Plot,Species))

# Corrplot
res2<-rcorr(as.matrix(TVUH))
corrplot(res2$r, type="upper", order="hclust",
         p.mat = res2$P, sig.level = 0.07, insig = "blank")




# PCA
TVUFfinal <- TVUF[complete.cases(TVUF), ]
TVUF.pca <- prcomp(TVUFfinal, center = TRUE,scale. = TRUE)
summary(TVUF.pca)
biplot(TVUF.pca, scale = 0)






#################### Relació entre espècies en el polen depositat als estigmes

relacionsespecies <- group_by(pollentotal, Plot, Species) %>%
  summarise(Mean_pollen=mean(Total))%>%
  complete(Species, Plot) %>%
  distinct() %>%
  spread(Species,Mean_pollen)

ggplot(relacionsespecies) +
  geom_point(aes(ROF,TVUF)) +
  theme_classic() +
  coord_cartesian(xlim = c(0, 32), ylim = c(0, 32))

ggplot(relacionsespecies) +
  geom_point(aes(TVUH,TVUF)) +
  theme_classic() +
  coord_cartesian(xlim = c(0, 32), ylim = c(0, 32))

ggplot(relacionsespecies) +
  geom_point(aes(ROF,TVUH)) +
  theme_classic() +
  coord_cartesian(xlim = c(0, 22), ylim = c(0, 22))





