
source("funcionalitat/netejar dades.R")

library(ggplot2)
library(ggExtra)
library(corrplot)
library(Hmisc)
# install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
library(vegan)
library(betapart)


dataclean <- select(dataanalysis, Species, Plot, Flowers_with_pollen, Mean_pollen, Mean_Homospecific, 
                    Mean_Heterospecific, Ratio_Heterosp_Homosp, 
                    # Mean_weigth_viables, Seed_set, Avorted_fruits, Avorted_total, Fruit_set,
                    Pollinator_richness, Shannon_Diversity, HB_Visitation_rate, Wild_Visitation_rate, H2, 
                    Proportion_HB, Flower_relative_abundance) %>%
            mutate(Visitation_rate = HB_Visitation_rate+Wild_Visitation_rate)

dataclean$TVU_pollen <- flors$TVU_pollen
dataclean$ROF_pollen <- flors$ROF_pollen


## boxplots
ggplot(dataclean, aes(y=Ratio_Heterosp_Homosp, x=Species)) +
  geom_boxplot() +
  theme_classic()
# 
# #### TEMPERATURA I BITXOS
# ggplot(dataanalysis) +
#   geom_point(aes(T_Max,Wild_Visitation_rate, colour=Species)) + 
#   geom_smooth(aes(T_Max,Wild_Visitation_rate, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="T_Max", y="Wild_Visitation_rate")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ## grafic proporcio apis i wild
# TVUF <- filter(datafunction, Species =="ROF") %>%
#   mutate(Honeybees = HB_Visitation_rate/Visitation_rate) %>%
#   mutate(Wild = Wild_Visitation_rate/Visitation_rate) %>%
#   select(Honeybees, Wild) %>%
#   gather(Pollinator,Proportion,-Plot)



####################      ROF      #############################
ROF <- filter(dataclean, Species =="ROF")%>%
  select(Pollinator_richness, Shannon_Diversity, HB_Visitation_rate, Wild_Visitation_rate, H2, Visitation_rate,
         Proportion_HB, TVU_pollen, ROF_pollen
         )

Pollinator_richness, Shannon_Diversity, HB_Visitation_rate, Wild_Visitation_rate, H2, Visitation_rate,
Proportion_HB, TVU_pollen, ROF_pollen

ROF <- ROF %>%
  mutate(Homospecific_pollen = ROF_pollen) %>%
  mutate(Heterospecific_pollen = TVU_pollen) %>%
  mutate(Pollen_Heterosp_Total = Heterospecific_pollen/(ROF_pollen+TVU_pollen))

ggplot(ROF, aes(y=Pollen_Heterosp_Total)) +
  geom_boxplot() +
  theme_classic()

hist(ROF$Pollen_Heterosp_Total)

ROF <- as.data.frame(ROF) %>%
  select(.,-Plot)
ROF <- ROF[-29,]

# Corrplot
res2<-rcorr(as.matrix(ROF))
corrplot(res2$r, type="upper", order="hclust",
         p.mat = res2$P, sig.level = 0.05, insig = "blank")
# Taula correlacions
ROFnetwork <- as.data.frame(ROF)
ROFnetwork <- ROFnetwork[,-1]
chart.Correlation(ROFnetwork, histogram=TRUE, pch=19)


#########################     TVUF     #############################
TVUF <- filter(dataclean, Species =="TVUF")%>%
  select( Pollinator_richness, Shannon_Diversity, HB_Visitation_rate, Wild_Visitation_rate, H2, Visitation_rate,
         Proportion_HB, Flower_relative_abundance)
,
         Pollinator_richness, Shannon_Diversity, HB_Visitation_rate, Wild_Visitation_rate, H2, Visitation_rate,
         Proportion_HB, Flower_relative_abundance
TVUF <- TVUF %>%
  mutate(Homospecific_pollen = TVU_pollen) %>%
  mutate(Heterospecific_pollen = ROF_pollen) %>%
  mutate(Pollen_Heterosp_Total = Heterospecific_pollen/(ROF_pollen+TVU_pollen))

ggplot(TVUF, aes(y=Pollen_Heterosp_Total)) +
  geom_boxplot() +
  theme_classic()

hist(ROF$Pollen_Heterosp_Total)

TVUF <- TVUF %>%
  left_join(databaserecursos, by = "Plot") %>%
  mutate(Homospecific_pollen = TVU_pollen) %>%
  mutate(Heterospecific_pollen = ROF_pollen) %>%
  mutate(Pollen_Heterosp_Total = Heterospecific_pollen/Pollen_total)

TVUF <- as.data.frame(TVUF) %>%
  select(.,-Plot)



# Corrplot
res2<-rcorr(as.matrix(TVUF))
corrplot(res2$r, type="upper", order="hclust",
         p.mat = res2$P, sig.level = 0.05, insig = "blank")

#######################     TVUH      ##########################
TVUH <- filter(dataclean, Species =="ROF")%>%
  select(Pollinator_richness, Shannon_Diversity, HB_Visitation_rate, Wild_Visitation_rate, H2, Visitation_rate,
         Proportion_HB, Flower_relative_abundance)

, 
Pollinator_richness, Shannon_Diversity, HB_Visitation_rate, Wild_Visitation_rate, H2, Visitation_rate,
Proportion_HB, Flower_relative_abundance

TVUH <- TVUH %>%
  left_join(databaserecursos, by = "Plot") %>%
  mutate(Homospecific_pollen = TVU_pollen) %>%
  mutate(Heterospecific_pollen = ROF_pollen) %>%
  mutate(Pollen_Heterosp_Total = Heterospecific_pollen/Pollen_total)

TVUH <- as.data.frame(TVUH) %>%
  select(.,-Plot)



# Corrplot
res2<-rcorr(as.matrix(TVUH))
corrplot(res2$r, type="upper", order="hclust",
         p.mat = res2$P, sig.level = 0.07, insig = "blank")



################################## models lineals

aa <- lm(Ratio_Heterosp_Homosp ~ Pollen_Heterosp_Total + H2, data = TVUF)
summary(aa)



aa <- lm(Ratio_Heterosp_Homosp ~ Pollen_Heterosp_Total + H2, data = ROF)
summary(aa)



aa <- lm(Ratio_Heterosp_Homosp ~ Pollen_Heterosp_Total + H2, data = TVUH)
summary(aa)





######### composicio

##  ROF
diversitatROF <- censos %>%
  dplyr::filter(., Species == "ROF") %>%
  spread(Pollinator, Abundance) %>%
  InsertRow(., NewRow = numeric(81),RowNum = 29)
diversitatROF[is.na(diversitatROF)] <- 0
diversitatROF <- diversitatROF[,-(1:2)]
composicioROF<-bray.part(diversitatROF)

##  TVUF
diversitatTVUF <- censos %>%
  dplyr::filter(., Species == "TVUF") %>%
  spread(Pollinator, Abundance) 
diversitatTVUF[is.na(diversitatTVUF)] <- 0
diversitatTVUF <- diversitatTVUF[,-(1:2)]
composicioTVUF<-bray.part(diversitatTVUF)

##  TVUH
diversitatTVUH <- (dplyr::filter(censos, Species == "TVUH")) %>%
  spread(Pollinator, Abundance) %>%
  InsertRow(., NewRow = numeric(57),RowNum = 7)%>%
  InsertRow(., NewRow = numeric(57),RowNum = 18)%>%
  InsertRow(., NewRow = numeric(57),RowNum = 23)%>%
  InsertRow(., NewRow = numeric(57),RowNum = 25)
diversitatTVUH[is.na(diversitatTVUH)] <- 0
diversitatTVUH <- diversitatTVUH[,-(1:2)]
composicioTVUH <-bray.part(diversitatTVUH)

Ratio_Heterosp_HomospdistTVUF <- dist(TVUF$Ratio_Heterosp_Homosp)
Ratio_Heterosp_HomospdistTVUH <- dist(TVUH$Ratio_Heterosp_Homosp)
Ratio_Heterosp_HomospdistROF <- dist(ROF$Ratio_Heterosp_Homosp)
Mean_pollenROFdist <- dist(ROF$Mean_pollen)
Mean_pollenTVUFdist <- dist(TVUF$Mean_pollen)
Mean_pollenTVUHdist <- dist(TVUH$Mean_pollen)

mantel(composicioTVUF$bray, Ratio_Heterosp_HomospdistTVUF, method = "pearson", permutations = 999, na.rm = FALSE)
mantel(composicioTVUH$bray, Ratio_Heterosp_HomospdistTVUH, method = "pearson", permutations = 999, na.rm = FALSE)
mantel(composicioROF$bray, Ratio_Heterosp_HomospdistROF, method = "pearson", permutations = 999, na.rm = FALSE)

mantel(composicioROF$bray, Mean_pollenROFdist, method = "pearson", permutations = 999, na.rm = FALSE)
mantel(composicioTVUF$bray, Mean_pollenTVUFdist, method = "pearson", permutations = 999, na.rm = FALSE)
mantel(composicioTVUH$bray, Mean_pollenTVUHdist, method = "pearson", permutations = 999, na.rm = FALSE)



# PCA
TVUFfinal <- TVUF[complete.cases(TVUF), ]
TVUF.pca <- prcomp(TVUFfinal, center = TRUE,scale. = TRUE)
summary(TVUF.pca)
biplot(TVUF.pca, scale = 0)

### GLM
ROF <- filter(dataanalysis, Species =="TVUF")
lm1 <- lm(ROF$Pollinated_ovules ~ ROF$Mean_Homospecific)
summary(lm1)
plot(lm1)
glm1 <- glm(ROF$Pollinated_ovules ~ ROF$Mean_Homospecific, family = quasi(link = "identity", variance = "constant"))
summary(glm1)
plot(glm1)
hist(glm1$residuals)
plot(ROF$Pollinated_ovules ~ ROF$Mean_Homospecific)
