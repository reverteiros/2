
source("funcionalitat/netejar dades.R")


library(ggplot2)
library(ggExtra)
library(corrplot)
library(Hmisc)
# install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")


dataclean <- select(dataanalysis, Species, Plot, Flowers_with_pollen, Mean_Homospecific, Mean_Heterospecific, 
                    Mean_weigth_viables, Seed_set, Avorted_fruits, Avorted_total, Fruit_set,
                    Pollinator_richness, Diversity, HB_Visitation_rate, Wild_Visitation_rate, H2, 
                    Shannon_diversity, d, weighted.closeness, Proportion_HB,
                    Flower_relative_abundance ) 

## boxplots
ggplot(dataanalysis, aes(y=Heterospecific_only, x=Species)) +
  geom_boxplot() +
  theme_classic()

#### TEMPERATURA I BITXOS
ggplot(dataanalysis) +
  geom_point(aes(T_Max,Wild_Visitation_rate, colour=Species)) + 
  geom_smooth(aes(T_Max,Wild_Visitation_rate, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="T_Max", y="Wild_Visitation_rate")+
  scale_colour_manual(values = c("red", "blue", "green3"))

ggplot(dataanalysis) +
  geom_point(aes(T_Max,HB_Visitation_rate, colour=Species)) + 
  geom_smooth(aes(T_Max,HB_Visitation_rate, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="T_Max", y="HB_Visitation_rate")+
  scale_colour_manual(values = c("red", "blue", "green3"))

## grafic proporcio apis i wild
TVUF <- filter(datafunction, Species =="ROF") %>%
  mutate(Honeybees = HB_Visitation_rate/Visitation_rate) %>%
  mutate(Wild = Wild_Visitation_rate/Visitation_rate) %>%
  select(Honeybees, Wild) %>%
  gather(Pollinator,Proportion,-Plot)

### heterospecific
ROFnetwork <- filter(dataanalysis, Species =="TVUH")%>%
  select(Flowers_with_Heterospecific,Heterospecific_only,Mean_Heterospecific)
ROFnetwork <- as.data.frame(ROFnetwork)
ROFnetwork <- ROFnetwork[,-1]
chart.Correlation(ROFnetwork, histogram=TRUE, pch=19)

ggplot(dataanalysis) +
  geom_jitter(aes(Flowers_with_Heterospecific,Heterospecific_only, colour=Species)) +
  geom_smooth(aes(Flowers_with_Heterospecific,Heterospecific_only, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="Flowers_with_Heterospecific", y="Heterospecific_only")+
  scale_colour_manual(values = c("red", "blue", "green3"))

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

------------------------------------------------------------------------------------------------

#### ROF
ROF <- filter(dataanalysis, Species =="ROF")%>%
  select(Mean_Homospecific,Mean_Heterospecific,Pollinator_richness,Diversity,HB_Visitation_rate,Wild_Visitation_rate,H2,Shannon_diversity,d,weighted.closeness) 

ROF <- as.data.frame(ROF) %>%
  select(.,-Plot)
ROF <- ROF[-29,]

# PCA
ROFfinal <- ROF[complete.cases(ROF), ]
ROF.pca <- prcomp(ROFfinal, center = TRUE,scale. = TRUE)
summary(ROF.pca)
biplot(ROF.pca, scale = 0)
# Corrplot
res2<-rcorr(as.matrix(ROF))
corrplot(res2$r, type="upper", order="hclust",
         p.mat = res2$P, sig.level = 0.05, insig = "blank")

ROFnetwork <- filter(dataanalysis, Species =="ROF")%>%
  select(Pollinator_richness,Diversity,Wild_Visitation_rate,HB_Visitation_rate,H2,Shannon_diversity,d,weighted.closeness,Proportion_HB,Mean_Homospecific,Mean_Heterospecific,Flower_relative_abundance)
ROFnetwork <- as.data.frame(ROFnetwork)
ROFnetwork <- ROFnetwork[,-1]
chart.Correlation(ROFnetwork, histogram=TRUE, pch=19)




#### TVUF
TVUF <- filter(dataanalysis, Species =="TVUF")%>%
  select(Mean_Homospecific,Mean_Heterospecific,Pollinator_richness,Diversity,Wild_Visitation_rate,HB_Visitation_rate,Seed_set,Fruit_set,Avorted,Flowers_with_pollen,Pollinated_ovules,Mean_weigth_viables,H2,Shannon_diversity,d,weighted.closeness)
TVUF <- as.data.frame(TVUF) %>%
  select(.,-Plot)

# PCA
TVUFfinal <- TVUF[complete.cases(TVUF), ]
TVUF.pca <- prcomp(TVUFfinal, center = TRUE,scale. = TRUE)
summary(TVUF.pca)
biplot(TVUF.pca, scale = 0)
# Corrplot
res2<-rcorr(as.matrix(TVUF))
corrplot(res2$r, type="upper", order="hclust",
         p.mat = res2$P, sig.level = 0.05, insig = "blank")
# Taula de correlacions amb grafics
# install.packages("PerformanceAnalytics")
TVUFnetwork <- filter(dataanalysis, Species =="TVUF")%>%
  select(Pollinator_richness,Diversity,Wild_Visitation_rate,HB_Visitation_rate,H2,Shannon_diversity,d,weighted.closeness,Flower_relative_abundance)
TVUFnetwork2 <- as.data.frame(TVUFnetwork)
TVUFnetwork2 <- TVUFnetwork2[,-1]
TVUFnetwork2$proporcioF <- flors$proporcioF
chart.Correlation(TVUFnetwork2, histogram=TRUE, pch=19)

TVUFfunction <- filter(dataanalysis, Species =="TVUF")%>%
  select(Flowers_with_pollen,Mean_Homospecific,Mean_Heterospecific,Seed_set, Pollinated_ovules, Fruit_set, Avorted_fruits,Avorted_total,Mean_weigth_viables,Seed_viability,Flower_relative_abundance)
TVUFfunction2 <- as.data.frame(TVUFfunction)
TVUFfunction2 <- TVUFfunction2[,-1]
TVUFfunction2$proporcioF <- flors$proporcioF
chart.Correlation(TVUFfunction2, histogram=TRUE, pch=19)

TVUFeffects <- filter(dataanalysis, Species =="TVUF")%>%
  select(Pollinator_richness,Wild_Visitation_rate,HB_Visitation_rate,H2,d,weighted.closeness,Flowers_with_pollen,Mean_Homospecific,Mean_Heterospecific,Seed_set, Fruit_set, Avorted_fruits,Avorted_total,Mean_weigth_viables)
TVUFeffects <- as.data.frame(TVUFeffects)
TVUFeffects <- TVUFeffects[,-1]
chart.Correlation(TVUFeffects, histogram=TRUE, pch=19)


#### TVUH
TVUH <- filter(dataanalysis, Species =="TVUH") %>%
  select(Mean_Homospecific,Mean_Heterospecific,Pollinator_richness,Diversity,Wild_Visitation_rate,HB_Visitation_rate,Seed_set,Fruit_set,Pollinated_ovules,Avorted,Mean_weigth_viables,Seed_viability,H2,Shannon_diversity,d,weighted.closeness)
TVUH <- as.data.frame(TVUH) %>%
  select(.,-Plot)

# PCA
TVUHfinal <- TVUH[complete.cases(TVUH),]
TVUH.pca <- prcomp(TVUHfinal, center = TRUE,scale. = TRUE)
summary(TVUH.pca)
biplot(TVUH.pca, scale = 0)
# Corrplot
res2<-rcorr(as.matrix(TVUH))
corrplot(res2$r, type="upper", order="hclust",
         p.mat = res2$P, sig.level = 0.07, insig = "blank")
# Taula de correlacions amb grafics
TVUHnetwork <- filter(dataanalysis, Species =="TVUH")%>%
  select(Pollinator_richness,Diversity,Wild_Visitation_rate,HB_Visitation_rate,H2,Shannon_diversity,d,weighted.closeness,Flower_relative_abundance)
TVUHnetwork <- as.data.frame(TVUHnetwork)
TVUHnetwork <- TVUHnetwork[,-1]
TVUHnetwork$proporcioF <- flors$proporcioF
chart.Correlation(TVUHnetwork, histogram=TRUE, pch=19)

TVUHfunction <- filter(dataanalysis, Species =="TVUH")%>%
  select(Flowers_with_pollen,Mean_Homospecific,Mean_Heterospecific,Seed_set, Pollinated_ovules, Fruit_set, Avorted_fruits,Avorted_total,Mean_weigth_viables,Seed_viability,Flower_relative_abundance)
TVUHfunction <- as.data.frame(TVUHfunction)
TVUHfunction <- TVUHfunction[,-1]
TVUHfunction$proporcioF <- flors$proporcioF
chart.Correlation(TVUHfunction, histogram=TRUE, pch=19)

TVUHeffects <- filter(dataanalysis, Species =="TVUH")%>%
  select(Pollinator_richness,Wild_Visitation_rate,HB_Visitation_rate,H2,d,weighted.closeness,Flowers_with_pollen,Mean_Homospecific,Mean_Heterospecific,Seed_set, Fruit_set, Avorted_fruits,Avorted_total,Mean_weigth_viables)
TVUHeffects <- as.data.frame(TVUHeffects)
TVUHeffects <- TVUHeffects[,-1]
chart.Correlation(TVUHeffects, histogram=TRUE, pch=19)

