
source("funcionalitat/netejar dades.R")
source("funcionalitat/index xarxes.R")

library(ggplot2)
library(ggExtra)
library(corrplot)
library(Hmisc)
library("PerformanceAnalytics")


ggplot(dataanalysis, aes(y=Heterospecific_only, x=Species)) +
  geom_boxplot() +
  theme_classic()

ggplot(dataanalysis) +
  geom_jitter(aes(Flowers_with_Heterospecific,Heterospecific_only, colour=Species)) +
  geom_smooth(aes(Flowers_with_Heterospecific,Heterospecific_only, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="Flowers_with_Heterospecific", y="Heterospecific_only")+
  scale_colour_manual(values = c("red", "blue", "green3"))


ROFnetwork <- filter(dataanalysis, Species =="TVUH")%>%
  select(Flowers_with_Heterospecific,Heterospecific_only,Mean_Heterospecific)
ROFnetwork <- as.data.frame(ROFnetwork)
ROFnetwork <- ROFnetwork[,-1]
chart.Correlation(ROFnetwork, histogram=TRUE, pch=19)


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
  select(Pollinator_richness,Diversity,Wild_Visitation_rate,HB_Visitation_rate,H2,Shannon_diversity,d,weighted.closeness,Proportion_HB,Mean_Homospecific,Mean_Heterospecific)
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
  select(Pollinator_richness,Diversity,Wild_Visitation_rate,HB_Visitation_rate,H2,Shannon_diversity,d,weighted.closeness)
TVUFnetwork2 <- as.data.frame(TVUFnetwork)
TVUFnetwork2 <- TVUFnetwork2[,-1]
chart.Correlation(TVUFnetwork2, histogram=TRUE, pch=19)

TVUFfunction <- filter(dataanalysis, Species =="TVUF")%>%
  select(Flowers_with_pollen,Mean_Homospecific,Mean_Heterospecific,Seed_set, Pollinated_ovules, Fruit_set, Avorted_fruits,Avorted_total,Mean_weigth_viables,Seed_viability)
TVUFfunction2 <- as.data.frame(TVUFfunction)
TVUFfunction2 <- TVUFfunction2[,-1]
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
# install.packages("PerformanceAnalytics")
TVUHnetwork <- filter(dataanalysis, Species =="TVUH")%>%
  select(Pollinator_richness,Diversity,Wild_Visitation_rate,HB_Visitation_rate,H2,Shannon_diversity,d,weighted.closeness)
TVUHnetwork <- as.data.frame(TVUHnetwork)
TVUHnetwork <- TVUHnetwork[,-1]
chart.Correlation(TVUHnetwork, histogram=TRUE, pch=19)

TVUHfunction <- filter(dataanalysis, Species =="TVUH")%>%
  select(Flowers_with_pollen,Mean_Homospecific,Mean_Heterospecific,Seed_set, Pollinated_ovules, Fruit_set, Avorted_fruits,Avorted_total,Mean_weigth_viables,Seed_viability)
TVUHfunction <- as.data.frame(TVUHfunction)
TVUHfunction <- TVUHfunction[,-1]
chart.Correlation(TVUHfunction, histogram=TRUE, pch=19)

TVUHeffects <- filter(dataanalysis, Species =="TVUH")%>%
  select(Pollinator_richness,Wild_Visitation_rate,HB_Visitation_rate,H2,d,weighted.closeness,Flowers_with_pollen,Mean_Homospecific,Mean_Heterospecific,Seed_set, Fruit_set, Avorted_fruits,Avorted_total,Mean_weigth_viables)
TVUHeffects <- as.data.frame(TVUHeffects)
TVUHeffects <- TVUHeffects[,-1]
chart.Correlation(TVUHeffects, histogram=TRUE, pch=19)


#################### TEMPERATURA I BITXOS
ggplot(dataanalysis) +
  geom_jitter(aes(T_Max,Wild_Visitation_rate, colour=Species)) + 
  geom_smooth(aes(T_Max,Wild_Visitation_rate, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="T_Max", y="Wild_Visitation_rate")+
  scale_colour_manual(values = c("red", "blue", "green3"))

ggplot(dataanalysis) +
  geom_jitter(aes(T_Max,HB_Visitation_rate, colour=Species)) + 
  geom_smooth(aes(T_Max,HB_Visitation_rate, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="T_Max", y="HB_Visitation_rate")+
  scale_colour_manual(values = c("red", "blue", "green3"))

# #################### RELACIONS ENTRE VARIABLES DESCRIPTORES
# ggplot(dataanalysis) +
#   geom_jitter(aes(Pollinator_richness,Diversity, colour=Species)) + 
#   geom_smooth(aes(Pollinator_richness,Diversity, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="Pollinator_richness", y="Diversity")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(HB_Visitation_rate,Diversity, colour=Species)) + 
#   geom_smooth(aes(HB_Visitation_rate,Diversity, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="HB_Visitation_rate", y="Diversity")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(Wild_Visitation_rate,Diversity, colour=Species)) + 
#   geom_smooth(aes(Wild_Visitation_rate,Diversity, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="Wild_Visitation_rate", y="Diversity")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(d,H2, colour=Species)) + 
#   geom_smooth(aes(d,H2, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="d'", y="H2")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(Avorted,Seed_set, colour=Species)) + 
#   geom_smooth(aes(Avorted,Seed_set, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="Avorted", y="Seed_set")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(Pollinated_ovules,Seed_set, colour=Species)) + 
#   geom_smooth(aes(Pollinated_ovules,Seed_set, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="Pollinated_ovules", y="Seed_set")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(Avorted,Fruit_set, colour=Species)) + 
#   geom_smooth(aes(Avorted,Fruit_set, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="Avorted", y="Fruit_set")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(Pollinated_ovules,Fruit_set, colour=Species)) + 
#   geom_smooth(aes(Pollinated_ovules,Fruit_set, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="Pollinated_ovules", y="Fruit_set")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(Pollinated_ovules,Avorted, colour=Species)) + 
#   geom_smooth(aes(Pollinated_ovules,Avorted, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="Pollinated_ovules", y="Avorted")+
#   scale_colour_manual(values = c("red", "blue", "green3"))+
#   ylim(0, 2.5)
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(Mean_Homospecific,Mean_Heterospecific, colour=Species)) + 
#   geom_smooth(aes(Mean_Homospecific,Mean_Heterospecific, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="Mean_Homospecific", y="Mean_Heterospecific")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
#
################## COMMUNITY DESCRIPTORS AND HOMOSPECIFIC
# ggplot(dataanalysis) +
#   geom_jitter(aes(Wild_Visitation_rate,Mean_Homospecific, colour=Species)) +
#   geom_smooth(aes(Wild_Visitation_rate,Mean_Homospecific, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="Wild_Visitation_rate", y="Mean_Homospecific")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(Diversity,Mean_Homospecific, colour=Species)) + 
#   geom_smooth(aes(Diversity,Mean_Homospecific, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="Diversity", y="Mean_Homospecific")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(Pollinator_richness,Mean_Homospecific, colour=Species)) + 
#   geom_smooth(aes(Pollinator_richness,Mean_Homospecific, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="Pollinator_richness", y="Mean_Homospecific")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(HB_Visitation_rate,Mean_Homospecific, colour=Species)) + 
#   geom_smooth(aes(HB_Visitation_rate,Mean_Homospecific, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="HB_Visitation_rate", y="Mean_Homospecific")+
#   scale_colour_manual(values = c("red", "blue", "green3"))+
#   xlim(0, 8)
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(d,Mean_Homospecific, colour=Species)) + 
#   geom_smooth(aes(d,Mean_Homospecific, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="d'", y="Mean_Homospecific")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(H2,Mean_Homospecific, colour=Species)) + 
#   geom_smooth(aes(H2,Mean_Homospecific, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="H2", y="Mean_Homospecific")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# #################### NETWORK SPECIALIZATION AND HETEROSPECIFIC
# ggplot(dataanalysis) +
#   geom_jitter(aes(weighted.closeness,Mean_Heterospecific, colour=Species)) + 
#   geom_smooth(aes(weighted.closeness,Mean_Heterospecific, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="weighted.closeness", y="Mean_Heterospecific")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(H2,Mean_Heterospecific, colour=Species)) + 
#   geom_smooth(aes(H2,Mean_Heterospecific, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="H2", y="Mean_Heterospecific")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(d,Mean_Heterospecific, colour=Species)) + 
#   geom_smooth(aes(d,Mean_Heterospecific, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="d'", y="Mean_Heterospecific")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(Shannon_diversity,Mean_Heterospecific, colour=Species)) + 
#   geom_smooth(aes(Shannon_diversity,Mean_Heterospecific, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="Shannon_diversity", y="Mean_Heterospecific")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(Diversity,Mean_Heterospecific, colour=Species)) + 
#   geom_smooth(aes(Diversity,Mean_Heterospecific, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="Diversity", y="Mean_Heterospecific")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(Pollinator_richness,Mean_Heterospecific, colour=Species)) + 
#   geom_smooth(aes(Pollinator_richness,Mean_Heterospecific, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="Pollinator_richness", y="Mean_Heterospecific")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# 
# ################### HOMOSPECIFIC AND POLLINATION SUCCESS
# ggplot(dataanalysis) +
#   geom_jitter(aes(Mean_Homospecific,Pollinated_ovules, colour=Species)) + 
#   geom_smooth(aes(Mean_Homospecific,Pollinated_ovules, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="Mean_Homospecific", y="Pollinated_ovules")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(Mean_Homospecific,Avorted, colour=Species)) + 
#   geom_smooth(aes(Mean_Homospecific,Avorted, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="Mean_Homospecific", y="Avorted")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(Mean_Homospecific,Seed_set, colour=Species)) + 
#   geom_smooth(aes(Mean_Homospecific,Seed_set, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="Mean_Homospecific", y="Seed_set")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ################### HETEROSPECIFIC AND POLLINATION SUCCESS
# ggplot(dataanalysis) +
#   geom_jitter(aes(Mean_Heterospecific,Pollinated_ovules, colour=Species)) + 
#   geom_smooth(aes(Mean_Heterospecific,Pollinated_ovules, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="Mean_Heterospecific", y="Pollinated_ovules")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(Mean_Heterospecific,Avorted, colour=Species)) + 
#   geom_smooth(aes(Mean_Heterospecific,Avorted, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="Mean_Heterospecific", y="Avorted")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(Mean_Heterospecific,Seed_set, colour=Species)) + 
#   geom_smooth(aes(Mean_Heterospecific,Seed_set, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="Mean_Heterospecific", y="Seed_set")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ################## HONEYBEES AND POLLINATIOON SUCCESS
# ggplot(dataanalysis) +
#   geom_jitter(aes(HB_Visitation_rate,Pollinated_ovules, colour=Species)) + 
#   geom_smooth(aes(HB_Visitation_rate,Pollinated_ovules, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="HB_Visitation_rate", y="Pollinated_ovules")+
#   scale_colour_manual(values = c("red", "blue", "green3"))+
#   xlim(0, 8)
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(HB_Visitation_rate,Avorted, colour=Species)) + 
#   geom_smooth(aes(HB_Visitation_rate,Avorted, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="HB_Visitation_rate", y="Avorted")+
#   scale_colour_manual(values = c("red", "blue", "green3"))+
#   xlim(0, 8)
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(HB_Visitation_rate,Seed_set, colour=Species)) + 
#   geom_smooth(aes(HB_Visitation_rate,Seed_set, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="HB_Visitation_rate", y="Seed_set")+
#   scale_colour_manual(values = c("red", "blue", "green3"))+
#   xlim(0, 8)
# 
# ################### WILD POLLINATORS AND POLLINATION SUCCESS
# ggplot(dataanalysis) +
#   geom_jitter(aes(Wild_Visitation_rate,Pollinated_ovules, colour=Species)) + 
#   geom_smooth(aes(Wild_Visitation_rate,Pollinated_ovules, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="Wild_Visitation_rate", y="Pollinated_ovules")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(Wild_Visitation_rate,Avorted, colour=Species)) + 
#   geom_smooth(aes(Wild_Visitation_rate,Avorted, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="Wild_Visitation_rate", y="Avorted")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(Wild_Visitation_rate,Seed_set, colour=Species)) + 
#   geom_smooth(aes(Wild_Visitation_rate,Seed_set, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="Wild_Visitation_rate", y="Seed_set")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# #################### NETWORK METRICS AND POLLLINATION SUCCESS
# ggplot(dataanalysis) +
#   geom_jitter(aes(H2,Pollinated_ovules, colour=Species)) + 
#   geom_smooth(aes(H2,Pollinated_ovules, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="H2", y="Pollinated_ovules")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(H2,Avorted, colour=Species)) + 
#   geom_smooth(aes(H2,Avorted, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="H2", y="Avorted")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(H2,Seed_set, colour=Species)) + 
#   geom_smooth(aes(H2,Seed_set, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="H2", y="Seed_set")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(weighted.closeness,Pollinated_ovules, colour=Species)) + 
#   geom_smooth(aes(weighted.closeness,Pollinated_ovules, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="weighted.closeness", y="Pollinated_ovules")+
#   scale_colour_manual(values = c("red", "blue", "green3"))+
#   xlim(0, 1.5)
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(weighted.closeness,Avorted, colour=Species)) + 
#   geom_smooth(aes(weighted.closeness,Avorted, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="weighted.closeness", y="Avorted")+
#   scale_colour_manual(values = c("red", "blue", "green3"))+
#   xlim(0, 1.5)
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(weighted.closeness,Seed_set, colour=Species)) + 
#   geom_smooth(aes(weighted.closeness,Seed_set, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="weighted.closeness", y="Seed_set")+
#   scale_colour_manual(values = c("red", "blue", "green3"))+
#   xlim(0, 1.5)
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(Shannon_diversity,Pollinated_ovules, colour=Species)) + 
#   geom_smooth(aes(Shannon_diversity,Pollinated_ovules, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="Shannon_diversity", y="Pollinated_ovules")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(Shannon_diversity,Avorted, colour=Species)) + 
#   geom_smooth(aes(Shannon_diversity,Avorted, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="Shannon_diversity", y="Avorted")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(Shannon_diversity,Seed_set, colour=Species)) + 
#   geom_smooth(aes(Shannon_diversity,Seed_set, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="Shannon_diversity", y="Seed_set")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(d,Pollinated_ovules, colour=Species)) + 
#   geom_smooth(aes(d,Pollinated_ovules, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="d'", y="Pollinated_ovules")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(d,Avorted, colour=Species)) + 
#   geom_smooth(aes(d,Avorted, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="d'", y="Avorted")+
#   scale_colour_manual(values = c("red", "blue", "green3"))
# 
# ggplot(dataanalysis) +
#   geom_jitter(aes(d,Seed_set, colour=Species)) + 
#   geom_smooth(aes(d,Seed_set, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="d'", y="Seed_set")+
#   scale_colour_manual(values = c("red", "blue", "green3"))



ggplot(TVUF, aes(y=Diversity, x=Pollinator_richness)) + 
  geom_point() + 
  theme_classic() +
  labs(title = "TVUH")


## grafic proporcio apis i wild
TVUF <- filter(datafunction, Species =="ROF") %>%
  mutate(Honeybees = HB_Visitation_rate/Visitation_rate) %>%
  mutate(Wild = Wild_Visitation_rate/Visitation_rate) %>%
  select(Honeybees, Wild) %>%
  gather(Pollinator,Proportion,-Plot)

ggplot(TVUF, aes(y=Proportion, x=Pollinator)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(title = "ROF")

