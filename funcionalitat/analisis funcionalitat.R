
source("funcionalitat/netejar dades2.R")
source("funcionalitat/index xarxes.R")

library(ggplot2)
library(ggExtra)
library(corrplot)
library(Hmisc)


dataanalysis <- datafunction %>%
  left_join(dprime,by=c("Plot","Species")) %>%
  left_join(closenesss,by=c("Plot","Species"))%>%
  mutate(Proportion_Homospecific = Mean_Homospecific / Mean_pollen) %>%
  mutate(Proportion_Heterospecific = Mean_Heterospecific / Mean_pollen)%>%
  left_join(networkmetrics, by="Plot") 


#### ROF
ROF <- filter(dataanalysis, Species =="ROF")%>%
  select(Mean_Homospecific,Mean_Heterospecific,Pollinator_richness,Diversity,HB_Visitation_rate,Wild_Visitation_rate,H2,Shannon_diversity,d,weighted.closeness) 

ROF <- as.data.frame(ROF) %>%
  select(.,-Plot)
ROF$T_Max <- database2$T_Max
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


#### TVUF
TVUF <- filter(dataanalysis, Species =="TVUF")%>%
  select(Mean_Homospecific,Mean_Heterospecific,Pollinator_richness,Diversity,Wild_Visitation_rate,HB_Visitation_rate,Seed_set,Pollinated_ovules,Avorted,Mean_weigth_viables,H2,Shannon_diversity,d,weighted.closeness)
TVUF <- as.data.frame(TVUF) %>%
  select(.,-Plot)
TVUF$T_Max <- database2$T_Max

# PCA
TVUFfinal <- TVUF[complete.cases(TVUF), ]
TVUF.pca <- prcomp(TVUFfinal, center = TRUE,scale. = TRUE)
summary(TVUF.pca)
biplot(TVUF.pca, scale = 0)
# Corrplot
res2<-rcorr(as.matrix(TVUF))
corrplot(res2$r, type="upper", order="hclust",
         p.mat = res2$P, sig.level = 0.05, insig = "blank")


#### TVUH
TVUH <- filter(dataanalysis, Species =="TVUH") %>%
  select(Mean_Homospecific,Mean_Heterospecific,Pollinator_richness,Diversity,Wild_Visitation_rate,HB_Visitation_rate,Seed_set,Pollinated_ovules,Avorted,Mean_weigth_viables,Seed_viability,H2,Shannon_diversity,d,weighted.closeness)
TVUH <- as.data.frame(TVUH) %>%
  select(.,-Plot)
TVUH$T_Max <- database2$T_Max

# PCA
TVUHfinal <- TVUH[complete.cases(TVUH),]
TVUH.pca <- prcomp(TVUHfinal, center = TRUE,scale. = TRUE)
summary(TVUH.pca)
biplot(TVUH.pca, scale = 0)
# Corrplot
res2<-rcorr(as.matrix(TVUH))
corrplot(res2$r, type="upper", order="hclust",
         p.mat = res2$P, sig.level = 0.07, insig = "blank")


#################### COMMUNITY DESCRIPTORS AND HOMOSPECIFIC
ggplot(dataanalysis) +
  geom_jitter(aes(Wild_Visitation_rate,Mean_Homospecific, colour=Species)) + 
  geom_smooth(aes(Wild_Visitation_rate,Mean_Homospecific, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="Wild_Visitation_rate", y="Mean_Homospecific")

ggplot(dataanalysis) +
  geom_jitter(aes(Diversity,Mean_Homospecific, colour=Species)) + 
  geom_smooth(aes(Diversity,Mean_Homospecific, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="Diversity", y="Mean_Homospecific")

ggplot(dataanalysis) +
  geom_jitter(aes(Pollinator_richness,Mean_Homospecific, colour=Species)) + 
  geom_smooth(aes(Pollinator_richness,Mean_Homospecific, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="Pollinator_richness", y="Mean_Homospecific")

ggplot(dataanalysis) +
  geom_jitter(aes(HB_Visitation_rate,Mean_Homospecific, colour=Species)) + 
  geom_smooth(aes(HB_Visitation_rate,Mean_Homospecific, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="HB_Visitation_rate", y="Mean_Homospecific")

ggplot(dataanalysis) +
  geom_jitter(aes(d,Mean_Homospecific, colour=Species)) + 
  geom_smooth(aes(d,Mean_Homospecific, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="d'", y="Mean_Homospecific")

ggplot(dataanalysis) +
  geom_jitter(aes(H2,Mean_Homospecific, colour=Species)) + 
  geom_smooth(aes(H2,Mean_Homospecific, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="H2", y="Mean_Homospecific")

#################### NETWORK SPECIALIZATION AND HETEROSPECIFIC
ggplot(dataanalysis) +
  geom_jitter(aes(weighted.closeness,Mean_Heterospecific, colour=Species)) + 
  geom_smooth(aes(weighted.closeness,Mean_Heterospecific, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="weighted.closeness", y="Mean_Heterospecific")

ggplot(dataanalysis) +
  geom_jitter(aes(H2,Mean_Heterospecific, colour=Species)) + 
  geom_smooth(aes(H2,Mean_Heterospecific, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="H2", y="Mean_Heterospecific")

ggplot(dataanalysis) +
  geom_jitter(aes(d,Mean_Heterospecific, colour=Species)) + 
  geom_smooth(aes(d,Mean_Heterospecific, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="d'", y="Mean_Heterospecific")

ggplot(dataanalysis) +
  geom_jitter(aes(Shannon_diversity,Mean_Heterospecific, colour=Species)) + 
  geom_smooth(aes(Shannon_diversity,Mean_Heterospecific, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="Shannon_diversity", y="Mean_Heterospecific")

ggplot(dataanalysis) +
  geom_jitter(aes(Diversity,Mean_Heterospecific, colour=Species)) + 
  geom_smooth(aes(Diversity,Mean_Heterospecific, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="Diversity", y="Mean_Heterospecific")

ggplot(dataanalysis) +
  geom_jitter(aes(Pollinator_richness,Mean_Heterospecific, colour=Species)) + 
  geom_smooth(aes(Pollinator_richness,Mean_Heterospecific, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="Pollinator_richness", y="Mean_Heterospecific")


################### HOMOSPECIFIC AND POLLINATION SUCCESS
ggplot(dataanalysis) +
  geom_jitter(aes(Mean_Homospecific,Pollinated_ovules, colour=Species)) + 
  geom_smooth(aes(Mean_Homospecific,Pollinated_ovules, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="Mean_Homospecific", y="Pollinated_ovules")

ggplot(dataanalysis) +
  geom_jitter(aes(Mean_Homospecific,Avorted, colour=Species)) + 
  geom_smooth(aes(Mean_Homospecific,Avorted, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="Mean_Homospecific", y="Avorted")

ggplot(dataanalysis) +
  geom_jitter(aes(Mean_Homospecific,Seed_set, colour=Species)) + 
  geom_smooth(aes(Mean_Homospecific,Seed_set, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="Mean_Homospecific", y="Seed_set")

################### HETEROSPECIFIC AND POLLINATION SUCCESS
ggplot(dataanalysis) +
  geom_jitter(aes(Mean_Heterospecific,Pollinated_ovules, colour=Species)) + 
  geom_smooth(aes(Mean_Heterospecific,Pollinated_ovules, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="Mean_Heterospecific", y="Pollinated_ovules")

ggplot(dataanalysis) +
  geom_jitter(aes(Mean_Heterospecific,Avorted, colour=Species)) + 
  geom_smooth(aes(Mean_Heterospecific,Avorted, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="Mean_Heterospecific", y="Avorted")

ggplot(dataanalysis) +
  geom_jitter(aes(Mean_Heterospecific,Seed_set, colour=Species)) + 
  geom_smooth(aes(Mean_Heterospecific,Seed_set, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="Mean_Heterospecific", y="Seed_set")

################## HONEYBEES AND POLLINATIOON SUCCESS
ggplot(dataanalysis) +
  geom_jitter(aes(HB_Visitation_rate,Pollinated_ovules, colour=Species)) + 
  geom_smooth(aes(HB_Visitation_rate,Pollinated_ovules, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="HB_Visitation_rate", y="Pollinated_ovules")

ggplot(dataanalysis) +
  geom_jitter(aes(HB_Visitation_rate,Avorted, colour=Species)) + 
  geom_smooth(aes(HB_Visitation_rate,Avorted, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="HB_Visitation_rate", y="Avorted")

ggplot(dataanalysis) +
  geom_jitter(aes(HB_Visitation_rate,Seed_set, colour=Species)) + 
  geom_smooth(aes(HB_Visitation_rate,Seed_set, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="HB_Visitation_rate", y="Seed_set")

################### WILD POLLINATORS AND POLLINATION SUCCESS
ggplot(dataanalysis) +
  geom_jitter(aes(Wild_Visitation_rate,Pollinated_ovules, colour=Species)) + 
  geom_smooth(aes(Wild_Visitation_rate,Pollinated_ovules, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="Wild_Visitation_rate", y="Pollinated_ovules")

ggplot(dataanalysis) +
  geom_jitter(aes(Wild_Visitation_rate,Avorted, colour=Species)) + 
  geom_smooth(aes(Wild_Visitation_rate,Avorted, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="Wild_Visitation_rate", y="Avorted")

ggplot(dataanalysis) +
  geom_jitter(aes(Wild_Visitation_rate,Seed_set, colour=Species)) + 
  geom_smooth(aes(Wild_Visitation_rate,Seed_set, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="Wild_Visitation_rate", y="Seed_set")

#################### NETWORK METRICS AND POLLLINATION SUCCESS
ggplot(dataanalysis) +
  geom_jitter(aes(H2,Pollinated_ovules, colour=Species)) + 
  geom_smooth(aes(H2,Pollinated_ovules, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="H2", y="Pollinated_ovules")

ggplot(dataanalysis) +
  geom_jitter(aes(H2,Avorted, colour=Species)) + 
  geom_smooth(aes(H2,Avorted, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="H2", y="Avorted")

ggplot(dataanalysis) +
  geom_jitter(aes(H2,Seed_set, colour=Species)) + 
  geom_smooth(aes(H2,Seed_set, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="H2", y="Seed_set")

ggplot(dataanalysis) +
  geom_jitter(aes(weighted.closeness,Pollinated_ovules, colour=Species)) + 
  geom_smooth(aes(weighted.closeness,Pollinated_ovules, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="weighted.closeness", y="Pollinated_ovules")

ggplot(dataanalysis) +
  geom_jitter(aes(weighted.closeness,Avorted, colour=Species)) + 
  geom_smooth(aes(weighted.closeness,Avorted, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="weighted.closeness", y="Avorted")

ggplot(dataanalysis) +
  geom_jitter(aes(weighted.closeness,Seed_set, colour=Species)) + 
  geom_smooth(aes(weighted.closeness,Seed_set, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="weighted.closeness", y="Seed_set")

ggplot(dataanalysis) +
  geom_jitter(aes(Shannon_diversity,Pollinated_ovules, colour=Species)) + 
  geom_smooth(aes(Shannon_diversity,Pollinated_ovules, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="Shannon_diversity", y="Pollinated_ovules")

ggplot(dataanalysis) +
  geom_jitter(aes(Shannon_diversity,Avorted, colour=Species)) + 
  geom_smooth(aes(Shannon_diversity,Avorted, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="Shannon_diversity", y="Avorted")

ggplot(dataanalysis) +
  geom_jitter(aes(Shannon_diversity,Seed_set, colour=Species)) + 
  geom_smooth(aes(Shannon_diversity,Seed_set, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="Shannon_diversity", y="Seed_set")

ggplot(dataanalysis) +
  geom_jitter(aes(d,Pollinated_ovules, colour=Species)) + 
  geom_smooth(aes(d,Pollinated_ovules, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="d'", y="Pollinated_ovules")

ggplot(dataanalysis) +
  geom_jitter(aes(d,Avorted, colour=Species)) + 
  geom_smooth(aes(d,Avorted, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="d'", y="Avorted")

ggplot(dataanalysis) +
  geom_jitter(aes(d,Seed_set, colour=Species)) + 
  geom_smooth(aes(d,Seed_set, colour=Species), method=lm, se=FALSE) +
  theme_classic() +
  labs(x="d'", y="Seed_set")



ggplot(datafunction, aes(y=Avorted, x=Species)) + 
  geom_boxplot() + 
  theme_classic() +
  ylim(0,4)

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

