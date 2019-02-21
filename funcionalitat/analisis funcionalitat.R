
source("funcionalitat/netejar dades.R")
source("funcionalitat/index xarxes.R")

library(ggplot2)
library(ggExtra)
library(corrplot)
library(Hmisc)
library(devtools)
# install_github("vqv/ggbiplot")
library(ggbiplot)

datafunction <- datafunction %>%
  mutate(Proportion_Homospecific = Mean_Homospecific / Mean_pollen) %>%
  mutate(Proportion_Heterospecific = Mean_Heterospecific / Mean_pollen)


#### ROF
ROF <- filter(datafunction, Species =="ROF") %>%
  left_join(networkmetrics, by="Plot") %>%
  select(Mean_Homospecific,Mean_Heterospecific,Pollinator_richness,Diversity,HB_Visitation_rate,Wild_Visitation_rate,H2,Shannon_diversity) 

ROF <- as.data.frame(ROF) %>%
  select(.,-Plot)
ROF$T_Max <- database2$T_Max
ROF$dROF <- unlist(networkmetrics$dROF)
ROF$dApis <- unlist(networkmetrics$dApis)
ROF <- ROF[-29,]

# PCA
ROF.pca <- prcomp(ROF, center = TRUE,scale. = TRUE)
summary(ROF.pca)
biplot(ROF.pca, scale = 0)
# Corrplot
res2<-rcorr(as.matrix(ROF))
corrplot(res2$r, type="upper", order="hclust",
         p.mat = res2$P, sig.level = 0.07, insig = "blank")


#### TVUF
TVUF <- filter(datafunction, Species =="TVUF")%>%
  left_join(networkmetrics, by="Plot") %>%
  select(Mean_Homospecific,Mean_Heterospecific,Pollinator_richness,Diversity,Wild_Visitation_rate,HB_Visitation_rate,Seed_set,Pollinated_ovules,Avorted,Mean_weigth_viables,H2,Shannon_diversity)
TVUF <- as.data.frame(TVUF) %>%
  select(.,-Plot)
TVUF$T_Max <- database2$T_Max
TVUF$dApis <- unlist(networkmetrics$dApis)
TVUF$dTVUF <- unlist(networkmetrics$dTVUF)

# PCA
TVUF.pca <- prcomp(TVUF, center = TRUE,scale. = TRUE)
summary(TVUF.pca)
biplot(TVUF.pca, scale = 0)
# Corrplot
res2<-rcorr(as.matrix(TVUF))
corrplot(res2$r, type="upper", order="hclust",
         p.mat = res2$P, sig.level = 0.07, insig = "blank")


#### TVUH
TVUH <- filter(datafunction, Species =="TVUH")%>%
  left_join(networkmetrics, by="Plot") %>%
  select(Mean_Homospecific,Mean_Heterospecific,Pollinator_richness,Diversity,Wild_Visitation_rate,HB_Visitation_rate,Seed_set,Pollinated_ovules,Avorted,Mean_weigth_viables,Seed_viability,H2,Shannon_diversity)
TVUH <- as.data.frame(TVUH) %>%
  select(.,-Plot)
TVUH$T_Max <- database2$T_Max
TVUH$dApis <- unlist(networkmetrics$dApis)
TVUH$dTVUH <- unlist(networkmetrics$dTVUH)

# PCA
TVUH <- TVUH[-c(7,18,23,25,28),]
TVUH.pca <- prcomp(TVUH, center = TRUE,scale. = TRUE)
summary(TVUH.pca)
biplot(TVUH.pca, scale = 0)
# Corrplot
res2<-rcorr(as.matrix(TVUH))
corrplot(res2$r, type="upper", order="hclust",
         p.mat = res2$P, sig.level = 0.07, insig = "blank")




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
  geom_point() + 
  theme_classic() +
  labs(title = "ROF")


a <- lm(TVUF$Pollinator_richness~TVUF$Mean_Heterospecific)
summary(a)
plot(TVUF$Pollinator_richness~TVUF$Mean_Heterospecific, pch=20, col="black")
abline(a)

###### Correlation matrix between network indices and temperature
database2 <- read.table("dades/Database3.txt",header=T)

networkmetrics$T_Max <- database2$T_Max
networkmetrics <- networkmetrics[,-4]#remove plot column
res2<-rcorr(as.matrix(networkmetrics))
corrplot(res2$r, type="upper", order="hclust",
         p.mat = res2$P, sig.level = 0.1, insig = "blank")

length(!is.na(networkmetrics$dTVUH))

networkmetrics$Plot <- c(1:40)

datafunctionalitynetwork <- datafunction %>%
  dplyr::left_join(., networkmetrics, by = c("Plot")) 

ROF <- filter(datafunctionalitynetwork, Species =="ROF")
TVUF <- filter(datafunctionalitynetwork, Species =="TVUF")
TVUH <- filter(datafunctionalitynetwork, Species =="TVUH")


ggplot(TVUH, aes(y=Mean_Homospecific, x=as.numeric(dTVUH))) + 
  geom_point(alpha=0.3) + 
  geom_smooth() +
  theme_classic() 



# 
# ### Shannon diversity is highly correlated with everything. Remove
# networkmetrics <- networkmetrics %>%
#   select(-Shannon_diversity)
# 
# res2<-rcorr(as.matrix(networkmetrics))
# corrplot(res2$r, type="upper", order="hclust", 
#          p.mat = res2$P, sig.level = 0.01, insig = "blank")
# 
# ## d' ROF is correlated with everything too....
# 
# plot(unlist(networkmetrics$dROF)~unlist(networkmetrics$dTVUF))
# plot(unlist(networkmetrics$dROF)~unlist(networkmetrics$dTVUH))
# plot(unlist(networkmetrics$dTVUH)~unlist(networkmetrics$dTVUF))

## proporcio femelles de thymus
# hist(flors$proporcioF)


# ### Anàlisis a nivell de planta
# 
# pollen <- group_by(pollentotal, Plot, Species, Plant) %>%
#   summarise(Samples_pollen=n(),Flowers_with_pollen=mean(Pollen_presence),
#             Mean_pollen=mean(Total),Mean_Homospecific=mean(Homospecific),
#             Mean_Heterospecific=mean(Heterospecific))%>%
#   complete(Species, Plot) %>%
#   distinct()
# 
# fruits <- droplevels(dplyr::filter(seedsraw, !is.na(Avorted) & Total == 4)) %>%
#   mutate(Pollinated = Avorted + Seed) %>%
#   mutate(Proportion_avorted = Avorted / Pollinated) %>%
#   mutate(Fruits = if_else(Seed > 0, 1,0)) %>%
#   group_by(Plot, Species, Plant) %>%
#   summarise(Samples_seeds=n(),Fruits=sum(Fruits),Percent_pollination=(mean(Pollinated)/4*100),Proportion_avorted=mean(Proportion_avorted))%>%
#   mutate(Fruit_set=(Fruits/Samples_seeds)) %>%
#   select(., -c(Fruits)) %>%
#   complete(Species, Plot, Plant) %>%
#   distinct()
# 
# fruitandseedset <- droplevels(dplyr::filter(seedsraw, !is.na(Avorted) & Total == 4)) %>%
#   mutate(Pollinated = Avorted + Seed) %>%
#   mutate(Fruits = if_else(Seed > 0, 1,0)) %>%
#   filter(.,Fruits==1) %>%
#   group_by(Plot, Species, Plant) %>%
#   summarise(Seed_set=mean(Seed))%>%
#   left_join(fruits, by = c("Plot","Species","Plant"))%>%
#   complete(Species, Plot, Plant) %>%
#   distinct()
# 
# 
# datafunctionalityperplant <- pollen %>%
#   dplyr::left_join(., fruitandseedset, by = c("Species","Plot","Plant"))
# 


