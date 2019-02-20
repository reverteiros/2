
source("funcionalitat/netejar dades.R")
source("funcionalitat/index xarxes.R")

library(ggplot2)
library(ggExtra)
library(corrplot)
library(Hmisc)

### Anàlisis a nivell de planta 

pollen <- group_by(pollentotal, Plot, Species, Plant) %>% 
  summarise(Samples_pollen=n(),Flowers_with_pollen=mean(Pollen_presence),
            Mean_pollen=mean(Total),Mean_Homospecific=mean(Homospecific),
            Mean_Heterospecific=mean(Heterospecific))%>%
  complete(Species, Plot) %>%
  distinct() 

fruits <- droplevels(dplyr::filter(seedsraw, !is.na(Avorted) & Total == 4)) %>% 
  mutate(Pollinated = Avorted + Seed) %>% 
  mutate(Proportion_avorted = Avorted / Pollinated) %>%
  mutate(Fruits = if_else(Seed > 0, 1,0)) %>%
  group_by(Plot, Species, Plant) %>% 
  summarise(Samples_seeds=n(),Fruits=sum(Fruits),Percent_pollination=(mean(Pollinated)/4*100),Proportion_avorted=mean(Proportion_avorted))%>%
  mutate(Fruit_set=(Fruits/Samples_seeds)) %>%
  select(., -c(Fruits)) %>%
  complete(Species, Plot, Plant) %>%
  distinct() 

fruitandseedset <- droplevels(dplyr::filter(seedsraw, !is.na(Avorted) & Total == 4)) %>% 
  mutate(Pollinated = Avorted + Seed) %>% 
  mutate(Fruits = if_else(Seed > 0, 1,0)) %>%
  filter(.,Fruits==1) %>%
  group_by(Plot, Species, Plant) %>% 
  summarise(Seed_set=mean(Seed))%>%
  left_join(fruits, by = c("Plot","Species","Plant"))%>%
  complete(Species, Plot, Plant) %>%
  distinct() 


datafunctionalityperplant <- pollen %>%
  dplyr::left_join(., fruitandseedset, by = c("Species","Plot","Plant")) 



########################################################

ROF <- filter(datafunctionalityperplant, Species =="ROF")
TVUF <- filter(datafunctionalityperplant, Species =="TVUF")%>%
  select(Mean_Homospecific,Percent_pollination,Seed_set,Fruit_set,Proportion_avorted,Flowers_with_pollen)

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

TVUF[is.nan(TVUF)] <- 0

TVUH <- filter(datafunction, Species =="TVUH") %>%
  select(Mean_Homospecific,Percent_pollination,Seed_set,Fruit_set,Proportion_avorted,Flowers_with_pollen)

TVUH[is.nan(TVUH)] <- 0
 

a <- lm(TVUH$Fruit_set~TVUH$Percent_pollination)
summary(a)# significatiu
plot(TVUH$Fruit_set~TVUH$Percent_pollination)
abline(a)

ggplot(TVUF, aes(y=Seed_set, x=Proportion_avorted)) + 
  geom_point(alpha=0.3) + 
  geom_smooth() +
  theme_classic() +
  xlim(0, 1)

res2<-rcorr(as.matrix(TVUH))
corrplot(res2$r, type="upper", order="hclust",
         p.mat = res2$P, sig.level = 0.01, insig = "blank")




#hist(ROF$Mean_Heterospecific)
hist(TVUF$SD_weight_viables,xlim=c(0,0.15),ylim=c(0,20),breaks=5)
hist(TVUF$Mean_weigth_viables,xlim=c(0.05,0.25),breaks=5)



a <- lm(TVUH$Mean_Homospecific~(TVUH$Pollinator_richness))
summary(a)# significatiu
plot(TVUH$Mean_Homospecific~(TVUH$Pollinator_richness))

hist(TVUH$Mean_Homospecific)
hist(log(TVUH$Pollinator_richness))

a <- lm(TVUH$Mean_Homospecific~TVUH$HB_Visitation_rate)
summary(a)# significatiu
plot(TVUH$Mean_Homospecific~TVUH$HB_Visitation_rate)

a <- lm(TVUH$Mean_Homospecific~TVUH$Visitation_rate)
summary(a)# significatiu
plot(TVUH$Mean_Homospecific~TVUH$Visitation_rate)

a <- lm(TVUH$Mean_Homospecific~TVUH$Wild_Visitation_rate)
summary(a)# significatiu
plot(TVUH$Mean_Homospecific~TVUH$Wild_Visitation_rate)

hist((sqrt(TVUH$Seed_set)))
hist((TVUH$Flowers_with_pollen))

ggplot(TVUH, aes(y=Fruit_set, x=TVUH$Flowers_with_pollen)) + 
  geom_point(alpha=0.3) + 
  geom_smooth() +
  theme_classic() 


#### veure distribucions de la deposició de polen i llavors

hist(ROF$Mean_Homospecific, ylim=c(0,400))
hist(ROF$Flowers_with_pollen, ylim=c(0,400))

hist(TVUF$Mean_Homospecific, ylim=c(0,400))
hist(TVUF$Flowers_with_pollen, ylim=c(0,400))
hist(TVUF$Seed_set, ylim=c(0,200))
hist(TVUF$Percent_pollination, ylim=c(0,200))
hist(TVUF$Fruit_set, ylim=c(0,200))

hist(TVUH$Mean_Homospecific, ylim=c(0,400))
hist(TVUH$Flowers_with_pollen, ylim=c(0,400))
hist(TVUH$Seed_set, ylim=c(0,200))
hist(TVUH$Percent_pollination, ylim=c(0,200))
hist(TVUH$Fruit_set, ylim=c(0,200))



ROF <- filter(datafunction, Species =="ROF")

hist(ROF$Samples_pollen)
hist(ROF$Flowers_with_pollen)
hist(ROF$Mean_pollens)           ## normal
hist(ROF$SD_pollen)              ## normal
hist(ROF$Mean_Homospecific)      ## normal
hist(ROF$SD_Homospecific)        ## normal
hist((ROF$Mean_Heterospecific))  ## no normal



TVUF <- filter(datafunction, Species =="TVUF")

hist(TVUF$Samples_pollen)
hist(TVUF$Flowers_with_pollen)
hist(TVUF$Mean_pollen)         ## more or less normal
hist(TVUF$SD_pollen)           ## normal
hist(TVUF$Mean_Homospecific)   ## more or less normal
hist(TVUF$SD_Homospecific)     ## more or less normal
hist(TVUF$Mean_Heterospecific) ## zero inflated
hist(TVUF$Mean_weigth)         ## skewed
hist(TVUF$SD_weight)           ## normal
hist(TVUF$Mean_weigth_viables) ## normal
hist(TVUF$SD_weight_viables)   ## normal
hist(TVUF$Fruit_set)           ## more or less normal
hist(TVUF$Seed_set)            ## more or less normal
hist(TVUF$Seed_viability)      ## skewed
hist(TVUF$Weighted_seeds)      ## skewed
hist(TVUF$Samples_seeds)       ## skewed
hist(TVUF$Percent_pollination) ## skewed


TVUH <- filter(datafunction, Species =="TVUH")

hist(TVUH$Samples_pollen)
hist(TVUH$Flowers_with_pollen)
hist(TVUH$Mean_pollen)         ## skewed
hist(TVUH$SD_pollen)           ## more or less normal
hist(TVUH$Mean_Homospecific)   ## skewed
hist(TVUH$SD_Homospecific)     ## more or less normal
hist(TVUH$Mean_Heterospecific) ## zero inflated
hist(TVUH$Percent_pollination) ## skewed
hist(TVUH$Fruit_set)           ## normal
hist(TVUH$Samples_seeds)       ## skewed
hist(TVUH$Seed_set)            ## more or less normal
hist(TVUH$Mean_weigth)         ## normal
hist(TVUH$SD_weight)           ## normal
hist(TVUH$Seed_viability)      ## skewed
hist(TVUH$Mean_weigth_viables) ## normal
hist(TVUH$SD_weight_viables)   ## normal
hist(TVUH$Weighted_seeds)      ## skewed


###### Correlation matrix between network indices and temperature
database2 <- read.table("dades/Database3.txt",header=T)

networkmetrics$T_Max <- database2$T_Max
networkmetrics <- networkmetrics[,-4]#remove plot column
res2<-rcorr(as.matrix(networkmetrics))
corrplot(res2$r, type="upper", order="hclust",
         p.mat = res2$P, sig.level = 0.01, insig = "blank")

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
hist(flors$proporcioF)
