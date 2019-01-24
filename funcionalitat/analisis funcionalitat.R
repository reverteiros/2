
source("funcionalitat/netejar dades.R")
source("funcionalitat/index xarxes.R")

###### Correlation matrix between network indices and temperature
database2 <- read.table("dades/Database3.txt",header=T)
library(corrplot)
library(Hmisc)
networkmetrics$T_Max <- database2$T_Max
networkmetrics <- networkmetrics[,-4]#remove plot column
res2<-rcorr(as.matrix(networkmetrics))
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank")

length(!is.na(networkmetrics$dTVUH))


### Shannon diversity is highly correlated with everything. Remove
networkmetrics <- networkmetrics %>%
  select(-Shannon_diversity)

res2<-rcorr(as.matrix(networkmetrics))
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank")

## d' ROF is correlated with everything too....

plot(unlist(networkmetrics$dROF)~unlist(networkmetrics$dTVUF))
plot(unlist(networkmetrics$dROF)~unlist(networkmetrics$dTVUH))
plot(unlist(networkmetrics$dTVUH)~unlist(networkmetrics$dTVUF))



hist(networkmetrics$Nestedness)          ## normal
hist(networkmetrics$H2)                  ## normal
hist(networkmetrics$Shannon_diversity)
hist(unlist(networkmetrics$dROF))        ## almost normal
hist(sqrt(unlist(networkmetrics$dTVUF))) ## normal
hist(sqrt(unlist(networkmetrics$dTVUH))) ## almost normal
hist(sqrt(networkmetrics$T_Max) )        ## almost normal



ROF <- filter(datafunction, Species =="ROF")

hist(ROF$Samples_pollen)
hist(ROF$Flowers_with_pollen)
hist(ROF$Mean_pollen_all_flowers)

hist(ROF$Mean_pollen_flowers_with_pollen)
hist(ROF$Mean_Homospecific_flowers_with_pollen)
hist(ROF$Mean_Heterospecific_flowers_with_pollen)

hist(ROF$Mean_pollen_all_flowers)            ## normal
hist(ROF$SD_pollen_all_flowers)              ## normal
hist(ROF$Mean_Homospecific_all_flowers)      ## normal
hist(ROF$SD_Homospecific_all_flowers)        ## normal
hist((ROF$Mean_Heterospecific_all_flowers))  ## no normal

a <- lm(database2$Honeybees_ROF_rate~ROF$Flowers_with_pollen)
summary(a) 
a <- lm(networkmetrics$Nestedness~ROF$Flowers_with_pollen)
summary(a) 
a <- lm(networkmetrics$H2~ROF$Flowers_with_pollen)
summary(a) # significatiu negatiu
plot((networkmetrics$H2~ROF$Flowers_with_pollen))
abline(a)
a <- lm(unlist(networkmetrics$dROF)~ROF$Flowers_with_pollen)
summary(a) 

a <- lm(database2$Honeybees_ROF_rate~ROF$Mean_Heterospecific_flowers_with_pollen)
summary(a) 
a <- lm(networkmetrics$Nestedness~ROF$Mean_Heterospecific_flowers_with_pollen)
summary(a) 
a <- lm(networkmetrics$H2~ROF$Mean_Heterospecific_flowers_with_pollen)
summary(a) 
a <- lm(unlist(networkmetrics$dROF)~ROF$Mean_Heterospecific_flowers_with_pollen)
summary(a) 

a <- lm(database2$Honeybees_ROF_rate~ROF$Mean_Homospecific_flowers_with_pollen)
summary(a) 
a <- lm(networkmetrics$Nestedness~ROF$Mean_Homospecific_flowers_with_pollen)
summary(a) 
a <- lm(networkmetrics$H2~ROF$Mean_Homospecific_flowers_with_pollen)
summary(a) # significatiu negatiu
a <- lm(unlist(networkmetrics$dROF)~ROF$Mean_Homospecific_flowers_with_pollen)
summary(a) 


TVUF <- filter(datafunction, Species =="TVUF")

hist(TVUF$Samples_pollen)
hist(TVUF$Flowers_with_pollen)

hist(TVUF$Mean_pollen_flowers_with_pollen)
hist(TVUF$Mean_Homospecific_flowers_with_pollen)
hist(TVUF$Mean_Heterospecific_flowers_with_pollen)

hist(TVUF$Flowers_with_pollen)         ## more or less normal
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
hist(TVUF$Samples_seeds)      ## skewed
hist(TVUF$Percent_pollination)      ## skewed



a <- lm(database2$Honeybees_TVU_rate~TVUF$Mean_Heterospecific_flowers_with_pollen)
summary(a)
a <- lm(networkmetrics$Nestedness~TVUF$Mean_Heterospecific_flowers_with_pollen)
summary(a)
a <- lm(networkmetrics$Shannon_diversity~TVUF$Mean_Heterospecific_flowers_with_pollen)
summary(a)
a <- lm(networkmetrics$H2~TVUF$Mean_Heterospecific_flowers_with_pollen)
summary(a)
a <- lm(unlist(networkmetrics$dTVUF)~TVUF$Mean_Heterospecific_flowers_with_pollen)
summary(a) # significatiu positiu

a <- lm(database2$Honeybees_TVU_rate~TVUF$Mean_Homospecific_flowers_with_pollen)
summary(a)
a <- lm(networkmetrics$Nestedness~TVUF$Mean_Homospecific_flowers_with_pollen)
summary(a)
a <- lm(networkmetrics$Shannon_diversity~TVUF$Mean_pollen_flowers_with_pollen)
summary(a)
a <- lm(networkmetrics$H2~TVUF$Mean_Homospecific_flowers_with_pollen)
summary(a)
a <- lm(unlist(networkmetrics$dTVUF)~TVUF$Mean_Homospecific_flowers_with_pollen)
summary(a)

a <- lm(database2$Honeybees_TVU_rate~TVUF$Flowers_with_pollen)
summary(a) # significatiu positiu
a <- lm(networkmetrics$Nestedness~TVUF$Flowers_with_pollen)
summary(a)
a <- lm(networkmetrics$Shannon_diversity~TVUF$Flowers_with_pollen)
summary(a) # significatiu negatiu
a <- lm(networkmetrics$H2~TVUF$Flowers_with_pollen)
summary(a)
a <- lm(unlist(networkmetrics$dTVUF)~TVUF$Flowers_with_pollen)
summary(a) # significatiu negatiu


TVUH <- filter(datafunction, Species =="TVUH")

hist(TVUH$Samples_pollen)
hist(TVUH$Flowers_with_pollen)

hist(TVUH$Mean_pollen_flowers_with_pollen)
hist(TVUH$Mean_Homospecific_flowers_with_pollen)
hist(TVUH$Mean_Heterospecific_flowers_with_pollen)

hist(TVUH$Flowers_with_pollen)         ## more or less normal
hist(TVUH$Mean_pollen)         ## skewed
hist(TVUH$SD_pollen)           ## more or less normal
hist(TVUH$Mean_Homospecific)   ## skewed
hist(TVUH$SD_Homospecific)     ## more or less normal
hist(TVUH$Mean_Heterospecific) ## zero inflated
hist(TVUH$Percent_pollination)      ## skewed
hist(TVUH$Fruit_set)           ## normal
hist(TVUH$Samples_seeds)      ## skewed
hist(TVUH$Seed_set)            ## more or less normal
hist(TVUH$Mean_weigth)         ## normal
hist(TVUH$SD_weight)           ## normal
hist(TVUH$Seed_viability)      ## skewed
hist(TVUH$Mean_weigth_viables) ## normal
hist(TVUH$SD_weight_viables)   ## normal
hist(TVUH$Weighted_seeds)      ## skewed

a <- lm(database2$Honeybees_TVU_rate~TVUH$Mean_Heterospecific_flowers_with_pollen)
summary(a)# no significatiu
a <- lm(networkmetrics$Nestedness~TVUH$Mean_Heterospecific_flowers_with_pollen)
summary(a)# no significatiu
a <- lm(networkmetrics$Shannon_diversity~TVUH$Mean_Heterospecific_flowers_with_pollen)
summary(a)# no significatiu
a <- lm(networkmetrics$H2~TVUH$Mean_Heterospecific_flowers_with_pollen)
summary(a)# significatiu negatiu
a <- lm(unlist(networkmetrics$dTVUF)~TVUH$Mean_Heterospecific_flowers_with_pollen)
summary(a) # no

a <- lm(database2$Honeybees_TVU_rate~TVUH$Mean_Homospecific_flowers_with_pollen)
summary(a)
a <- lm(networkmetrics$Nestedness~TVUH$Mean_Homospecific_flowers_with_pollen)
summary(a)
a <- lm(networkmetrics$Shannon_diversity~TVUH$Mean_pollen_flowers_with_pollen)
summary(a)
a <- lm(networkmetrics$H2~TVUH$Mean_Homospecific_flowers_with_pollen)
summary(a)
a <- lm(unlist(networkmetrics$dTVUF)~TVUH$Mean_Homospecific_flowers_with_pollen)
summary(a)

a <- lm(database2$Honeybees_TVU_rate~TVUH$Flowers_with_pollen)
summary(a) #no
a <- lm(networkmetrics$Nestedness~TVUH$Flowers_with_pollen)
summary(a)
a <- lm(networkmetrics$Shannon_diversity~TVUH$Flowers_with_pollen)
summary(a) 
a <- lm(networkmetrics$H2~TVUH$Flowers_with_pollen)
summary(a) # significatiu negatiu
a <- lm(unlist(networkmetrics$dTVUF)~TVUH$Flowers_with_pollen)
summary(a)  
