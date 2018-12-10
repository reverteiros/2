




#lo primer que s.ha de fer es decidir les transformacions de les dades per a que siguin normals, llavors ja després anirà tot, perquè si transformes les dades també les has de transformar per fer el model de correlacions entre variables








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
hist(unlist(networkmetrics$dROF))        ## almost normal
hist(sqrt(unlist(networkmetrics$dTVUF))) ## normal
hist(sqrt(unlist(networkmetrics$dTVUH))) ## almost normal
hist(sqrt(networkmetrics$T_Max) )        ## almost normal



ROF <- filter(datafunction, Species =="ROF")

hist(ROF$Mean_pollen)         ## normal
hist(ROF$SD_pollen)           ## normal
hist(ROF$Mean_Homospecific)   ## normal
hist(ROF$SD_Homospecific)     ## normal
hist((ROF$Mean_Heterospecific)) ## no normal

a <- lm(database2$Honeybees_ROF_rate~ROF$Mean_pollen)
summary(a)
a <- lm(database2$Honeybees_ROF_rate~ROF$SD_pollen)
summary(a)
a <- lm(networkmetrics$Nestedness~ROF$Mean_pollen)
summary(a)
a <- lm(networkmetrics$H2~ROF$Mean_pollen)
summary(a)

TVUF <- filter(datafunction, Species =="TVUF")

hist(TVUF$Mean_pollen)         ## more or less normal
hist(TVUF$SD_pollen)           ## normal
hist(TVUF$Mean_Homospecific)   ## more or less normal
hist(TVUF$SD_Homospecific)     ## more or less normal
hist(TVUF$Mean_Heterospecific) ## zero inflated
hist(TVUF$Mean_weigth)         ## skewed
hist(TVUF$Percent_embryo)      ## skewed
hist(TVUF$Fruit_set)           ## more or less normal
hist(TVUF$Seed_set)            ## more or less normal

a <- lm(database2$Honeybees_TVU_rate~TVUF$Mean_pollen)
summary(a)
a <- lm(database2$Honeybees_TVU_rate~TVUF$SD_pollen)
summary(a)
a <- lm(networkmetrics$Nestedness~TVUF$Mean_pollen)
summary(a)
a <- lm(networkmetrics$Connectance~TVUF$Mean_pollen)
summary(a)
a <- lm(networkmetrics$`Shannon diversity`~TVUF$Mean_pollen)
summary(a)
a <- lm(networkmetrics$H2~TVUF$Mean_pollen)
summary(a)
a <- lm(networkmetrics$Generality~TVUF$Mean_pollen)
summary(a)
a <- lm(TVUF$Mean_Homospecific~TVUF$Mean_Heterospecific)
summary(a)
# nothing shos relationship with mean pollen deposition
plot(TVUF$Mean_Homospecific~TVUF$Mean_Heterospecific)
# looks like when there is few homospecific heterospecific rises and vice versa

a <- lm(database2$Honeybees_TVU_rate~TVUF$Mean_Homospecific)
summary(a)
a <- lm(networkmetrics$Nestedness~TVUF$Mean_Homospecific)
summary(a)
a <- lm(networkmetrics$Connectance~TVUF$Mean_Homospecific)
summary(a)
a <- lm(networkmetrics$`Shannon diversity`~TVUF$Mean_Homospecific)
summary(a)
a <- lm(networkmetrics$H2~TVUF$Mean_Homospecific)
summary(a)
a <- lm(networkmetrics$Generality~TVUF$Mean_Homospecific)
summary(a)


TVUH <- filter(datafunction, Species =="TVUH")

hist(TVUH$Mean_pollen)         ## skewed
hist(TVUH$SD_pollen)           ## more or less normal
hist(TVUH$Mean_Homospecific)   ## skewed
hist(TVUH$SD_Homospecific)     ## more or less normal
hist(TVUH$Mean_Heterospecific) ## zero inflated
hist(TVUH$Mean_weigth)         ## normal
hist(TVUH$Percent_embryo)      ## skewed
hist(TVUH$Fruit_set)           ## normal
hist(TVUH$Seed_set)            ## more or less normal

