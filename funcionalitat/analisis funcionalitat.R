

source("funcionalitat/netejar dades.R")
source("funcionalitat/index xarxes.R")

###### Correlation matrix between network indices and temperature
database2 <- read.table("dades/Database3.txt",header=T)
library(corrplot)
library(Hmisc)
networkmetrics$T_Max <- database2$T_Max
networkmetrics <- networkmetrics[,-7]
res2<-rcorr(as.matrix(networkmetrics))
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank")



ROF <- filter(datafunction, Species =="ROF")

hist(ROF$Mean_pollen)
hist(ROF$SD_pollen)
hist(ROF$Mean_Homospecific)
hist(ROF$SD_Homospecific)
hist(ROF$Mean_Heterospecific)

a <- lm(database2$Honeybees_ROF_rate~ROF$Mean_pollen)
summary(a)
a <- lm(database2$Honeybees_ROF_rate~ROF$SD_pollen)
summary(a)
a <- lm(networkmetrics$Nestedness~ROF$Mean_pollen)
summary(a)
a <- lm(networkmetrics$Connectance~ROF$Mean_pollen)
summary(a)
a <- lm(networkmetrics$`Shannon diversity`~ROF$Mean_pollen)
summary(a)
a <- lm(networkmetrics$H2~ROF$Mean_pollen)
summary(a)
a <- lm(networkmetrics$Generality~ROF$Mean_pollen)
summary(a)

TVUF <- filter(datafunction, Species =="TVUF")

hist(TVUF$Mean_pollen)
hist(TVUF$SD_pollen)
hist(TVUF$Mean_Homospecific)
hist(TVUF$SD_Homospecific)
hist(TVUF$Mean_Heterospecific)
hist(TVUF$Mean_weigth)
hist(TVUF$Percent_embryo)
hist(TVUF$Fruit_set)
hist(TVUF$Seed_set)

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

hist(TVUH$Mean_pollen)
hist(TVUH$SD_pollen)
hist(TVUH$Mean_Homospecific)
hist(TVUH$SD_Homospecific)
hist(TVUH$Mean_Heterospecific)
hist(TVUH$Mean_weigth)
hist(TVUH$Percent_embryo)
hist(TVUH$Fruit_set)
hist(TVUH$Seed_set)

