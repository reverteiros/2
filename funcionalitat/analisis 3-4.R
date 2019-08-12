

source("funcionalitat/netejar dades fruits i llavors.R")
source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades plantes.R")
source("funcionalitat/netejar dades databases analisis.R")
source("funcionalitat/analisis mitjana per parcela.R")

library(lme4)
library(MuMIn)


######################################### Flowers with POLLEN - FRUIT SET ####################################

################# TVUF

fitTVUFpollenfruits <- lmer(Fruit_set~Flowers_with_pollen+Mean_Homospecific+Pollinator_richness+(1|Plot), data=TVUHpollenfruitsperplanta)  

hist(resid(fitTVUFpollenfruits))

options(na.action = "na.fail")
dd <- dredge(fitTVUFpollenfruits)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]])



fitTVUFpollenfruits <- lmer(Fruit_set~Flowers_with_pollen+Mean_Homospecific+Functional_group_Rocka+(1|Plot), data=TVUFtotperplanta)  

hist(resid(fitTVUFpollenfruits))

options(na.action = "na.fail")
dd <- dredge(fitTVUFpollenfruits)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]])


fitTVUFpollenfruits <- lmer(Fruit_set~Flowers_with_pollen+Mean_Homospecific+Bee_VR+Diptera_VR+Honeybees_VR+Lepidoptera_VR+(1|Plot), data=TVUFtotperplanta)  

hist(TVUFtotperplanta$Honeybees_VR)

hist(resid(fitTVUFpollenfruits))

options(na.action = "na.fail")
dd <- dredge(fitTVUFpollenfruits)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]]) 



############## TVUH

fitTVUHpollenfruits <- lmer(Fruit_set~Flowers_with_pollen+Mean_Homospecific+Functional_group_Rocka+(1|Plot), data=TVUHtotperplanta)  

hist(resid(fitTVUHpollenfruits))

dd <- dredge(fitTVUHpollenfruits)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])


fitTVUHpollenfruits <- lmer(Fruit_set~Flowers_with_pollen+Mean_Homospecific+Pollinator_richness+(1|Plot), data=TVUHtotperplanta)  

hist(resid(fitTVUHpollenfruits))

dd <- dredge(fitTVUHpollenfruits)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])






######################################### MEAN POLLEN - SEED SET ####################################

################# TVUF

fitTVUFpollenfruits <- lmer(Seed_set~Flowers_with_pollen+Mean_Homospecific+Pollinator_richness+(1|Plot), data=TVUFtotperplanta)  

hist(resid(fitTVUFpollenfruits))

options(na.action = "na.fail")
dd <- dredge(fitTVUFpollenfruits)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]])



fitTVUFpollenfruits <- lmer(Fruit_set~Flowers_with_pollen+Mean_Homospecific+Functional_group_Rocka+(1|Plot), data=TVUFtotperplanta)  

hist(resid(fitTVUFpollenfruits))

options(na.action = "na.fail")
dd <- dredge(fitTVUFpollenfruits)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]])


fitTVUFpollenfruits <- lmer(Fruit_set~Flowers_with_pollen+Mean_Homospecific+Bee_VR+Diptera_VR+Honeybees_VR+Lepidoptera_VR+(1|Plot), data=TVUFtotperplanta)  

hist(TVUFtotperplanta$Honeybees_VR)

hist(resid(fitTVUFpollenfruits))

options(na.action = "na.fail")
dd <- dredge(fitTVUFpollenfruits)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]]) 



############## TVUH

fitTVUHpollenfruits <- lmer(Fruit_set~Flowers_with_pollen+Mean_Homospecific+Functional_group_Rocka+(1|Plot), data=TVUHtotperplanta)  

hist(resid(fitTVUHpollenfruits))

dd <- dredge(fitTVUHpollenfruits)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])


fitTVUHpollenfruits <- lmer(Fruit_set~Flowers_with_pollen+Mean_Homospecific+Pollinator_richness+(1|Plot), data=TVUHtotperplanta)  

hist(resid(fitTVUHpollenfruits))

dd <- dredge(fitTVUHpollenfruits)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])




