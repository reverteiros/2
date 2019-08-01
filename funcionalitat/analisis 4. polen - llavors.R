
source("funcionalitat/netejar dades fruits i llavors.R")
source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades plantes.R")
source("funcionalitat/netejar dades databases analisis.R")

library(lme4)
library(MuMIn)


######################################### MEAN POLLEN - FRUIT SET ####################################

###### TVUF

hist(TVUFpollenfruitsperplanta$ProporcioF)             #normal

################# Fruit set

fitTVUFpollenfruits <- lmer(Fruit_set~Mean_Homospecific+ProporcioF+Mean_Heterospecific+(1|Plot),data=TVUFpollenfruitsperplanta)  

hist(resid(fitTVUFpollenfruits))

options(na.action = "na.fail")
dd <- dredge(fitTVUFpollenfruits)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])


################# Fecundity

fitTVUFpollenfecundity <- lmer(Fecundity~Mean_Homospecific+ProporcioF+Mean_Heterospecific+(1|Plot), data=TVUFpollenfruitsperplanta)  

hist(resid(fitTVUFpollenfecundity))

dd <- dredge(fitTVUFpollenfecundity)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])


################# Seed set

fitTVUFpollenseeds <- lmer(Seed_set~Mean_Homospecific+ProporcioF+Mean_Heterospecific+(1|Plot), data=TVUFpollenseedsperplanta)  

hist(resid(fitTVUFpollenseeds))

dd <- dredge(fitTVUFpollenseeds)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])



############################################ TVUH

############## Fruit set

fitTVUHpollenfruits <- lmer(Fruit_set~Mean_Homospecific+ProporcioF+Mean_Heterospecific+(1|Plot), data=TVUHpollenfruitsperplanta)  

hist(resid(fitTVUHpollenfruits))

dd <- dredge(fitTVUHpollenfruits)
subset(dd, delta < 2)
Csummary(get.models(dd, 1)[[1]])


############## Fecundity

fitTVUHpollenfecundity <- lmer(Fecundity~Mean_Homospecific+ProporcioF+Mean_Heterospecific+(1|Plot), data=TVUHpollenfruitsperplanta)  

hist(resid(fitTVUHpollenfecundity))

dd <- dredge(fitTVUHpollenfecundity)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])


############## Seed set

fitTVUHpollenseeds <- lmer(Seed_set~Mean_Homospecific+ProporcioF+Mean_Heterospecific+(1|Plot), data=TVUHpollenseedsperplanta)  

hist(resid(fitTVUHpollenseeds))

dd <- dredge(fitTVUHpollenseeds)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])
