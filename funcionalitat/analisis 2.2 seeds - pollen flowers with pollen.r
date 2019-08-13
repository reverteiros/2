

source("funcionalitat/netejar dades databases analisis.R")

library(lme4)
library(MuMIn)
library(lmerTest)

TVUFtotperplantafiltered <- TVUFtotperplanta %>%
  filter(Homospecific_presence > -1) %>%
  filter(Fruit_set > -1) %>%
  filter(Mean_Homospecific < 8.1)

TVUHtotperplantafiltered <- TVUHtotperplanta %>%
  filter(Homospecific_presence > -1) %>%
  filter(Fruit_set > -1)%>%
  filter(Mean_Homospecific < 8.1)

################################# FLOWERS WITH POLLEN - FRUIT SET ##################################

################# TVUF

fitTVUFpollenfruits <- lmer(Fruit_set~Mean_Homospecific+Functional_group_Rocka+logVisitation_rate+Homospecific_presence+ProporcioF+(1|Plot), data=TVUFtotperplantafiltered)  

hist(resid(fitTVUFpollenfruits))

options(na.action = "na.fail")
dd <- dredge(fitTVUFpollenfruits)
subset(dd, delta < 2)

######

fitTVUFpollenfruits <- lmer(Fruit_set~Mean_Homospecific+Homospecific_presence*(Proportion_HB+Proportion_Bee+Proportion_Diptera+Proportion_Lepidoptera)+(1|Plot), data=TVUFtotperplantafiltered)  

hist(resid(fitTVUFpollenfruits))

options(na.action = "na.fail")
dd <- dredge(fitTVUFpollenfruits)
subset(dd, delta < 2)


############## TVUH

fitTVUHpollenfruits <- lmer(Fruit_set~Mean_Homospecific+Functional_group_Rocka+logVisitation_rate+ProporcioF+Homospecific_presence+(1|Plot), data=TVUHtotperplantafiltered)  

hist(resid(fitTVUHpollenfruits))

dd <- dredge(fitTVUHpollenfruits)
subset(dd, delta < 2)

######
fitTVUHpollenfruits <- lmer(Fruit_set~Mean_Homospecific+Homospecific_presence*(Proportion_HB+Proportion_Bee+Proportion_Diptera+Proportion_Coleoptera)+(1|Plot), data=TVUHtotperplantafiltered)  

hist(resid(fitTVUHpollenfruits))

dd <- dredge(fitTVUHpollenfruits)
subset(dd, delta < 2)



################################ Fecundity - mean pollen flowers with pollen?



################################# Seed set - mean pollen flowers with pollen

TVUFtotperplantafiltered <- TVUFtotperplanta %>%
  filter(Mean_Homospecific > -1) %>%
  filter(Seed_set > -1) %>%
  filter(Mean_Homospecific<8.1)

TVUHtotperplantafiltered <- TVUHtotperplanta %>%
  filter(Mean_Homospecific > -1) %>%
  filter(Seed_set > -1)%>%
  filter(Mean_Homospecific<8.1)

### TVUF

fitTVUFpollenseeds <- lmer(Seed_set~Mean_Homospecific+ProporcioF+Functional_group_Rocka+logVisitation_rate+(1|Plot), data=TVUFtotperplantafiltered)  

hist(resid(fitTVUFpollenseeds))

dd <- dredge(fitTVUFpollenseeds)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]])


######

fitTVUFpollenfruits <- lmer(Seed_set~Mean_Homospecific*(Proportion_HB+Proportion_Bee+Proportion_Diptera+Proportion_Lepidoptera)+(1|Plot), data=TVUFtotperplantafiltered)  

hist(resid(fitTVUFpollenfruits))

options(na.action = "na.fail")
dd <- dredge(fitTVUFpollenfruits)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]])

### TVUH

hist(sqrt(TVUHtotperplantafiltered$Seed_set))

fitTVUHpollenseeds <- lmer((Seed_set)~log(Mean_Homospecific)+ProporcioF+logPollinator_richness+logVisitation_rate+(1|Plot), data=TVUHtotperplantafiltered)  

hist(resid(fitTVUHpollenseeds))

dd <- dredge(fitTVUHpollenseeds)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])


######
fitTVUHpollenfruits <- lmer(Seed_set~Mean_Homospecific*(Proportion_HB+Proportion_Bee+Proportion_Diptera)+(1|Plot), data=TVUHtotperplantafiltered)  

hist(resid(fitTVUHpollenfruits))

dd <- dredge(fitTVUHpollenfruits)
subset(dd, delta < 2)
# summary(get.models(dd, 2)[[1]])




grupstaxonomicsspread2 <- grupstaxonomicsspread %>%
  filter(Species =="TVUH")
hist(grupstaxonomicsspread2$Lepidoptera,xlim=c(-1,3))
