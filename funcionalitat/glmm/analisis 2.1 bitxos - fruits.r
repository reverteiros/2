
source("funcionalitat/netejar dades databases analisis.R")

library(lme4)
library(MuMIn)
library(lmerTest)

# si volem filtrar per les arees amb llimitacio de polen ha de ser l'analisi amb parceles, no amb 
# individus
#
# TVUFtotperplantafiltered <- TVUFtotperplanta %>%
#   filter(Homospecific_presence > -1) %>%
#   filter(Fruit_set > -1) %>%
#   filter(Mean_Total < 8.1)
# 
# TVUHtotperplantafiltered <- TVUHtotperplanta %>%
#   filter(Homospecific_presence > -1) %>%
#   filter(Fruit_set > -1)%>%
#   filter(Mean_Total < 8.1)

################# TVUF

fitTVUFpollenfruits <- lmer(Fruit_set~logVisitation_rate+ProporcioF+Pollinator_richness+Proportion_HB+Proportion_Bee+Proportion_Diptera+Proportion_Lepidoptera+(1|Plot), data=TVUFtotperplanta)  

hist(resid(fitTVUFpollenfruits))

options(na.action = "na.fail")
dd <- dredge(fitTVUFpollenfruits)
subset(dd, delta < 2)


############## TVUH

fitTVUHpollenfruits <- lmer(Fruit_set~logVisitation_rate+ProporcioF+Pollinator_richness+Proportion_HB+Proportion_Bee+Proportion_Diptera+Proportion_Coleoptera+(1|Plot), data=TVUHtotperplanta)  

hist(resid(fitTVUHpollenfruits))

dd <- dredge(fitTVUHpollenfruits)
subset(dd, delta < 2)

