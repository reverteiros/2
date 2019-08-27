
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

### TVUF

fitTVUFpollenseeds <- lmer(Seed_set~ProporcioF+logPollinator_richness+logVisitation_rate+Proportion_HB+Proportion_Bee+Proportion_Diptera+(1|Plot), data=TVUFtotperplantafiltered)  

hist(resid(fitTVUFpollenseeds))

options(na.action = "na.fail")
dd <- dredge(fitTVUFpollenseeds)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]])


### TVUH

fitTVUHpollenseeds <- lmer(Seed_set~ProporcioF+logPollinator_richness+logVisitation_rate+Proportion_HB+Proportion_Bee+Proportion_Diptera+(1|Plot), data=TVUHtotperplantafiltered)  

hist(resid(fitTVUHpollenseeds))

dd <- dredge(fitTVUHpollenseeds)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])

