
source("funcionalitat/netejar dades databases analisis.R")

library(lme4)
library(MuMIn)
library(lmerTest)

################# TVUF

hist(log(TVUFtotperplanta$Avorted))

fit <- lmer(logAvorted~logVisitation_rate+ProporcioF+Pollinator_richness+Proportion_HB+Proportion_Bee+(1|Plot), data=TVUFtotperplantaavorted)  

hist(resid(fit))

options(na.action = "na.fail")
dd <- dredge(fit)
subset(dd, delta < 2)


############## TVUH

fit <- lmer(logAvorted~logVisitation_rate+ProporcioF+Pollinator_richness+Proportion_HB+Proportion_Bee+(1|Plot), data=TVUHtotperplantaavorted)  

hist(resid(fit))

dd <- dredge(fit)
subset(dd, delta < 2)
