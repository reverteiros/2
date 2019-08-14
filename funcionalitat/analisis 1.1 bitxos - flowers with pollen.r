
source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades plantes.R")
source("funcionalitat/netejar dades databases analisis.R")

library(lme4)
library(MuMIn)


######################################### ROF #########################

fitROFTotal_tot <- glmer(Total_presence~Pollinator_richness+logVisitation_rate+Proportion_HB+Proportion_Bee+Proportion_Diptera+Proportion_Coleoptera+(1|Plot/Plant), data=ROFpollenbitxos, family=binomial)  

hist(resid(fitROFTotal_tot))

options(na.action = "na.fail")
dd <- dredge(fitROFTotal_tot)
subset(dd, delta < 2)
summary(get.models(dd, 2)[[1]])

######################################### TVUF #########################

fitROFTotal_tot <- glmer(Total_presence~Pollinator_richness+logVisitation_rate+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera+Proportion_Lepidoptera+(1|Plot/Plant), data=TVUFpollenbitxos, family=binomial)  

hist(resid(fitROFTotal_tot))

dd <- dredge(fitROFTotal_tot)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]])

######################################### TVUH #########################

fitROFTotal_tot <- glmer(Homospecific_presence~logPollinator_richness+logVisitation_rate+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera+Proportion_Coleoptera+(1|Plot/Plant), data=TVUHpollenbitxos, family=binomial)  

hist(resid(fitROFTotal_tot))

dd <- dredge(fitROFTotal_tot)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])

