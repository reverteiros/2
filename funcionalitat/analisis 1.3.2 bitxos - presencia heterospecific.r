
source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades plantes.R")
source("funcionalitat/netejar dades databases analisis.R")

library(lme4)
library(MuMIn)




######################################### TVUF #########################

## Functional_group_Rocka

fitROFTotal_tot <- glmer(Heterospecific_presence~Functional_group_Rocka+logVisitation_rate+ProporcioF+H2+(1|Plot/Plant), data=TVUFpollenbitxos, family=binomial)  

hist(resid(fitROFTotal_tot))

options(na.action = "na.fail")
dd <- dredge(fitROFTotal_tot)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])

## Pollinator richness

fitROFTotal_tot <- glmer(Heterospecific_presence~Pollinator_richness+logVisitation_rate+ProporcioF+H2+(1|Plot/Plant), data=TVUFpollenbitxos, family=binomial)  

hist(resid(fitROFTotal_tot))

dd <- dredge(fitROFTotal_tot)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])


## Taxonomic groups per separat 

roftot <- glmer(Heterospecific_presence~Bee_VR+Lepidoptera_VR+Diptera_VR+Honeybees_VR+(1|Plot/Plant), data=TVUFpollenbitxos, family=binomial(link = "logit"))  

hist(resid(roftot))

dd <- dredge(roftot)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])


roftot <- lmer(Proportion_Heterosp_Stigma~Proportion_HB+Proportion_Bee+Proportion_Diptera+Proportion_Lepidoptera+(1|Plot/Plant), data=TVUFpollenbitxoswtna)  



ggplot(data = TVUHpollenbitxos, aes(x=Visitation_rate, y=Homospecific_presence)) + 
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  theme_classic()







