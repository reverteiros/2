
source("funcionalitat/netejar dades databases analisis.R")

library(lme4)
library(MuMIn)


######################################### ROF #########################

fit <- glmer(Total_presence~Pollinator_richness+logVisitation_rate+Proportion_HB+Proportion_Bee+Proportion_Diptera+Proportion_Lepidoptera+(1|Plot/Plant), data=ROFpollenbitxos, family=binomial)

fit <- glmer(Total_presence~Pollinator_richness+logVisitation_rate+Proportion_HB+Proportion_Wild+(1|Plot/Plant), data=ROFpollenbitxos, family=binomial)  

hist(resid(fit))

options(na.action = "na.fail")
dd <- dredge(fit)
subset(dd, delta < 2)### mirar com demanar r2
summary(get.models(dd, 1)[[1]])

######################################### TVUF #########################

fit <- glmer(Total_presence~Pollinator_richness+logVisitation_rate+ProporcioF+Proportion_HB+Proportion_Bee+(1|Plot/Plant), data=TVUFpollenbitxos, family=binomial)  
#fit <- glmer(Total_presence~Pollinator_richness+logVisitation_rate+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera+Proportion_Lepidoptera+(1|Plot/Plant), data=TVUFpollenbitxos, family=binomial) 

hist(resid(fit))

dd <- dredge(fit)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]])

######################################### TVUH #########################

fit <- glmer(Homospecific_presence~logPollinator_richness+logVisitation_rate+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera+Proportion_Coleoptera+(1|Plot/Plant), data=TVUHpollenbitxos, family=binomial)  

hist(resid(fit))

dd <- dredge(fit)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]])

