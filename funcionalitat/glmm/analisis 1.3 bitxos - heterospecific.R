
source("funcionalitat/netejar dades databases analisis.R")

library(lme4)
library(MuMIn)
library(lmerTest)


######################################### TVUF #########################

################## heterospecific presence

fit <- glmer(Heterospecific_presence~generality+Proportion_Heterosp_Community+Proportion_HB+Proportion_Bee+Proportion_Diptera+Proportion_Lepidoptera+(1|Plot/Plant), family=binomial,data=TVUFpollenbitxos)

hist(resid(fit))

options(na.action = "na.fail")
dd <- dredge(fit)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]])

################## proportion heterospecific

fit <- lmer(Proportion_Heterosp_Stigma~generality+Proportion_Heterosp_Community+Proportion_HB+Proportion_Bee+Proportion_Diptera+Proportion_Lepidoptera+(1|Plot/Plant), data=TVUFheterospecificflowerswithheterospecific)  

hist(resid(fit))

options(na.action = "na.fail")
dd <- dredge(fit)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]])


