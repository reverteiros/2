
source("funcionalitat/netejar dades databases analisis.R")

library(lme4)
library(MuMIn)
library(lmerTest)


######################################### TVUF #########################

hist(TVUFpollenbitxoswtna$Pollinator_richness)    
hist(TVUFpollenbitxoswtna$logPollinator_richness) 
hist(TVUFpollenbitxoswtna$Proportion_Heterosp_Stigma)
hist(TVUFpollenbitxoswtna$Proportion_Heterosp_Community)
hist(TVUFpollenbitxoswtna$logProportion_Heterosp_Community)
hist(TVUFpollenbitxoswtna$H2)                             


fitTVUFratioglm_tot <- lmer(Proportion_Heterosp_Stigma~H2+Proportion_Heterosp_Community+Proportion_HB+Proportion_Bee+Proportion_Diptera+Proportion_Lepidoptera+(1|Plot/Plant), data=TVUFpollenbitxoswtna)  

hist(resid(fitTVUFratioglm_tot))

options(na.action = "na.fail")
dd <- dredge(fitTVUFratioglm_tot)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]])
