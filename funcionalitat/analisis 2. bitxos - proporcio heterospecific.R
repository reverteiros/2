
source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades plantes.R")
source("funcionalitat/netejar dades databases analisis.R")

library(lme4)
library(MuMIn)
library(lmerTest)

################################## RATIO HOMOSP-HETEROSP #######################

###### TVUF

hist(TVUFpollenbitxos$Pollinator_richness)    
hist(TVUFpollenbitxos$logPollinator_richness) 
hist(TVUFpollenbitxos$Proportion_Homosp_Stigma)
hist(TVUFpollenbitxos$Proportion_Homosp_Community)
hist(TVUFpollenbitxos$sqrtProportion_Homosp_Community)
hist(TVUFpollenbitxos$H2)                             

## 

fitTVUFratioglm_tot <- lmer(Proportion_Homosp_Stigma~logFunctional_group_Rocka+logPollinator_richness+H2+(1|Plot/Plant), data=TVUFpollenbitxoswtna)  

hist(resid(fitTVUFratioglm_tot))

options(na.action = "na.fail")
dd <- dredge(fitTVUFratioglm_tot)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])




#'  Massa poques dades per testar l'efecte sobre el pol·len heterospecífic, massa poques flors amb presència de pol·len heterospecífic i no em crec la falta de senyal.
#'  
#' ###### ROF
#' 
#' ROFratio <- ROFpollen %>%
#'   mutate(Grains_Homospecific = ROF_pollen) %>%
#'   mutate(Grains_Heterospecific = Other_pollen+TVU_pollen) %>%
#'   mutate(Grains_Total = ROF_pollen+Other_pollen+TVU_pollen) %>%
#'   mutate(Proportion_Homosp_Community = Grains_Homospecific/Grains_Total)%>%
#'   mutate(Proportion_Homosp_Stigma = Homospecific / Total) %>%
#'   filter(Total > 0)
#' 
#' ROFratioglm <- ROFratio %>%
#'   left_join(datapollinatorsall,by=c("Species","Plot"))
#' 
#' hist(ROFratioglm$Proportion_Homosp_Community)    #normal
#' hist(ROFratioglm$H2)                             #normal
#' hist(ROFratioglm$Proportion_Homosp_Stigma)    #normal
#' 
#' 
#' ## model incloent totes les variables juntes
#' 
#' fitROFratioglm_tot <- lmer(Proportion_Homosp_Stigma~Proportion_Homosp_Community+H2+Functional_group_Rocka+(1|Plot/Plant), data=ROFratioglm)  
#' summary(fitROFratioglm_tot) 
#' 
#' 
#' # selecció de models
#' 
#' options(na.action = "na.fail")
#' dd <- dredge(fitROFratioglm_tot)
#' subset(dd, delta < 2)
#' #'Best' model
#' summary(get.models(dd, 1)[[1]])
#' 
#' 
#' 
#' 
#' ###### TVUH
#' 
#' TVUHratio <- TVUHpollen%>%
#'   mutate(Grains_Homospecific = TVU_pollen) %>%
#'   mutate(Grains_Heterospecific = Other_pollen+ROF_pollen) %>%
#'   mutate(Grains_Total = ROF_pollen+Other_pollen+TVU_pollen) %>%
#'   mutate(Proportion_Homosp_Community = Grains_Homospecific/Grains_Total) %>%
#'   mutate(Proportion_Homosp_Stigma = Homospecific / Total) 
#' 
#' 
#' TVUHratioglm <- TVUHratio %>%
#'   left_join(datapollinatorsall,by=c("Species","Plot"))%>%
#'   mutate(sqrtProportion_Homosp_Community = sqrt(Proportion_Homosp_Community)) 
#' 
#' hist(TVUHratioglm$Proportion_Homosp_Stigma)    #skewed
#' hist(TVUHratioglm$Proportion_Homosp_Community)    #skewed
#' hist(TVUHratioglm$sqrtProportion_Homosp_Community)#normal
#' hist(TVUHratioglm$H2)                             #normal
#' 
#' TVUHratioglmwithoutnas <- TVUHratioglm %>%
#'   filter(Proportion_Homosp_Stigma > -100) %>%
#'   filter(!is.na(Functional_group_Rocka))
#' 
#' 
#' ## model incloent totes les variables juntes
#' 
#' fitTVUHratioglm_tot <- lmer(Proportion_Homosp_Stigma~sqrtProportion_Homosp_Community+H2+Functional_group_Rocka+(1|Plot/Plant), data=TVUHratioglmwithoutnas)  
#' summary(fitTVUHratioglm_tot) ## res significatiu
#' 
#' 
#' 
#' # selecció de models
#' 
#' options(na.action = "na.fail")
#' dd <- dredge(fitTVUHratioglm_tot)
#' subset(dd, delta < 2)
#' #'Best' model
#' summary(get.models(dd, 1)[[1]])



