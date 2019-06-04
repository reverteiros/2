
source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades plantes.R")

library(lme4)
library(MuMIn)
library(lmerTest)

########################################### RATIO HOMOSP-HETEROSP #############################

###### ROF

ROFratio <- ROFpollen %>%
  mutate(Grains_Homospecific = ROF_pollen) %>%
  mutate(Grains_Heterospecific = Other_pollen+TVU_pollen) %>%
  mutate(Grains_Total = ROF_pollen+Other_pollen+TVU_pollen) %>%
  mutate(Proportion_Homosp_Community = Grains_Homospecific/Grains_Total)%>%
  mutate(Proportion_Homosp_Stigma = Homospecific / Total) %>%
  filter(Total > 0)

ROFratioglm <- ROFratio %>%
  left_join(datapollinatorsall,by=c("Species","Plot"))

hist(ROFratioglm$Proportion_Homosp_Community)    #normal
hist(ROFratioglm$H2)                             #normal

fitROFratioglm_prophomosp <- lmer(Proportion_Homosp_Stigma~Proportion_Homosp_Community+(1|Plot/Plant), data=ROFratioglm)  
summary(fitROFratioglm_prophomosp) # significatiu

fitROFratioglm_h2 <- lmer(Proportion_Homosp_Stigma~H2+(1|Plot/Plant), data=ROFratioglm)  
summary(fitROFratioglm_h2) 



## model incloent totes les variables juntes

fitROFratioglm_tot <- lmer(Proportion_Homosp_Stigma~Proportion_Homosp_Community+H2+(1|Plot/Plant), data=ROFratioglm)  
summary(fitROFratioglm_tot) 


# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitROFratioglm_tot)
subset(dd, delta < 4)
#'Best' model
summary(get.models(dd, 4)[[1]])



###### TVUF

TVUFratio <- TVUFpollen%>%
  mutate(Grains_Homospecific = TVU_pollen) %>%
  mutate(Grains_Heterospecific = Other_pollen+ROF_pollen) %>%
  mutate(Grains_Total = ROF_pollen+Other_pollen+TVU_pollen) %>%
  mutate(Proportion_Homosp_Community = Grains_Homospecific/Grains_Total)%>%
  mutate(Proportion_Homosp_Stigma = Homospecific / Total) 

TVUFratioglm <- TVUFratio %>%
  left_join(datapollinatorsall,by=c("Species","Plot"))%>%
  mutate(sqrtProportion_Homosp_Community = sqrt(Proportion_Homosp_Community)) 

hist(TVUFratioglm$Proportion_Homosp_Community)    #skewed
hist(TVUFratioglm$sqrtProportion_Homosp_Community)#normal
hist(TVUFratioglm$H2)                             #normal


TVUFratioglmwithoutnas <- TVUFratioglm %>%
  filter(Proportion_Homosp_Stigma > -100) 

fitTVUFratioglm_sqrtprophomosp <- lmer(Proportion_Homosp_Stigma~sqrtProportion_Homosp_Community+(1|Plot/Plant), data=TVUFratioglmwithoutnas)  
summary(fitTVUFratioglm_sqrtprophomosp) 

fitTVUFratioglm_h2 <- lmer(Proportion_Homosp_Stigma~H2+(1|Plot/Plant), data=TVUFratioglmwithoutnas)  
summary(fitTVUFratioglm_h2) 


## model incloent totes les variables juntes

fitTVUHratioglm_tot <- lmer(Proportion_Homosp_Stigma~sqrtProportion_Homosp_Community+H2+(1|Plot/Plant), data=TVUFratioglmwithoutnas)  
summary(fitTVUHratioglm_tot) ## res significatiu


# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitTVUHratioglm_tot)
subset(dd, delta < 4)
#'Best' model
summary(get.models(dd, 1)[[1]])





###### TVUH

TVUHratio <- TVUHpollen%>%
  mutate(Grains_Homospecific = TVU_pollen) %>%
  mutate(Grains_Heterospecific = Other_pollen+ROF_pollen) %>%
  mutate(Grains_Total = ROF_pollen+Other_pollen+TVU_pollen) %>%
  mutate(Proportion_Homosp_Community = Grains_Homospecific/Grains_Total) %>%
  mutate(Proportion_Homosp_Stigma = Homospecific / Total) 


TVUHratioglm <- TVUHratio %>%
  left_join(datapollinatorsall,by=c("Species","Plot"))%>%
  mutate(sqrtProportion_Homosp_Community = sqrt(Proportion_Homosp_Community)) 

hist(TVUHratioglm$Proportion_Homosp_Community)    #skewed
hist(TVUHratioglm$sqrtProportion_Homosp_Community)#normal
hist(TVUHratioglm$H2)                             #normal

TVUHratioglmwithoutnas <- TVUHratioglm %>%
  filter(Proportion_Homosp_Stigma > -100)

fitTVUHratioglm_sqrtprophomosp <- lmer(Proportion_Homosp_Stigma~sqrtProportion_Homosp_Community+(1|Plot/Plant), data=TVUHratioglmwithoutnas)  
summary(fitTVUHratioglm_sqrtprophomosp) 

fitTVUHratioglm_h2 <- lmer(Proportion_Homosp_Stigma~H2+(1|Plot/Plant), data=TVUHratioglmwithoutnas)  
summary(fitTVUHratioglm_h2) 



## model incloent totes les variables juntes

fitTVUHratioglm_tot <- lmer(Proportion_Homosp_Stigma~sqrtProportion_Homosp_Community+H2+(1|Plot/Plant), data=TVUHratioglmwithoutnas)  
summary(fitTVUHratioglm_tot) ## res significatiu



