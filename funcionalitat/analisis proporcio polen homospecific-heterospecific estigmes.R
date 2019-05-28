
source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades plantes.R")

library(lme4)
library(MuMIn)

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

fitROFratioglm_h2 <- glmer(Proportion_Homosp_Stigma~H2+(1|Plot/Plant), data=ROFratioglm, family=poisson)  
summary(fitROFratioglm_h2) 

ROFratio$Proportion_Homosp_Stigma

## model incloent totes les variables juntes

fitROFratioglm_tot <- glmer(Total~Proportion_Homosp_Community+H2+(1|Plot/Plant), data=ROFratioglm, family=poisson)  
summary(fitROFratioglm_tot) ## el primer significatiu



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

fitTVUFratioglm_sqrtprophomosp <- glmer(Total~sqrtProportion_Homosp_Community+(1|Plot/Plant), data=TVUFratioglm, family=poisson)  
summary(fitTVUFratioglm_sqrtprophomosp) 

fitTVUFratioglm_h2 <- glmer(Total~H2+(1|Plot/Plant), data=TVUFratioglm, family=poisson)  
summary(fitTVUFratioglm_h2) 



## model incloent totes les variables juntes

fitTVUHratioglm_tot <- glmer(Total~sqrtProportion_Homosp_Community+H2+(1|Plot/Plant), data=TVUHratioglm, family=poisson)  
summary(fitTVUHratioglm_tot) ## res significatiu



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

fitTVUHratioglm_sqrtprophomosp <- glmer(Total~sqrtProportion_Homosp_Community+(1|Plot/Plant), data=TVUHratioglm, family=poisson)  
summary(fitTVUHratioglm_sqrtprophomosp) 

fitTVUHratioglm_h2 <- glmer(Total~H2+(1|Plot/Plant), data=TVUHratioglm, family=poisson)  
summary(fitTVUHratioglm_h2) 



## model incloent totes les variables juntes

fitTVUHratioglm_tot <- glmer(Total~sqrtProportion_Homosp_Community+H2+(1|Plot/Plant), data=TVUHratioglm, family=poisson)  
summary(fitTVUHratioglm_tot) ## res significatiu



