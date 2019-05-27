
source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades plantes.R")

library(lme4)


######################################### MEAN POLLEN ####################################

###### ROF
databaseglmROF <- ROFpollen %>%
  left_join(datapollinatorsall,by=c("Species","Plot")) %>%
  mutate(logVisitation_rate = log(Visitation_rate)) %>%
  mutate(logHB_Visitation_rate = log(HB_Visitation_rate)) %>%
  filter(logHB_Visitation_rate < 0)

hist(databaseglmROF$Pollinator_richness)
hist(databaseglmROF$Visitation_rate)
hist(databaseglmROF$logVisitation_rate)
hist(databaseglmROF$Shannon_Diversity)
hist(databaseglmROF$HB_Visitation_rate)
hist(databaseglmROF$logHB_Visitation_rate)

fitROFTotal_richness <- glmer(Total~Pollinator_richness+(1|Plot/Plant), data=databaseglmROF, family=poisson)  
summary(fitROFTotal_richness)

fitROFTotal_vr <- glmer(Total~Visitation_rate+(1|Plot/Plant), data=databaseglmROF, family=poisson)  
summary(fitROFTotal_vr)

fitROFTotal_logvr <- glmer(Total~logVisitation_rate+(1|Plot/Plant), data=databaseglmROF, family=poisson)  
summary(fitROFTotal_logvr)

fitROFTotal_diversity <- glmer(Total~Shannon_Diversity+(1|Plot/Plant), data=databaseglmROF, family=poisson)  
summary(fitROFTotal_diversity)

fitROFTotal_loghb <- glmer(Total~logHB_Visitation_rate+(1|Plot/Plant), data=databaseglmROF, family=poisson)  
summary(fitROFTotal_loghb)


###### TVUF
databaseglmTVUF <- TVUFpollen %>%
  left_join(datapollinatorsall,by=c("Species","Plot"))

fitTVUFTotal_richness <- glmer(Total~Pollinator_richness+(1|Plot/Plant), data=databaseglmTVUF, family=poisson)  
summary(fitTVUFTotal_richness) ### significatiu!

fitTVUFTotal_vr <- glmer(Total~Visitation_rate+(1|Plot/Plant), data=databaseglmTVUF, family=poisson)  
summary(fitTVUFTotal_vr)

fitTVUFTotal_diversity <- glmer(Total~Shannon_Diversity+(1|Plot/Plant), data=databaseglmTVUF, family=poisson)  
summary(fitTVUFTotal_diversity)

fitTVUFTotal_hb <- glmer(Total~HB_Visitation_rate+(1|Plot/Plant), data=databaseglmTVUF, family=poisson)  
summary(fitTVUFTotal_hb) ### casi significatiu


###### TVUH
databaseglmTVUH <- TVUHpollen %>%
  left_join(datapollinatorsall,by=c("Species","Plot"))

fitTVUHTotal_richness <- glmer(Total~Pollinator_richness+(1|Plot/Plant), data=databaseglmTVUH, family=poisson)  
summary(fitTVUHTotal_richness) 

fitTVUHTotal_vr <- glmer(Total~Visitation_rate+(1|Plot/Plant), data=databaseglmTVUH, family=poisson)  
summary(fitTVUHTotal_vr) ## significatiu negatiu!!

fitTVUHTotal_diversity <- glmer(Total~Shannon_Diversity+(1|Plot/Plant), data=databaseglmTVUH, family=poisson)  
summary(fitTVUHTotal_diversity)

fitTVUHTotal_hb <- glmer(Total~HB_Visitation_rate+(1|Plot/Plant), data=databaseglmTVUH, family=poisson)  
summary(fitTVUHTotal_hb) ### significatiu negatiu!!
