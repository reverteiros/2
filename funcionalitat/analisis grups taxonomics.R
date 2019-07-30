

source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades plantes.R")

library(lme4)
library(MuMIn)

grupstaxonomicsspread <- grupstaxonomics %>%
  spread(Taxonomic_group, Abundance) %>%
  select(-c(Heteroptera,Mecoptera))
grupstaxonomicsspread[is.na(grupstaxonomicsspread)] <- 0
names(grupstaxonomicsspread) <- c("Plot","Species","Bee","Coleoptera","Diptera","Honeybees","Lepidoptera","Wasp")


###### ROF

grupstaxonomicsanalisisROF <- ROFpollen %>%
  left_join(datapollinatorsall,by=c("Species","Plot")) %>%
  left_join(grupstaxonomicsspread,by=c("Species","Plot")) %>%
  mutate(Bee_VR = (Bee*1000/Flower_Abundance)) %>%
  mutate(Coleoptera_VR = (Coleoptera*1000/Flower_Abundance)) %>%
  mutate(Diptera_VR = (Diptera*1000/Flower_Abundance)) %>%
  mutate(Lepidoptera_VR = (Lepidoptera*1000/Flower_Abundance)) %>%
  mutate(Wasp_VR = (Wasp*1000/Flower_Abundance)) %>%
  mutate(Honeybees_VR = (Honeybees*1000/Flower_Abundance)) 


# hist(databaseglmROF$Pollinator_richness)   #normal
hist(grupstaxonomicsanalisis$Bee_VR)        #skewed
hist(grupstaxonomicsanalisis$Coleoptera_VR)     #normal
hist(grupstaxonomicsanalisis$Diptera_VR)      #normal
hist(grupstaxonomicsanalisis$Lepidoptera_VR) #normalLepidoptera_VR
hist(grupstaxonomicsanalisis$Wasp_VR) #normalLepidoptera_VR
hist(grupstaxonomicsanalisis$Honeybees_VR) #normalLepidoptera_VR


roftot <- glmer(Total~Bee_VR+Coleoptera_VR+Diptera_VR+Honeybees_VR+(1|Plot/Plant), data=grupstaxonomicsanalisis, family=poisson)  
summary(roftot) ## res significatiu

hist(resid(roftot))

# selecció de models

options(na.action = "na.fail")
dd <- dredge(roftot)
subset(dd, delta < 2)
#'Best' model
summary(get.models(dd, 1)[[1]])






###### TVUF

grupstaxonomicsanalisisTVUF <- TVUFpollen %>%
  left_join(datapollinatorsall,by=c("Species","Plot")) %>%
  left_join(grupstaxonomicsspread,by=c("Species","Plot")) %>%
  mutate(Bee_VR = (Bee*1000/Flower_Abundance)) %>%
  mutate(Coleoptera_VR = (Coleoptera*1000/Flower_Abundance)) %>%
  mutate(Diptera_VR = (Diptera*1000/Flower_Abundance)) %>%
  mutate(Lepidoptera_VR = (Lepidoptera*1000/Flower_Abundance)) %>%
  mutate(Wasp_VR = (Wasp*1000/Flower_Abundance)) %>%
  mutate(Honeybees_VR = (Honeybees*1000/Flower_Abundance)) 


# hist(databaseglmROF$Pollinator_richness)   #normal
hist(grupstaxonomicsanalisisTVUF$Bee_VR)        #skewed
hist(grupstaxonomicsanalisisTVUF$Coleoptera_VR)     #normal
hist(grupstaxonomicsanalisisTVUF$Diptera_VR)      #normal
hist(grupstaxonomicsanalisisTVUF$Lepidoptera_VR) #normalLepidoptera_VR
hist(grupstaxonomicsanalisisTVUF$Wasp_VR) #normalLepidoptera_VR
hist(grupstaxonomicsanalisisTVUF$Honeybees_VR) #normalLepidoptera_VR


tvuftot <- glmer(Total~Bee_VR+Diptera_VR+Honeybees_VR+Lepidoptera_VR+Wasp_VR+(1|Plot/Plant), data=grupstaxonomicsanalisisTVUF, family=poisson)  
summary(tvuftot) ## res significatiu

hist(resid(tvuftot))

# selecció de models

options(na.action = "na.fail")
dd <- dredge(tvuftot)
subset(dd, delta < 2)
#'Best' model
summary(get.models(dd, 1)[[1]])






###### TVUH

grupstaxonomicsanalisisTVUH <- TVUHpollen %>%
  left_join(datapollinatorsall,by=c("Species","Plot")) %>%
  left_join(grupstaxonomicsspread,by=c("Species","Plot")) %>%
  mutate(Bee_VR = (Bee*1000/Flower_Abundance)) %>%
  mutate(Coleoptera_VR = (Coleoptera*1000/Flower_Abundance)) %>%
  mutate(Diptera_VR = (Diptera*1000/Flower_Abundance)) %>%
  mutate(Lepidoptera_VR = (Lepidoptera*1000/Flower_Abundance)) %>%
  mutate(Wasp_VR = (Wasp*1000/Flower_Abundance)) %>%
  mutate(Honeybees_VR = (Honeybees*1000/Flower_Abundance)) %>%
  filter(Plot != 7)


# hist(databaseglmROF$Pollinator_richness)   #normal
hist(grupstaxonomicsanalisisTVUH$Bee_VR)        #skewed
hist(grupstaxonomicsanalisisTVUH$Coleoptera_VR)     #normal
hist(grupstaxonomicsanalisisTVUH$Diptera_VR)      #normal
hist(grupstaxonomicsanalisisTVUH$Lepidoptera_VR) #normalLepidoptera_VR
hist(grupstaxonomicsanalisisTVUH$Wasp_VR) #normalLepidoptera_VR
hist(grupstaxonomicsanalisisTVUH$Honeybees_VR) #normalLepidoptera_VR


tvuhtot <- glmer(Total~Bee_VR+Coleoptera_VR+Diptera_VR+Honeybees_VR+(1|Plot/Plant), data=grupstaxonomicsanalisisTVUH, family=poisson)  
summary(tvuhtot) ## res significatiu

hist(resid(tvuhtot))

# selecció de models

options(na.action = "na.fail")
dd <- dredge(tvuhtot)
subset(dd, delta < 2)
#'Best' model
summary(get.models(dd, 1)[[1]])
