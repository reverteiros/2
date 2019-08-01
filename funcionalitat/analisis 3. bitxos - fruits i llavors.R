
source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades fruits i llavors.R")
source("funcionalitat/netejar dades databases analisis.R")

library(lme4)
library(MuMIn)


####################################### Fruit set

###### TVUF

hist(TVUFfruitsbitxos$Pollinator_richness)    #skewed
hist(TVUFfruitsbitxos$logPollinator_richness) #normal
hist(TVUFfruitsbitxos$Visitation_rate)        #skewed
hist(TVUFfruitsbitxos$logVisitation_rate)     #normal
hist(TVUFfruitsbitxos$Shannon_Diversity)      #normal
hist(TVUFfruitsbitxos$Functional_group_Rocka) #normal


## FUNCTIONAL GROUPS

fitTVUFfruits_fg <- glmer(Fruits~ProporcioF+Functional_group_Rocka+logVisitation_rate+(1|Plot/Plant), data=TVUFfruitsbitxos, family=binomial)  

hist(resid(fitTVUFfruits_fg))

options(na.action = "na.fail")
dd <- dredge(fitTVUFfruits_fg)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])

## RICHNESS

fitTVUFfruits_rich <- glmer(Fruits~ProporcioF+logPollinator_richness+logVisitation_rate+(1|Plot/Plant), data=TVUFfruitsbitxos, family=binomial)  

hist(resid(fitTVUFfruits_rich))

options(na.action = "na.fail")
dd <- dredge(fitTVUFfruits_rich)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])


###### TVUH

hist(TVUHfruitsbitxos$Pollinator_richness)   
hist(TVUHfruitsbitxos$logPollinator_richness) 
hist(TVUHfruitsbitxos$Visitation_rate)      
hist(TVUHfruitsbitxos$logVisitation_rate)    
hist(TVUHfruitsbitxos$Shannon_Diversity)      
hist(TVUHfruitsbitxos$Functional_group_Rocka) 
hist(TVUHfruitsbitxos$logFunctional_group_Rocka)
hist(TVUHfruitsbitxos$Seed) 


## model incloent totes les variables juntes

fitTVUHfruits_fg <- glmer(Fruits~ProporcioF+logFunctional_group_Rocka+logVisitation_rate+(1|Plot/Plant), data=TVUHfruitsbitxos, family=binomial)  
summary(fitTVUHfruits_fg) ## res significatiu


hist(resid(fitTVUHfruits_fg))

# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitTVUHfruits_fg)
subset(dd, delta < 2)
#'Best' model
summary(get.models(dd, 1)[[1]])



####################################### Llavors

###### TVUF

## model incloent totes les variables juntes - FUNCTIONAL GROUPS

fitTVUFseeds_fg <- glmer(Seed~ProporcioF+Functional_group_Rocka+logVisitation_rate+(1|Plot/Plant), data=databaseglmTVUFseeds, family=poisson)  
summary(fitTVUFseeds_fg) ## res significatiu

hist(resid(fitTVUFseeds_fg))

# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitTVUFseeds_fg)
subset(dd, delta < 2)
#'Best' model
summary(get.models(dd, 1)[[1]])



###### TVUH

databaseglmTVUHseeds <- databaseglmTVUHsoratio %>%
  filter(Fruits == 1) 


## model incloent totes les variables juntes

fitTVUHseeds_fg <- glmer(Seed~ProporcioF+logPollinator_richness+logVisitation_rate+(1|Plot/Plant), data=databaseglmTVUHseeds, family=poisson)  
summary(fitTVUHseeds_fg) ## res significatiu



# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitTVUHseeds_fg)
subset(dd, delta < 2)
#'Best' model
summary(get.models(dd, 1)[[1]])




################### separant grups taxonomics bixos

## Fruit set
## TVUF

grupstaxonomicsfruitsTVUF <- databaseglmTVUFsoratio %>%
  left_join(grupstaxonomicsspread,by=c("Species","Plot")) %>%
  mutate(Bee_VR = (Bee*1000/(3*Flower_Abundance))) %>%
  mutate(Coleoptera_VR = (Coleoptera*1000/(3*Flower_Abundance))) %>%
  mutate(Diptera_VR = (Diptera*1000/(3*Flower_Abundance))) %>%
  mutate(Lepidoptera_VR = (Lepidoptera*1000/(3*Flower_Abundance))) %>%
  mutate(Wasp_VR = (Wasp*1000/(3*Flower_Abundance))) %>%
  mutate(Honeybees_VR = (Honeybees*1000/(3*Flower_Abundance)))


## model incloent totes les variables juntes

fitTVUFtaxonimicfruits <- glmer(Fruits~Bee_VR+Coleoptera_VR+Diptera_VR+Honeybees_VR+Lepidoptera_VR+(1|Plot/Plant), data=grupstaxonomicsfruitsTVUF, family=binomial)  
summary(fitTVUFtaxonimicfruits) ## res significatiu

hist(resid(fitTVUFtaxonimicfruits))

# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitTVUFtaxonimicfruits)
subset(dd, delta < 2)
#'Best' model
summary(get.models(dd, 1)[[1]])



## TVUH
grupstaxonomicsfruitsTVUH <- databaseglmTVUHsoratio %>%
  left_join(grupstaxonomicsspread,by=c("Species","Plot")) %>%
  mutate(Bee_VR = (Bee*1000/(3*Flower_Abundance))) %>%
  mutate(Coleoptera_VR = (Coleoptera*1000/(3*Flower_Abundance))) %>%
  mutate(Diptera_VR = (Diptera*1000/(3*Flower_Abundance))) %>%
  mutate(Lepidoptera_VR = (Lepidoptera*1000/(3*Flower_Abundance))) %>%
  mutate(Wasp_VR = (Wasp*1000/(3*Flower_Abundance))) %>%
  mutate(Honeybees_VR = (Honeybees*1000/(3*Flower_Abundance)))

## model incloent totes les variables juntes

fitTVUHtaxonimicfruits <- glmer(Fruits~Bee_VR+Coleoptera_VR+Diptera_VR+Honeybees_VR+(1|Plot/Plant), data=grupstaxonomicsfruitsTVUH, family=binomial)  
summary(fitTVUHtaxonimicfruits) ## res significatiu

# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitTVUHtaxonimicfruits)
subset(dd, delta < 2)
#'Best' model
summary(get.models(dd, 1)[[1]])


## Seed set
## TVUF

fitTVUFtaxonimicseeds <- glmer(Seed~Bee_VR+Coleoptera_VR+Diptera_VR+Honeybees_VR+Lepidoptera_VR+(1|Plot/Plant), data=grupstaxonomicsseedsTVUF, family=poisson)  

dd <- dredge(fitTVUFtaxonimicseeds)
subset(dd, delta < 2)

## TVUH

fitTVUHtaxonimicseeds <- glmer(Seed~Bee_VR+Coleoptera_VR+Diptera_VR+Honeybees_VR+(1|Plot/Plant), data=grupstaxonomicsseedsTVUH, family=poisson)  

dd <- dredge(fitTVUHtaxonimicseeds)
subset(dd, delta < 2)
