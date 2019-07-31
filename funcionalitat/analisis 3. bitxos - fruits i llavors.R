
source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades fruits i llavors.R")
source("funcionalitat/analisis 1.1 separant grups taxonomics.R")

library(lme4)
library(MuMIn)


####################################### Fecunditat

###### TVUF

proporciomorfs <- read.table("dades/flors quantitatiu separant thymus morfs.txt",header=T) %>%
  select(., TVUF, TVUH) %>%
  mutate(Plot = c(1:40)) %>%
  mutate(TVU = TVUF+TVUH) %>%
  mutate(ProporcioF = TVUF / TVU) %>%
  select(., c(Plot,ProporcioF))


databaseglmTVUFsoratio <- fruitset %>%
  filter(Species == "TVUF") %>%
  left_join(datapollinatorsall,by=c("Species","Plot"))%>%
  left_join(proporciomorfs,by=c("Plot"))%>%
  mutate(logPollinator_richness = log(Pollinator_richness)) %>%
  mutate(logVisitation_rate = log(Visitation_rate)) 

hist(databaseglmTVUFfruits$Pollinator_richness)    #skewed
hist(databaseglmTVUFfruits$logPollinator_richness) #normal
hist(databaseglmTVUFfruits$Visitation_rate)        #skewed
hist(databaseglmTVUFfruits$logVisitation_rate)     #normal
hist(databaseglmTVUFfruits$Shannon_Diversity)      #normal
hist(databaseglmTVUFfruits$Functional_group_Rocka) #normal


## model incloent totes les variables juntes - FUNCTIONAL GROUPS

fitTVUFfruits_fg <- glmer(Fruits~ProporcioF+Functional_group_Rocka+logVisitation_rate+(1|Plot/Plant), data=databaseglmTVUFsoratio, family=binomial)  
summary(fitTVUFfruits_fg) ## res significatiu


hist(resid(fitTVUFfruits_fg))

# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitTVUFfruits_fg)
subset(dd, delta < 2)
#'Best' model
# summary(get.models(dd, 1)[[1]])



###### TVUH

databaseglmTVUHsoratio <- fruitset %>%
  filter(Species == "TVUH") %>%
  left_join(datapollinatorsall,by=c("Species","Plot"))%>%
  left_join(proporciomorfs,by=c("Plot"))%>%
  mutate(logPollinator_richness = log(Pollinator_richness)) %>%
  mutate(logVisitation_rate = log(Visitation_rate)) %>%
  mutate(logFunctional_group_Rocka = log(Functional_group_Rocka)) %>%
  filter(Pollinator_richness > 0)

hist(databaseglmTVUHsoratio$Pollinator_richness)    #skewed
hist(databaseglmTVUHsoratio$logPollinator_richness) #normal
hist(databaseglmTVUHsoratio$Visitation_rate)        #skewed
hist(databaseglmTVUHsoratio$logVisitation_rate)     #normal
hist(databaseglmTVUHsoratio$Shannon_Diversity)      #normal
hist(databaseglmTVUHsoratio$Functional_group_Rocka) #sweked
hist(databaseglmTVUHsoratio$logFunctional_group_Rocka) #normal
hist(databaseglmTVUHsoratio$Seed) #normal


## model incloent totes les variables juntes

fitTVUHfruits_fg <- glmer(Fruits~ProporcioF+logFunctional_group_Rocka+logVisitation_rate+(1|Plot/Plant), data=databaseglmTVUHsoratio, family=binomial)  
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

databaseglmTVUFseeds <- databaseglmTVUFsoratio %>%
  filter(Fruits == 1) 


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
grupstaxonomicsseedsTVUF <- databaseglmTVUFseeds %>%
  left_join(grupstaxonomicsspread,by=c("Species","Plot")) %>%
  mutate(Bee_VR = (Bee*1000/(3*Flower_Abundance))) %>%
  mutate(Coleoptera_VR = (Coleoptera*1000/(3*Flower_Abundance))) %>%
  mutate(Diptera_VR = (Diptera*1000/(3*Flower_Abundance))) %>%
  mutate(Lepidoptera_VR = (Lepidoptera*1000/(3*Flower_Abundance))) %>%
  mutate(Wasp_VR = (Wasp*1000/(3*Flower_Abundance))) %>%
  mutate(Honeybees_VR = (Honeybees*1000/(3*Flower_Abundance)))

## model incloent totes les variables juntes

fitTVUFtaxonimicseeds <- glmer(Seed~Bee_VR+Coleoptera_VR+Diptera_VR+Honeybees_VR+Lepidoptera_VR+(1|Plot/Plant), data=grupstaxonomicsseedsTVUF, family=poisson)  
summary(fitTVUFtaxonimicseeds) ## res significatiu

# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitTVUFtaxonimicseeds)
subset(dd, delta < 2)
#'Best' model
summary(get.models(dd, 1)[[1]])



## TVUH
grupstaxonomicsseedsTVUH <- databaseglmTVUHseeds %>%
  left_join(grupstaxonomicsspread,by=c("Species","Plot")) %>%
  mutate(Bee_VR = (Bee*1000/(3*Flower_Abundance))) %>%
  mutate(Coleoptera_VR = (Coleoptera*1000/(3*Flower_Abundance))) %>%
  mutate(Diptera_VR = (Diptera*1000/(3*Flower_Abundance))) %>%
  mutate(Lepidoptera_VR = (Lepidoptera*1000/(3*Flower_Abundance))) %>%
  mutate(Wasp_VR = (Wasp*1000/(3*Flower_Abundance))) %>%
  mutate(Honeybees_VR = (Honeybees*1000/(3*Flower_Abundance)))

## model incloent totes les variables juntes

fitTVUHtaxonimicseeds <- glmer(Seed~Bee_VR+Coleoptera_VR+Diptera_VR+Honeybees_VR+(1|Plot/Plant), data=grupstaxonomicsseedsTVUH, family=poisson)  
summary(fitTVUHtaxonimicseeds) ## res significatiu

# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitTVUHtaxonimicseeds)
subset(dd, delta < 2)
#'Best' model
summary(get.models(dd, 1)[[1]])
