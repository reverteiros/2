
source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades fruits i llavors.R")
source("funcionalitat/netejar dades databases analisis.R")

library(lme4)
library(MuMIn)


####################################### Fruit set

###### TVUF

hist(TVUFfruitsbitxos$Pollinator_richness)   
hist(TVUFfruitsbitxos$logPollinator_richness)
hist(TVUFfruitsbitxos$Visitation_rate)       
hist(TVUFfruitsbitxos$logVisitation_rate)    
hist(TVUFfruitsbitxos$Shannon_Diversity)     
hist(TVUFfruitsbitxos$Functional_group_Rocka)


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


## FUNCTIONAL GROUPS

fitTVUHfruits_fg <- glmer(Fruits~ProporcioF+logFunctional_group_Rocka+logVisitation_rate+(1|Plot/Plant), data=TVUHfruitsbitxos, family=binomial)  

hist(resid(fitTVUHfruits_fg))

dd <- dredge(fitTVUHfruits_fg)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])

## RICHNESS

fitTVUHfruits_rich <- glmer(Fruits~ProporcioF+logPollinator_richness+logVisitation_rate+(1|Plot/Plant), data=TVUHfruitsbitxos, family=binomial)  

hist(resid(fitTVUHfruits_rich))

options(na.action = "na.fail")
dd <- dredge(fitTVUHfruits_rich)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])


####################################### Llavors

###### TVUF

## FUNCTIONAL GROUPS

fitTVUFseeds_fg <- glmer(Seed~ProporcioF+Functional_group_Rocka+logVisitation_rate+(1|Plot/Plant), data=TVUFseedsbitxos, family=poisson)  

hist(resid(fitTVUFseeds_fg))

dd <- dredge(fitTVUFseeds_fg)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])

## RICHNESS

fitTVUFseeds_rich <- glmer(Fruits~ProporcioF+logPollinator_richness+logVisitation_rate+(1|Plot/Plant), data=TVUFseedsbitxos, family=binomial)  

hist(resid(fitTVUFseeds_rich))

options(na.action = "na.fail")
dd <- dredge(fitTVUFseeds_rich)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])


###### TVUH

## FUNCTIONAL GROUPS

fitTVUHseeds_fg <- glmer(Seed~ProporcioF+logPollinator_richness+logVisitation_rate+(1|Plot/Plant), data=TVUHseedsbitxos, family=poisson)  

hist(resid(fitTVUFseeds_fg))

dd <- dredge(fitTVUHseeds_fg)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])

## RICHNESS

fitTVUHseeds_rich <- glmer(Fruits~ProporcioF+logPollinator_richness+logVisitation_rate+(1|Plot/Plant), data=TVUHseedsbitxos, family=binomial)  

hist(resid(fitTVUHseeds_rich))

options(na.action = "na.fail")
dd <- dredge(fitTVUHseeds_rich)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])


################### separant grups taxonomics bixos

## Fruit set

## TVUF

fitTVUFtaxonimicfruits <- glmer(Fruits~Bee_VR+Coleoptera_VR+Diptera_VR+Honeybees_VR+Lepidoptera_VR+(1|Plot/Plant), data=TVUFfruitsbitxos, family=binomial)  

hist(resid(fitTVUFtaxonimicfruits))

dd <- dredge(fitTVUFtaxonimicfruits)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])


## TVUH

fitTVUHtaxonimicfruits <- glmer(Fruits~Bee_VR+Coleoptera_VR+Diptera_VR+Honeybees_VR+(1|Plot/Plant), data=TVUHfruitsbitxos, family=binomial)  

hist(resid(fitTVUHtaxonimicfruits))

dd <- dredge(fitTVUHtaxonimicfruits)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])


## Seed set
## TVUF

fitTVUFtaxonimicseeds <- glmer(Seed~Bee_VR+Coleoptera_VR+Diptera_VR+Honeybees_VR+Lepidoptera_VR+(1|Plot/Plant), data=TVUFseedsbitxos, family=poisson)  

hist(resid(fitTVUHtaxonimicfruits))

dd <- dredge(fitTVUFtaxonimicseeds)
subset(dd, delta < 2)

## TVUH

fitTVUHtaxonimicseeds <- glmer(Seed~Bee_VR+Coleoptera_VR+Diptera_VR+Honeybees_VR+(1|Plot/Plant), data=TVUHseedsbitxos, family=poisson)  

hist(resid(fitTVUHtaxonimicfruits))

dd <- dredge(fitTVUHtaxonimicseeds)
subset(dd, delta < 2)
