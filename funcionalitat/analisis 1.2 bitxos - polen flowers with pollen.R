
source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades plantes.R")
source("funcionalitat/netejar dades databases analisis.R")

# install.packages("glmmADMB", repos="http://R-Forge.R-project.org")
library(glmmADMB)
library(lme4)
library(MuMIn)


######################################### MEAN POLLEN ####################################

###################### ROF

hist(ROFpollenbitxos$Pollinator_richness)   
hist(ROFpollenbitxos$Visitation_rate)       
hist(ROFpollenbitxos$logVisitation_rate)    
hist(ROFpollenbitxos$Functional_group_Rocka) 
hist(ROFpollenbitxos$Bee_VR)        
hist(ROFpollenbitxos$Coleoptera_VR) 
hist(ROFpollenbitxos$Diptera_VR)    
hist(ROFpollenbitxos$Lepidoptera_VR) 
hist(ROFpollenbitxos$Wasp_VR) 
hist(ROFpollenbitxos$Honeybees_VR) 

## Functional_group_Rocka
fitROFTotal_tot <- glmer(Total~Functional_group_Rocka+logVisitation_rate+(1|Plot/Plant), data=ROFpollenflowerswithpollen, family=poisson)  

hist(resid(fitROFTotal_tot))

options(na.action = "na.fail")
dd <- dredge(fitROFTotal_tot)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])

## Pollinator_richness

fitROFTotal_tot <- glmer(Total~Pollinator_richness+logVisitation_rate+(1|Plot/Plant), data=ROFpollenflowerswithpollen, family=poisson)  

hist(resid(fitROFTotal_tot))

dd <- dredge(fitROFTotal_tot)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])

## Taxonomic groups per separat 

roftot <- glmer(Total~Bee_VR+Coleoptera_VR+Diptera_VR+Honeybees_VR+(1|Plot/Plant), data=ROFpollenflowerswithpollen, family=poisson)  

hist(resid(roftot))

dd <- dredge(roftot)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])



## Taxonomic groups proporcions

roftot <- glmer(Total~Proportion_HB+Proportion_Bee+Proportion_Diptera+Proportion_Coleoptera+(1|Plot/Plant), data=ROFpollenflowerswithpollen, family=poisson)  

hist(resid(roftot))

dd <- dredge(roftot)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]])



###################### TVUF

hist(TVUFpollenflowerswithpollen$Total)    
hist(TVUFpollenflowerswithpollen$Pollinator_richness)    
hist(TVUFpollenflowerswithpollen$logPollinator_richness) 
hist(TVUFpollenflowerswithpollen$Visitation_rate)        
hist(TVUFpollenflowerswithpollen$logVisitation_rate)     
hist(TVUFpollenflowerswithpollen$Functional_group_Rocka)
hist(TVUFpollenflowerswithpollen$ProporcioF)             
hist(TVUFpollenflowerswithpollen$Bee_VR)       
hist(TVUFpollenflowerswithpollen$Coleoptera_VR)   
hist(TVUFpollenflowerswithpollen$Diptera_VR)      
hist(TVUFpollenflowerswithpollen$Lepidoptera_VR) 
hist(TVUFpollenflowerswithpollen$Wasp_VR) 
hist(TVUFpollenflowerswithpollen$Honeybees_VR)

## Functional_group_Rocka

fitTVUFTotal_fg <- glmer(Total~Functional_group_Rocka+ProporcioF+logVisitation_rate+(1|Plot/Plant), data=TVUFpollenflowerswithpollen, family="poisson")  

hist(resid(fitTVUFTotal_fg))

options(na.action = "na.fail")
dd <- dredge(fitTVUFTotal_fg)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])

## Pollinator_richness

fitTVUFTotal_rich <- glmer(Total~Pollinator_richness+ProporcioF+logVisitation_rate+(1|Plot/Plant), data=TVUFpollenflowerswithpollen, family=poisson)  

hist(resid(fitTVUFTotal_rich))

dd <- dredge(fitTVUFTotal_rich)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])

## Taxonomic groups per separat 

tvuftot <- glmer(Total~Bee_VR+Lepidoptera_VR+Diptera_VR+Honeybees_VR+(1|Plot/Plant), data=TVUFpollenflowerswithpollen, family=poisson)  

hist(resid(tvuftot))

dd <- dredge(tvuftot)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])



## Taxonomic groups proporcions

roftot <- glmer(Total~Proportion_HB+Proportion_Bee+Proportion_Diptera+Proportion_Lepidoptera+(1|Plot/Plant), data=TVUFpollenflowerswithpollen, family=poisson)  

hist(resid(roftot))

dd <- dredge(roftot)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]])




###################### TVUH

hist(TVUHpollenbitxos$Pollinator_richness)    
hist(TVUHpollenbitxos$logPollinator_richness) 
hist(TVUHpollenbitxos$Visitation_rate)        
hist(TVUHpollenbitxos$logVisitation_rate)     
hist(TVUHpollenbitxos$Functional_group_Rocka)
hist(TVUHpollenbitxos$ProporcioF)             
hist(TVUHpollenbitxos$Bee_VR)       
hist(TVUHpollenbitxos$Coleoptera_VR)   
hist(TVUHpollenbitxos$Diptera_VR)      
hist(TVUHpollenbitxos$Lepidoptera_VR) 
hist(TVUHpollenbitxos$Wasp_VR) 
hist(TVUHpollenbitxos$Honeybees_VR)

## Functional_group_Rocka

fitTVUHTotal_fg <- glmer(Total~Functional_group_Rocka+ProporcioF+logVisitation_rate+(1|Plot/Plant), data=TVUHpollenflowerswithpollen, family=poisson)  

hist(resid(fitTVUHTotal_fg))

options(na.action = "na.fail")
dd <- dredge(fitTVUHTotal_fg)
# summary(get.models(dd, 1)[[1]])

## Pollinator_richness

fitTVUHTotal_rich <- glmer(Total~Pollinator_richness+ProporcioF+logVisitation_rate+(1|Plot/Plant), data=TVUHpollenflowerswithpollen, family=poisson) 

hist(resid(fitTVUHTotal_rich))

dd <- dredge(fitTVUHTotal_rich)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])

## Taxonomic groups per separat 

tvuhtot <- glmer(Total~Bee_VR+Coleoptera_VR+Diptera_VR+Honeybees_VR+(1|Plot/Plant), data=TVUHpollenflowerswithpollen, family=poisson)  

hist(resid(tvuhtot))

dd <- dredge(tvuhtot)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])



## Taxonomic groups proporcions

roftot <- glmer(Total~Proportion_HB+Proportion_Bee+Proportion_Diptera+Proportion_Lepidoptera+(1|Plot/Plant), data=TVUHpollenflowerswithpollen, family=poisson)  

hist(resid(roftot))

dd <- dredge(roftot)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]])
