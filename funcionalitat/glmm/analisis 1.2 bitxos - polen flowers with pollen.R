
source("funcionalitat/netejar dades databases analisis.R")

library(lme4)
library(MuMIn)

######################################### ROF #########################

fit <- glmer(Total~Pollinator_richness+logVisitation_rate+Proportion_HB+Proportion_Bee+Proportion_Diptera+Proportion_Coleoptera+(1|Plot/Plant), data=ROFpollenflowerswithpollen, family=poisson)  

hist(resid(fit))

options(na.action = "na.fail")
dd <- dredge(fit)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])


######################################### TVUF #########################

hist(TVUFpollenflowerswithpollen$Total)    
hist(TVUFpollenflowerswithpollen$Pollinator_richness)    
hist(TVUFpollenflowerswithpollen$logPollinator_richness) 
hist(TVUFpollenflowerswithpollen$Visitation_rate)        
hist(TVUFpollenflowerswithpollen$logVisitation_rate)     
hist(TVUFpollenflowerswithpollen$ProporcioF)             
hist(TVUFpollenflowerswithpollen$Bee_VR)       
hist(TVUFpollenflowerswithpollen$Coleoptera_VR)   
hist(TVUFpollenflowerswithpollen$Diptera_VR)      
hist(TVUFpollenflowerswithpollen$Lepidoptera_VR) 
hist(TVUFpollenflowerswithpollen$Wasp_VR) 
hist(TVUFpollenflowerswithpollen$Honeybees_VR)

## Pollinator_richness

fit <- glmer(Total~Pollinator_richness+ProporcioF+logVisitation_rate+Proportion_HB+Proportion_Bee+Proportion_Diptera+Proportion_Lepidoptera+(1|Plot/Plant), data=TVUFpollenflowerswithpollen, family=poisson)  

hist(resid(fit))

dd <- dredge(fit)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])


######################################### TVUH #########################

hist(TVUHpollenbitxos$Pollinator_richness)    
hist(TVUHpollenbitxos$logPollinator_richness) 
hist(TVUHpollenbitxos$Visitation_rate)        
hist(TVUHpollenbitxos$logVisitation_rate)     
hist(TVUHpollenbitxos$ProporcioF)             
hist(TVUHpollenbitxos$Bee_VR)       
hist(TVUHpollenbitxos$Coleoptera_VR)   
hist(TVUHpollenbitxos$Diptera_VR)      
hist(TVUHpollenbitxos$Lepidoptera_VR) 
hist(TVUHpollenbitxos$Wasp_VR) 
hist(TVUHpollenbitxos$Honeybees_VR)


fit <- glmer(Total~Pollinator_richness+ProporcioF+logVisitation_rate+Proportion_HB+Proportion_Bee+Proportion_Diptera+Proportion_Lepidoptera+(1|Plot/Plant), data=TVUHpollenflowerswithpollen, family=poisson) 

hist(resid(fit))

dd <- dredge(fit)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])

