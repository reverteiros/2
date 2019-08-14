
source("funcionalitat/netejar dades mitjana per parcela.R")

library(MuMIn)

################################### ROF ####################################

meandataperplotROF <- meandataperplot %>%
  filter(Species=="ROF") %>%
  filter(!is.na(Pollinator_richness))

hist(meandataperplotROF$Visitation_rate)        #skewed
hist(meandataperplotROF$logVisitation_rate)     #normal
hist(meandataperplotROF$Functional_group_Rocka) #normal


###### Mean pollen

fitROFpollen <- lm(Mean_pollen~Functional_group_Rocka+logVisitation_rate, data=meandataperplotROF)
options(na.action = "na.fail")
dd <- dredge(fitROFpollen)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]])

hist(resid(fitROFpollen))


################################### TVUF ####################################

meandataperplotTVUF <- meandataperplot %>%
  filter(Species=="TVUF") 

hist(meandataperplotTVUF$Mean_pollen)                 #skewed
hist(meandataperplotTVUF$logMean_pollen)              #normal
hist(meandataperplotTVUF$Fecundity)                   #normal
hist(meandataperplotTVUF$Fruit_set)                   #normal
hist(meandataperplotTVUF$Seed_set)                    #normal
hist(meandataperplotTVUF$Visitation_rate)             #skewed
hist(meandataperplotTVUF$logVisitation_rate)          #normal
hist(meandataperplotTVUF$Functional_group_Rocka)      #skewed
hist(meandataperplotTVUF$logFunctional_group_Rocka)   #normal
hist(meandataperplotTVUF$ProporcioF)                  #normal
hist(meandataperplotTVUF$Pollinator_richness)         #skewed
hist(meandataperplotTVUF$logPollinator_richness)      #normal


###### Mean pollen

fitTVUFpollen <- lm(logMean_pollen~ProporcioF+logFunctional_group_Rocka+logVisitation_rate, data=meandataperplotTVUF)
dd <- dredge(fitTVUFpollen)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]])

hist(resid(fitTVUFpollen))


###### Fecunditat

fitTVUFfecundity <- lm(Fecundity~ProporcioF+logFunctional_group_Rocka+logVisitation_rate, data=meandataperplotTVUF)  
dd <- dredge(fitTVUFfecundity)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]])

hist(resid(fitTVUFfecundity))


###### Fruit set

fitTVUFfruitset <- lm(Fruit_set~ProporcioF+logFunctional_group_Rocka+logVisitation_rate, data=meandataperplotTVUF)  
dd <- dredge(fitTVUFfruitset)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]])

hist(resid(fitTVUFfruitset))


###### Seed set

fitTVUFseeds <- lm(Seed_set~ProporcioF+logFunctional_group_Rocka+logVisitation_rate, data=meandataperplotTVUF)  
dd <- dredge(fitTVUFseeds)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]])

hist(resid(fitTVUFseeds))


################################### TVUH ####################################

meandataperplotTVUH <- meandataperplot %>%
  filter(Species=="TVUH") %>%
  filter(!is.na(Pollinator_richness))

hist(meandataperplotTVUH$Mean_pollen)               #skewed
hist(meandataperplotTVUH$logMean_pollen)            #normal
hist(meandataperplotTVUH$Fecundity)                 #normal
hist(meandataperplotTVUH$Fruit_set)                 #normal
hist(meandataperplotTVUH$Seed_set)                  #normal
hist(meandataperplotTVUH$Pollinator_richness)       #skewed
hist(meandataperplotTVUH$logPollinator_richness)    #normal
hist(meandataperplotTVUH$Visitation_rate)           #skewed
hist(meandataperplotTVUH$logVisitation_rate)        #normal
hist(meandataperplotTVUH$Functional_group_Rocka)    #sweked
hist(meandataperplotTVUH$logFunctional_group_Rocka) #normal

######  Mean pollen

fitTVUHpollen <- lm(logMean_pollen~ProporcioF+logFunctional_group_Rocka+logVisitation_rate, data=meandataperplotTVUH)
dd <- dredge(fitTVUHpollen)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]])

hist(resid(fitTVUHpollen))


####### Fecunditat

fitTVUHfecundity <- lm(Fecundity~ProporcioF+logFunctional_group_Rocka+logVisitation_rate, data=meandataperplotTVUH)  
dd <- dredge(fitTVUHfecundity)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]])

hist(resid(fitTVUHfecundity))


###### Fruit set

fitTVUHfruitset <- lm(Fruit_set~ProporcioF+logFunctional_group_Rocka+logVisitation_rate, data=meandataperplotTVUH)  
dd <- dredge(fitTVUHfruitset)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]])

hist(resid(fitTVUHfruitset))


###### Seed set

fitTVUHseeds <- lm(Seed_set~ProporcioF+logFunctional_group_Rocka+logVisitation_rate, data=meandataperplotTVUH)  
dd <- dredge(fitTVUHseeds)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]])

hist(resid(fitTVUHseeds))
