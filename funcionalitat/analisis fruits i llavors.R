
source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades fruits i llavors.R")


library(lme4)
library(MuMIn)


####################################### Fecunditat

###### TVUF

flors <- read.table("dades/flors quantitatiu separant thymus morfs.txt",header=T) %>%
  select(., TVUF, TVUH)%>%
  tidyr::gather(Species, "Flower_Abundance",1:2) 

flors$Plot <- c(1:40)

flors <- flors %>%
  group_by(Plot) %>% 
  summarise(TVU_abundance=sum(Flower_Abundance))


databaseglmTVUFsoratio <- fruitset %>%
  filter(Species == "TVUF") %>%
  left_join(datapollinatorsall,by=c("Species","Plot"))%>%
  left_join(numerogrupsfuncionals,by=c("Species","Plot")) %>%
  left_join(flors,by=c("Plot"))%>%
  mutate(ProporcioF = Flower_Abundance/TVU_abundance)%>%
  mutate(logPollinator_richness = log(Pollinator_richness)) %>%
  mutate(logVisitation_rate = log(Visitation_rate)) 

hist(databaseglmTVUFfruits$Pollinator_richness)    #skewed
hist(databaseglmTVUFfruits$logPollinator_richness) #normal
hist(databaseglmTVUFfruits$Visitation_rate)        #skewed
hist(databaseglmTVUFfruits$logVisitation_rate)     #normal
hist(databaseglmTVUFfruits$Shannon_Diversity)      #normal
hist(databaseglmTVUFfruits$Functional_group_Rocka) #normal


## model incloent totes les variables juntes - FUNCTIONAL GROUPS

fitTVUFsoratio_fg <- glmer(Seed~ProporcioF+Functional_group_Rocka+logVisitation_rate+(1|Plot/Plant), data=databaseglmTVUFsoratio, family=poisson)  
summary(fitTVUFsoratio_fg) ## res significatiu


# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitTVUFsoratio_fg)
subset(dd, delta < 2)
#'Best' model
# summary(get.models(dd, 1)[[1]])



###### TVUH

databaseglmTVUHsoratio <- fruitset %>%
  filter(Species == "TVUH") %>%
  left_join(datapollinatorsall,by=c("Species","Plot"))%>%
  left_join(numerogrupsfuncionals,by=c("Species","Plot")) %>%
  left_join(flors,by=c("Plot"))%>%
  mutate(ProporcioF = Flower_Abundance/TVU_abundance)%>%
  mutate(logPollinator_richness = log(Pollinator_richness)) %>%
  mutate(logVisitation_rate = log(Visitation_rate)) %>%
  mutate(logHB_Visitation_rate = log(HB_Visitation_rate)) %>%
  mutate(logFunctional_group_Rocka = log(Functional_group_Rocka)) %>%
  filter(Pollinator_richness > 0)

hist(databaseglmTVUHfruits$Pollinator_richness)    #skewed
hist(databaseglmTVUHfruits$logPollinator_richness) #normal
hist(databaseglmTVUHfruits$Visitation_rate)        #skewed
hist(databaseglmTVUHfruits$logVisitation_rate)     #normal
hist(databaseglmTVUHfruits$Shannon_Diversity)      #normal
hist(databaseglmTVUHfruits$Functional_group_Rocka) #sweked
hist(databaseglmTVUHfruits$logFunctional_group_Rocka) #normal



## model incloent totes les variables juntes

fitTVUHsoratio_fg <- glmer(Seed~ProporcioF+logFunctional_group_Rocka+logVisitation_rate+(1|Plot/Plant), data=databaseglmTVUHsoratio, family=poisson)  
summary(fitTVUHfruits_fg) ## res significatiu



# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitTVUHsoratio_fg)
subset(dd, delta < 2)
#'Best' model
summary(get.models(dd, 1)[[1]])



####################################### Fruits

###### TVUF

flors <- read.table("dades/flors quantitatiu separant thymus morfs.txt",header=T) %>%
  select(., TVUF, TVUH)%>%
  tidyr::gather(Species, "Flower_Abundance",1:2) 

flors$Plot <- c(1:40)

flors <- flors %>%
  group_by(Plot) %>% 
  summarise(TVU_abundance=sum(Flower_Abundance))
  

databaseglmTVUFfruits <- fruitset %>%
  filter(Species == "TVUF") %>%
  left_join(datapollinatorsall,by=c("Species","Plot"))%>%
  left_join(numerogrupsfuncionals,by=c("Species","Plot")) %>%
  left_join(flors,by=c("Plot"))%>%
  mutate(ProporcioF = Flower_Abundance/TVU_abundance)%>%
  mutate(logPollinator_richness = log(Pollinator_richness)) %>%
  mutate(logVisitation_rate = log(Visitation_rate)) 

hist(databaseglmTVUFfruits$Fruits)    #skewed

hist(databaseglmTVUFfruits$Pollinator_richness)    #skewed
hist(databaseglmTVUFfruits$logPollinator_richness) #normal
hist(databaseglmTVUFfruits$Visitation_rate)        #skewed
hist(databaseglmTVUFfruits$logVisitation_rate)     #normal
hist(databaseglmTVUFfruits$Shannon_Diversity)      #normal
hist(databaseglmTVUFfruits$Functional_group_Rocka) #normal


## model incloent totes les variables juntes - FUNCTIONAL GROUPS

fitTVUFfruits_fg <- glmer(Fruits~ProporcioF+Functional_group_Rocka+logVisitation_rate+(1|Plot/Plant), data=databaseglmTVUFfruits, family=binomial)  
summary(fitTVUFfruits_fg) ## res significatiu


# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitTVUFfruits_fg)
subset(dd, delta < 2)
#'Best' model
# summary(get.models(dd, 1)[[1]])


###### TVUH

databaseglmTVUHfruits <- fruitset %>%
  filter(Species == "TVUH") %>%
  left_join(datapollinatorsall,by=c("Species","Plot"))%>%
  left_join(numerogrupsfuncionals,by=c("Species","Plot")) %>%
  left_join(flors,by=c("Plot"))%>%
  mutate(ProporcioF = Flower_Abundance/TVU_abundance)%>%
  mutate(logPollinator_richness = log(Pollinator_richness)) %>%
  mutate(logVisitation_rate = log(Visitation_rate)) %>%
  mutate(logHB_Visitation_rate = log(HB_Visitation_rate)) %>%
  mutate(logFunctional_group_Rocka = log(Functional_group_Rocka)) %>%
  filter(Pollinator_richness > 0)

hist(databaseglmTVUHfruits$Pollinator_richness)    #skewed
hist(databaseglmTVUHfruits$logPollinator_richness) #normal
hist(databaseglmTVUHfruits$Visitation_rate)        #skewed
hist(databaseglmTVUHfruits$logVisitation_rate)     #normal
hist(databaseglmTVUHfruits$Shannon_Diversity)      #normal
hist(databaseglmTVUHfruits$Functional_group_Rocka) #sweked
hist(databaseglmTVUHfruits$logFunctional_group_Rocka) #normal



## model incloent totes les variables juntes

fitTVUHfruits_fg <- glmer(Fruits~ProporcioF+logFunctional_group_Rocka+logVisitation_rate+(1|Plot/Plant), data=databaseglmTVUHfruits, family=binomial)  
summary(fitTVUHfruits_fg) ## res significatiu



# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitTVUHfruits_fg)
subset(dd, delta < 2)
#'Best' model
summary(get.models(dd, 1)[[1]])




####################################### Llavors

###### TVUF

databaseglmTVUFseeds <- databaseglmTVUFfruits %>%
  filter(Fruits == 1) 


## model incloent totes les variables juntes - FUNCTIONAL GROUPS

fitTVUFseeds_fg <- glmer(Seed~ProporcioF+Functional_group_Rocka+logVisitation_rate+(1|Plot/Plant), data=databaseglmTVUFseeds, family=poisson)  
summary(fitTVUFseeds_fg) ## res significatiu


# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitTVUFseeds_fg)
subset(dd, delta < 2)
#'Best' model
summary(get.models(dd, 1)[[1]])



###### TVUH

databaseglmTVUHseeds <- databaseglmTVUHfruits %>%
  filter(Fruits == 1) 


## model incloent totes les variables juntes

fitTVUHseeds_fg <- glmer(Seed~ProporcioF+logFunctional_group_Rocka+logVisitation_rate+(1|Plot/Plant), data=databaseglmTVUHseeds, family=poisson)  
summary(fitTVUHseeds_fg) ## res significatiu



# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitTVUHseeds_fg)
subset(dd, delta < 2)
#'Best' model
summary(get.models(dd, 1)[[1]])

