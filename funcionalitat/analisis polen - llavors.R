

source("funcionalitat/netejar dades fruits i llavors.R")
source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades plantes.R")


library(lme4)
library(MuMIn)



######################################### MEAN POLLEN - FRUIT SET ####################################

###### TVUF
flors <- read.table("dades/flors quantitatiu separant thymus morfs.txt",header=T) %>%
  select(., TVUF, TVUH) %>%
  mutate(Plot = c(1:40)) %>%
  mutate(TVU = TVUF+TVUH) %>%
  mutate(ProporcioF = TVUF / TVU) %>%
  select(., c(Plot,ProporcioF))

databaseglmTVUF <- TVUFpollen %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Mean_pollen=mean(Homospecific))
  
databaseglmTVUFpollenfruits <- fruitset %>%
  filter(Species == "TVUF") %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Fruit_set=mean(Fruits),Fecundity=mean(Seed))%>%
  full_join(databaseglmTVUF,by=c("Plot", "Plant","Species"))%>%
  left_join(datapollinatorsall,by=c("Species","Plot"))%>%
  left_join(numerogrupsfuncionals,by=c("Species","Plot")) %>%
  left_join(flors,by="Plot")%>%
  mutate(logPollinator_richness = log(Pollinator_richness)) %>%
  mutate(logVisitation_rate = log(Visitation_rate)) %>%
  filter(Fruit_set > -1) %>%
  filter(Mean_pollen > -1) 


databaseglmTVUFpollenseeds <- fruitset %>%
  filter(Species == "TVUF") %>%
  filter(Fruits == 1) %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Seed_set=mean(Seed))%>%
  full_join(databaseglmTVUF,by=c("Plot", "Plant","Species"))%>%
  left_join(datapollinatorsall,by=c("Species","Plot"))%>%
  left_join(numerogrupsfuncionals,by=c("Species","Plot")) %>%
  left_join(flors,by="Plot")%>%
  mutate(logPollinator_richness = log(Pollinator_richness)) %>%
  mutate(logVisitation_rate = log(Visitation_rate)) %>%
  filter(Seed_set > -1) %>%
  filter(Mean_pollen > -1) 

hist(databaseglmTVUFpollenseeds$Pollinator_richness)    #skewed
hist(databaseglmTVUFpollenseeds$logPollinator_richness) #normal
hist(databaseglmTVUFpollenseeds$Visitation_rate)        #skewed
hist(databaseglmTVUFpollenseeds$logVisitation_rate)     #normal
hist(databaseglmTVUFpollenseeds$Functional_group_Rocka) #normal
hist(databaseglmTVUFpollenseeds$ProporcioF)             #normal


################# Fruit set

fitTVUFpollenfruits <- lmer(Fruit_set~Mean_pollen+ProporcioF+Functional_group_Rocka+logVisitation_rate+(1|Plot),data=databaseglmTVUFpollenfruits)  
summary(fitTVUFpollenfruits) ## res significatiu


# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitTVUFpollenfruits)
subset(dd, delta < 2)
#'Best' model
summary(get.models(dd, 1)[[1]])


################# Fecundity

fitTVUFpollenfecundity <- lmer(Fecundity~Mean_pollen+ProporcioF+Functional_group_Rocka+logVisitation_rate+(1|Plot), data=databaseglmTVUFpollenfruits)  
summary(fitTVUFpollenfecundity) ## res significatiu

# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitTVUFpollenfecundity)
subset(dd, delta < 2)
#'Best' model
summary(get.models(dd, 1)[[1]])


################# Seed set

fitTVUFpollenseeds <- lmer(Seed_set~Mean_pollen+ProporcioF+logVisitation_rate+Functional_group_Rocka+(1|Plot), data=databaseglmTVUFpollenseeds)  
summary(fitTVUFpollenseeds) ## res significatiu

# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitTVUFpollenseeds)
subset(dd, delta < 2)
#'Best' model
summary(get.models(dd, 1)[[1]])



############################################ TVUH

flors <- read.table("dades/flors quantitatiu separant thymus morfs.txt",header=T) %>%
  select(., TVUF, TVUH) %>%
  mutate(Plot = c(1:40)) %>%
  mutate(TVU = TVUF+TVUH) %>%
  mutate(ProporcioF = TVUF / TVU) %>%
  select(., c(Plot,ProporcioF))

databaseglmTVUH <- TVUHpollen %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Mean_pollen=mean(Homospecific))

databaseglmTVUHpollenfruits <- fruitset %>%
  filter(Species == "TVUH") %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Fruit_set=mean(Fruits),Fecundity=mean(Seed))%>%
  full_join(databaseglmTVUH,by=c("Plot", "Plant","Species"))%>%
  left_join(datapollinatorsall,by=c("Species","Plot"))%>%
  left_join(numerogrupsfuncionals,by=c("Species","Plot")) %>%
  left_join(flors,by="Plot")%>%
  mutate(logPollinator_richness = log(Pollinator_richness)) %>%
  mutate(logVisitation_rate = log(Visitation_rate)) %>%
  mutate(logFunctional_group_Rocka = log(Functional_group_Rocka)) %>%
  filter(Fruit_set > -1) %>%
  filter(Mean_pollen > -1) %>%
  filter(Pollinator_richness > -10) %>%
  select(.,-c(Pollinator_abundance,Flower_Abundance))


databaseglmTVUHpollenseeds <- fruitset %>%
  filter(Species == "TVUH") %>%
  filter(Fruits == 1) %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Seed_set=mean(Seed))%>%
  full_join(databaseglmTVUH,by=c("Plot", "Plant","Species"))%>%
  left_join(datapollinatorsall,by=c("Species","Plot"))%>%
  left_join(numerogrupsfuncionals,by=c("Species","Plot")) %>%
  left_join(flors,by="Plot")%>%
  mutate(logPollinator_richness = log(Pollinator_richness)) %>%
  mutate(logVisitation_rate = log(Visitation_rate)) %>%
  filter(Seed_set > -1) %>%
  filter(Mean_pollen > -1) %>%
  filter(Pollinator_richness > -10)

hist(databaseglmTVUH$Pollinator_richness)    #skewed
hist(databaseglmTVUH$logPollinator_richness) #normal
hist(databaseglmTVUH$Visitation_rate)        #skewed
hist(databaseglmTVUH$logVisitation_rate)     #normal
hist(databaseglmTVUH$HB_Visitation_rate)     #sweked
hist(databaseglmTVUH$logHB_Visitation_rate)  #normal
hist(databaseglmTVUH$Functional_group_Rocka) #sweked
hist(databaseglmTVUH$logFunctional_group_Rocka) #normal



############## Fruit set

fitTVUHpollenfruits <- lmer(Fruit_set~Mean_pollen+ProporcioF+logFunctional_group_Rocka+logVisitation_rate+(1|Plot), data=databaseglmTVUHpollenfruits)  
summary(fitTVUHpollenfruits) ## res significatiu


# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitTVUHpollenfruits)
subset(dd, delta < 2)
#'Best' model
summary(get.models(dd, 1)[[1]])


############## Fecundity

fitTVUHpollenfecundity <- lmer(Fecundity~Mean_pollen+ProporcioF+logFunctional_group_Rocka+logVisitation_rate+(1|Plot), data=databaseglmTVUHpollenfruits)  
summary(fitTVUHpollenfecundity) ## res significatiu

# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitTVUHpollenfecundity)
subset(dd, delta < 2)
#'Best' model
summary(get.models(dd, 1)[[1]])


############## Seed set

fitTVUHpollenseeds <- lmer(Seed_set~Mean_pollen+ProporcioF+logFunctional_group_Rocka+logVisitation_rate+(1|Plot), data=databaseglmTVUHpollenseeds)  
summary(fitTVUHpollenseeds) ## res significatiu

# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitTVUHpollenseeds)
subset(dd, delta < 2)
#'Best' model
summary(get.models(dd, 1)[[1]])
