
source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades plantes.R")
source("funcionalitat/grups funcionals.R")

library(lme4)
library(MuMIn)



######################################### MEAN POLLEN ####################################

###### ROF
databaseglmROF <- ROFpollen %>%
  group_by(Plot, Species) %>% 
  summarise(Mean_pollen=mean(Total))%>%
  left_join(datapollinatorsall,by=c("Species","Plot")) %>%
  left_join(numerogrupsfuncionals,by=c("Species","Plot")) %>%
  mutate(logVisitation_rate = log(Visitation_rate))

# hist(databaseglmROF$Pollinator_richness)  #normal
hist(databaseglmROF$Visitation_rate)        #skewed
hist(databaseglmROF$logVisitation_rate)     #normal
hist(databaseglmROF$Functional_group_Rocka) #normal


## model incloent totes les variables juntes

fitROFTotal_tot <- lm(Mean_pollen~Functional_group_Rocka+logVisitation_rate, data=databaseglmROF)
summary(fitROFTotal_tot) ## res significatiu

hist(resid(fitROFTotal_tot))

# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitROFTotal_tot)
subset(dd, delta < 2)
#'Best' model
summary(get.models(dd, 1)[[1]])




###### TVUF
flors <- read.table("dades/flors quantitatiu separant thymus morfs.txt",header=T) %>%
  select(., TVUF, TVUH) %>%
  mutate(Plot = c(1:40)) %>%
  mutate(TVU = TVUF+TVUH) %>%
  mutate(ProporcioF = TVUF / TVU) %>%
  select(., c(Plot,ProporcioF))

databaseglmTVUF <- TVUFpollen %>%
  group_by(Plot, Species) %>% 
  summarise(Mean_pollen=mean(Total))%>%
  left_join(datapollinatorsall,by=c("Species","Plot")) %>%
  left_join(numerogrupsfuncionals,by=c("Species","Plot")) %>%
  mutate(logVisitation_rate = log(Visitation_rate))%>%
  mutate(logFunctional_group_Rocka = log(Functional_group_Rocka))%>%
  mutate(logPollinator_richness = log(Pollinator_richness))%>%
  left_join(flors,by="Plot")



hist(databaseglmTVUF$Visitation_rate)           #skewed
hist(databaseglmTVUF$logVisitation_rate)        #normal
hist(databaseglmTVUF$Functional_group_Rocka)    #normal
hist(databaseglmTVUF$logFunctional_group_Rocka) #normal
hist(databaseglmTVUF$ProporcioF)               #normal
hist(databaseglmTVUF$Pollinator_richness)               #skewed
hist(databaseglmTVUF$logPollinator_richness)               #normal

## model incloent totes les variables juntes

fitTVUFTotal_tot <- lm(Mean_pollen~ProporcioF+logFunctional_group_Rocka+logVisitation_rate, data=databaseglmTVUF)
summary(fitTVUFTotal_tot) ## res significatiu


# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitTVUFTotal_tot)
subset(dd, delta < 2)
#'Best' model
summary(get.models(dd, 1)[[1]])

## model incloent totes les variables juntes

fitTVUFTotal_tot <- lm(Mean_pollen~ProporcioF+logPollinator_richness+logVisitation_rate, data=databaseglmTVUF)
summary(fitTVUFTotal_tot) ## res significatiu


# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitTVUFTotal_tot)
subset(dd, delta < 2)
#'Best' model
summary(get.models(dd, 1)[[1]])

###### TVUH
databaseglmTVUH <- TVUHpollen %>%
  group_by(Plot, Species) %>% 
  summarise(Mean_pollen=mean(Total))%>%
  left_join(datapollinatorsall,by=c("Species","Plot")) %>%
  left_join(numerogrupsfuncionals,by=c("Species","Plot")) %>%
  mutate(logVisitation_rate = log(Visitation_rate))%>%
  mutate(logFunctional_group_Rocka = log(Functional_group_Rocka))%>%
  mutate(logPollinator_richness = log(Pollinator_richness))%>%
  left_join(flors,by="Plot")%>%
  filter(Pollinator_richness > -10)

hist(databaseglmTVUH$Pollinator_richness)    #skewed
hist(databaseglmTVUH$logPollinator_richness) #normal
hist(databaseglmTVUH$Visitation_rate)        #skewed
hist(databaseglmTVUH$logVisitation_rate)     #normal
hist(databaseglmTVUH$Functional_group_Rocka) #sweked
hist(databaseglmTVUH$logFunctional_group_Rocka) #normal



## model incloent totes les variables juntes

fitTVUHTotal_tot <- lm(Mean_pollen~ProporcioF+logFunctional_group_Rocka+logVisitation_rate, data=databaseglmTVUH)
summary(fitTVUHTotal_tot) ## res significatiu



# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitTVUHTotal_tot)
subset(dd, delta < 2)
#'Best' model
summary(get.models(dd, 1)[[1]])


## model incloent totes les variables juntes

fitTVUHTotal_tot <- lm(Mean_pollen~ProporcioF+logPollinator_richness+logVisitation_rate, data=databaseglmTVUH)
summary(fitTVUHTotal_tot) ## res significatiu



# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitTVUHTotal_tot)
subset(dd, delta < 2)
#'Best' model
summary(get.models(dd, 1)[[1]])
