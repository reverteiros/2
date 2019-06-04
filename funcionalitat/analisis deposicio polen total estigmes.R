
source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades plantes.R")
source("funcionalitat/grups funcionals.R")

library(lme4)
library(MuMIn)


hist(databaseglmROF$Heterospecific,xlim=c(-1,8),breaks=20)
hist(databaseglmTVUF$Heterospecific,xlim=c(-1,80),breaks=200)
hist(databaseglmTVUH$Heterospecific,xlim=c(-1,20),breaks=30)


######################################### MEAN POLLEN ####################################

###### ROF
databaseglmROF <- ROFpollen %>%
  left_join(datapollinatorsall,by=c("Species","Plot")) %>%
  left_join(numerogrupsfuncionals,by=c("Species","Plot")) %>%
  mutate(logVisitation_rate = log(Visitation_rate))

# hist(databaseglmROF$Pollinator_richness)   #normal
hist(databaseglmROF$Visitation_rate)        #skewed
hist(databaseglmROF$logVisitation_rate)     #normal
hist(databaseglmROF$Shannon_Diversity)      #normal
hist(databaseglmROF$Functional_group_Rocka) #normal


## model incloent totes les variables juntes

fitROFTotal_tot <- glmer(Total~Functional_group_Rocka+logVisitation_rate+Shannon_Diversity+(1|Plot/Plant), data=databaseglmROF, family=poisson)  
summary(fitROFTotal_tot) ## res significatiu

# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitROFTotal_tot)
subset(dd, delta < 4)
#'Best' model
summary(get.models(dd, 1)[[1]])

## model incloent totes les variables juntes

fitROFTotal_tot <- glmer(Total~Pollinator_richness+logVisitation_rate+Shannon_Diversity+(1|Plot/Plant), data=databaseglmROF, family=poisson)  
summary(fitROFTotal_tot) ## res significatiu

# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitROFTotal_tot)
subset(dd, delta < 4)
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
  left_join(datapollinatorsall,by=c("Species","Plot"))%>%
  left_join(numerogrupsfuncionals,by=c("Species","Plot")) %>%
  mutate(logPollinator_richness = log(Pollinator_richness)) %>%
  mutate(logVisitation_rate = log(Visitation_rate)) %>%
  left_join(flors,by="Plot")

hist(databaseglmTVUF$Pollinator_richness)    #skewed
hist(databaseglmTVUF$logPollinator_richness) #normal
hist(databaseglmTVUF$Visitation_rate)        #skewed
hist(databaseglmTVUF$logVisitation_rate)     #normal
hist(databaseglmTVUF$Shannon_Diversity)      #normal
hist(databaseglmTVUF$Functional_group_Rocka) #normal
hist(databaseglmTVUF$ProporcioF)             #normal


## model incloent totes les variables juntes

fitTVUFTotal_tot <- glmer(Total~ProporcioF+Functional_group_Rocka+logVisitation_rate+Shannon_Diversity+(1|Plot/Plant), data=databaseglmTVUF, family=poisson)  
summary(fitTVUFTotal_tot) ## res significatiu


# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitTVUFTotal_tot)
subset(dd, delta < 4)
#'Best' model
summary(get.models(dd, 1)[[1]])



###### TVUH
databaseglmTVUH <- TVUHpollen %>%
  left_join(datapollinatorsall,by=c("Species","Plot"))%>%
  left_join(numerogrupsfuncionals,by=c("Species","Plot")) %>%
  mutate(logPollinator_richness = log(Pollinator_richness)) %>%
  mutate(logVisitation_rate = log(Visitation_rate)) %>%
  mutate(logHB_Visitation_rate = log(HB_Visitation_rate)) %>%
  mutate(logFunctional_group_Rocka = log(Functional_group_Rocka)) %>%
  # filter(Plant != 11) %>%
  left_join(flors,by="Plot")%>%
  filter(Pollinator_richness > -10)

hist(databaseglmTVUH$Pollinator_richness)    #skewed
hist(databaseglmTVUH$logPollinator_richness) #normal
hist(databaseglmTVUH$Visitation_rate)        #skewed
hist(databaseglmTVUH$logVisitation_rate)     #normal
hist(databaseglmTVUH$Shannon_Diversity)      #normal
hist(databaseglmTVUH$HB_Visitation_rate)     #sweked
hist(databaseglmTVUH$logHB_Visitation_rate)  #normal
hist(databaseglmTVUH$Functional_group_Rocka) #sweked
hist(databaseglmTVUH$logFunctional_group_Rocka) #normal



## model incloent totes les variables juntes

fitTVUHTotal_tot <- glmer(Total~ProporcioF+logFunctional_group_Rocka+logVisitation_rate+Shannon_Diversity+(1|Plot/Plant), data=databaseglmTVUH, family=poisson)  
summary(fitTVUHTotal_tot) ## res significatiu



# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitTVUHTotal_tot)
subset(dd, delta < 4)
#'Best' model
summary(get.models(dd, 1)[[1]])
summary(get.models(dd, 2)[[1]])



