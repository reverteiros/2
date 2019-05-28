
source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades plantes.R")

library(lme4)
library(MuMIn)


######################################### MEAN POLLEN ####################################

###### ROF
databaseglmROF <- ROFpollen %>%
  left_join(datapollinatorsall,by=c("Species","Plot")) %>%
  mutate(logVisitation_rate = log(Visitation_rate)) %>%
  mutate(logHB_Visitation_rate = log(HB_Visitation_rate))

hist(databaseglmROF$Pollinator_richness)   #normal
hist(databaseglmROF$Visitation_rate)       #skewed
hist(databaseglmROF$logVisitation_rate)    #normal
hist(databaseglmROF$Shannon_Diversity)     #normal
hist(databaseglmROF$HB_Visitation_rate)    #sweked
hist(databaseglmROF$logHB_Visitation_rate) #normal

fitROFTotal_richness <- glmer(Total~Pollinator_richness+(1|Plot/Plant), data=databaseglmROF, family=poisson)  
summary(fitROFTotal_richness)

# fitROFTotal_vr <- glmer(Total~Visitation_rate+(1|Plot/Plant), data=databaseglmROF, family=poisson)  
# summary(fitROFTotal_vr)

fitROFTotal_logvr <- glmer(Total~logVisitation_rate+(1|Plot/Plant), data=databaseglmROF, family=poisson)  
summary(fitROFTotal_logvr)

fitROFTotal_diversity <- glmer(Total~Shannon_Diversity+(1|Plot/Plant), data=databaseglmROF, family=poisson)  
summary(fitROFTotal_diversity)

databaseglmROFwithoutnas <- databaseglmROF %>%
  filter(logHB_Visitation_rate > -100) 

fitROFTotal_loghb <- glmer(Total~logHB_Visitation_rate+(1|Plot/Plant), data=databaseglmROFwithoutnas, family=poisson)  
summary(fitROFTotal_loghb)




## model incloent totes les variables juntes

fitROFTotal_tot <- glmer(Total~Pollinator_richness+logVisitation_rate+Shannon_Diversity+logHB_Visitation_rate+(1|Plot/Plant), data=databaseglmROFwithoutnas, family=poisson)  
summary(fitROFTotal_tot) ## res significatiu

# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitROFTotal_tot)
subset(dd, delta < 4)
#'Best' model
summary(get.models(dd, 4)[[1]])



###### TVUF
databaseglmTVUF <- TVUFpollen %>%
  left_join(datapollinatorsall,by=c("Species","Plot"))%>%
  mutate(logPollinator_richness = log(Pollinator_richness)) %>%
  mutate(logVisitation_rate = log(Visitation_rate)) %>%
  mutate(logHB_Visitation_rate = log(HB_Visitation_rate))

hist(databaseglmTVUF$Pollinator_richness)    #skewed
hist(databaseglmTVUF$logPollinator_richness) #normal
hist(databaseglmTVUF$Visitation_rate)        #skewed
hist(databaseglmTVUF$logVisitation_rate)     #normal
hist(databaseglmTVUF$Shannon_Diversity)      #normal
hist(databaseglmTVUF$HB_Visitation_rate)     #sweked
hist(databaseglmTVUF$logHB_Visitation_rate)  #normal


# fitTVUFTotal_richness <- glmer(Total~Pollinator_richness+(1|Plot/Plant), data=databaseglmTVUF, family=poisson)  
# summary(fitTVUFTotal_richness) ### significatiu!

fitTVUFTotal_logrichness <- glmer(Total~logPollinator_richness+(1|Plot/Plant), data=databaseglmTVUF, family=poisson)  
summary(fitTVUFTotal_logrichness) ### significatiu!

# fitTVUFTotal_vr <- glmer(Total~Visitation_rate+(1|Plot/Plant), data=databaseglmTVUF, family=poisson)  
# summary(fitTVUFTotal_vr)

fitTVUFTotal_logvr <- glmer(Total~logVisitation_rate+(1|Plot/Plant), data=databaseglmTVUF, family=poisson)  
summary(fitTVUFTotal_logvr)

fitTVUFTotal_diversity <- glmer(Total~Shannon_Diversity+(1|Plot/Plant), data=databaseglmTVUF, family=poisson)  
summary(fitTVUFTotal_diversity)

# fitTVUFTotal_hb <- glmer(Total~HB_Visitation_rate+(1|Plot/Plant), data=databaseglmTVUF, family=poisson)  
# summary(fitTVUFTotal_hb) ### casi significatiu

databaseglmTVUFwithoutnas <- databaseglmTVUF %>%
  filter(logHB_Visitation_rate > -100) 

fitTVUFTotal_loghb <- glmer(Total~logHB_Visitation_rate+(1|Plot/Plant), data=databaseglmTVUFwithoutnas, family=poisson)  
summary(fitTVUFTotal_loghb) 




## model incloent totes les variables juntes

fitTVUFTotal_tot <- glmer(Total~logPollinator_richness+logVisitation_rate+Shannon_Diversity+logHB_Visitation_rate+(1|Plot/Plant), data=databaseglmTVUFwithoutnas, family=poisson)  
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
  mutate(logPollinator_richness = log(Pollinator_richness)) %>%
  mutate(logVisitation_rate = log(Visitation_rate)) %>%
  mutate(logHB_Visitation_rate = log(HB_Visitation_rate))

hist(databaseglmTVUH$Pollinator_richness)    #skewed
hist(databaseglmTVUH$logPollinator_richness) #normal
hist(databaseglmTVUH$Visitation_rate)        #skewed
hist(databaseglmTVUH$logVisitation_rate)     #normal
hist(databaseglmTVUH$Shannon_Diversity)      #normal
hist(databaseglmTVUH$HB_Visitation_rate)     #sweked
hist(databaseglmTVUH$logHB_Visitation_rate)  #normal

# fitTVUHTotal_richness <- glmer(Total~Pollinator_richness+(1|Plot/Plant), data=databaseglmTVUH, family=poisson)  
# summary(fitTVUHTotal_richness) 

fitTVUHTotal_logrichness <- glmer(Total~logPollinator_richness+(1|Plot/Plant), data=databaseglmTVUH, family=poisson)  
summary(fitTVUHTotal_logrichness) 

# fitTVUHTotal_vr <- glmer(Total~Visitation_rate+(1|Plot/Plant), data=databaseglmTVUH, family=poisson)  
# summary(fitTVUHTotal_vr) ## significatiu negatiu!!

fitTVUHTotal_logvr <- glmer(Total~logVisitation_rate+(1|Plot/Plant), data=databaseglmTVUH, family=poisson)
summary(fitTVUHTotal_logvr) ## casi significatiu 

fitTVUHTotal_diversity <- glmer(Total~Shannon_Diversity+(1|Plot/Plant), data=databaseglmTVUH, family=poisson)  
summary(fitTVUHTotal_diversity)

# fitTVUHTotal_hb <- glmer(Total~HB_Visitation_rate+(1|Plot/Plant), data=databaseglmTVUH, family=poisson)  
# summary(fitTVUHTotal_hb) 

databaseglmTVUHwithoutnas <- databaseglmTVUH %>%
  filter(logHB_Visitation_rate > -100) 

fitTVUHTotal_loghb <- glmer(Total~logHB_Visitation_rate+(1|Plot/Plant), data=databaseglmTVUHwithoutnas, family=poisson)  
summary(fitTVUHTotal_loghb) ### significatiu negatiu!!



## model incloent totes les variables juntes

fitTVUHTotal_tot <- glmer(Total~logPollinator_richness+logVisitation_rate+Shannon_Diversity+logHB_Visitation_rate+(1|Plot/Plant), data=databaseglmTVUHwithoutnas, family=poisson)  
summary(fitTVUHTotal_tot) ## res significatiu



# selecció de models

options(na.action = "na.fail")
dd <- dredge(fitTVUHTotal_tot)
subset(dd, delta < 4)
#'Best' model
summary(get.models(dd, 1)[[1]])






#################### Relació entre espècies en el polen depositat als estigmes

relacionsespecies <- group_by(pollentotal, Plot, Species) %>% 
  summarise(Mean_pollen=mean(Total))%>%
  complete(Species, Plot) %>%
  distinct() %>%
  spread(Species,Mean_pollen)

ggplot(relacionsespecies) +
  geom_point(aes(ROF,TVUF)) +
  theme_classic() +
  coord_cartesian(xlim = c(0, 32), ylim = c(0, 32))

ggplot(relacionsespecies) +
  geom_point(aes(TVUH,TVUF)) +
  theme_classic() +
  coord_cartesian(xlim = c(0, 32), ylim = c(0, 32))

ggplot(relacionsespecies) +
  geom_point(aes(ROF,TVUH)) +
  theme_classic() +
  coord_cartesian(xlim = c(0, 22), ylim = c(0, 22))


