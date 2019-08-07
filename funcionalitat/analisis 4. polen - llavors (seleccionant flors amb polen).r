

source("funcionalitat/netejar dades fruits i llavors.R")
source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades plantes.R")
source("funcionalitat/netejar dades databases analisis.R")

library(lme4)
library(MuMIn)


TVUFpollenperplanta <- TVUFpollenbitxos %>%
  # filter(Homospecific>0) %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Mean_Homospecific=mean(Homospecific),Mean_Heterospecific=mean(Heterospecific),Homospecific_presence=mean(Homospecific_presence))

hist(TVUFpollenperplanta$Homospecific_presence)


TVUHpollenperplanta <- TVUHpollenbitxos %>%
  # filter(Homospecific>0) %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Mean_Homospecific=mean(Homospecific),Mean_Heterospecific=mean(Heterospecific),Homospecific_presence=mean(Homospecific_presence))

hist(TVUHpollenperplanta$Homospecific_presence)


TVUFpollenfruitsperplanta <- fruitset %>%
  filter(Species == "TVUF") %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Fruit_set=mean(Fruits),Fecundity=mean(Seed))%>%
  left_join(TVUFpollenperplanta,by=c("Plot", "Plant","Species"))%>%
  left_join(proporciomorfs,by="Plot")%>%
  filter(Mean_Homospecific > -1) 

TVUFpollenseedsperplanta <- seedset %>%
  filter(Species == "TVUF") %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Seed_set=mean(Seed))%>%
  left_join(TVUFpollenperplanta,by=c("Plot", "Plant","Species"))%>%
  left_join(proporciomorfs,by="Plot")%>%
  filter(Mean_Homospecific > -1) 

TVUHpollenfruitsperplanta <- fruitset %>%
  filter(Species == "TVUH") %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Fruit_set=mean(Fruits),Fecundity=mean(Seed))%>%
  left_join(TVUHpollenperplanta,by=c("Plot", "Plant","Species"))%>%
  left_join(proporciomorfs,by="Plot")%>%
  filter(Mean_Homospecific > -1) 

TVUHpollenseedsperplanta <- seedset %>%
  filter(Species == "TVUH") %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Seed_set=mean(Seed))%>%
  left_join(TVUHpollenperplanta,by=c("Plot", "Plant","Species"))%>%
  left_join(proporciomorfs,by="Plot")%>%
  filter(Mean_Homospecific > -1) 




################################# FLOWERS WITH POLLEN - FRUIT SET ##################################

################# TVUF

fitTVUFpollenfruits <- lmer(Fruit_set~Mean_Homospecific+ProporcioF+Homospecific_presence+(1|Plot), data=TVUFpollenfruitsperplanta)  

hist(resid(fitTVUFpollenfruits))

options(na.action = "na.fail")
dd <- dredge(fitTVUFpollenfruits)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])


############## TVUH

fitTVUHpollenfruits <- lmer(Fruit_set~Mean_Homospecific+ProporcioF+Homospecific_presence+(1|Plot), data=TVUHpollenfruitsperplanta)  

hist(resid(fitTVUHpollenfruits))

dd <- dredge(fitTVUHpollenfruits)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])




################################# Seed set - mean pollen flowers with pollen


TVUFpollenperplanta <- TVUFpollenbitxos %>%
  filter(Homospecific>0) %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Mean_Homospecific=mean(Homospecific))


TVUHpollenperplanta <- TVUHpollenbitxos %>%
  filter(Homospecific>0) %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Mean_Homospecific=mean(Homospecific))


TVUFpollenseedsperplanta <- seedset %>%
  filter(Species == "TVUF") %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Seed_set=mean(Seed))%>%
  left_join(TVUFpollenperplanta,by=c("Plot", "Plant","Species"))%>%
  left_join(proporciomorfs,by="Plot")%>%
  filter(Mean_Homospecific > -1) 

TVUHpollenseedsperplanta <- seedset %>%
  filter(Species == "TVUH") %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Seed_set=mean(Seed))%>%
  left_join(TVUHpollenperplanta,by=c("Plot", "Plant","Species"))%>%
  left_join(proporciomorfs,by="Plot")%>%
  filter(Mean_Homospecific > -1) 


### TVUF

fitTVUFpollenseeds <- lmer((Seed_set)~(Mean_Homospecific)+ProporcioF+(1|Plot), data=TVUFpollenseedsperplanta)  

hist(resid(fitTVUFpollenseeds))

dd <- dredge(fitTVUFpollenseeds)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])

plot(TVUFpollenseedsperplanta$Mean_Homospecific~TVUFpollenseedsperplanta$Seed_set)

### TVUH

hist(log(TVUHpollenseedsperplanta$Seed_set))

fitTVUHpollenseeds <- lmer(log(Seed_set)~log(Mean_Homospecific)+ProporcioF+(1|Plot), data=TVUHpollenseedsperplanta)  

hist(resid(fitTVUHpollenseeds))

dd <- dredge(fitTVUHpollenseeds)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])







