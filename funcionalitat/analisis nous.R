

source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades plantes.R")
source("funcionalitat/netejar dades fruits i llavors.R")

library(MuMIn)
require(devtools)
library(tidyverse)

proporcioF <- read.table("dades/flors quantitatiu separant thymus morfs.txt",header=T) %>%
  select(., TVUF, TVUH) %>%
  mutate(Plot = c(1:40)) %>%
  mutate(TVU = TVUF+TVUH) %>%
  mutate(ProporcioF = TVUF / TVU) %>%
  select(., c(Plot,ProporcioF))

pollenperplot <- pollenclean %>%
  group_by(Plot, Species) %>% 
  summarise(Mean_pollen=mean(Total))

seeds <- fruitset %>%
  filter(Fruits == 1) %>%
  group_by(Plot, Species) %>% 
  summarise(Seed_set=mean(Seed))

fruitsandfecundity <- fruitset %>%
  group_by(Plot, Species) %>% 
  summarise(Fecundity=mean(Seed),Fruit_set=mean(Fruits))

meandataperplot <- datapollinatorsall %>%
  left_join(fruitsandfecundity,by=c("Species","Plot")) %>%
  left_join(seeds,by=c("Species","Plot")) %>%
  left_join(pollenperplot,by=c("Species","Plot")) %>%
  left_join(proporcioF,by="Plot")%>%
  mutate(logPollinator_richness = log(Pollinator_richness)) %>%
  mutate(logVisitation_rate = log(Visitation_rate)) %>%
  mutate(logFunctional_group_Rocka = log(Functional_group_Rocka))%>%
  mutate(logMean_pollen = log(Mean_pollen))

# flower abundance per plot
flors <- read.table("dades/flors quantitatiu separant thymus morfs.txt",header=T) %>%
  select(., ROF, TVUF, TVUH)%>%
  tidyr::gather(Species, "Flower_Abundance",1:3) 
flors$Plot = c(1:40)


datagroups <- read.table("dades/pollinator groups.txt",header=T) %>%
  select(Plot,Species,Pollinator_group,Abundance)%>%
  filter(.,Species =="ROF" | Species =="TVUF" | Species =="TVUH") %>%
  group_by(Plot,Species,Pollinator_group) %>% 
  summarise(Abundance=sum(Abundance)) %>%
  left_join(flors, by = c("Plot","Species")) %>%
  mutate(Visitation_rate = Abundance/(3*Flower_Abundance)*1000) %>%
  filter(.,Pollinator_group != "Mecoptera" & Pollinator_group != "Heteroptera") %>%
  left_join(pollenperplot,by=c("Plot","Species"))%>%
  select(-c(Abundance,Flower_Abundance))


################################### ROF ####################################

datagroupsROF <- datagroups %>%
  filter(Species=="ROF") %>%
  spread(Pollinator_group,Visitation_rate)
datagroupsROF[is.na(datagroupsROF)] <- 0

###### Mean pollen

fitROFpollen <- lm(Mean_pollen~Bee+Coleoptera+Diptera+Honeybees, data=datagroupsROF)
options(na.action = "na.fail")
dd <- dredge(fitROFpollen)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]])

hist(resid(fitROFpollen))

plot(datagroupsROF$Mean_pollen~datagroupsROF$Lepidoptera)

################################### TVUF ####################################

datagroupsTVUF <- datagroups %>%
  filter(Species=="TVUF") %>%
  spread(Pollinator_group,Visitation_rate)
datagroupsTVUF[is.na(datagroupsTVUF)] <- 0

###### Mean pollen

fitTVUFpollen <- lm(Mean_pollen~Bee+Coleoptera+Diptera+Honeybees+Lepidoptera+Wasp, data=datagroupsTVUF)
options(na.action = "na.fail")
dd <- dredge(fitTVUFpollen)
subset(dd, delta < 2)
summary(get.models(dd, 1)[[1]])

hist(resid(fitTVUFpollen))

plot(datagroupsTVUF$Mean_pollen~datagroupsTVUF$Honeybees)
