
source("funcionalitat/analisis mitjana per parcela.R")

#################### Relació entre espècies 

### visitation rate
meanVR <- meandataperplot %>%
  select(Plot,Species,Visitation_rate)%>%
  spread(Species,Visitation_rate) %>%
  mutate(logTVUH = log(TVUH))%>%
  mutate(logTVUF = log(TVUF))

hist(meanVR$logTVUH)
hist(meanVR$logTVUF) 

mod <- lm(logTVUH~logTVUF,data=meanVR,na.action=na.omit)
summary(mod)
hist(mod$residuals)
plot(logTVUH~logTVUF,data=meanVR)
abline(mod)

### functional groups
meanfunctionalgroups <- meandataperplot %>%
  select(Plot,Species,Functional_group_Rocka)%>%
  spread(Species,Functional_group_Rocka)%>%
  mutate(logTVUH = log(TVUH))%>%
  mutate(logTVUF = log(TVUF))

hist(meanfunctionalgroups$logTVUH)
hist(meanfunctionalgroups$logTVUF) 

mod <- lm(logTVUH~logTVUF,data=meanfunctionalgroups,na.action=na.omit)
summary(mod)
hist(mod$residuals)
plot(logTVUH~logTVUF,data=meanfunctionalgroups)
abline(mod)

### richness
meanrichness <- meandataperplot %>%
  select(Plot,Species,Pollinator_richness)%>%
  spread(Species,Pollinator_richness)%>%
  mutate(logTVUH = log(TVUH))%>%
  mutate(logTVUF = log(TVUF))

hist(meanrichness$logTVUH)
hist(meanrichness$logTVUF) 

mod <- lm(logTVUH~logTVUF,data=meanrichness,na.action=na.omit)
summary(mod)
hist(mod$residuals)
plot(logTVUH~logTVUF,data=meanrichness)
abline(mod)

### pollen
meanpollen <- meandataperplot %>%
  select(Plot,Species,Mean_pollen)%>%
  spread(Species,Mean_pollen)%>%
  mutate(logTVUH = log(TVUH))%>%
  mutate(logTVUF = log(TVUF))

hist(meanpollen$logTVUH)
hist(meanpollen$logTVUF) 

mod <- lm(logTVUH~logTVUF,data=meanpollen,na.action=na.omit)
summary(mod)
hist(mod$residuals)
plot(logTVUH~logTVUF,data=meanpollen)
abline(mod)

### fecundity
meanfecundity <- meandataperplot %>%
  select(Plot,Species,Fecundity)%>%
  spread(Species,Fecundity)%>%
  mutate(logTVUH = log(TVUH))%>%
  mutate(logTVUF = log(TVUF))

hist(meanfecundity$logTVUH)
hist(meanfecundity$logTVUF) 

mod <- lm(logTVUH~logTVUF,data=meanfecundity,na.action=na.omit)
summary(mod)
hist(mod$residuals)
plot(logTVUH~logTVUF,data=meanfecundity)
abline(mod)

## fruit set
meanfruitset <- meandataperplot %>%
  select(Plot,Species,Fruit_set)%>%
  spread(Species,Fruit_set)

hist(meanfruitset$TVUH)
hist(meanfruitset$TVUF) 

mod <- lm(TVUH~TVUF,data=meanfruitset,na.action=na.omit)
summary(mod)
hist(mod$residuals)
plot(TVUH~TVUF,data=meanfruitset)
abline(mod)

## seed set
meanseedset <- meandataperplot %>%
  select(Plot,Species,Seed_set)%>%
  spread(Species,Seed_set)

hist(meanseedset$TVUH)
hist(meanseedset$TVUF) 

mod <- lm(TVUH~TVUF,data=meanseedset,na.action=na.omit)
summary(mod)
hist(mod$residuals)
plot(TVUH~TVUF,data=meanseedset)
abline(mod)



####################### Relació entre variables

ggplot(meandataperplot) +
  geom_point(aes(Visitation_rate,Mean_pollen,colour=Species)) +
  theme_classic() +
  facet_grid(Species ~ .)
