
source("funcionalitat/netejar dades mitjana per parcela.R")

library(MuMIn)
library(caret)

################################### ROF ####################################

###### Pollen presence

fit <- glm(Homospecific_presence~Pollinator_richness+logVisitation_rate+Flower_Abundance+Proportion_HB+Proportion_Bee+Proportion_Diptera,family=binomial, weights=Individuals_pollen,data=meandataperplotROF)

car::vif(fit)
hist(resid(fit))

options(na.action = "na.fail")
dd <- dredge(fit,extra="adjR^2")
subset(dd, delta < 2)
ddd <- subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 

###### Mean pollen

fit <- lm(log(Mean_Homospecific)~Pollinator_richness+logVisitation_rate+Flower_Abundance+Proportion_HB+Proportion_Bee+Proportion_Diptera, data=meandataperplotROF)

car::vif(fit)
hist(resid(fit))

dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
subset(dd, delta < 2)
avgmod.95delta2 <- model.avg(ddd,extra="adjR^2") 
summary(avgmod.95delta2) 


################################### TVUF ####################################

meandataperplotTVUH2 <- meandataperplotTVUH %>%
  mutate(Flowers_H = Flower_Abundance) %>%
  select(Plot, Flowers_H) 

meandataperplotTVUF2 <- meandataperplotTVUF %>%
  left_join(meandataperplotTVUH2, by = "Plot")

meandataperplotTVUF2[is.na(meandataperplotTVUF2)] <- 0

meandataperplotTVUF2 <- meandataperplotTVUF2 %>%
  mutate(FlowersTVU = Flower_Abundance+Flowers_H)

meandataperplotTVUF3 <- meandataperplotTVUF2 %>%
  select(Plot, FlowersTVU) 

meandataperplotTVUH3 <- meandataperplotTVUH %>%
  left_join(meandataperplotTVUF3, by = "Plot")

###### Pollen presence

fit <- glm(Homospecific_presence~Pollinator_richness+Visitation_rate+FlowersTVU+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera,family=binomial, weights=Individuals_pollen, data=meandataperplotTVUF2)

car::vif(fit)
hist(resid(fit))

dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
subset(dd, delta < 2)
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 

###### Mean pollen

fit <- lm(log(Mean_Homospecific)~Pollinator_richness+Visitation_rate+FlowersTVU+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera, data=meandataperplotTVUF2)

car::vif(fit)
hist(resid(fit))

dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
subset(dd, delta < 2)
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 

###### Heterospecific presence

fit <- glm(Heterospecific_presence~loggenerality+Pollinator_richness+FlowersTVU+Visitation_rate+Proportion_Heterosp_Community+Proportion_HB+Proportion_Bee+Proportion_Diptera,family=binomial, weights=Individuals_pollen, data=meandataperplotTVUF2)

car::vif(fit)
hist(resid(fit))

dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
subset(dd, delta < 2)
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 

###### Fruit set

fit <- glm(Fruit_set~Pollinator_richness+Visitation_rate+ProporcioF+FlowersTVU+Proportion_HB+Proportion_Bee+Proportion_Diptera,family=binomial, weights=Individuals_fruits, data=meandataperplotTVUF2)  

car::vif(fit)
hist(resid(fit))

dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
subset(dd, delta < 2)
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 

###### Seed set

fit <- lm(Seed_set~Pollinator_richness+Visitation_rate+FlowersTVU+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera, data=meandataperplotTVUF2)  

car::vif(fit)
hist(resid(fit))

dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
subset(dd, delta < 2)
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 


################################### TVUH ####################################

###### Pollen presence

fit <- glm(Homospecific_presence~Pollinator_richness+FlowersTVU+logVisitation_rate+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera,family=binomial, weights=Individuals_pollen, data=meandataperplotTVUH3)

car::vif(fit)
hist(resid(fit))

dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
subset(dd, delta < 2)
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 

###### Mean pollen

fit <- lm(log(Mean_Homospecific)~Pollinator_richness+FlowersTVU+logVisitation_rate+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera, data=meandataperplotTVUH3)

car::vif(fit)
hist(resid(fit))

dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
subset(dd, delta < 2)
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 

###### Fruit set

fit <- glm(Fruit_set~Pollinator_richness+logVisitation_rate+FlowersTVU+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera,family=binomial, weights=Individuals_fruits, data=meandataperplotTVUH3)  

car::vif(fit)
hist(resid(fit))

dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
subset(dd, delta < 2)
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 

###### Seed set

fit <- lm(Seed_set~Pollinator_richness+ProporcioF+logVisitation_rate+FlowersTVU+Proportion_HB+Proportion_Bee+Proportion_Diptera, data=meandataperplotTVUH3)  

car::vif(fit)
hist(resid(fit))

dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
subset(dd, delta < 2)
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 

