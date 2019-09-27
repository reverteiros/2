
source("funcionalitat/netejar dades mitjana per parcela.R")

library(MuMIn)
library(caret)

################################### ROF ####################################

###### Pollen presence

fit <- glm(Homospecific_presence~Pollinator_richness+logVisitation_rate+Proportion_HB+Proportion_Bee+Proportion_Diptera,family=binomial, weights=Individuals_pollen,data=meandataperplotROF)

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

fit <- lm(log(Mean_Homospecific)~Pollinator_richness+logVisitation_rate+Proportion_HB+Proportion_Bee+Proportion_Diptera, data=meandataperplotROF)

car::vif(fit)
hist(resid(fit))

dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
subset(dd, delta < 2)
avgmod.95delta2 <- model.avg(ddd,extra="adjR^2") 
summary(avgmod.95delta2) 


################################### TVUF ####################################

###### Pollen presence

fit <- glm(Homospecific_presence~Pollinator_richness+Visitation_rate+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera,family=binomial, weights=Individuals_pollen, data=meandataperplotTVUF)

car::vif(fit)
hist(resid(fit))

dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
subset(dd, delta < 2)
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 

###### Mean pollen

fit <- lm(log(Mean_Homospecific)~Pollinator_richness+Visitation_rate+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera, data=meandataperplotTVUF)

car::vif(fit)
hist(resid(fit))

dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
subset(dd, delta < 2)
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 

###### Heterospecific presence

fit <- glm(Heterospecific_presence~loggenerality+Pollinator_richness+Visitation_rate+Proportion_Heterosp_Community+Proportion_HB+Proportion_Bee+Proportion_Diptera,family=binomial, weights=Individuals_pollen, data=meandataperplotTVUF)

car::vif(fit)
hist(resid(fit))

dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
subset(dd, delta < 2)
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 

###### Fruit set

fit <- glm(Fruit_set~Pollinator_richness+Visitation_rate+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera,family=binomial, weights=Individuals_fruits, data=meandataperplotTVUF)  

car::vif(fit)
hist(resid(fit))

dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
subset(dd, delta < 2)
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 

###### Seed set

fit <- lm(Seed_set~Pollinator_richness+Visitation_rate+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera, data=meandataperplotTVUF)  

car::vif(fit)
hist(resid(fit))

dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
subset(dd, delta < 2)
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 


################################### TVUH ####################################

###### Pollen presence

fit <- glm(Homospecific_presence~Pollinator_richness+logVisitation_rate+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera,family=binomial, weights=Individuals_pollen, data=meandataperplotTVUH)

car::vif(fit)
hist(resid(fit))

dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
subset(dd, delta < 2)
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 

###### Mean pollen

fit <- lm(log(Mean_Homospecific)~Pollinator_richness+logVisitation_rate+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera, data=meandataperplotTVUH)

car::vif(fit)
hist(resid(fit))

dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
subset(dd, delta < 2)
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 

###### Fruit set

fit <- glm(Fruit_set~Pollinator_richness+logVisitation_rate+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera,family=binomial, weights=Individuals_fruits, data=meandataperplotTVUH)  

car::vif(fit)
hist(resid(fit))

dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
subset(dd, delta < 2)
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 

###### Seed set

fit <- lm(Seed_set~Pollinator_richness+ProporcioF+logVisitation_rate+Proportion_HB+Proportion_Bee+Proportion_Diptera, data=meandataperplotTVUH)  

car::vif(fit)
hist(resid(fit))

dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
subset(dd, delta < 2)
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 

