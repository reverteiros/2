
source("funcionalitat/netejar dades mitjana per parcela.R")

library(MuMIn)
library(caret)

################################### ROF ####################################

###### Pollen presence

fit <- glm(Total_presence~Pollinator_richness+Visitation_rate+Proportion_HB+Proportion_Bee+Proportion_Diptera,family=binomial, weights=Individuals_pollen,data=meandataperplotROF)
summary(fit)

car::vif(fit)

hist(resid(fit))

options(na.action = "na.fail")
dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 
confint(avgmod.95delta2)

###### Mean pollen

fit <- lm(log(Mean_Total)~Pollinator_richness+Visitation_rate+Proportion_HB+Proportion_Bee+Proportion_Diptera, data=meandataperplotROF)
summary(fit)

car::vif(fit)

hist(resid(fit))

options(na.action = "na.fail")
dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 
confint(avgmod.95delta2)


################################### TVUF ####################################

###### Pollen presence

fit <- glm(Total_presence~Pollinator_richness+Visitation_rate+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera,family=binomial, weights=Individuals_pollen, data=meandataperplotTVUF)
summary(fit)

car::vif(fit)

hist(resid(fit))

options(na.action = "na.fail")
dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 
confint(avgmod.95delta2)

###### Mean pollen

fit <- lm(log(Mean_Total)~Pollinator_richness+Visitation_rate+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera, data=meandataperplotTVUF)
summary(fit)

car::vif(fit)

hist(resid(fit))

options(na.action = "na.fail")
dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 
confint(avgmod.95delta2)

###### Proporcio Heterospecific
fit <- glm(Proportion_Heterosp_Stigma~generality+Proportion_Heterosp_Community+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera,family=binomial, weights=Individuals_heterospecific, data=meandataperplotTVUFheterosp)
summary(fit)

car::vif(fit)

hist(resid(fit))

options(na.action = "na.fail")
dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 
confint(avgmod.95delta2)

###### Heterospecific presence

fit <- glm(Heterospecific_presence~generality+Pollinator_richness+Visitation_rate+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera,family=binomial, weights=Individuals_pollen, data=meandataperplotTVUF)
summary(fit)

car::vif(fit)

hist(resid(fit))

options(na.action = "na.fail")
dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 
confint(avgmod.95delta2)

###### Fruit set

fit <- glm(Fruit_set~Pollinator_richness+Visitation_rate+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera,family=binomial, weights=Individuals_fruits, data=meandataperplotTVUF)  
summary(fit)

car::vif(fit)

hist(resid(fit))

options(na.action = "na.fail")
dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 
confint(avgmod.95delta2)

###### Seed set

fit <- lm(log(Seed_set)~Pollinator_richness+Visitation_rate+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera, data=meandataperplotTVUF)  
summary(fit)

car::vif(fit)

hist(resid(fit))

options(na.action = "na.fail")
dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 
confint(avgmod.95delta2)


###### Avorted

fit <- lm(Avorted~Pollinator_richness+Visitation_rate+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera, data=meandataperplotTVUF)  
summary(fit)

car::vif(fit)

hist(resid(fit))

options(na.action = "na.fail")
dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 
confint(avgmod.95delta2)


################################### TVUH ####################################

###### Pollen presence

fit <- glm(Total_presence~Pollinator_richness+Visitation_rate+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera,family=binomial, weights=Individuals_pollen, data=meandataperplotTVUH)
summary(fit)

car::vif(fit)

hist(resid(fit))

options(na.action = "na.fail")
dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 
confint(avgmod.95delta2)

###### Mean pollen

fit <- lm(log(Mean_Total)~Pollinator_richness+Visitation_rate+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera, data=meandataperplotTVUH)
summary(fit)

car::vif(fit)

hist(resid(fit))

options(na.action = "na.fail")
dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 
confint(avgmod.95delta2)

###### Fruit set

fit <- lm(Fruit_set~Pollinator_richness+Visitation_rate+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera, weights=Individuals_fruits, data=meandataperplotTVUH)  
summary(fit)

car::vif(fit)

hist(resid(fit))

options(na.action = "na.fail")
dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 
confint(avgmod.95delta2)

###### Seed set

fit <- lm(log(Seed_set)~Pollinator_richness+Visitation_rate+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera, data=meandataperplotTVUH)  
summary(fit)

car::vif(fit)

hist(resid(fit))

options(na.action = "na.fail")
dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 
confint(avgmod.95delta2)


###### Avorted

fit <- lm(Avorted~Pollinator_richness+Visitation_rate+ProporcioF+Proportion_HB+Proportion_Bee+Proportion_Diptera, data=meandataperplotTVUH)  
summary(fit)

car::vif(fit)

hist(resid(fit))

options(na.action = "na.fail")
dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) 
confint(avgmod.95delta2)
