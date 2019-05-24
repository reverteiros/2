
source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades plantes.R")

library(ggplot2)
library(ggExtra)
library(corrplot)
library(Hmisc)
# install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
library(vegan)
library(betapart)

datapollinatorsall
ROFpollen
TVUFpollen
TVUHpollen


fit1 <- glm(pay~gender/year, data=Dat, family=Gamma(link = "log"))  

hist(dataclean$Mean_pollen)
hist(dataclean$Ratio_Heterosp_Homosp)

hist(dataclean$Pollinator_richness)
hist(dataclean$Shannon_Diversity)
hist(dataclean$HB_Visitation_rate)
hist(dataclean$H2)
