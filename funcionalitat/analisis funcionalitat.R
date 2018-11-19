
source("funcionalitat/netejar dades.R")
source("funcionalitat/index xarxes.R")

networkmetrics
datafunction

ROF <- filter(datafunction, Species =="ROF")

hist(ROF$Mean_pollen)
hist(ROF$SD_pollen)
hist(ROF$Mean_Homospecific)
hist(ROF$SD_Homospecific)
hist(ROF$Mean_Heterospecific)

TVUF <- filter(datafunction, Species =="TVUF")

hist(TVUF$Mean_pollen)
hist(TVUF$SD_pollen)
hist(TVUF$Mean_Homospecific)
hist(TVUF$SD_Homospecific)
hist(TVUF$Mean_Heterospecific)
hist(TVUF$Mean_weigth)
hist(TVUF$Percent_embryo)
hist(TVUF$Fruit_set)
hist(TVUF$Seed_set)


TVUH <- filter(datafunction, Species =="TVUH")

hist(TVUH$Mean_pollen)
hist(TVUH$SD_pollen)
hist(TVUH$Mean_Homospecific)
hist(TVUH$SD_Homospecific)
hist(TVUH$Mean_Heterospecific)


