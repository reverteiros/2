

source("funcionalitat/netejar dades fruits i llavors.R")
source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades plantes.R")
source("funcionalitat/netejar dades databases analisis.R")
source("funcionalitat/analisis mitjana per parcela.R")

library(lme4)
library(MuMIn)




TVUFfiltered <- meandataperplot%>%
  filter(Species=="TVUF") 


plot(TVUFfiltered$Functional_group_Rocka~TVUFfiltered$Pollinator_richness)
abline(coef = c(0,1))



TVUFfiltered <- meandataperplot%>%
  filter(Species=="TVUF") %>%
  filter(Mean_Homospecific < 10)

TVUHfiltered <- meandataperplot%>%
  filter(Species=="TVUH")%>%
  filter(Mean_Homospecific < 10)



######################################### POLLEN - FRUIT SET ####################################

################# TVUF

fitTVUFpollenfruits <- lm(Fruit_set~Mean_Homospecific+Pollinator_richness, data=TVUFfiltered)  

hist(resid(fitTVUFpollenfruits))

options(na.action = "na.fail")
dd <- dredge(fitTVUFpollenfruits)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])



fitTVUFpollenfruits <- lm(Fruit_set~Visitation_rate+Functional_group_Rocka, data=TVUFfiltered)  

hist(resid(fitTVUFpollenfruits))

options(na.action = "na.fail")
dd <- dredge(fitTVUFpollenfruits)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])


############## TVUH

fitTVUHpollenfruits <- lm(Fruit_set~Mean_Homospecific+ProporcioF, data=TVUHfiltered)  

hist(resid(fitTVUHpollenfruits))

dd <- dredge(fitTVUHpollenfruits)
subset(dd, delta < 2)
# summary(get.models(dd, 1)[[1]])






piecewiseTVUF <- lm(Fruit_set ~ Mean_Homospecific*(Mean_Homospecific < 8) + Mean_Homospecific*(Mean_Homospecific > 8),data=TVUFpiecewise)
summary(piecewiseTVUF)

piecewiseTVUF <- lm(Fruit_set ~ Mean_Homospecific,data=TVUFpiecewise)
summary(piecewiseTVUF)

plot(TVUFpiecewise$Fruit_set~TVUFpiecewise$Mean_Homospecific)
curve((0.412706 -0.193755) + (0.017806+0.039346)*x, add=T, from=1, to=8)
# curve((0.412706 + 0.1533492) +0.017806*x, add=T, from=10, to=max(x))
abline(v=8, lty=3)

piecewiseTVUH <- lm(Fruit_set ~ Mean_Homospecific*(Mean_Homospecific < 10) + Mean_Homospecific*(Mean_Homospecific > 10),data=TVUHpollenfruitsperplanta)
summary(piecewiseTVUH)



curve(((Intercept) + x < 15TRUE) + (x + x:x<15TRUE)*x, add=T, from=1, to=10)
curve(((Intercept) + x > 15TRUE) -x*x, add=T, from=10, to=max(x))


