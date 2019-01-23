
library(piecewiseSEM)

source("funcionalitat/analisis funcionalitat.R")

## Path analysis

femella = list(
  lm(Fruit_set ~ Flowers_with_pollen + Mean_Homospecific_flowers_with_pollen, data = TVUF),
  lm(Percent_pollination ~ Mean_Homospecific_flowers_with_pollen + Flowers_with_pollen, data = TVUF),
  lm(Seed_set ~ Percent_pollination, data = TVUF),
  lm(Mean_weigth ~ Seed_set, data = TVUF),
  lm(Seed_viability ~ Seed_set, data = TVUF))

hermafrodita = list(
  lm(Fruit_set ~ Flowers_with_pollen + Mean_Homospecific_flowers_with_pollen, data = TVUH),
  lm(Percent_pollination ~ Mean_Homospecific_flowers_with_pollen + Flowers_with_pollen, data = TVUH),
  lm(Seed_set ~ Percent_pollination, data = TVUH),
  lm(Mean_weigth ~ Seed_set, data = TVUH),
  lm(Seed_viability ~ Seed_set, data = TVUH))


coef.table = sem.coefs(femella, TVUF)
(coef.table = sem.coefs(femella, TVUF))
sem.plot(femella, TVUF, coef.table)
sem.model.fits(femella)

coef.table = sem.coefs(hermafrodita, TVUH)
(coef.table = sem.coefs(hermafrodita, TVUH))
sem.plot(hermafrodita, TVUF, coef.table)
sem.model.fits(hermafrodita)