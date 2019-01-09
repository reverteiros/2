
library(piecewiseSEM)

database2 <- read.table("dades/Database3.txt",header=T)
database3<-read.table("dades/Databasealt.txt", header=T)
database2$Altura <- database3$Altura

hist(database2$Altura)

datalog <- log(database2)

## Path analysis wild abundance
wildabundance = list(
  lm(Honeybeerate ~ OVERALL_Flowers + T_Max + Altura, data = datalog),
  lm(Wild ~ OVERALL_Flowers + Honeybeerate + T_Max + Altura, data = datalog))

wildabundance = list(
  lm(Honeybeerate ~ OVERALL_Flowers + T_Max , data = datalog),
  lm(Wild ~ OVERALL_Flowers + Honeybeerate + T_Max, data = datalog))


coef.table = sem.coefs(wildabundance, datalog)
(coef.table = sem.coefs(wildabundance, datalog))
sem.plot(wildabundance, datalog, coef.table)
sem.model.fits(wildabundance)

## Wild visit rate
wildrate = list(
  lm(Honeybeerate ~ OVERALL_Flowers + T_Max, data = datalog),
  lm(Wild_rate ~ OVERALL_Flowers + Honeybeerate + T_Max, data = datalog))

wildrate = list(
  lm(Honeybeerate ~ OVERALL_Flowers + T_Max + Altura, data = datalog),
  lm(Wild_rate ~ OVERALL_Flowers + Honeybeerate + T_Max + Altura, data = datalog))

coef.table = sem.coefs(wildrate, datalog)
(coef.table = sem.coefs(wildrate, datalog))
sem.plot(wildrate, datalog, coef.table)
sem.model.fits(wildrate)

## Wild richness
wildrichness = list(
  lm(Honeybeerate ~ OVERALL_Flowers + T_Max + flower_richness, data = datalog),
  lm(Pollinator_species ~ OVERALL_Flowers + Honeybeerate + T_Max + flower_richness, data = datalog))

wildrichness = list(
  lm(Honeybeerate ~ OVERALL_Flowers + T_Max + flower_richness + Altura, data = datalog),
  lm(Pollinator_species ~ OVERALL_Flowers + Honeybeerate + T_Max + flower_richness + Altura, data = datalog))

coef.table = sem.coefs(wildrichness, datalog)
(coef.table = sem.coefs(wildrichness, datalog))
sem.plot(wildrichness, datalog, coef.table)
sem.model.fits(wildrichness)
