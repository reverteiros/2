
library(piecewiseSEM)

database2 <- read.table("Data.txt",header=T)
names(database2)

database2$pollen <- database2$ROF_pollen+database2$TVU_pollen
database2$nectar <- database2$ROF_nectar+database2$TVU_nectar

datalog <- log(database2)


wildabundance = list(
  lm(Honeybee_Visit_Rate ~ Overall_flowers + T_Max, data = datalog),
  lm(Wild_Abundance ~ Overall_flowers + Honeybee_Visit_Rate + T_Max, data = datalog))

(coef.table = sem.coefs(wildabundance, datalog))
sem.plot(wildabundance, datalog, standardize = "scale")

wildrate = list(
  lm(Honeybee_Visit_Rate ~ Overall_flowers + T_Max, data = datalog),
  lm(Wild_Visit_Rate ~ Overall_flowers + Honeybee_Visit_Rate + T_Max, data = datalog))

(coef.table = sem.coefs(wildrate, datalog))
sem.plot(wildrate, datalog, standardize = "scale")

wildrichness = list(
  lm(Honeybee_Visit_Rate ~ Overall_flowers + T_Max + Flower_Richness, data = datalog),
  lm(Pollinator_Richness ~ Overall_flowers + Honeybee_Visit_Rate + T_Max + Flower_Richness, data = datalog))

(coef.table = sem.coefs(wildrichness, datalog))
sem.plot(wildrichness, datalog, standardize = "scale")

wildabundancenectar = list(
  lm(Honeybee_Visit_Rate ~ nectar + T_Max, data = datalog),
  lm(Wild_Abundance ~ nectar + Honeybee_Visit_Rate + T_Max, data = datalog))

(coef.table = sem.coefs(wildabundancenectar, datalog))
sem.plot(wildabundancenectar, datalog, standardize = "scale")

wildabundancepollen = list(
  lm(Honeybee_Visit_Rate ~ pollen + T_Max, data = datalog),
  lm(Wild_Abundance ~ pollen + Honeybee_Visit_Rate + T_Max, data = datalog))

(coef.table = sem.coefs(wildabundancepollen, datalog))
sem.plot(wildabundancepollen, datalog, standardize = "scale")

wildvisitratenectar = list(
  lm(Honeybee_Visit_Rate ~ nectar + T_Max, data = datalog),
  lm(Wild_Visit_Rate ~ nectar + Honeybee_Visit_Rate + T_Max, data = datalog))

(coef.table = sem.coefs(wildvisitratenectar, datalog))
sem.plot(wildvisitratenectar, datalog, standardize = "scale")


sem.fit(pathlist, database2)
## dona error perque totes les vies estan testades, aquesta funcio et mira les vies no testades
sem.model.fits(wildabundancepollen)
(coef.table = sem.coefs(wildabundancepollen, database2))
sem.plot(wildabundancepollen, database2, standardize = "scale")
