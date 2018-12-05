library(piecewiseSEM)

############ ROSMARINUS

datalog <- log(database2)

## Path analysis wild abundance
wildabundance = list(
  lm(Honeybeerate ~ OVERALL_Flowers + T_Max, data = datalog),
  lm(Wild ~ OVERALL_Flowers + Honeybeerate + T_Max, data = datalog))

coef.table = sem.coefs(wildabundance, datalog)
(coef.table = sem.coefs(wildabundance, datalog))
sem.plot(wildabundance, datalog, coef.table)
sem.model.fits(wildabundance)