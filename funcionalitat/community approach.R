
library(piecewiseSEM)

############ ROSMARINUS

# datalog <- log(database2)

ROF <- filter(datafunction, Species =="ROF")

## Path analysis wild abundance
rofpollen = list(
  lm(Mean_Heterospecific ~ Pollinator_richness + Pollinator_abundance + Wild_Visitation_rate + HB_Visitation_rate, data = ROF),
  lm(Mean_Homospecific ~ Pollinator_richness + Pollinator_abundance + Wild_Visitation_rate + HB_Visitation_rate, data = ROF))

(coef.table = sem.coefs(rofpollen, ROF))
sem.plot(rofpollen, rofcomunity, coef.table)
sem.model.fits(rofpollen)


############ THYMUS FEMALE

## 
rofpollen = list(
  lm(Fruit_set ~ Mean_Homospecific +  Mean_Heterospecific, data = ROF),
  lm(Seed_set ~ Fruit_set + Mean_Homospecific +  Mean_Heterospecific + Pollination, data = ROF))

(coef.table = sem.coefs(rofpollen, rofcomunity))
sem.plot(rofpollen, rofcomunity, coef.table)
sem.model.fits(rofpollen)