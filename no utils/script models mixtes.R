

setwd("D:/Usuarios/s.reverte/Desktop/dades")

library(nlme)

datum<-read.csv("datum.csv", header = TRUE,dec = ".")
names(datum)

datum$proportion.shared.subnetwork<-proportion.shared.subnetwork$proportion.shared.subnetwork
  

m1<-lme(OS.quant.senseapis ~ distancia, random = ~ 1|proportion.shared.subnetwork, data=datum)

summary(m1)

anova(m1)

a1<-lm(datum$proportion.shared.subnetwork~datum$distancia)
summary(a1)

plot(datum$proportion.shared.subnetwork~datum$distancia)
abline(a1)
