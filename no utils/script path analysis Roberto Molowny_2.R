
setwd("D:/Usuarios/s.reverte/OneDrive - CREAF/TESI/DADES/Path")

library(lavaan)

database2 <- read.table("Data.txt",header=T)

names(database2)
database2$pollen <- database2$ROF_pollen+database2$TVU_pollen
database2$nectar <- database2$ROF_nectar+database2$TVU_nectar

datalog <- log(database2)


##################### MODEL 1
model1 <- "
Honeybee_Visit_Rate ~ T_Max +pollen
Wild_Abundance ~ T_Max + Honeybee_Visit_Rate +  pollen
"
res1 <- sem(model1,data=datalog,estimates="MLR")
sink("model1.txt")
print(summary(res1,fit.measures=T))
sink()



plot(resid(lll2)~resid(lll))


lll <- lm(datalog$Honeybee_Visit_Rate~datalog$T_Max+datalog$nectar+datalog$pollen)
lll2 <- lm(datalog$Wild_Abundance~datalog$T_Max+datalog$nectar+datalog$pollen)
summary(lll)
resid(lll)


##################### MODEL 1
model1 <- "
Honeybee_Visit_Rate ~ T_Max + Overall_flowers
Wild_Abundance ~ T_Max + Honeybee_Visit_Rate + Overall_flowers
"
res1 <- sem(model1,data=datalog)
sink("model1.txt")
print(summary(res1,fit.measures=T))
sink()


##################### MODEL 2
model2 <- "
Honeybee_Abundance ~ T_Max + Overall_flowers
Wild_Visit_Rate ~ T_Max + Honeybee_Abundance + Overall_flowers 
"
res2 <- sem(model2,data=datalog,estimates="MLR")
sink("model2.txt")
print(summary(res2,fit.measures=T))
sink()


##################### MODEL 2
model2 <- "
Honeybee_Visit_Rate ~ T_Max + Overall_flowers
Wild_Visit_Rate ~ T_Max + Honeybee_Visit_Rate + Overall_flowers 
"
res2 <- sem(model2,data=datalog,estimates="MLR")
sink("model2.txt")
print(summary(res2,fit.measures=T))
sink()

##################### MODEL 3

model3 <- "
Honeybee_Visit_Rate ~ T_Max + Overall_flowers + Flower_Richness
Pollinator_Richness ~ T_Max + Overall_flowers + Honeybee_Visit_Rate +  Flower_Richness
"
res3 <- sem(model3,data=datalog,estimates="MLR")
sink("model3.txt")
print(summary(res3,fit.measures=T))
sink()

cor.test(datalog$Overall_flowers,datalog$pollen)
plot(datalog$Overall_flowers~datalog$nectar)

##################### MODEL 4

model4 <- "
Honeybee_Visit_Rate ~ T_Max + ROF_nectar + TVU_Abundance
Wild_Visit_Rate ~ T_Max + ROF_Abundance + Honeybee_Visit_Rate + TVU_Abundance
"
res4 <- sem(model4,data=datalog,estimates="MLR")
sink("model4.txt")
print(summary(res4,fit.measures=T))
sink()

