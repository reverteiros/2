
database2 <- read.table("Data.txt",header=T)

database2$pollen <- database2$ROF_pollen+database2$TVU_pollen
database2$nectar <- database2$ROF_nectar+database2$TVU_nectar

datalog <- log(database2)

names(database2)


# model honeybee
b <- lm(datalog$Honeybee_Visit_Rate~datalog$T_Max+datalog$Overall_flowers)
summary(b)
# model amb residus honeybee
a <- lm(datalog$Wild_Visit_Rate~resid(b)+datalog$T_Max+datalog$Overall_flowers)
summary(a)
# model amb honeybee normal
a <- lm(datalog$Wild_Visit_Rate~datalog$Honeybee_Visit_Rate+datalog$T_Max+datalog$Overall_flowers)
summary(a)