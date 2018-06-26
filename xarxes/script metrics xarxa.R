
setwd("D:/Usuarios/s.reverte/OneDrive - CREAF/TESI/DADES/xarxes/sense apis")

library(bipartite)


taula1<-read.table("taula1.txt", header=T,colnames(1))
taula2<-read.table("taula2.txt", header=T,colnames(1))
taula3<-read.table("taula3.txt", header=T,colnames(1))
taula4<-read.table("taula4.txt", header=T,colnames(1))
taula5<-read.table("taula5.txt", header=T,colnames(1))
taula6<-read.table("taula6.txt", header=T,colnames(1))
taula7<-read.table("taula7.txt", header=T,colnames(1))
taula8<-read.table("taula8.txt", header=T,colnames(1))
taula9<-read.table("taula9.txt", header=T,colnames(1))
taula10<-read.table("taula10.txt", header=T,colnames(1))
taula11<-read.table("taula11.txt", header=T,colnames(1))
taula12<-read.table("taula12.txt", header=T,colnames(1))
taula13<-read.table("taula13.txt", header=T,colnames(1))
taula14<-read.table("taula14.txt", header=T,colnames(1))
taula15<-read.table("taula15.txt", header=T,colnames(1))
taula16<-read.table("taula16.txt", header=T,colnames(1))
taula17<-read.table("taula17.txt", header=T,colnames(1))
taula18<-read.table("taula18.txt", header=T,colnames(1))
taula19<-read.table("taula19.txt", header=T,colnames(1))
taula20<-read.table("taula20.txt", header=T,colnames(1))
taula21<-read.table("taula21.txt", header=T,colnames(1))
taula22<-read.table("taula22.txt", header=T,colnames(1))
taula23<-read.table("taula23.txt", header=T,colnames(1))
taula24<-read.table("taula24.txt", header=T,colnames(1))
taula25<-read.table("taula25.txt", header=T,colnames(1))
taula26<-read.table("taula26.txt", header=T,colnames(1))
taula27<-read.table("taula27.txt", header=T,colnames(1))
taula28<-read.table("taula28.txt", header=T,colnames(1))
taula29<-read.table("taula29.txt", header=T,colnames(1))
taula30<-read.table("taula30.txt", header=T,colnames(1))
taula31<-read.table("taula31.txt", header=T,colnames(1))
taula32<-read.table("taula32.txt", header=T,colnames(1))
taula33<-read.table("taula33.txt", header=T,colnames(1))
taula34<-read.table("taula34.txt", header=T,colnames(1))
taula35<-read.table("taula35.txt", header=T,colnames(1))
taula36<-read.table("taula36.txt", header=T,colnames(1))
taula37<-read.table("taula37.txt", header=T,colnames(1))
taula38<-read.table("taula38.txt", header=T,colnames(1))
taula39<-read.table("taula39.txt", header=T,colnames(1))
taula40<-read.table("taula40.txt", header=T,colnames(1))


t1<-as.matrix(taula1)
t2<-as.matrix(taula2)
t3<-as.matrix(taula3)
t4<-as.matrix(taula4)
t5<-as.matrix(taula5)
t6<-as.matrix(taula6)
t7<-as.matrix(taula7)
t8<-as.matrix(taula8)
t9<-as.matrix(taula9)
t10<-as.matrix(taula10)
t11<-as.matrix(taula11)
t12<-as.matrix(taula12)
t13<-as.matrix(taula13)
t14<-as.matrix(taula14)
t15<-as.matrix(taula15)
t16<-as.matrix(taula16)
t17<-as.matrix(taula17)
t18<-as.matrix(taula18)
t19<-as.matrix(taula19)
t20<-as.matrix(taula20)
t21<-as.matrix(taula21)
t22<-as.matrix(taula22)
t23<-as.matrix(taula23)
t24<-as.matrix(taula24)
t25<-as.matrix(taula25)
t26<-as.matrix(taula26)
t27<-as.matrix(taula27)
t28<-as.matrix(taula28)
t29<-as.matrix(taula29)
t30<-as.matrix(taula30)
t31<-as.matrix(taula31)
t32<-as.matrix(taula32)
t33<-as.matrix(taula33)
t34<-as.matrix(taula34)
t35<-as.matrix(taula35)
t36<-as.matrix(taula36)
t37<-as.matrix(taula37)
t38<-as.matrix(taula38)
t39<-as.matrix(taula39)
t40<-as.matrix(taula40)


index<-matrix(0,40,5)

### FC

index[1,1]<-fc(t1, dist="euclidean", method="average", weighted=TRUE)
index[2,1]<-fc(t2, dist="euclidean", method="average", weighted=TRUE)
index[3,1]<-fc(t3, dist="euclidean", method="average", weighted=TRUE)
index[4,1]<-fc(t4, dist="euclidean", method="average", weighted=TRUE)
index[5,1]<-fc(t5, dist="euclidean", method="average", weighted=TRUE)
index[6,1]<-fc(t6, dist="euclidean", method="average", weighted=TRUE)
index[7,1]<-fc(t7, dist="euclidean", method="average", weighted=TRUE)
index[8,1]<-fc(t8, dist="euclidean", method="average", weighted=TRUE)
index[9,1]<-fc(t9, dist="euclidean", method="average", weighted=TRUE)
index[10,1]<-fc(t10, dist="euclidean", method="average", weighted=TRUE)
index[11,1]<-fc(t11, dist="euclidean", method="average", weighted=TRUE)
index[12,1]<-fc(t12, dist="euclidean", method="average", weighted=TRUE)
index[13,1]<-fc(t13, dist="euclidean", method="average", weighted=TRUE)
index[14,1]<-fc(t14, dist="euclidean", method="average", weighted=TRUE)
index[15,1]<-fc(t15, dist="euclidean", method="average", weighted=TRUE)
index[16,1]<-fc(t16, dist="euclidean", method="average", weighted=TRUE)
index[17,1]<-fc(t17, dist="euclidean", method="average", weighted=TRUE)
index[18,1]<-fc(t18, dist="euclidean", method="average", weighted=TRUE)
index[19,1]<-fc(t19, dist="euclidean", method="average", weighted=TRUE)
index[20,1]<-fc(t20, dist="euclidean", method="average", weighted=TRUE)
index[21,1]<-fc(t21, dist="euclidean", method="average", weighted=TRUE)
index[22,1]<-fc(t22, dist="euclidean", method="average", weighted=TRUE)
index[23,1]<-fc(t23, dist="euclidean", method="average", weighted=TRUE)
index[24,1]<-fc(t24, dist="euclidean", method="average", weighted=TRUE)
index[25,1]<-fc(t25, dist="euclidean", method="average", weighted=TRUE)
index[26,1]<-fc(t26, dist="euclidean", method="average", weighted=TRUE)
index[27,1]<-fc(t27, dist="euclidean", method="average", weighted=TRUE)
index[28,1]<-fc(t28, dist="euclidean", method="average", weighted=TRUE)
index[29,1]<-fc(t29, dist="euclidean", method="average", weighted=TRUE)
index[30,1]<-fc(t30, dist="euclidean", method="average", weighted=TRUE)
index[31,1]<-fc(t31, dist="euclidean", method="average", weighted=TRUE)
index[32,1]<-fc(t32, dist="euclidean", method="average", weighted=TRUE)
index[33,1]<-fc(t33, dist="euclidean", method="average", weighted=TRUE)
index[34,1]<-fc(t34, dist="euclidean", method="average", weighted=TRUE)
index[35,1]<-fc(t35, dist="euclidean", method="average", weighted=TRUE)
index[36,1]<-fc(t36, dist="euclidean", method="average", weighted=TRUE)
index[37,1]<-fc(t37, dist="euclidean", method="average", weighted=TRUE)
index[38,1]<-fc(t38, dist="euclidean", method="average", weighted=TRUE)
index[39,1]<-fc(t39, dist="euclidean", method="average", weighted=TRUE)
index[40,1]<-fc(t40, dist="euclidean", method="average", weighted=TRUE)

## H2'
H2fun(t1, H2_integer=TRUE)

index[1,2:5]<-H2fun(t1, H2_integer=TRUE)
index[2,2:5]<-H2fun(t2, H2_integer=TRUE)
index[3,2:5]<-H2fun(t3, H2_integer=TRUE)
index[4,2:5]<-H2fun(t4, H2_integer=TRUE)
index[5,2:5]<-H2fun(t5, H2_integer=TRUE)
index[6,2:5]<-H2fun(t6, H2_integer=TRUE)
index[7,2:5]<-H2fun(t7, H2_integer=TRUE)
index[8,2:5]<-H2fun(t8, H2_integer=TRUE)
index[9,2:5]<-H2fun(t9, H2_integer=TRUE)
index[10,2:5]<-H2fun(t10, H2_integer=TRUE)
index[11,2:5]<-H2fun(t11, H2_integer=TRUE)
index[12,2:5]<-H2fun(t12, H2_integer=TRUE)
index[13,2:5]<-H2fun(t13, H2_integer=TRUE)
index[14,2:5]<-H2fun(t14, H2_integer=TRUE)
index[15,2:5]<-H2fun(t15, H2_integer=TRUE)
index[16,2:5]<-H2fun(t16, H2_integer=TRUE)
index[17,2:5]<-H2fun(t17, H2_integer=TRUE)
index[18,2:5]<-H2fun(t18, H2_integer=TRUE)
index[19,2:5]<-H2fun(t19, H2_integer=TRUE)
index[20,2:5]<-H2fun(t20, H2_integer=TRUE)
index[21,2:5]<-H2fun(t21, H2_integer=TRUE)
index[22,2:5]<-H2fun(t22, H2_integer=TRUE)
index[23,2:5]<-H2fun(t23, H2_integer=TRUE)
index[24,2:5]<-H2fun(t24, H2_integer=TRUE)
index[25,2:5]<-H2fun(t25, H2_integer=TRUE)
index[26,2:5]<-H2fun(t26, H2_integer=TRUE)
index[27,2:5]<-H2fun(t27, H2_integer=TRUE)
index[28,2:5]<-H2fun(t28, H2_integer=TRUE)
index[29,2:5]<-H2fun(t29, H2_integer=TRUE)
index[30,2:5]<-H2fun(t30, H2_integer=TRUE)
index[31,2:5]<-H2fun(t31, H2_integer=TRUE)
index[32,2:5]<-H2fun(t32, H2_integer=TRUE)
index[33,2:5]<-H2fun(t33, H2_integer=TRUE)
index[34,2:5]<-H2fun(t34, H2_integer=TRUE)
index[35,2:5]<-H2fun(t35, H2_integer=TRUE)
index[36,2:5]<-H2fun(t36, H2_integer=TRUE)
index[37,2:5]<-H2fun(t37, H2_integer=TRUE)
index[38,2:5]<-H2fun(t38, H2_integer=TRUE)
index[39,2:5]<-H2fun(t39, H2_integer=TRUE)
index[40,2:5]<-H2fun(t40, H2_integer=TRUE)

### d'

dd<-read.table("d.txt", header=T)
datata <- as.data.frame(index)
names(datata) <- c("FC","H2","d_rof","d_tvu","d_mitjana")
datata[,3:5] <- dd[,2:4]


### Path analysis

database2 <- read.table("Data.txt",header=T)

names(database2)

database2[,18:22] <- datata

library(betapart)
library(readxl)
library(lavaan)
library(semPlot)
library(dplyr)


range01 <- function(x){(x-mean(x))/sd(x)}

database2 <- database2[-25,]
database2 <- database2[-28,]

database2$ROF_Abundance <- range01(database2$ROF_Abundance)
database2$TVU_Abundance <- range01(database2$TVU_Abundance)
database2$Other_Flowers_Abundance <- range01(database2$Other_Flowers_Abundance)
database2$Flower_Richness <- range01(database2$Flower_Richness)
database2$d_rof <- range01(database2$d_rof)
database2$d_tvu <- range01(database2$d_tvu)
database2$d_mitjana <- range01(database2$d_mitjana)
database2$T_Max <- range01(database2$T_Max)
database2$H2 <- range01(database2$H2)
database2$Honeybee_Visit_Rate <- range01(database2$Honeybee_Visit_Rate)
database2$FC <- range01(database2$FC)


##################### MODEL 1
model1 <- "
Honeybee_Visit_Rate ~ T_Max + TVU_Abundance + ROF_Abundance + Other_Flowers_Abundance
FC ~ T_Max + TVU_Abundance + Honeybee_Visit_Rate + ROF_Abundance + Other_Flowers_Abundance
"
res1 <- sem(model1,data=database2,estimates="MLR")
sink("model1.txt")
print(summary(res1,fit.measures=T))
sink()

aa <- lm(database2$H2~database2$Honeybee_Abundance)
summary(aa)
plot(database2$H2~database2$Honeybee_Abundance)
abline(aa)

aa <- lm(database2$FC~database2$Honeybee_Abundance)
summary(aa)
plot(database2$FC~database2$Honeybee_Abundance)
abline(aa)

aa <- lm(database2$d_mitjana~database2$Honeybee_Abundance)
summary(aa)
plot(database2$d_mitjana~database2$Honeybee_Abundance)
abline(aa)

aa <- lm(database2$d_rof~database2$Honeybee_Abundance)
summary(aa)
plot(database2$d_rof~database2$Honeybee_Abundance)
abline(aa)

aa <- lm(database2$d_tvu~database2$Honeybee_Abundance)
summary(aa)
plot(database2$d_tvu~database2$Honeybee_Abundance)
abline(aa)


aa <- lm(database2$H2~database2$Honeybee_Visit_Rate)
summary(aa)
plot(database2$H2~database2$Honeybee_Visit_Rate)
abline(aa)

aa <- lm(database2$FC~database2$Honeybee_Visit_Rate)
summary(aa)
plot(database2$FC~database2$Honeybee_Visit_Rate)
abline(aa)


aa <- lm(database2$d_mitjana~database2$Honeybee_Visit_Rate)
summary(aa)
plot(database2$d_mitjana~database2$Honeybee_Visit_Rate)
abline(aa)

aa <- lm(database2$d_rof~database2$Honeybee_Visit_Rate)
summary(aa)
plot(database2$d_rof~database2$Honeybee_Visit_Rate)
abline(aa)


aa <- lm(database2$d_tvu~database2$Honeybee_Visit_Rate)
summary(aa)
plot(database2$d_tvu~database2$Honeybee_Visit_Rate)
abline(aa)



aa <- lm(database2$d_mitjana~database2$Pollinator_Richness)
summary(aa)
plot(database2$d_mitjana~database2$Pollinator_Richness)
abline(aa)

aa <- lm(database2$d_mitjana~database2$H2)
summary(aa)
plot(database2$d_mitjana~database2$H2)
abline(aa)




names(database2)

database2 <- database2[,-(13:14)]

library(corrplot)
library(Hmisc)
res2<-rcorr(as.matrix(database2))
?corrplot
corrplot(res2$r, type = "upper", tl.col = "black", tl.srt = 45)

res2$r
