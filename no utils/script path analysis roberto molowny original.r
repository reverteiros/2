rm(list=ls(all=TRUE))
assign("last.warning", NULL, envir = baseenv())

library(readxl)
library(lavaan)
library(semPlot)
library(dplyr)

get.data <- function(type="log") {
  a <- read_excel("Sara Reverté path analysis_v2.xlsx",sheet = "Data",na=c("","NA"))
  if (type=="") type <- "nothing"
  f <- function(x,type) {
    switch(type,
           log=log(x+1),
           sqrt=sqrt(x),
           nothing=x
        )
  }  
  a <- a %>% mutate(PLOT=factor(PLOT),
                  ROF_Abundance=f(ROF_Abundance,type=type),
                  TVU_Abundance=f(TVU_Abundance,type=type),
                  Other_Flowers_Abundance=f(Other_Flowers_Abundance,type=type),
                  ROF_Abundance=f(ROF_Abundance,type=type),
                  Wild_Abundance=f(Wild_Abundance,type=type))
  
# Get rid of NA in Temperature.
  a <- a[!is.na(a$T_Max),]
  return(a)
}



##################### MODEL 1

model1 <- "
  Honeybee_Visit_Rate ~ T_Max + ROF_Abundance + TVU_Abundance + Other_Flowers_Abundance
  Wild_Abundance ~ T_Max + ROF_Abundance + TVU_Abundance + Other_Flowers_Abundance + Honeybee_Visit_Rate
"
# res1.1 <- sem(model1,data=get.data("log"))
# print(summary(res1.1,fit.measures=T))
# semPaths(res1.1,whatLabels="std",what="std",title=F)
# 
# res1.2 <- sem(model1,data=get.data("sqrt"))
# print(summary(res1.2,fit.measures=T))
# semPaths(res1.2,whatLabels="std",what="std")

res1.3 <- sem(model1,data=get.data(""))
sink("model1.txt")
print(summary(res1.3,fit.measures=T))
sink()
pdf("Model 1.pdf")
semPaths(res1.3,whatLabels="std",what="std",main="Model 1")
text(-1.3,1.3,"Model 1")
dev.off()

##################### MODEL 2
model2 <- "
  Honeybee_Visit_Rate ~ T_Max + ROF_Abundance + TVU_Abundance + Other_Flowers_Abundance
  Wild_Visit_Rate ~ T_Max + ROF_Abundance + TVU_Abundance + Other_Flowers_Abundance + Honeybee_Visit_Rate
"
# res2.1 <- sem(model2,data=get.data("log"))
# print(summary(res2.1,fit.measures=T))
# semPaths(res2.1,whatLabels="std",what="std")
# 
# res2.2 <- sem(model2,data=get.data("sqrt"))
# print(summary(res2.2,fit.measures=T))
# semPaths(res2.2,whatLabels="std",what="std")

res2.3 <- sem(model2,data=get.data(""))
sink("model2.txt")
print(summary(res2.3,fit.measures=T))
sink()
pdf("Model 2.pdf")
semPaths(res2.3,whatLabels="std",what="std")
text(-1.3,1.3,"Model 2")
dev.off()

##################### MODEL 3
model3 <- "
  Honeybee_Visit_Rate ~ T_Max + ROF_Abundance + TVU_Abundance + Other_Flowers_Abundance
  Bees ~ T_Max + ROF_Abundance + TVU_Abundance + Other_Flowers_Abundance + Honeybee_Visit_Rate
"
# res3.1 <- sem(model3,data=get.data("log"))
# print(summary(res3.1,fit.measures=T))
# semPaths(res3.1,whatLabels="std",what="std")
# 
# res3.2 <- sem(model3,data=get.data("sqrt"))
# print(summary(res3.2,fit.measures=T))
# semPaths(res3.2,whatLabels="std",what="std")

res3.3 <- sem(model3,data=get.data(""))
sink("model3.txt")
print(summary(res3.3,fit.measures=T))
sink()
pdf("Model 3.pdf")
semPaths(res3.3,whatLabels="std",what="std")
text(-1.3,1.3,"Model 3")
dev.off()


##################### MODEL 4
model4 <- "
  Honeybee_Visit_Rate ~ T_Max + ROF_Abundance + TVU_Abundance + Other_Flowers_Abundance
  Bee_Visit_Rate ~ T_Max + ROF_Abundance + TVU_Abundance + Other_Flowers_Abundance + Honeybee_Visit_Rate
"
# res4.1 <- sem(model4,data=get.data("log"))
# print(summary(res4.1,fit.measures=T))
# semPaths(res4.1,whatLabels="std",what="std")
# 
# res4.2 <- sem(model4,data=get.data("sqrt"))
# print(summary(res4.2,fit.measures=T))
# semPaths(res4.2,whatLabels="std",what="std")

res4.3 <- sem(model4,data=get.data(""))
sink("model4.txt")
print(summary(res4.3,fit.measures=T))
sink()
pdf("Model 4.pdf")
semPaths(res4.3,whatLabels="std",what="std")
text(-1.3,1.3,"Model 4")
dev.off()


##################### MODEL 5
model5 <- "
Honeybee_Visit_Rate ~ T_Max + Flower_Richness + ROF_Abundance + TVU_Abundance + Other_Flowers_Abundance
Pollinator_Richness ~ T_Max + Flower_Richness + ROF_Abundance + TVU_Abundance + Other_Flowers_Abundance + Honeybee_Visit_Rate
"
# res5.1 <- sem(model5,data=get.data("log"))
# print(summary(res5.1,fit.measures=T))
# semPaths(res5.1,whatLabels="std",what="std")
# 
# res5.2 <- sem(model5,data=get.data("sqrt"))
# print(summary(res5.2,fit.measures=T))
# semPaths(res5.2,whatLabels="std",what="std")

res5.3 <- sem(model5,data=get.data(""))
sink("model5.txt")
print(summary(res5.3,fit.measures=T))
sink()
pdf("Model 5.pdf")
semPaths(res5.3,whatLabels="std",what="std")
text(-1.3,1.3,"Model 5")
dev.off()


##################### MODEL 6
model6 <- "
  Honeybee_Visit_Rate_ROF ~ ROF_Abundance + ROF_percentage + No_ROF_Abundance
  Wild_Visit_Rate_ROF ~ ROF_Abundance + ROF_percentage + No_ROF_Abundance + Honeybee_Visit_Rate_ROF
"
# res6.1 <- sem(model6,data=get.data("log"))
# print(summary(res6.1,fit.measures=T))
# semPaths(res6.1,whatLabels="std",what="std")
# 
# res6.2 <- sem(model6,data=get.data("sqrt"))
# print(summary(res6.2,fit.measures=T))
# semPaths(res6.2,whatLabels="std",what="std")

res6.3 <- sem(model6,data=get.data(""))
sink("model6.txt")
print(summary(res6.3,fit.measures=T))
sink()
pdf("Model 6.pdf")
semPaths(res6.3,whatLabels="std",what="std")
text(-1.3,1.3,"Model 6")
dev.off()

##################### MODEL 7
model7 <- "
Honeybee_Visit_Rate_TVU ~ TVU_Abundance + TVU_percentage + No_TVU_Abundance
Wild_Visit_Rate_TVU ~ TVU_Abundance + TVU_percentage + No_TVU_Abundance + Honeybee_Visit_Rate_TVU
"
# res7.1 <- sem(model7,data=get.data("log"))
# print(summary(res7.1,fit.measures=T))
# semPaths(res7.1,whatLabels="std",what="std")
# 
# res7.2 <- sem(model7,data=get.data("sqrt"))
# print(summary(res7.2,fit.measures=T))
# semPaths(res7.2,whatLabels="std",what="std")

res7.3 <- sem(model7,data=get.data(""))
sink("model7.txt")
print(summary(res7.3,fit.measures=T))
sink()
pdf("Model 7.pdf")
semPaths(res7.3,whatLabels="std",what="std")
text(-1.3,1.3,"Model 7")
dev.off()





