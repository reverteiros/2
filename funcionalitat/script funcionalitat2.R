
setwd("C:/Users/saret/OneDrive - CREAF/TESI/Capitol Funcionalitat")

tvuf <- read.table("tvuf.txt",header=T)
tvuh <- read.table("tvuh.txt",header=T)
rof <- read.table("rof.txt",header=T)

names(tvuf)
names(tvuh)
names(rof)

tvuf$proporcio_mortes <- 1-(tvuf$percent_vives/100)
tvuf$Mean_seeds2 <- tvuf$Mean_seeds - tvuf$Mean_seeds*tvuf$proporcio_mortes
tvuf$Avorted_seeds2 <- tvuf$Avorted_seeds + (tvuf$Mean_seeds-tvuf$Mean_seeds2)
tvuf$pollinated_ovules <- tvuf$Mean_seeds2 + tvuf$Avorted_seeds2

tvuh$proporcio_mortes <- 1-(tvuh$percent_vives/100)
tvuh$Mean_seeds2 <- tvuh$Mean_seeds - tvuh$Mean_seeds*tvuh$proporcio_mortes
tvuh$Avorted_seeds2 <- tvuh$Avorted_seeds + (tvuh$Mean_seeds-tvuh$Mean_seeds2)
tvuh$pollinated_ovules <- tvuh$Mean_seeds2 + tvuh$Avorted_seeds2

rof$Homospecific <- rof$ROF
rof$Heterospecific <- rof$TVU+rof$OTHERS



range01 <- function(x){(x-mean(x))/sd(x)}

tvuf$Mean_weight <- range01(tvuf$Mean_weight)
tvuf$Fruits <- range01(tvuf$Fruits)
tvuf$Mean_seeds2 <- range01(tvuf$Mean_seeds2)
tvuf$Avorted_seeds2 <- range01(tvuf$Avorted_seeds2)
tvuf$Abundance <- range01(tvuf$Abundance)
tvuf$visit_rate <- range01(tvuf$visit_rate)
tvuf$Richness <- range01(tvuf$Richness)
tvuf$TVUF_Flowers <- range01(tvuf$TVUF_Flowers)
tvuf$Homospecific <- range01(tvuf$Homospecific)
tvuf$Heterospecific <- range01(tvuf$Heterospecific)
tvuf$HB <- range01(tvuf$HB)
tvuf$HB_rate <- range01(tvuf$HB_rate)
tvuf$Percent_honeybees <- range01(tvuf$Percent_honeybees)
tvuf$Wild <- range01(tvuf$Wild)
tvuf$Wild_rate <- range01(tvuf$Wild_rate)
tvuf$pollinated_ovules <- range01(tvuf$pollinated_ovules)


tvuh <- tvuh[,-(5:7)]
tvuh <- tvuh[,-(3)]
tvuh <- tvuh[-(7),]
tvuh <- tvuh[-(27),]
tvuh <- tvuh[-(17),]

tvuh$Mean_weight <- range01(tvuh$Mean_weight)
tvuh$Fruits <- range01(tvuh$Fruits)
tvuh$Mean_seeds2 <- range01(tvuh$Mean_seeds2)
tvuh$Avorted_seeds2 <- range01(tvuh$Avorted_seeds2)
tvuh$Abundance <- range01(tvuh$Abundance)
tvuh$visit_rate <- range01(tvuh$visit_rate)
tvuh$Richness <- range01(tvuh$Richness)
tvuh$tvuh_Flowers <- range01(tvuh$TVUH_Flowers)
tvuh$Homospecific <- range01(tvuh$Homospecific)
tvuh$Heterospecific <- range01(tvuh$Heterospecific)
tvuh$HB <- range01(tvuh$HB)
tvuh$HB_rate <- range01(tvuh$HB_rate)
tvuh$Percent_honeybees <- range01(tvuh$Percent_honeybees)
tvuh$Wild <- range01(tvuh$Wild)
tvuh$Wild_rate <- range01(tvuh$Wild_rate)
tvuh$pollinated_ovules <- range01(tvuh$pollinated_ovules)

rof$Homospecific <- range01(rof$Homospecific)
rof$Heterospecific <- range01(rof$Heterospecific)
rof$Richness <- range01(rof$Richness)
rof$Wild_Visit_Rate_ROF <- range01(rof$Wild_Visit_Rate_ROF)
rof$Honeybee_Visit_Rate_ROF <- range01(rof$Honeybee_Visit_Rate_ROF)


hist(rof$Homospecific)
hist(tvuf$Homospecific)
hist(tvuh$Homospecific)

##################### MODEL rof
model0 <- "
Homospecific ~ Honeybee_Visit_Rate_ROF + Richness + Wild_Visit_Rate_ROF
Heterospecific ~ Honeybee_Visit_Rate_ROF + Richness + Wild_Visit_Rate_ROF
"
res0.3 <- sem(model0,data=rof,estimates="MLR")
sink("model0.txt")
print(summary(res0.3,fit.measures=T))
sink()


names(tvuh)
##################### MODEL tvuh
model0 <- "
Homospecific ~ HB_rate + Richness + Wild_rate
Heterospecific ~ HB_rate + Richness + Wild_rate
#Mean_weight ~ Homospecific + Heterospecific
#Mean_seeds2 ~ Homospecific + Heterospecific
Fruits ~ Homospecific + Heterospecific
"
res0.3 <- sem(model0,data=tvuh,estimates="MLR")
sink("model0.txt")
print(summary(res0.3,fit.measures=T))
sink()


##################### MODEL tvuf
model0 <- "
Homospecific ~ HB_rate + Richness + Wild_rate
Heterospecific ~ HB_rate + Richness + Wild_rate
Mean_weight ~ Homospecific + Heterospecific
pollinated_ovules ~ Homospecific + Heterospecific
Fruits ~ Homospecific + Heterospecific
"
res0.3 <- sem(model0,data=tvuf,estimates="MLR")
sink("model0.txt")
print(summary(res0.3,fit.measures=T))
sink()
