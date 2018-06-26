
setwd("C:/Users/saret/OneDrive - CREAF/TESI/Capitol Funcionalitat")

tvuf <- read.table("tvuf.txt",header=T)
tvuh <- read.table("tvuh.txt",header=T)

names(tvuf)
names(tvuh)

tvuf$proporcio_mortes <- 1-(tvuf$percent_vives/100)
tvuf$Mean_seeds2 <- tvuf$Mean_seeds - tvuf$Mean_seeds*tvuf$proporcio_mortes
tvuf$Avorted_seeds2 <- tvuf$Avorted_seeds + (tvuf$Mean_seeds-tvuf$Mean_seeds2)

tvuh$proporcio_mortes <- 1-(tvuh$percent_vives/100)
tvuh$Mean_seeds2 <- tvuh$Mean_seeds - tvuh$Mean_seeds*tvuh$proporcio_mortes
tvuh$Avorted_seeds2 <- tvuh$Avorted_seeds + (tvuh$Mean_seeds-tvuh$Mean_seeds2)

aa <- lm(tvuh$Homospecific~tvuf$Homospecific)
summary(aa)

plot(tvuf$Abundance~tvuf$Homospecific)
plot(tvuf$visit_rate~tvuf$Homospecific)
plot(tvuf$Richness~tvuf$Homospecific)
plot(tvuf$Abundance~tvuf$Heterospecific)
plot(tvuf$visit_rate~tvuf$Heterospecific)
plot(tvuf$Richness~tvuf$Heterospecific)

plot(tvuf$Abundance~tvuf$TVUF_Flowers)
plot(tvuf$visit_rate~tvuf$TVUF_Flowers)
plot(tvuf$Richness~tvuf$TVUF_Flowers)
plot(tvuf$Homospecific~tvuf$TVUF_Flowers)
plot(tvuf$Heterospecific~tvuf$TVUF_Flowers)

plot(tvuf$Abundance~tvuf$Fruits)
plot(tvuf$visit_rate~tvuf$Fruits)
plot(tvuf$Richness~tvuf$Fruits)
plot(tvuf$HB~tvuf$Fruits)
plot(tvuf$Percent_honeybees~tvuf$Fruits)
plot(tvuf$Homospecific~tvuf$Fruits)
plot(tvuf$Heterospecific~tvuf$Fruits)
plot(tvuf$Mean_seeds2~tvuf$Fruits)

plot(tvuf$Abundance~tvuf$Mean_seeds2)
plot(tvuf$visit_rate~tvuf$Mean_seeds2)
plot(tvuf$Richness~tvuf$Mean_seeds2)
plot(tvuf$Homospecific~tvuf$Mean_seeds2)
plot(tvuf$Heterospecific~tvuf$Mean_seeds2)

plot(tvuf$Abundance~tvuf$Avorted_seeds2)
plot(tvuf$visit_rate~tvuf$Avorted_seeds2)
plot(tvuf$Richness~tvuf$Avorted_seeds2)
plot(tvuf$Homospecific~tvuf$Avorted_seeds2)
plot(tvuf$Heterospecific~tvuf$Avorted_seeds2)
plot(tvuf$Mean_weight~tvuf$Avorted_seeds2)
plot(tvuf$Mean_seeds2~tvuf$Avorted_seeds2)

plot(tvuf$Abundance~tvuf$Mean_weight)
plot(tvuf$visit_rate~tvuf$Mean_weight)
plot(tvuf$Richness~tvuf$Mean_weight)
plot(tvuf$Homospecific~tvuf$Mean_weight)
plot(tvuf$Heterospecific~tvuf$Mean_weight)

plot(tvuh$Abundance~tvuh$Homospecific)
plot(tvuh$visit_rate~tvuh$Homospecific)
plot(tvuh$Richness~tvuh$Homospecific)
plot(tvuh$Abundance~tvuh$Heterospecific)
plot(tvuh$visit_rate~tvuh$Heterospecific)
plot(tvuh$Richness~tvuh$Heterospecific)

plot(tvuh$Abundance~tvuh$TVUH_Flowers)
plot(tvuh$visit_rate~tvuh$TVUH_Flowers)
plot(tvuh$Richness~tvuh$TVUH_Flowers)
plot(tvuh$Homospecific~tvuh$TVUH_Flowers)
plot(tvuh$Heterospecific~tvuh$TVUH_Flowers)

plot(tvuh$Abundance~tvuh$Fruits)
plot(tvuh$visit_rate~tvuh$Fruits)
plot(tvuh$Richness~tvuh$Fruits)
plot(tvuh$Homospecific~tvuh$Fruits)
plot(tvuh$Heterospecific~tvuh$Fruits)

plot(tvuh$Abundance~tvuh$Mean_seeds)
plot(tvuh$visit_rate~tvuh$Mean_seeds)
plot(tvuh$Richness~tvuh$Mean_seeds)
plot(tvuh$Homospecific~tvuh$Mean_seeds)
plot(tvuh$Heterospecific~tvuh$Mean_seeds)

plot(tvuh$Avorted_seeds~tvuh$Percent_honeybees)
plot(tvuh$Avorted_seeds~tvuh$Abundance)
plot(tvuh$Avorted_seeds~tvuh$visit_rate)
plot(tvuh$Avorted_seeds~tvuh$Richness)
plot(tvuh$Avorted_seeds~tvuh$Fruits)
plot(tvuh$Avorted_seeds~tvuh$Mean_seeds)
plot(tvuh$Avorted_seeds~tvuh$Richness)


plot(tvuh$Abundance~tvuh$Mean_weight)
plot(tvuh$visit_rate~tvuh$Mean_weight)
plot(tvuh$Richness~tvuh$Mean_weight)
plot(tvuh$Homospecific~tvuh$Mean_weight)
plot(tvuh$Heterospecific~tvuh$Mean_weight)

plot(tvuh$Heterospecific~tvuf$Heterospecific)
plot(tvuh$Homospecific~tvuf$Homospecific)
plot(tvuh$Mean_weight~tvuf$Mean_weight,xlim=c(0.10,0.25),ylim=c(0.10,0.25))
plot(tvuh$Fruits~tvuf$Fruits)
plot(tvuh$Mean_seeds~tvuf$Mean_seeds,xlim=c(0,3),ylim=c(0,3))

hist(tvuf$Heterospecific)
table(tvuf$Heterospecific)

hist(tvuh$Heterospecific)
table(tvuh$Heterospecific)

hist(tvuf$Fruits)
hist(tvuh$Fruits)

hist(tvuf$Mean_weight)
hist(tvuh$Mean_weight)

