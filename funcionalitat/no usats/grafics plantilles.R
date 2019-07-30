
source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades.R")
source("funcionalitat/grups funcionals.R")

library(ggplot2)
library(ggExtra)
library(corrplot)
library(Hmisc)
library("PerformanceAnalytics")
library(vegan)
library(betapart)


## ggplots normals
ggplot(datanalysis) +
  geom_point(aes(Visitation_rate,Mean_pollen,colour=Species)) +
  geom_smooth(aes(Visitation_rate,Mean_pollen))+
  theme_classic() #+
# coord_cartesian(xlim = c(0, 32), ylim = c(0, 32))


## boxplots
ggplot(dataclean, aes(y=Ratio_Heterosp_Homosp, x=Species)) +
  geom_boxplot() +
  theme_classic()


## Corrplot
ROF <- as.data.frame(ROF) %>%
  select(.,-c(Plot,Species))
ROF <- ROF[-29,]

res2<-rcorr(as.matrix(ROF))
corrplot(res2$r, type="upper", order="hclust",
         p.mat = res2$P, sig.level = 0.05, insig = "blank")


## Taula correlacions
ROFnetwork <- as.data.frame(ROF)
ROFnetwork <- ROFnetwork[,-1]
chart.Correlation(ROFnetwork, histogram=TRUE, pch=19)


## PCA
TVUFfinal <- TVUF[complete.cases(TVUF), ]
TVUF.pca <- prcomp(TVUFfinal, center = TRUE,scale. = TRUE)
summary(TVUF.pca)
biplot(TVUF.pca, scale = 0)


