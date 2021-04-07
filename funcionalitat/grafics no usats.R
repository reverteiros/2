
source("funcionalitat/netejar dades mitjana per parcela.R")

library(ggplot2)
library(ggExtra)
library(corrplot)
library(Hmisc)


### grafics amb histogrames laterals
p <- ggplot(TVUFpollenbitxoswtna, aes(Heterospecific, Total)) + geom_point() + theme_classic()
ggExtra::ggMarginal(p, type = "histogram")


## histograma estecificant ample banda
qplot(TVUFpollenbitxos$Heterospecific,
      geom="histogram",
      binwidth = 0.5,  
      xlim=c(-1, 80))

## Corrplot
res2<-rcorr(as.matrix(TVUH))
corrplot(res2$r, type="upper", order="hclust",
         p.mat = res2$P, sig.level = 0.05, insig = "blank")

## Taula correlacions
chart.Correlation(TVUH, histogram=TRUE, pch=19)

## logit regression presencia polen - taxa de visites

ggplot(data = TVUFpollenbitxos, aes(x=Visitation_rate, y=Homospecific_presence)) + 
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  theme_classic()
