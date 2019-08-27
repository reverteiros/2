
source("funcionalitat/netejar dades mitjana per parcela.R")

library(ggplot2)
library(ggExtra)
library(corrplot)
library(Hmisc)


### grafics amb histogrames laterals

library(ggplot2)
p <- ggplot(TVUFpollenbitxoswtna, aes(Heterospecific, Total)) + geom_point() + theme_classic()
ggExtra::ggMarginal(p, type = "histogram")


## histograma estecificant ample banda
qplot(TVUFpollenbitxos$Heterospecific,
      geom="histogram",
      binwidth = 0.5,  
      xlim=c(-1, 80))



## Corrplot
ROF <- TVUFpollenbitxos %>%
  left_join(pollenpergraph,by=c("Plot","Species"))%>%
  filter(Species=="ROF") %>%
  select(-c(Fecundity,Fruit_set,Pollinated_ovules,Avorted_total,Seed_set,Avorted_per_fruit,ProporcioF,logVisitation_rate,logMean_pollen,logFunctional_group_Rocka,logPollinator_richness,Pollinator_abundance,Flower_Abundance,Mean_pollen))
ROF <- as.data.frame(ROF) %>%
  select(.,-c(Plot,Species))
ROF <- ROF[-29,]

res2<-rcorr(as.matrix(ROF))
corrplot(res2$r, type="upper", order="hclust",
         p.mat = res2$P, sig.level = 0.05, insig = "blank")


## Taula correlacions
chart.Correlation(ROF, histogram=TRUE, pch=19)


## Corrplot
TVUF <- meandataperplot %>%
  filter(Species=="TVUF") %>%
  select(Species,Visitation_rate,Pollinator_richness,generality,Proportion_HB,Proportion_Bee,Proportion_Diptera,Proportion_Lepidoptera)

TVUF <- as.data.frame(TVUF) %>%
  select(.,-c(Plot,Species))

## Taula correlacions
chart.Correlation(TVUF, histogram=TRUE, pch=19)





## Corrplot
TVUH <- meandataperplot %>%
  filter(Species=="TVUH") %>%
  select(-c(ProporcioF,logVisitation_rate,logMean_pollen,logFunctional_group_Rocka,logPollinator_richness,Pollinator_abundance,Flower_Abundance,Mean_pollen,Fecundity,Fruit_set,Pollinated_ovules,Avorted_total,Seed_set,Avorted_per_fruit))
# select(Fecundity,Fruit_set,Pollinated_ovules,Avorted_total,Seed_set,Avorted_per_fruit,Mean_pollen,Mean_Homospecific,Mean_Heterospecific)

TVUH <- as.data.frame(TVUH) %>%
  select(.,-c(Plot,Species))
TVUH <- TVUH[-c(7,18,23,25),]

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
