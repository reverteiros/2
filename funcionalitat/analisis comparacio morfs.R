

source("funcionalitat/netejar dades mitjana per parcela.R")

library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggplot2)
library(ggExtra)
library(corrplot)
library(Hmisc)
library(grid)
library(plyr)



###### Proportion of flowers with homospecific

meandataperplotwtROF <- meandataperplot %>%
  filter(Species != "ROF") %>%
  select(Species,Homospecific_presence)%>%
  spread(Species,Homospecific_presence)

t.test(meandataperplotwtROF$TVUF, meandataperplotwtROF$TVUH, paired = TRUE, alternative = "two.sided")

###### Proportion of flowers with homospecific

meandataperplotwtROF <- meandataperplot %>%
  filter(Species != "ROF") %>%
  select(Species,Homospecific_presence)%>%
  spread(Species,Homospecific_presence)

t.test(meandataperplotwtROF$TVUF, meandataperplotwtROF$TVUH, paired = TRUE, alternative = "two.sided")

###### Proportion of flowers with homospecific

meandataperplotwtROF <- meandataperplot %>%
  filter(Species != "ROF") %>%
  select(Species,Mean_Homospecific)%>%
  spread(Species,Mean_Homospecific)

t.test(meandataperplotwtROF$TVUF, meandataperplotwtROF$TVUH, paired = TRUE, alternative = "two.sided")

###### Fruit set

meandataperplotwtROF <- meandataperplot %>%
  filter(Species != "ROF") %>%
  select(Species,Heterospecific_presence)%>%
  spread(Species,Heterospecific_presence)

t.test(meandataperplotwtROF$TVUF, meandataperplotwtROF$TVUH, paired = TRUE, alternative = "two.sided")

###### Seed set

meandataperplotwtROF <- meandataperplot %>%
  filter(Species != "ROF") %>%
  select(Species,Seed_set)%>%
  spread(Species,Seed_set)

t.test(meandataperplotwtROF$TVUF, meandataperplotwtROF$TVUH, paired = TRUE, alternative = "two.sided")




###### Pollinator richness

meandataperplotwtROF <- meandataperplot %>%
  filter(Species != "ROF") %>%
  select(Species,Pollinator_richness)%>%
  spread(Species,Pollinator_richness)

t.test(meandataperplotwtROF$TVUF, meandataperplotwtROF$TVUH, paired = TRUE, alternative = "two.sided")


###### Visitation rate

meandataperplotwtROF <- meandataperplot %>%
  filter(Species != "ROF") %>%
  select(Species,Visitation_rate)%>%
  spread(Species,Visitation_rate)

t.test(meandataperplotwtROF$TVUF, meandataperplotwtROF$TVUH, paired = TRUE, alternative = "two.sided")


###### Proportion of honey bees

meandataperplotwtROF <- meandataperplot %>%
  filter(Species != "ROF") %>%
  select(Species,Proportion_HB)%>%
  spread(Species,Proportion_HB)

t.test(meandataperplotwtROF$TVUF, meandataperplotwtROF$TVUH, paired = TRUE, alternative = "two.sided")


###### Proportion of wild bees

meandataperplotwtROF <- meandataperplot %>%
  filter(Species != "ROF") %>%
  select(Species,Proportion_Bee)%>%
  spread(Species,Proportion_Bee)

t.test(meandataperplotwtROF$TVUF, meandataperplotwtROF$TVUH, paired = TRUE, alternative = "two.sided")


###### Proportion of dipterans

meandataperplotwtROF <- meandataperplot %>%
  filter(Species != "ROF") %>%
  select(Species,Proportion_Diptera)%>%
  spread(Species,Proportion_Diptera)

t.test(meandataperplotwtROF$TVUF, meandataperplotwtROF$TVUH, paired = TRUE, alternative = "two.sided")
