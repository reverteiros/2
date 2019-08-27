
source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades plantes.R")
source("funcionalitat/netejar dades fruits i llavors.R")

library(ggplot2)
library(ggExtra)
library(corrplot)
library(Hmisc)
library(PerformanceAnalytics)

pollenpresence <- pollenclean %>%     #### binomial distribution
  mutate(Total_presence=if_else(Total>0,1,0)) %>%
  mutate(Heterospecific_presence=if_else(Heterospecific>0,1,0))%>%
  group_by(Plot, Species,Plant) %>% 
  summarise(Flowers_with_Total=mean(Total_presence),Heterospecific_presence=mean(Heterospecific_presence),Flowers=n())%>%
  group_by(Plot, Species) %>% 
  summarise(Total_presence=mean(Flowers_with_Total),Heterospecific_presence=mean(Heterospecific_presence),Flower_samples_pollen=sum(Flowers),Individuals_pollen=n())

pollenflowerswpollen <- pollenclean %>%   ### gaussian distribution
  filter(Total>0) %>%
  group_by(Plot, Species,Plant) %>% 
  summarise(Mean_Total=mean(Total))%>%
  group_by(Plot, Species) %>% 
  summarise(Mean_Total=mean(Mean_Total))%>%
  left_join(granspollen,by="Plot")%>%
  mutate(Proportion_Heterosp_Community = (Other_pollen_community+ROF_pollen_community)/(ROF_pollen_community+Other_pollen_community+TVU_pollen_community)) %>%
  select(-c(Other_pollen_community,ROF_pollen_community,TVU_pollen_community))

heterospecificflowerswheterospecific <- pollenclean %>%   ### Mean heterospecific gaussian distribution, proportion = binomial dustribution
  mutate(Total_presence=if_else(Total>0,1,0)) %>%
  mutate(Heterospecific_presence=if_else(Heterospecific>0,1,0))%>%
  filter(Heterospecific_presence>0) %>%
  group_by(Plot, Species,Plant) %>% 
  summarise(Mean_Heterospecific=mean(Heterospecific),Flowers=n())%>%
  group_by(Plot, Species) %>% 
  summarise(Mean_Heterospecific=mean(Mean_Heterospecific),Flower_samples_heterospecific=sum(Flowers),Individuals_heterospecific=n())%>%
  left_join(pollenflowerswpollen,by=c("Plot","Species"))%>%
  mutate(Proportion_Heterosp_Stigma=Mean_Heterospecific/Mean_Total) %>%
  select(Species,Mean_Heterospecific,Proportion_Heterosp_Stigma,Flower_samples_heterospecific,Individuals_heterospecific)


seeds <- fruitset %>%    ### gaussian distribution
  filter(Fruits == 1) %>%
  group_by(Plot, Species,Plant) %>% 
  summarise(Seed_set=mean(Seed))%>%
  group_by(Plot, Species) %>% 
  summarise(Seed_set=mean(Seed_set))

fruits <- fruitset %>%   #### binomial distribution
  group_by(Plot, Species,Plant) %>% 
  summarise(Fruit_set=mean(Fruits),Flowers=n(),Mean_Avorted=mean(Avorted))%>%
  group_by(Plot, Species) %>% 
  summarise(Fruit_set=mean(Fruit_set),Avorted = mean(Mean_Avorted),Individuals_fruits=n(),Flower_samples_fruits=sum(Flowers))

meandataperplot <- datapollinatorsall %>%
  left_join(fruits,by=c("Species","Plot")) %>%
  left_join(seeds,by=c("Species","Plot")) %>%
  left_join(pollenflowerswpollen,by=c("Species","Plot")) %>%
  left_join(pollenpresence,by=c("Species","Plot")) %>%
  left_join(heterospecificflowerswheterospecific,by=c("Species","Plot")) %>%
  left_join(proporciomorfs,by="Plot")

meandataperplotROF <- meandataperplot %>%
  filter(Species=="ROF")%>%
  filter(!is.na(Pollinator_richness))

meandataperplotTVUF <- meandataperplot %>%
  filter(Species=="TVUF")

meandataperplotTVUFheterosp <- meandataperplot %>%
  filter(Species=="TVUF")%>%
  filter(Proportion_Heterosp_Stigma>0)

meandataperplotTVUH <- meandataperplot %>%
  filter(Species=="TVUH")%>%
  filter(Pollinator_richness>0)

## Corrplot
ROF <- meandataperplot %>%
  filter(Species=="ROF") %>%
  select(Pollinator_richness,Visitation_rate,Proportion_HB,Proportion_Bee,Proportion_Diptera,Proportion_Coleoptera,Total_presence,Mean_Total)
ROF <- as.data.frame(ROF) %>%
  select(.,-c(Plot))
chart.Correlation(ROF, histogram=TRUE, pch=19)

## Corrplot
TVUF <- meandataperplot %>%
  filter(Species=="TVUF") %>%
  select(Pollinator_richness,Visitation_rate,Proportion_HB,Proportion_Bee,Proportion_Diptera,Proportion_Lepidoptera,Total_presence,Mean_Total)
TVUF <- as.data.frame(TVUF) %>%
  select(.,-c(Plot))
chart.Correlation(TVUF, histogram=TRUE, pch=19)

## Corrplot
TVUH <- meandataperplot %>%
  filter(Species=="TVUH") %>%
  select(Pollinator_richness,Visitation_rate,Proportion_HB,Proportion_Bee,Proportion_Diptera,Proportion_Lepidoptera,Total_presence,Mean_Total)
TVUH <- as.data.frame(TVUH) %>%
  select(.,-c(Plot))
chart.Correlation(TVUH, histogram=TRUE, pch=19)
