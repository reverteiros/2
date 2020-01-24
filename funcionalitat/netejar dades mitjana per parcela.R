
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
  mutate(Homospecific_presence=if_else(Homospecific>0,1,0))%>%
  group_by(Plot, Species,Plant) %>% 
  summarise(Flowers_with_Total=mean(Total_presence),Heterospecific_presence=mean(Heterospecific_presence),Flowers=n(),Homospecific_presence=mean(Homospecific_presence))%>%
  group_by(Plot, Species) %>% 
  summarise(Homospecific_presence=mean(Homospecific_presence),Total_presence=mean(Flowers_with_Total),Heterospecific_presence=mean(Heterospecific_presence),Flower_samples_pollen=sum(Flowers),Individuals_pollen=n())

pollenflowerswpollen <- pollenclean %>%   ### gaussian distribution
  filter(Total>0) %>%
  group_by(Plot, Species,Plant) %>% 
  summarise(Mean_Total=mean(Total))%>%
  group_by(Plot, Species) %>% 
  summarise(Mean_Total=mean(Mean_Total))%>%
  left_join(granspollen,by="Plot")%>%
  mutate(Proportion_Heterosp_Community = (Other_pollen_community+ROF_pollen_community)/(ROF_pollen_community+Other_pollen_community+TVU_pollen_community)) %>%
  select(-c(Other_pollen_community,ROF_pollen_community,TVU_pollen_community))

pollenflowerswhomospecific <- pollenclean %>%   ### gaussian distribution
  filter(Homospecific>0) %>%
  group_by(Plot, Species,Plant) %>% 
  summarise(Mean_Homospecific=mean(Homospecific))%>%
  group_by(Plot, Species) %>% 
  summarise(Mean_Homospecific=mean(Mean_Homospecific))%>%
  left_join(granspollen,by="Plot")%>%
  select(Plot,Species,Mean_Homospecific)

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


flowers_total <- read.table("dades/flors quantitatiu separant thymus morfs.txt",header=T) %>%
  tidyr::gather(Species, "Flower_Abundance") %>%
  mutate(Flower_Abundance = Flower_Abundance*3) 

flowers_total$Plot = c(1:40)

flowers_example <- flowers_total %>%
  group_by(Plot) %>% 
  summarise(Overall_flowers = sum(Flower_Abundance))


meandataperplot <- datapollinatorsall %>%
  left_join(fruits,by=c("Species","Plot")) %>%
  left_join(seeds,by=c("Species","Plot")) %>%
  left_join(pollenflowerswpollen,by=c("Species","Plot")) %>%
  left_join(pollenflowerswhomospecific,by=c("Species","Plot")) %>%
  left_join(pollenpresence,by=c("Species","Plot")) %>%
  left_join(proporciomorfs,by="Plot")%>%
  left_join(flowers_example,by="Plot") %>%
  mutate(Proportion_plant = Flower_Abundance/Overall_flowers)

meandataperplotROF <- meandataperplot %>%
  filter(Species=="ROF")%>%
  filter(Pollinator_abundance > 1)%>%
  mutate(logVisitation_rate = log(Visitation_rate)) 

meandataperplotTVUF <- meandataperplot %>%
  filter(Species=="TVUF") 

meandataperplotTVUH <- meandataperplot %>%
  filter(Species=="TVUH")%>%
  filter(Proportion_Bee>-1) %>%
  mutate(logVisitation_rate = log(Visitation_rate)) %>%
  mutate(sqrtProportion_Bee=sqrt((Proportion_Bee)))
  
