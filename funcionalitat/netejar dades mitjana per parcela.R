
source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades plantes.R")
source("funcionalitat/netejar dades fruits i llavors.R")


pollenpresence <- pollenclean %>%     #### binomial distribution
  mutate(Heterospecific_presence=if_else(Heterospecific>0,1,0))%>%
  mutate(Conspecific_presence=if_else(Conspecific>0,1,0))%>%
  group_by(Plot, Species,Plant) %>% 
  summarise(Heterospecific_presence=mean(Heterospecific_presence),Flowers=n(),Conspecific_presence=mean(Conspecific_presence))%>%
  group_by(Plot, Species) %>% 
  dplyr::summarise(Conspecific_presence=mean(Conspecific_presence),Heterospecific_presence=mean(Heterospecific_presence),Flower_samples_pollen=sum(Flowers),Individuals_pollen=n())

pollenflowerswconspecific <- pollenclean %>%   ### gaussian distribution
  filter(Conspecific>0) %>%
  group_by(Plot, Species,Plant) %>% 
  summarise(Mean_Conspecific=mean(Conspecific))%>%
  group_by(Plot, Species) %>% 
  summarise(Mean_Conspecific=mean(Mean_Conspecific))%>%
  select(Plot,Species,Mean_Conspecific)

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
  left_join(pollenflowerswconspecific,by=c("Species","Plot")) %>%
  left_join(pollenpresence,by=c("Species","Plot")) %>%
  left_join(proporciomorfs,by="Plot")

meandataperplotTVUF <- meandataperplot %>%
  filter(Species=="TVUF")


meandataperplotTVUH <- meandataperplot %>%
  filter(Species=="TVUH")%>%
  filter(Proportion_Bee>-1) %>%
  mutate(logVisitation_rate = log(Visitation_rate)) %>%
  mutate(sqrtProportion_Bee=sqrt((Proportion_Bee)))
  
