
source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades plantes.R")
source("funcionalitat/netejar dades fruits i llavors.R")


pollenpresence <- pollenclean %>%
  mutate(Total_presence=if_else(Total>0,1,0)) %>%
  mutate(Heterospecific_presence=if_else(Heterospecific>0,1,0))%>%
  group_by(Plot, Species) %>% 
  summarise(Total_presence=mean(Total_presence),Heterospecific_presence=mean(Heterospecific_presence))

pollenflowerswpollen <- pollenclean %>%
  filter(Total>0) %>%
  group_by(Plot, Species) %>% 
  summarise(Mean_pollen=mean(Total))

seeds <- fruitset %>%
  filter(Fruits == 1) %>%
  group_by(Plot, Species) %>% 
  summarise(Seed_set=mean(Seed),Avorted_per_fruit=mean(Avorted))

fruitsandfecundity <- fruitset %>%
  group_by(Plot, Species) %>% 
  summarise(Fruit_set=mean(Fruits),Avorted_total=mean(Avorted))

meandataperplot <- datapollinatorsall %>%
  left_join(fruitsandfecundity,by=c("Species","Plot")) %>%
  left_join(seeds,by=c("Species","Plot")) %>%
  left_join(pollenflowerswpollen,by=c("Species","Plot")) %>%
  left_join(pollenpresence,by=c("Species","Plot")) %>%
  left_join(proporciomorfs,by="Plot")%>%
  mutate(logMean_pollen = log(Mean_pollen))


