
source("funcionalitat/netejar dades fruits i llavors.R")
source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades plantes.R")


### analisis 1 bitxos - polen

ROFpollenbitxos <- ROFpollen %>%
  left_join(datapollinatorsall,by=c("Species","Plot"))  

ROFpollenflowerswithpollen <- ROFpollenbitxos %>%
  filter(Total>0)


TVUFpollenbitxos <- TVUFpollen %>%
  left_join(datapollinatorsall,by=c("Species","Plot"))%>%
  left_join(proporciomorfs,by="Plot")%>%
  mutate(Grains_Homospecific = TVU_pollen_community) %>%
  mutate(Grains_Heterospecific = Other_pollen_community+ROF_pollen_community) %>%
  mutate(Grains_Total = ROF_pollen_community+Other_pollen_community+TVU_pollen_community) %>%
  mutate(Proportion_Heterosp_Community = Grains_Heterospecific/Grains_Total) 

TVUFpollenflowerswithpollen <- TVUFpollenbitxos %>%
  filter(Total>0)

TVUFheterospecificflowerswithheterospecific <- TVUFpollenbitxos %>%
  filter(Heterospecific>0) %>%
  mutate(Proportion_Heterosp_Stigma = Heterospecific / Total)%>%
  mutate(logProportion_Heterosp_Community = log(Proportion_Heterosp_Community)) 
  


TVUHpollenbitxos <- TVUHpollen %>%
  left_join(datapollinatorsall,by=c("Species","Plot"))%>%
  left_join(proporciomorfs,by="Plot")

TVUHpollenflowerswithpollen <- TVUHpollenbitxos %>%
  filter(Total>0)


### analisis 2. bitxos - fruits i llavors

TVUFflowerswithpollenperplanta <- TVUFpollenbitxos %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Total_presence=mean(Total_presence))

TVUFpollenflowerswithpollenperplanta <- TVUFpollenflowerswithpollen%>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Mean_Total=mean(Total))

TVUFpollenfruitsperplanta <- fruitset %>%
  filter(Species == "TVUF") %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Fruit_set=mean(Fruits))

TVUFpollenseedsperplanta <- seedset %>%
  filter(Species == "TVUF") %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Seed_set=mean(Seed))

TVUFtotperplanta <- TVUFflowerswithpollenperplanta %>%
  full_join(TVUFpollenflowerswithpollenperplanta,by=c("Plot", "Plant","Species"))%>%
  full_join(TVUFpollenfruitsperplanta,by=c("Plot", "Plant","Species"))%>%
  full_join(TVUFpollenseedsperplanta,by=c("Plot", "Plant","Species"))%>%
  full_join(proporciomorfs,by="Plot")%>%
  full_join(datapollinatorsall,by=c("Species","Plot"))




TVUHflowerswithpollenperplanta <- TVUHpollenbitxos %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Total_presence=mean(Total_presence))

TVUHpollenflowerswithpollenperplanta <- TVUHpollenflowerswithpollen%>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Mean_Total=mean(Total))

TVUHpollenfruitsperplanta <- fruitset %>%
  filter(Species == "TVUH") %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Fruit_set=mean(Fruits))

TVUHpollenseedsperplanta <- seedset %>%
  filter(Species == "TVUH") %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Seed_set=mean(Seed))

TVUHtotperplanta <- TVUHflowerswithpollenperplanta %>%
  full_join(TVUHpollenflowerswithpollenperplanta,by=c("Plot", "Plant","Species"))%>%
  full_join(TVUHpollenfruitsperplanta,by=c("Plot", "Plant","Species"))%>%
  full_join(TVUHpollenseedsperplanta,by=c("Plot", "Plant","Species"))%>%
  left_join(proporciomorfs,by="Plot")%>%
  left_join(datapollinatorsall,by=c("Species","Plot"))
