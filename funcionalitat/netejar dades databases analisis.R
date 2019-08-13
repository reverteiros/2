
source("funcionalitat/netejar dades fruits i llavors.R")
source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades plantes.R")


### analisis 1.1 bitxos - flors amb polen

grupstaxonomicsspread <- grupstaxonomics %>%
  spread(Taxonomic_group, Abundance) 
grupstaxonomicsspread[is.na(grupstaxonomicsspread)] <- 0

ROFpollenbitxos <- ROFpollen %>%
  mutate(Homospecific_presence=if_else(Homospecific>0,1,0)) %>%
  mutate(Heterospecific_presence=if_else(Heterospecific>0,1,0)) %>%
  left_join(datapollinatorsall,by=c("Species","Plot")) %>%
  mutate(logVisitation_rate = log(Visitation_rate))%>%
  left_join(grupstaxonomicsspread,by=c("Species","Plot")) %>%
  mutate(Bee_VR = (Bee*1000/(Flower_Abundance))) %>%
  mutate(Coleoptera_VR = (Coleoptera*1000/(Flower_Abundance))) %>%
  mutate(Diptera_VR = (Diptera*1000/(Flower_Abundance))) %>%
  mutate(Lepidoptera_VR = (Lepidoptera*1000/(Flower_Abundance))) %>%
  mutate(Wasp_VR = (Wasp*1000/(Flower_Abundance))) %>%
  mutate(Honeybees_VR = (Honeybees*1000/(Flower_Abundance))) %>%
  mutate(Proportion_HB = Honeybees_VR/Visitation_rate) %>%
  mutate(Proportion_Bee = Bee_VR/Visitation_rate) %>%
  mutate(Proportion_Coleoptera = Coleoptera_VR/Visitation_rate) %>%
  mutate(Proportion_Diptera = Diptera_VR/Visitation_rate) %>%
  mutate(Proportion_Lepidoptera = Lepidoptera_VR/Visitation_rate) %>%
  mutate(Proportion_Wasp = Wasp_VR/Visitation_rate) 

ROFpollenflowerswithpollen <- ROFpollen %>%
  filter(Homospecific>0)%>%
  left_join(datapollinatorsall,by=c("Species","Plot")) %>%
  mutate(logVisitation_rate = log(Visitation_rate))%>%
  left_join(grupstaxonomicsspread,by=c("Species","Plot"))%>%
  mutate(Bee_VR = (Bee*1000/(Flower_Abundance))) %>%
  mutate(Coleoptera_VR = (Coleoptera*1000/(Flower_Abundance))) %>%
  mutate(Diptera_VR = (Diptera*1000/(Flower_Abundance))) %>%
  mutate(Lepidoptera_VR = (Lepidoptera*1000/(Flower_Abundance))) %>%
  mutate(Wasp_VR = (Wasp*1000/(Flower_Abundance))) %>%
  mutate(Honeybees_VR = (Honeybees*1000/(Flower_Abundance))) %>%
  mutate(Proportion_HB = Honeybees_VR/Visitation_rate)%>%
  mutate(Proportion_Bee = Bee_VR/Visitation_rate) %>%
  mutate(Proportion_Coleoptera = Coleoptera_VR/Visitation_rate) %>%
  mutate(Proportion_Diptera = Diptera_VR/Visitation_rate) %>%
  mutate(Proportion_Lepidoptera = Lepidoptera_VR/Visitation_rate) %>%
  mutate(Proportion_Wasp = Wasp_VR/Visitation_rate) 

TVUFpollenbitxos <- TVUFpollen %>%
  mutate(Homospecific_presence=if_else(Homospecific>0,1,0)) %>%
  mutate(Heterospecific_presence=if_else(Heterospecific>0,1,0)) %>%
  left_join(datapollinatorsall,by=c("Species","Plot"))%>%
  left_join(proporciomorfs,by="Plot")%>%
  mutate(logPollinator_richness = log(Pollinator_richness)) %>%
  mutate(logVisitation_rate = log(Visitation_rate)) %>%
  mutate(logFunctional_group_Rocka = log(Functional_group_Rocka)) %>%
  left_join(grupstaxonomicsspread,by=c("Species","Plot")) %>%
  mutate(Bee_VR = (Bee*1000/(Flower_Abundance))) %>%
  mutate(Coleoptera_VR = (Coleoptera*1000/(Flower_Abundance))) %>%
  mutate(Diptera_VR = (Diptera*1000/(Flower_Abundance))) %>%
  mutate(Lepidoptera_VR = (Lepidoptera*1000/(Flower_Abundance))) %>%
  mutate(Wasp_VR = (Wasp*1000/(Flower_Abundance))) %>%
  mutate(Honeybees_VR = (Honeybees*1000/(Flower_Abundance))) %>%
  mutate(Proportion_HB = Honeybees_VR/Visitation_rate)%>%
  mutate(Grains_Homospecific = TVU_pollen_community) %>%
  mutate(Grains_Heterospecific = Other_pollen_community+ROF_pollen_community) %>%
  mutate(Grains_Total = ROF_pollen_community+Other_pollen_community+TVU_pollen_community) %>%
  mutate(Proportion_Heterosp_Community = Grains_Heterospecific/Grains_Total)%>%
  mutate(Proportion_Heterosp_Stigma = Heterospecific / Total) %>%
  mutate(logProportion_Heterosp_Community = log(Proportion_Heterosp_Community)) %>%
  mutate(Proportion_Bee = Bee_VR/Visitation_rate) %>%
  mutate(Proportion_Coleoptera = Coleoptera_VR/Visitation_rate) %>%
  mutate(Proportion_Diptera = Diptera_VR/Visitation_rate) %>%
  mutate(Proportion_Lepidoptera = Lepidoptera_VR/Visitation_rate) %>%
  mutate(Proportion_Wasp = Wasp_VR/Visitation_rate) 

TVUFpollenbitxoswtna <- TVUFpollenbitxos %>%
  filter(Proportion_Heterosp_Stigma > -10)

TVUFpollenflowerswithpollen <- TVUFpollen %>%
  filter(Homospecific>0)%>%
  left_join(datapollinatorsall,by=c("Species","Plot"))%>%
  left_join(proporciomorfs,by="Plot")%>%
  mutate(logPollinator_richness = log(Pollinator_richness)) %>%
  mutate(logVisitation_rate = log(Visitation_rate)) %>%
  left_join(grupstaxonomicsspread,by=c("Species","Plot")) %>%
  mutate(Bee_VR = (Bee*1000/(Flower_Abundance))) %>%
  mutate(Coleoptera_VR = (Coleoptera*1000/(Flower_Abundance))) %>%
  mutate(Diptera_VR = (Diptera*1000/(Flower_Abundance))) %>%
  mutate(Lepidoptera_VR = (Lepidoptera*1000/(Flower_Abundance))) %>%
  mutate(Wasp_VR = (Wasp*1000/(Flower_Abundance))) %>%
  mutate(Honeybees_VR = (Honeybees*1000/(Flower_Abundance))) %>%
  mutate(Proportion_HB = Honeybees_VR/Visitation_rate)%>%
  mutate(Grains_Homospecific = TVU_pollen_community) %>%
  mutate(Grains_Heterospecific = Other_pollen_community+ROF_pollen_community) %>%
  mutate(Grains_Total = ROF_pollen_community+Other_pollen_community+TVU_pollen_community) %>%
  mutate(Proportion_Heterosp_Community = Grains_Heterospecific/Grains_Total)%>%
  mutate(Proportion_Heterosp_Stigma = Heterospecific / Total) %>%
  mutate(sqrtProportion_Heterosp_Community = sqrt(Proportion_Heterosp_Community)) %>%
  mutate(Proportion_Bee = Bee_VR/Visitation_rate) %>%
  mutate(Proportion_Coleoptera = Coleoptera_VR/Visitation_rate) %>%
  mutate(Proportion_Diptera = Diptera_VR/Visitation_rate) %>%
  mutate(Proportion_Lepidoptera = Lepidoptera_VR/Visitation_rate) %>%
  mutate(Proportion_Wasp = Wasp_VR/Visitation_rate) 


TVUHpollenbitxos <- TVUHpollen %>%
  mutate(Homospecific_presence=if_else(Homospecific>0,1,0)) %>%
  mutate(Heterospecific_presence=if_else(Heterospecific>0,1,0)) %>%
  left_join(datapollinatorsall,by=c("Species","Plot"))%>%
  left_join(proporciomorfs,by="Plot")%>%
  mutate(logPollinator_richness = log(Pollinator_richness)) %>%
  mutate(logVisitation_rate = log(Visitation_rate)) %>%
  left_join(grupstaxonomicsspread,by=c("Species","Plot")) %>%
  mutate(Bee_VR = (Bee*1000/(Flower_Abundance))) %>%
  mutate(Coleoptera_VR = (Coleoptera*1000/(Flower_Abundance))) %>%
  mutate(Diptera_VR = (Diptera*1000/(Flower_Abundance))) %>%
  mutate(Lepidoptera_VR = (Lepidoptera*1000/(Flower_Abundance))) %>%
  mutate(Wasp_VR = (Wasp*1000/(Flower_Abundance))) %>%
  mutate(Honeybees_VR = (Honeybees*1000/(Flower_Abundance))) %>%
  mutate(Proportion_HB = Honeybees_VR/Visitation_rate)%>%
  mutate(Grains_Homospecific = TVU_pollen_community) %>%
  mutate(Grains_Heterospecific = Other_pollen_community+ROF_pollen_community) %>%
  mutate(Grains_Total = ROF_pollen_community+Other_pollen_community+TVU_pollen_community) %>%
  mutate(Proportion_Heterosp_Community = Grains_Heterospecific/Grains_Total)%>%
  mutate(Proportion_Heterosp_Stigma = Heterospecific / Total) %>%
  mutate(sqrtProportion_Heterosp_Community = sqrt(Proportion_Heterosp_Community)) %>%
  filter(Pollinator_richness > -10)%>%
  mutate(Proportion_Bee = Bee_VR/Visitation_rate) %>%
  mutate(Proportion_Coleoptera = Coleoptera_VR/Visitation_rate) %>%
  mutate(Proportion_Diptera = Diptera_VR/Visitation_rate) %>%
  mutate(Proportion_Lepidoptera = Lepidoptera_VR/Visitation_rate) %>%
  mutate(Proportion_Wasp = Wasp_VR/Visitation_rate) 

TVUHpollenflowerswithpollen <- TVUHpollen %>%
  filter(Homospecific>0)%>%
  left_join(datapollinatorsall,by=c("Species","Plot"))%>%
  left_join(proporciomorfs,by="Plot")%>%
  mutate(logPollinator_richness = log(Pollinator_richness)) %>%
  mutate(logVisitation_rate = log(Visitation_rate)) %>%
  left_join(grupstaxonomicsspread,by=c("Species","Plot")) %>%
  mutate(Bee_VR = (Bee*1000/(Flower_Abundance))) %>%
  mutate(Coleoptera_VR = (Coleoptera*1000/(Flower_Abundance))) %>%
  mutate(Diptera_VR = (Diptera*1000/(Flower_Abundance))) %>%
  mutate(Lepidoptera_VR = (Lepidoptera*1000/(Flower_Abundance))) %>%
  mutate(Wasp_VR = (Wasp*1000/(Flower_Abundance))) %>%
  mutate(Honeybees_VR = (Honeybees*1000/(Flower_Abundance))) %>%
  mutate(Proportion_HB = Honeybees_VR/Visitation_rate)%>%
  mutate(Grains_Homospecific = TVU_pollen_community) %>%
  mutate(Grains_Heterospecific = Other_pollen_community+ROF_pollen_community) %>%
  mutate(Grains_Total = ROF_pollen_community+Other_pollen_community+TVU_pollen_community) %>%
  mutate(Proportion_Heterosp_Community = Grains_Heterospecific/Grains_Total)%>%
  mutate(Proportion_Heterosp_Stigma = Heterospecific / Total) %>%
  mutate(sqrtProportion_Heterosp_Community = sqrt(Proportion_Heterosp_Community)) %>%
  filter(Pollinator_richness > -10)%>%
  mutate(Proportion_Bee = Bee_VR/Visitation_rate) %>%
  mutate(Proportion_Coleoptera = Coleoptera_VR/Visitation_rate) %>%
  mutate(Proportion_Diptera = Diptera_VR/Visitation_rate) %>%
  mutate(Proportion_Lepidoptera = Lepidoptera_VR/Visitation_rate) %>%
  mutate(Proportion_Wasp = Wasp_VR/Visitation_rate) 

### analisis 2. polen - fruits i llavors tenint en compte els bitxos

TVUFflowerswithpollenperplanta <- TVUFpollenbitxos %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Homospecific_presence=mean(Homospecific_presence))

TVUFpollenflowerswithpollenperplanta <- TVUFpollenflowerswithpollen%>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Mean_Homospecific=mean(Homospecific))

TVUFpollenfruitsperplanta <- fruitset %>%
  filter(Species == "TVUF") %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Fruit_set=mean(Fruits),Fecundity=mean(Seed))

TVUFpollenseedsperplanta <- seedset %>%
  filter(Species == "TVUF") %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Seed_set=mean(Seed))
  # filter(Mean_Homospecific < 8.1) 

TVUFtotperplanta <- TVUFflowerswithpollenperplanta %>%
  full_join(TVUFpollenflowerswithpollenperplanta,by=c("Plot", "Plant","Species"))%>%
  full_join(TVUFpollenfruitsperplanta,by=c("Plot", "Plant","Species"))%>%
  full_join(TVUFpollenseedsperplanta,by=c("Plot", "Plant","Species"))%>%
  full_join(proporciomorfs,by="Plot")%>%
  full_join(datapollinatorsall,by=c("Species","Plot"))%>%
  mutate(logPollinator_richness = log(Pollinator_richness)) %>%
  mutate(logVisitation_rate = log(Visitation_rate)) %>%
  full_join(grupstaxonomicsspread,by=c("Species","Plot")) %>%
  mutate(Bee_VR = (Bee*1000/(Flower_Abundance))) %>%
  mutate(Coleoptera_VR = (Coleoptera*1000/(Flower_Abundance))) %>%
  mutate(Diptera_VR = (Diptera*1000/(Flower_Abundance))) %>%
  mutate(Lepidoptera_VR = (Lepidoptera*1000/(Flower_Abundance))) %>%
  mutate(Wasp_VR = (Wasp*1000/(Flower_Abundance))) %>%
  mutate(Honeybees_VR = (Honeybees*1000/(Flower_Abundance))) %>%
  mutate(Proportion_HB = Honeybees_VR/Visitation_rate)%>%
  # filter(Mean_Homospecific > -1)%>%
  mutate(Proportion_Bee = Bee_VR/Visitation_rate) %>%
  mutate(Proportion_Coleoptera = Coleoptera_VR/Visitation_rate) %>%
  mutate(Proportion_Diptera = Diptera_VR/Visitation_rate) %>%
  mutate(Proportion_Lepidoptera = Lepidoptera_VR/Visitation_rate) %>%
  mutate(Proportion_Wasp = Wasp_VR/Visitation_rate) 




TVUHflowerswithpollenperplanta <- TVUHpollenbitxos %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Homospecific_presence=mean(Homospecific_presence))

TVUHpollenflowerswithpollenperplanta <- TVUHpollenflowerswithpollen%>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Mean_Homospecific=mean(Homospecific))

TVUHpollenfruitsperplanta <- fruitset %>%
  filter(Species == "TVUH") %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Fruit_set=mean(Fruits),Fecundity=mean(Seed))

TVUHpollenseedsperplanta <- seedset %>%
  filter(Species == "TVUH") %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Seed_set=mean(Seed))
# filter(Mean_Homospecific < 8.1) 

TVUHtotperplanta <- TVUHflowerswithpollenperplanta %>%
  full_join(TVUHpollenflowerswithpollenperplanta,by=c("Plot", "Plant","Species"))%>%
  full_join(TVUHpollenfruitsperplanta,by=c("Plot", "Plant","Species"))%>%
  full_join(TVUHpollenseedsperplanta,by=c("Plot", "Plant","Species"))%>%
  left_join(proporciomorfs,by="Plot")%>%
  left_join(datapollinatorsall,by=c("Species","Plot"))%>%
  mutate(logPollinator_richness = log(Pollinator_richness)) %>%
  mutate(logVisitation_rate = log(Visitation_rate)) %>%
  left_join(grupstaxonomicsspread,by=c("Species","Plot")) %>%
  mutate(Bee_VR = (Bee*1000/(Flower_Abundance))) %>%
  mutate(Coleoptera_VR = (Coleoptera*1000/(Flower_Abundance))) %>%
  mutate(Diptera_VR = (Diptera*1000/(Flower_Abundance))) %>%
  mutate(Lepidoptera_VR = (Lepidoptera*1000/(Flower_Abundance))) %>%
  mutate(Wasp_VR = (Wasp*1000/(Flower_Abundance))) %>%
  mutate(Honeybees_VR = (Honeybees*1000/(Flower_Abundance))) %>%
  mutate(Proportion_HB = Honeybees_VR/Visitation_rate)%>%
  # filter(Mean_Homospecific > -1)%>%
  mutate(Proportion_Bee = Bee_VR/Visitation_rate) %>%
  mutate(Proportion_Coleoptera = Coleoptera_VR/Visitation_rate) %>%
  mutate(Proportion_Diptera = Diptera_VR/Visitation_rate) %>%
  mutate(Proportion_Lepidoptera = Lepidoptera_VR/Visitation_rate) %>%
  mutate(Proportion_Wasp = Wasp_VR/Visitation_rate) 
