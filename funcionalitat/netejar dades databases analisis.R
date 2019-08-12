
source("funcionalitat/netejar dades fruits i llavors.R")
source("funcionalitat/netejar dades polinitzadors.R")
source("funcionalitat/netejar dades plantes.R")


############################ ARREGLAT

### analisis 1.1 bitxos - flors amb polen

grupstaxonomicsspread <- grupstaxonomics %>%
  spread(Taxonomic_group, Abundance) %>%
  select(-c(Heteroptera,Mecoptera))
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
  mutate(Proportion_HB = Honeybees_VR/Visitation_rate)

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
  mutate(Proportion_HB = Honeybees_VR/Visitation_rate)

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
  mutate(Proportion_Homosp_Community = Grains_Homospecific/Grains_Total)%>%
  mutate(Proportion_Homosp_Stigma = Homospecific / Total) %>%
  mutate(sqrtProportion_Homosp_Community = sqrt(Proportion_Homosp_Community)) 

TVUFpollenbitxoswtna <- TVUFpollenbitxos %>%
  filter(Proportion_Homosp_Stigma > -10)

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
  mutate(Proportion_Homosp_Community = Grains_Homospecific/Grains_Total)%>%
  mutate(Proportion_Homosp_Stigma = Homospecific / Total) %>%
  mutate(sqrtProportion_Homosp_Community = sqrt(Proportion_Homosp_Community)) %>%
  mutate(Plot=as.factor(Plot)) %>%
  mutate(Plant=as.factor(Plant)) 


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
  mutate(Proportion_Homosp_Community = Grains_Homospecific/Grains_Total)%>%
  mutate(Proportion_Homosp_Stigma = Homospecific / Total) %>%
  mutate(sqrtProportion_Homosp_Community = sqrt(Proportion_Homosp_Community)) %>%
  filter(Pollinator_richness > -10)

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
  mutate(Proportion_Homosp_Community = Grains_Homospecific/Grains_Total)%>%
  mutate(Proportion_Homosp_Stigma = Homospecific / Total) %>%
  mutate(sqrtProportion_Homosp_Community = sqrt(Proportion_Homosp_Community)) %>%
  filter(Pollinator_richness > -10)

### analisis 3. polen - fruits i llavors tenint en compte els bitxos

TVUFpollenperplanta <- TVUFpollen %>%
  mutate(Homospecific_presence=if_else(Homospecific>0,1,0)) %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Mean_Homospecific=mean(Homospecific),Mean_Heterospecific=mean(Heterospecific),Mean_Total=mean(Total),Flowers_with_pollen=mean(Homospecific_presence))

TVUFpollenfruitsperplanta <- fruitset %>%
  filter(Species == "TVUF") %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Fruit_set=mean(Fruits),Fecundity=mean(Seed))%>%
  left_join(TVUFpollenperplanta,by=c("Plot", "Plant","Species"))%>%
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
  filter(Mean_Homospecific > -1)

TVUFtotperplanta <- seedset %>%
  filter(Species == "TVUF") %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Seed_set=mean(Seed))%>%
  full_join(TVUFpollenfruitsperplanta,by=c("Plot", "Plant","Species"))%>%
  filter(Mean_Homospecific > 0)


TVUHpollenperplanta <- TVUHpollen %>%
  mutate(Homospecific_presence=if_else(Homospecific>0,1,0)) %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Mean_Homospecific=mean(Homospecific),Mean_Heterospecific=mean(Heterospecific),Mean_Total=mean(Total),Flowers_with_pollen=mean(Homospecific_presence))


TVUHpollenfruitsperplanta <- fruitset %>%
  filter(Species == "TVUH")  %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Fruit_set=mean(Fruits),Fecundity=mean(Seed))%>%
  left_join(TVUHpollenperplanta,by=c("Plot", "Plant","Species"))%>%
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
  mutate(Proportion_HB = Honeybees_VR/Visitation_rate)


TVUHpollenperplanta <- TVUHpollen %>%
  mutate(Homospecific_presence=if_else(Homospecific>0,1,0)) %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Mean_Homospecific=mean(Homospecific),Mean_Heterospecific=mean(Heterospecific),Mean_Total=mean(Total),Flowers_with_pollen=mean(Homospecific_presence))

TVUHtotperplanta <- seedset %>%
  filter(Species == "TVUH") %>%
  group_by(Plot, Plant, Species) %>% 
  summarise(Seed_set=mean(Seed))%>%
  full_join(TVUHpollenfruitsperplanta,by=c("Plot", "Plant","Species"))%>%
  filter(Mean_Homospecific > -1)%>%
  filter(Pollinator_richness > -1)

