
require(devtools)
library(tidyverse)
library(DataCombine)
library(vegan)
source("funcionalitat/index xarxes.R")

############# POllen #######################################################################################
pollenraw<-read.table("dades/polen.txt",header=T)

names(pollenraw) <- c("Plot","Species","Plant","Flower","TVU","ROF","OTHERS","Total")

# recalculate total pollen column
pollenraw <- pollenraw %>%
  select(., -Total) %>%
  mutate(Total = (ROF + TVU + OTHERS))

# remove NAs in the dataset
pollenwtNA <- droplevels(dplyr::filter(pollenraw, !is.na(TVU) & !is.na(ROF)& !is.na(OTHERS)& !is.na(Total)))

# define homospecific and heterospecific pollen per species
ROFpollen <- filter(pollenwtNA, Species == "ROF") %>%
  mutate(Pollen_presence=if_else(Total>0,1,0)) %>%
  mutate(Homospecific = ROF) %>%
  mutate(Heterospecific = TVU+OTHERS)%>%
  mutate(Heterospecific_presence=if_else(Heterospecific>0,1,0)) %>%
  mutate(Homospecific_presence=if_else(Homospecific>0,1,0))  

TVUFpollen <- filter(pollenwtNA, Species == "TVUF") %>%
  mutate(Pollen_presence=if_else(Total>0,1,0)) %>%
  mutate(Homospecific = TVU) %>%
  mutate(Heterospecific = ROF+OTHERS)%>%
  mutate(Heterospecific_presence=if_else(Heterospecific>0,1,0)) %>%
  mutate(Homospecific_presence=if_else(Homospecific>0,1,0))  
  
TVUHpollen <- filter(pollenwtNA, Species == "TVUH") %>%
  mutate(Pollen_presence=if_else(Total>0,1,0)) %>%
  mutate(Homospecific = TVU) %>%
  mutate(Heterospecific = ROF+OTHERS) %>%
  mutate(Heterospecific_presence=if_else(Heterospecific>0,1,0)) %>%
  mutate(Homospecific_presence=if_else(Homospecific>0,1,0))  

pollentotal <- bind_rows(ROFpollen, TVUFpollen, TVUHpollen)

heterospecific <- pollentotal %>% 
  filter(.,Heterospecific_presence==1) %>%
  group_by(Plot, Species) %>% 
  summarise(Heterospecific_only=mean(Heterospecific))

# generate final pollen dataset
pollen <- group_by(pollentotal, Plot, Species) %>% 
  summarise(Samples_pollen=n(),Flowers_with_pollen=mean(Pollen_presence),
            Flowers_with_Homospecific=mean(Homospecific_presence),
            Flowers_with_Heterospecific=mean(Heterospecific_presence),
            Mean_pollen=mean(Total),Mean_Homospecific=mean(Homospecific),
            SD_Homospecific=sd(Homospecific),Mean_Heterospecific=mean(Heterospecific),
            SD_Heterospecific=sd(Heterospecific),
            Ratio_Heterosp_Homosp=(Mean_Heterospecific/Mean_Homospecific),
            Proporcio_Heterosp=(Mean_Heterospecific/Mean_pollen),
            Proporcio_Homosp=(Mean_Homospecific/Mean_pollen))%>%
  complete(Species, Plot) %>%
  distinct() %>%
  left_join(heterospecific)


############## Seed viability and weight

# seedsmeasured <- read.table("dades/llavors pesades i no pesades.txt",header=T)
# names(seedsmeasured) <- c("Plot","Pesades","No_pesades","Species")
# seedsmeasured <- seedsmeasured %>% mutate(TOTAL_llavors=Pesades+No_pesades)

seedweightraw <- read.table("dades/pesos llavors.txt",header=T)

# mean and SD weight of seed weight
seedweight <- seedweightraw %>%
  # filter(Embryo == "viable") %>%
  group_by(Species,Plot) %>% 
  summarise(Weighted_seeds=n(),Mean_weigth=mean(Weight),SD_weight=sd(Weight)) %>%
  complete(Species, Plot) %>%
  distinct() 

# mean and SD weight of seed weight only viable seeds
seedweightviables <- seedweightraw %>%
  filter(Embryo == "viable") %>%
  group_by(Species,Plot) %>% 
  summarise(Mean_weight_viables=mean(Weight),SD_weight_viables=sd(Weight)) %>%
  complete(Species, Plot) %>%
  distinct() %>%
  dplyr::left_join(., seedweight, by = c("Species","Plot"))

seedweightviablesraw <- seedweightraw %>%
  filter(Embryo == "viable")

hist(seedweightviablesraw$Weight,xlim=c(0,0.4),breaks=20)

seedweightnoviablesraw <- seedweightraw %>%
  filter(Embryo == "morta") 

hist(seedweightnoviablesraw$Weight,xlim=c(0,0.4),breaks=5)

# from the weighted seeds, proportion of them that has viable embryo
seedweightandviability <- seedweightraw %>%
  filter(!is.na(Embryo)) %>%
  mutate(Embryo_Numeric=if_else(Embryo=="morta",0,1)) %>%
  group_by(Species,Plot) %>% 
  summarise(Seed_viability=mean(Embryo_Numeric)) %>%
  complete(Species, Plot) %>%
  distinct() %>%
  dplyr::left_join(., seedweightviables, by = c("Species","Plot"))%>%
  mutate(Viable_seeds=Weighted_seeds*Seed_viability)%>%
  mutate(No_viable_seeds=Weighted_seeds*(1-Seed_viability))


############# Fruits and seeds

seedsraw<-read.table("dades/llavors i fruits.txt",header=T)

names(seedsraw) <- c("Plot","Species","Plant","Flower","Avorted","Ovule","Seed","Total")

# 
fruits <- droplevels(dplyr::filter(seedsraw, !is.na(Avorted) & Total == 4)) %>% 
  mutate(Pollinated = Avorted + Seed) %>% 
  mutate(Fruits = if_else(Seed > 0, 1,0)) %>%
  group_by(Plot, Species) %>% 
  summarise(Samples_seeds=n(),Fruits=sum(Fruits),Pollinated_ovules=(mean(Pollinated)),Avorted_total=mean(Avorted))%>%
  mutate(Fruit_set=(Fruits/Samples_seeds)) %>%
  select(., -c(Fruits))

# ggplot(fruits) +
#   geom_jitter(aes(Fruit_set,Pollinated_ovules, colour=Species)) +
#   geom_smooth(aes(Fruit_set,Pollinated_ovules, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="Fruit_set", y="Pollinated_ovules")+
#   scale_colour_manual(values = c("red", "blue", "green3"))+
#   xlim(0, 1)


fruitandseedset <- droplevels(dplyr::filter(seedsraw, !is.na(Avorted) & Total == 4)) %>% 
  mutate(Pollinated = Avorted + Seed) %>% 
  mutate(Fruits_llavor = if_else(Seed > 0, 1,0)) %>%
  filter(.,Fruits_llavor==1) %>%
  group_by(Plot, Species) %>% 
  summarise(Seed_set=mean(Seed),Avorted_fruits=mean(Avorted))%>%
  left_join(fruits, by = c("Plot","Species"))


############# JOIN ALL DATASETS

datafunctionality <- pollen %>%
  dplyr::left_join(., seedweightandviability, by = c("Species","Plot")) %>%
  dplyr::left_join(., fruitandseedset, by = c("Species","Plot")) 


# ggplot(datafunctionality) +
#   geom_jitter(aes(Fruits_pollinated,Seed_set, colour=Species)) +
#   geom_smooth(aes(Fruits_pollinated,Seed_set, colour=Species), method=lm, se=FALSE) +
#   theme_classic() +
#   labs(x="Fruits_pollinated", y="Seed_set")+
#   scale_colour_manual(values = c("red", "blue", "green3"))+
#   xlim(0, 1)


############# join pollinator database

database2 <- read.table("dades/Database3.txt",header=T)

censos <- read.table("dades/censos.txt",header=T)

grupsfuncionals <- read.table("dades/grups funcionals per planta.txt",header=T) %>%
  filter(.,Group!="Heteroptera"&Group!="Mecoptera") %>%
  group_by(Plot,Species) %>% 
  summarise(Functional_groups=n_distinct(Group)) %>%
  complete(Species, Plot) %>%
  distinct() 


names(censos) <- c("Plot","Pollinator","Species","Abundance")

# general abundance and richness in the plot
generalpollinators <- censos %>%
  group_by(Plot) %>% 
  summarise(Pollinator_abundance=sum(Abundance),Pollinator_richness=n_distinct(Pollinator))


# flower abundance per plot
flors <- read.table("dades/flors quantitatiu separant thymus morfs.txt",header=T) %>%
  mutate(Overall_flowers = rowSums(.)) %>%
  mutate(TVU = TVUH + TVUF) %>%
  mutate(proporcioF = TVUF/TVU) %>%
  mutate(ROFrelatiu = ROF/Overall_flowers)%>%
  mutate(TVUFrelatiu = TVUF/Overall_flowers)%>%
  mutate(TVUHrelatiu = TVUH/Overall_flowers)

flowerproportion <- select(flors, ROFrelatiu, TVUFrelatiu, TVUHrelatiu)%>%
  tidyr::gather(Species, "Flower_relative_abundance",1:3) 

flowerabundance <- select(flors, ROF, TVUF, TVUH)%>%
  tidyr::gather(Species, "Flower_Abundance",1:3) 

flowerabundance$Plot = c(1:40)
flowerabundance$Flower_relative_abundance = flowerproportion$Flower_relative_abundance

# Pollinators 
pollinators <- droplevels(dplyr::filter(censos, Species == "ROF" | Species == "TVUF" | Species == "TVUH")) %>% 
  group_by(Plot, Species) %>% 
  summarise(Pollinator_abundance=sum(Abundance),Pollinator_richness=n_distinct(Pollinator))%>%
  complete(Species, Plot) %>%
  distinct() %>%
  left_join(flowerabundance, by = c("Plot","Species")) %>%
  mutate(Visitation_rate = Pollinator_abundance/Flower_Abundance*1000)%>%
  left_join(grupsfuncionals,by=c("Plot","Species"))


## diversitat ROF
diversitatROF <- censos %>%
  dplyr::filter(., Species == "ROF") %>%
  spread(Pollinator, Abundance) %>%
  InsertRow(., NewRow = numeric(81),RowNum = 29)
diversitatROF[is.na(diversitatROF)] <- 0
diversitatROF <- diversitatROF[,-(1:2)]
diversitatROF <- diversity(diversitatROF, "shannon")
diversitatROF <- as.data.frame(diversitatROF)
names(diversitatROF) <- "Shannon_Diversity"
diversitatROF$Plot <- c(1:40)
diversitatROF$Species <- "ROF"

## diversitat TVUF
diversitatTVUF <- censos %>%
  dplyr::filter(., Species == "TVUF") %>%
  spread(Pollinator, Abundance) 
diversitatTVUF[is.na(diversitatTVUF)] <- 0
diversitatTVUF <- diversitatTVUF[,-(1:2)]
diversitatTVUF <- diversity(diversitatTVUF, "shannon")
diversitatTVUF <- as.data.frame(diversitatTVUF)
names(diversitatTVUF) <- "Shannon_Diversity"
diversitatTVUF$Plot <- c(1:40)
diversitatTVUF$Species <- "TVUF"

## diversitat TVUH
diversitatTVUH <- (dplyr::filter(censos, Species == "TVUH")) %>%
  spread(Pollinator, Abundance) %>%
  InsertRow(., NewRow = numeric(57),RowNum = 7)%>%
  InsertRow(., NewRow = numeric(57),RowNum = 18)%>%
  InsertRow(., NewRow = numeric(57),RowNum = 23)%>%
  InsertRow(., NewRow = numeric(57),RowNum = 25)
diversitatTVUH[is.na(diversitatTVUH)] <- 0
diversitatTVUH <- diversitatTVUH[,-(1:2)]
diversitatTVUH <- diversity(diversitatTVUH, "shannon")
diversitatTVUH <- as.data.frame(diversitatTVUH)
names(diversitatTVUH) <- "Shannon_Diversity"
diversitatTVUH$Plot <- c(1:40)
diversitatTVUH$Species <- "TVUH"


diversity <- diversitatTVUH %>%
  bind_rows(.,diversitatTVUF) %>%
  bind_rows(.,diversitatROF)

datafunctionality2 <- left_join(datafunctionality,pollinators, by = c("Plot","Species")) 

datafunctionality3 <- left_join(datafunctionality2,diversity, by = c("Plot","Species")) 


# Honeybees
Apis <- filter(censos, Pollinator =="Apis")

Apis2 <- droplevels(dplyr::filter(Apis, Species == "ROF" | Species == "TVUF" | Species == "TVUH")) %>% 
  group_by(Plot, Species) %>% 
  summarise(HB_abundance=sum(Abundance))%>%
  complete(Species, Plot) %>%
  distinct() %>%
  left_join(flowerabundance, by = c("Plot","Species")) 
Apis2[is.na(Apis2)] <- 0

datafunction <- left_join(datafunctionality3, Apis2, by = c("Plot","Species","Flower_Abundance","Flower_relative_abundance")) %>%
  mutate(Wild_abundance = Pollinator_abundance - HB_abundance) %>%
  mutate(HB_Visitation_rate = HB_abundance/Flower_Abundance*1000) %>%
  mutate(Wild_Visitation_rate = Wild_abundance/Flower_Abundance*1000) %>%
  mutate(Proportion_Wild = Wild_Visitation_rate/Visitation_rate) %>%
  mutate(Proportion_HB = HB_Visitation_rate/Visitation_rate)

database2 <- database2 %>%
  mutate(Plot = PLOT) %>%
  select(Plot,T_Max)

dataanalysis <- datafunction %>%
  left_join(dprime,by=c("Plot","Species")) %>%
  left_join(closenesss,by=c("Plot","Species"))%>%
  mutate(Proportion_Homospecific = Mean_Homospecific / Mean_pollen) %>%
  mutate(Proportion_Heterospecific = Mean_Heterospecific / Mean_pollen)%>%
  left_join(networkmetrics, by="Plot") %>%
  left_join(database2, by="Plot") %>%
  left_join(apis, by="Plot")






# ### Anàlisis a nivell de planta
# 
# pollen <- group_by(pollentotal, Plot, Species, Plant) %>%
#   summarise(Samples_pollen=n(),Flowers_with_pollen=mean(Pollen_presence),
#             Mean_pollen=mean(Total),Mean_Homospecific=mean(Homospecific),
#             Mean_Heterospecific=mean(Heterospecific))%>%
#   complete(Species, Plot) %>%
#   distinct()
# 
# fruits <- droplevels(dplyr::filter(seedsraw, !is.na(Avorted) & Total == 4)) %>%
#   mutate(Pollinated = Avorted + Seed) %>%
#   mutate(Proportion_avorted = Avorted / Pollinated) %>%
#   mutate(Fruits = if_else(Seed > 0, 1,0)) %>%
#   group_by(Plot, Species, Plant) %>%
#   summarise(Samples_seeds=n(),Fruits=sum(Fruits),Percent_pollination=(mean(Pollinated)/4*100),Proportion_avorted=mean(Proportion_avorted))%>%
#   mutate(Fruit_set=(Fruits/Samples_seeds)) %>%
#   select(., -c(Fruits)) %>%
#   complete(Species, Plot, Plant) %>%
#   distinct()
# 
# fruitandseedset <- droplevels(dplyr::filter(seedsraw, !is.na(Avorted) & Total == 4)) %>%
#   mutate(Pollinated = Avorted + Seed) %>%
#   mutate(Fruits = if_else(Seed > 0, 1,0)) %>%
#   filter(.,Fruits==1) %>%
#   group_by(Plot, Species, Plant) %>%
#   summarise(Seed_set=mean(Seed))%>%
#   left_join(fruits, by = c("Plot","Species","Plant"))%>%
#   complete(Species, Plot, Plant) %>%
#   distinct()
# 
# 
# datafunctionalityperplant <- pollen %>%
#   dplyr::left_join(., fruitandseedset, by = c("Species","Plot","Plant"))
# 



