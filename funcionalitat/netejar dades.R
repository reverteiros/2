
require(devtools)
library(tidyverse)
library(DataCombine)
library(vegan)

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
  mutate(Heterospecific = TVU+OTHERS)

TVUFpollen <- filter(pollenwtNA, Species == "TVUF") %>%
  mutate(Pollen_presence=if_else(Total>0,1,0)) %>%
  mutate(Homospecific = TVU) %>%
  mutate(Heterospecific = ROF+OTHERS)
  
TVUHpollen <- filter(pollenwtNA, Species == "TVUH") %>%
  mutate(Pollen_presence=if_else(Total>0,1,0)) %>%
  mutate(Homospecific = TVU) %>%
  mutate(Heterospecific = ROF+OTHERS)

pollentotal <- bind_rows(ROFpollen, TVUFpollen, TVUHpollen)

# generate final pollen dataset
pollen <- group_by(pollentotal, Plot, Species) %>% 
  summarise(Samples_pollen=n(),Flowers_with_pollen=mean(Pollen_presence),
            Mean_pollen=mean(Total),SD_pollen=sd(Total),Mean_Homospecific=mean(Homospecific),
            SD_Homospecific=sd(Homospecific),Mean_Heterospecific=mean(Heterospecific),
            SD_Heterospecific=sd(Heterospecific))%>%
  complete(Species, Plot) %>%
  distinct() 
  

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
  summarise(Mean_weigth_viables=mean(Weight),SD_weight_viables=sd(Weight)) %>%
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
  mutate(Proportion_avorted = Avorted / Pollinated) %>%
  mutate(Fruits = if_else(Seed > 0, 1,0)) %>%
  group_by(Plot, Species) %>% 
  summarise(Samples_seeds=n(),Fruits=sum(Fruits),Percent_pollination=(mean(Pollinated)/4*100),Proportion_avorted=mean(Proportion_avorted))%>%
  mutate(Fruit_set=(Fruits/Samples_seeds)) %>%
  select(., -c(Fruits)) 

fruitandseedset <- droplevels(dplyr::filter(seedsraw, !is.na(Avorted) & Total == 4)) %>% 
  mutate(Pollinated = Avorted + Seed) %>% 
  mutate(Fruits = if_else(Seed > 0, 1,0)) %>%
  filter(.,Fruits==1) %>%
  group_by(Plot, Species) %>% 
  summarise(Seed_set=mean(Seed))%>%
  left_join(fruits, by = c("Plot","Species"))


############# JOIN ALL DATASETS

datafunctionality <- pollen %>%
  dplyr::left_join(., seedweightandviability, by = c("Species","Plot")) %>%
  dplyr::left_join(., fruitandseedset, by = c("Species","Plot")) 


############# join pollinator database

database2 <- read.table("dades/Database3.txt",header=T)

censos <- read.table("dades/censos.txt",header=T)

names(censos) <- c("Plot","Pollinator","Species","Abundance")

# general abundance and richness in the plot
generalpollinators <- censos %>%
  group_by(Plot) %>% 
  summarise(Pollinator_abundance=sum(Abundance),Pollinator_richness=n_distinct(Pollinator))


# flower abundance per plot
flors <- read.table("dades/flors quantitatiu separant thymus morfs.txt",header=T)

flors$TVU <- flors$TVUH + flors$TVUF
flors$proporcioF <- flors$TVUF*100/flors$TVU

hist(flors$proporcioF )
flowerabundance <- select(flors, TVUF, ROF, TVUH)%>%
  tidyr::gather(Species, "Flower_Abundance",1:3) 

flowerabundance$Plot = c(1:40)

# Pollinators 
pollinators <- droplevels(dplyr::filter(censos, Species == "ROF" | Species == "TVUF" | Species == "TVUH")) %>% 
  group_by(Plot, Species) %>% 
  summarise(Pollinator_abundance=sum(Abundance),Pollinator_richness=n_distinct(Pollinator))%>%
  complete(Species, Plot) %>%
  distinct() %>%
  left_join(flowerabundance, by = c("Plot","Species")) %>%
  mutate(Visitation_rate = Pollinator_abundance/Flower_Abundance*1000)


## diversitat ROF
diversitatROF <- censos %>%
  dplyr::filter(., Species == "ROF") %>%
  spread(Pollinator, Abundance) %>%
  InsertRow(., NewRow = numeric(81),RowNum = 29)
diversitatROF[is.na(diversitatROF)] <- 0
diversitatROF <- diversitatROF[,-(1:2)]
diversitatROF <- diversity(diversitatROF, "shannon")
diversitatROF <- as.data.frame(diversitatROF)
names(diversitatROF) <- "Diversity"
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
names(diversitatTVUF) <- "Diversity"
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
names(diversitatTVUH) <- "Diversity"
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
  left_join(flowerabundance, by = c("Plot","Species")) %>%
  mutate(HB_Visitation_rate = HB_abundance/Flower_Abundance*1000)


datafunction <- left_join(datafunctionality3, Apis2, by = c("Plot","Species","Flower_Abundance")) %>%
  mutate(Wild_Visitation_rate = Visitation_rate - HB_Visitation_rate) #%>%
  # select(., -c(Pollinator_abundance, HB_abundance, Flower_Abundance)) 


