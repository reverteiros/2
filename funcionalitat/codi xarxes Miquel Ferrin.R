
require(devtools)
library(tidyverse)
library(bipartite)

setwd("D:/Usuarios/s.reverte/Downloads")

data <- read.table("DATA MIX C.txt",header=T) 

#### Generar una base de dades on cada interacció de cada parcel·la és una fila. Trec files on no hi ha parasitoid
datafiltered <- data %>%
  mutate(Code = paste(Area,SiteID)) %>%
  group_by(Code, N_Host_Spp,N_parasite_Spp) %>%
  summarize(Interactions=sum(No_parasitized_broocells_sp1)) %>%
  filter(!is.na(N_parasite_Spp))

### Database hostes
Hosts <- group_by(data, Area, SiteID, N_Host_Spp) %>% 
  select(Area,SiteID,N_Host_Spp,No_broodcells)%>%
  summarise(Abundance=sum(No_broodcells)) %>%
  spread(.,N_Host_Spp,Abundance) %>%
  mutate(Code = paste(Area,SiteID)) 
Hosts[is.na(Hosts)] <- 0

### Database parasitoids
Parasitoids <- group_by(data, Area, SiteID, N_parasite_Spp) %>% 
  select(Area,SiteID,N_parasite_Spp,No_parasitized_broocells_sp1)%>%
  summarise(Abundance=sum(No_parasitized_broocells_sp1)) %>%
  filter(!is.na(N_parasite_Spp)) %>%
  spread(.,N_parasite_Spp,Abundance)%>%
  mutate(Code = paste(Area,SiteID)) 
Parasitoids[is.na(Parasitoids)] <- 0

### Vector agrupació per nmds
VectorParceles <- Hosts %>%
  select(Area,Code)


### GARRAF metaxarxa

DataGarraf <- data %>%
  filter(Area == "Garraf") %>%
  select(N_Host_Spp,N_parasite_Spp,No_parasitized_broocells_sp1,Area, SiteID) %>%
  mutate(Code = paste(Area,SiteID)) 

MetawebGarraf<-data.frame(frame2webs(DataGarraf, varnames= c("N_Host_Spp", "N_parasite_Spp", "Area","No_parasitized_broocells_sp1"), emptylist="F"))

plotweb(MetawebGarraf, method="cca")

## per parcel·la

garraf1 <- datafiltered[datafiltered$Code == "Garraf 1",] %>%
  spread(N_parasite_Spp, Interactions) 
garraf1[is.na(garraf1)] <- 0
garraf1 <- as.data.frame(garraf1)
rownames(garraf1) <- garraf1$N_Host_Spp
garraf1 <- garraf1[,-(1:2)]
garraf1 <- as.matrix(garraf1)


.....
...

llista<-list(garraf1,garraf2,garraf3,....)


### OLOT

DataOlot <- data %>%
  filter(Area == "Olot") %>%
  select(N_Host_Spp,N_parasite_Spp,No_parasitized_broocells_sp1) %>%
  mutate(Site = "Olot")

MetawebOlot<-data.frame(frame2webs(DataOlot, varnames= c("N_Host_Spp", "N_parasite_Spp", "Site","No_parasitized_broocells_sp1"), emptylist="F"))

plotweb(MetawebOlot, method="cca")


## per parcel·la (poso olot 1 per posar algo, mira els codis que tenen les parceles)

olot1 <- datafiltered[datafiltered$Code == "Olot 1",] %>%
  spread(N_parasite_Spp, Interactions) 
olot1[is.na(olot1)] <- 0
olot1 <- as.data.frame(olot1)
rownames(olot1) <- olot1$N_Host_Spp
olot1 <- olot1[,-(1:2)]
olot1 <- as.matrix(olot1)


.....
...

llista<-list(olot1,olot2,....)

