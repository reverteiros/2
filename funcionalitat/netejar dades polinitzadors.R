
require(devtools)
library(tidyverse)
library(DataCombine)
library(vegan)
library(betapart)
source("funcionalitat/index xarxes.R")


censos <- read.table("dades/censos.txt",header=T)
names(censos) <- c("Plot","Pollinator","Species","Abundance")


############# flower abundance per plot
flors <- read.table("dades/flors quantitatiu separant thymus morfs.txt",header=T) %>%
  select(., ROF, TVUF, TVUH)%>%
  tidyr::gather(Species, "Flower_Abundance",1:3) 

flors$Plot = c(1:40)

# Pollinators 
pollinators <- droplevels(dplyr::filter(censos, Species == "ROF" | Species == "TVUF" | Species == "TVUH")) %>% 
  group_by(Plot, Species) %>% 
  summarise(Pollinator_abundance=sum(Abundance),Pollinator_richness=n_distinct(Pollinator))%>%
  complete(Species, Plot) %>%
  distinct() %>%
  left_join(flors, by = c("Plot","Species")) %>%
  mutate(Visitation_rate = Pollinator_abundance/Flower_Abundance*1000)

###### join all datasets
datapollinatorsall <- pollinators %>%
  left_join(networkmetrics, by="Plot") %>%
  select(.,-Shannon_diversity)


# ## diversitat ROF
# diversitatROF <- censos %>%
#   dplyr::filter(., Species == "ROF") %>%
#   spread(Pollinator, Abundance) %>%
#   InsertRow(., NewRow = numeric(81),RowNum = 29)
# diversitatROF[is.na(diversitatROF)] <- 0
# diversitatROF <- diversitatROF[,-(1:2)]
# diversitatROF <- diversity(diversitatROF, "shannon")
# diversitatROF <- as.data.frame(diversitatROF)
# names(diversitatROF) <- "Shannon_Diversity"
# diversitatROF$Plot <- c(1:40)
# diversitatROF$Species <- "ROF"
# 
# ## diversitat TVUF
# diversitatTVUF <- censos %>%
#   dplyr::filter(., Species == "TVUF") %>%
#   spread(Pollinator, Abundance) 
# diversitatTVUF[is.na(diversitatTVUF)] <- 0
# diversitatTVUF <- diversitatTVUF[,-(1:2)]
# diversitatTVUF <- diversity(diversitatTVUF, "shannon")
# diversitatTVUF <- as.data.frame(diversitatTVUF)
# names(diversitatTVUF) <- "Shannon_Diversity"
# diversitatTVUF$Plot <- c(1:40)
# diversitatTVUF$Species <- "TVUF"
# 
# ## diversitat TVUH
# diversitatTVUH <- (dplyr::filter(censos, Species == "TVUH")) %>%
#   spread(Pollinator, Abundance) %>%
#   InsertRow(., NewRow = numeric(57),RowNum = 7)%>%
#   InsertRow(., NewRow = numeric(57),RowNum = 18)%>%
#   InsertRow(., NewRow = numeric(57),RowNum = 23)%>%
#   InsertRow(., NewRow = numeric(57),RowNum = 25)
# diversitatTVUH[is.na(diversitatTVUH)] <- 0
# diversitatTVUH <- diversitatTVUH[,-(1:2)]
# diversitatTVUH <- diversity(diversitatTVUH, "shannon")
# diversitatTVUH <- as.data.frame(diversitatTVUH)
# names(diversitatTVUH) <- "Shannon_Diversity"
# diversitatTVUH$Plot <- c(1:40)
# diversitatTVUH$Species <- "TVUH"
# 
# 
# diversity <- diversitatTVUH %>%
#   bind_rows(.,diversitatTVUF) %>%
#   bind_rows(.,diversitatROF)

# datapollinators <- left_join(pollinators,diversity, by = c("Plot","Species")) 



# # Honeybees
# Apis <- filter(censos, Pollinator =="Apis")
# 
# Apis2 <- droplevels(dplyr::filter(Apis, Species == "ROF" | Species == "TVUF" | Species == "TVUH")) %>% 
#   group_by(Plot, Species) %>% 
#   summarise(HB_abundance=sum(Abundance))%>%
#   complete(Species, Plot) %>%
#   distinct() %>%
#   left_join(flors, by = c("Plot","Species")) 
# Apis2[is.na(Apis2)] <- 0
# 
# # wild pollinators
# datapollinators2 <- left_join(datapollinators, Apis2, by = c("Plot","Species","Flower_Abundance")) %>%
#   mutate(Wild_abundance = Pollinator_abundance - HB_abundance) %>%
#   mutate(HB_Visitation_rate = HB_abundance/Flower_Abundance*1000) %>%
#   mutate(Wild_Visitation_rate = Wild_abundance/Flower_Abundance*1000) %>%
#   mutate(Proportion_Wild = Wild_Visitation_rate/Visitation_rate) %>%
#   mutate(Proportion_HB = HB_Visitation_rate/Visitation_rate)



#################################general pollinators dades
# # general abundance and richness in the plot
# generalpollinators <- censos %>%
#   group_by(Plot) %>% 
#   summarise(Pollinator_abundance=sum(Abundance),Pollinator_richness=n_distinct(Pollinator))

