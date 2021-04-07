
library(dplyr)


############# Pollen estigmes 
pollenraw<-read.table("dades/polen.txt",header=T) %>%
  filter(.,Species!="ROF")

names(pollenraw) <- c("Plot","Species","Plant","Flower","TVU","ROF","OTHERS","Total")

# recalculate total pollen column
pollenraw <- pollenraw %>%
  select(., -Total) %>%
  mutate(Total = (ROF + TVU + OTHERS))

# remove NAs in the dataset
pollenwtNA <- droplevels(dplyr::filter(pollenraw, !is.na(TVU) & !is.na(ROF)& !is.na(OTHERS)& !is.na(Total)))

# define homospecific and heterospecific pollen per species
pollenclean <- pollenwtNA %>%
  mutate(Conspecific = TVU) %>%
  mutate(Heterospecific = ROF+OTHERS)



proporciomorfs <- read.table("dades/flors quantitatiu separant thymus morfs.txt",header=T) %>%
  select(., TVUF, TVUH) %>%
  mutate(Plot = c(1:40)) %>%
  mutate(TVU = TVUF+TVUH) %>%
  mutate(ProporcioH = TVUH / TVU) %>%
  select(., c(Plot,ProporcioH))

