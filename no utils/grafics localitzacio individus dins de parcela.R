
source("funcionalitat/netejar dades plantes.R")
source("funcionalitat/netejar dades fruits i llavors.R")

require(devtools)
library(tidyverse)
library(vegan)
library(sp)
library(ape)

# 
fruitset <- droplevels(dplyr::filter(seedsraw, Total == 4)) %>% 
  mutate(Pollinated = Avorted + Seed) %>% 
  mutate(Fruits = if_else(Seed > 0, 1,0)) %>%
  group_by(Plot, Species, Plant) %>% 
  summarise(Fruit_set=mean(Fruits),samples=n())

hist(fruitset$samples)

seedset <- droplevels(dplyr::filter(seedsraw, Total == 4)) %>% 
  mutate(Pollinated = Avorted + Seed) %>% 
  mutate(Fruits = if_else(Seed > 0, 1,0)) %>%
  filter(.,Fruits==1) %>%
  group_by(Plot, Species, Plant) %>% 
  summarise(Seed_set = mean(Seed))

fitness <- meanpollenperplot %>%
  left_join(fruitset, by=c("Plot","Species","Plant")) %>%
  left_join(seedset, by=c("Plot","Species","Plant"))

coordenades <- read.table("dades/coordenades plantes.txt",header=T) %>%
  left_join(fitness, by=c("Plot","Species","Plant")) %>%
  filter(!is.na(X))%>%
  filter(Plot==25) %>%
  mutate(Species2 = as.factor(Species))


coordenades.sp <- SpatialPointsDataFrame(coords=coordenades[,c("X","Y")],data=coordenades[,c("Species2","Plant","Mean_pollen")])
summary(coordenades.sp)

#### plot points colour per species per plot

spplot(obj=coordenades.sp, zcol=c("Species2"),
       col.regions=c("blue","red","green"), key.space="right",
       scales=list(draw=TRUE), aspect="fill",
       xlab=colnames(coordinates(coordenades.sp))[1],
       ylab=colnames(coordinates(coordenades.sp))[2])

ggplot(coordenades, aes(x = X, y = Y)) + 
  geom_point(aes(color = Species, size = Seed_set)) +
  scale_color_manual(values = c("blue", "red", "green")) +
  theme_bw() +
  theme(legend.position = "top")



