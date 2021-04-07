


# ############## Seed viability and weight
# 
# seedweightraw <- read.table("dades/pesos llavors.txt",header=T)
# 
# # seed weight
# seedweight <- seedweightraw %>%
#   # filter(Embryo == "viable") %>%
#   group_by(Species,Plot) %>%
#   complete(Species, Plot) %>%
#   distinct() 
# 
# # from the weighted seeds, proportion of them that has viable embryo
# seedweightandviability <- seedweightraw %>%
#   filter(!is.na(Embryo)) %>%
#   mutate(Embryo_Numeric=if_else(Embryo=="morta",0,1)) %>%
#   complete(Species, Plot) %>%
#   distinct() %>%
#   dplyr::left_join(., seedweight, by = c("Species","Plot","Embryo","Individual","Weight"))
# 

############# Fruits and seeds

seedsraw<-read.table("dades/llavors i fruits.txt",header=T)

names(seedsraw) <- c("Plot","Species","Plant","Flower","Avorted","Ovule","Seed","Total")

# 
fruitset <- droplevels(dplyr::filter(seedsraw, Total == 4)) %>% 
  mutate(Pollinated = Avorted + Seed) %>% 
  mutate(Fruits = if_else(Seed > 0, 1,0)) 


seedset <- fruitset %>%
  filter(.,Fruits==1) 
