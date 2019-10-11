require(devtools)
library(tidyverse)

setwd("D:/Usuarios/s.reverte/Downloads")

database <- read.table("db_nidosMN_mod.txt",header=T) 

### Tasa de parasitismo
Parasitoids  <- rowSums(database[,5:35])
database$Parasitoids <- Parasitoids
database$Parasitism_rate <- database$Parasitoids/database$n_cells

### Redes de parasitismo
Metanetwork <- database %>%
  select(.,-c("plot_nest","n_cells"))%>%
  group_by(plot, host) %>%
  summarise_all(funs(sum))%>%
  select(.,-c("Parasitoids","Parasitism_rate"))
