
require(devtools)
library(tidyverse)

a<-read.table("dades/Dades censos2.txt", header=T)
altura <- read.table("dades/Databasealt.txt",header=T)

hist(altura$Altura)

filtered <- dplyr::select(a, Parcela, Solitary, Group, Frequencia)

wild <- filtered %>% filter(., Solitary == "Solitary")

group <- group_by(wild, Parcela, Group) %>% 
  summarise(Abundance=sum(Frequencia)) %>%
  complete(Group, Parcela) %>%
  distinct()  %>%
  mutate(Abundance0=if_else((is.na(Abundance)),0, Abundance))

group[is.na(group)] <- 0


filtered2 <- group %>% filter(., Parcela == 40)

ggplot(data=filtered2, aes(x=Group,y=Abundance,fill=Group)) +
  geom_bar(stat="identity") +
  ylim(c(0,20))+
  theme(axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position = "none") +
  scale_fill_manual(values=c("grey11","darkolivegreen4","darkmagenta","green4","orangered3","navyblue","indianred4","yellow4","thistle4"))


filtered2 <- group %>% filter(., Group == "Wasp")
plot(filtered2$Abundance~altura$Altura,main="Wasp")


