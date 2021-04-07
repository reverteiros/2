
setwd("C:/Users/saret/Desktop/Clips 4.3")

library(dplyr)
library(tidyr)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggplot2)
library(ggExtra)

####♥
surveys <- read.table("polls3.txt",header=T) %>%
  arrange(Percentage) %>%
  mutate(Pollinator = factor(Pollinator, levels=c("Bees", "Coleopterans", "Ants", "Dipterans", "Lepidopterans", "Wasps")))%>%
  group_by(Pollinator, Group) %>% 
  dplyr::summarize(Percentage_mean=mean(Percentage),Percentage_sd=sd(Percentage),n=n()) %>%
  mutate(Percentage_se=Percentage_sd/sqrt(n))  


ggplot(data = surveys, aes(x=Pollinator,y=Percentage_mean,fill=Pollinator)) + 
  geom_bar( aes(x=Pollinator, y=Percentage_mean), stat="identity")+
  geom_errorbar( aes(x=Pollinator, ymin=Percentage_mean-Percentage_se, ymax=Percentage_mean+Percentage_se), width=0.4, colour="black")+
  facet_grid(. ~ Group)+
  theme_classic()+
  labs( y = "Percentage of visits")+
  theme(legend.position = "none")+ 
  theme(axis.text.x = element_text(angle = 90,hjust=1))


surveys <- read.table("pollssites.txt",header=T) %>%
  arrange(Percentage) %>%
  mutate(Pollinator = factor(Pollinator, levels=c("Bees", "Coleopterans", "Ants", "Dipterans", "Lepidopterans", "Wasps")))%>%
  group_by(Site,Pollinator, Group) %>% 
  dplyr::summarize(Percentage_mean=mean(Percentage),Percentage_sd=sd(Percentage),n=n()) %>%
  mutate(Percentage_se=Percentage_sd/sqrt(n))  


ggplot(data = surveys, aes(x=Pollinator,y=Percentage_mean,fill=Pollinator)) + 
  geom_bar( aes(x=Pollinator, y=Percentage_mean), stat="identity")+
  geom_errorbar( aes(x=Pollinator, ymin=Percentage_mean-Percentage_se, ymax=Percentage_mean+Percentage_se), width=0.4, colour="black")+
  facet_grid(Group ~ Site)+
  theme_classic()+
  labs( y = "Percentage of visits")+
  theme(legend.position = "none")+ 
  theme(axis.text.x = element_text(angle = 90,hjust=1))
