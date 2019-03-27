
source("funcionalitat/netejar dades.R")
source("funcionalitat/index xarxes.R")

library(ggplot2)
library(ggExtra)
library(corrplot)
library(Hmisc)
library("PerformanceAnalytics")

grups<-read.table("dades/grups funcionals per planta.txt",header=T) %>%
  spread(Group,Abundance) %>%
  complete(Species, Plot) %>%
  distinct() %>%
  select(-c(Heteroptera,Mecoptera,Honeybees))
grups[is.na(grups)] <- 0


dataanalysis <- dataanalysis %>%
  left_join(grups) %>%
  mutate(Wasp_Visitation_rate = Wasp/Flower_Abundance*1000) %>%
  mutate(Lepidoptera_Visitation_rate = Lepidoptera/Flower_Abundance*1000) %>%
  mutate(Bee_Visitation_rate = Bee/Flower_Abundance*1000) %>%
  mutate(Coleoptera_Visitation_rate = Coleoptera/Flower_Abundance*1000) %>%
  mutate(Diptera_Visitation_rate = Diptera/Flower_Abundance*1000)


ROFnetwork <- filter(dataanalysis, Species =="ROF")%>%
  select(Wasp_Visitation_rate,Lepidoptera_Visitation_rate,HB_Visitation_rate,Bee_Visitation_rate,Coleoptera_Visitation_rate,Diptera_Visitation_rate,Mean_Homospecific,Mean_Heterospecific)
ROFnetwork <- as.data.frame(ROFnetwork)
ROFnetwork <- ROFnetwork[,-1]
chart.Correlation(ROFnetwork, histogram=TRUE, pch=19)


TVUFfunction <- filter(dataanalysis, Species =="TVUF")%>%
  select(Wasp_Visitation_rate,Lepidoptera_Visitation_rate,HB_Visitation_rate,Bee_Visitation_rate,Coleoptera_Visitation_rate,Diptera_Visitation_rate,Mean_Homospecific,Mean_Heterospecific)
TVUFfunction2 <- as.data.frame(TVUFfunction)
TVUFfunction2 <- TVUFfunction2[,-1]
chart.Correlation(TVUFfunction2, histogram=TRUE, pch=19)


TVUHfunction <- filter(dataanalysis, Species =="TVUH")%>%
  select(Wasp_Visitation_rate,Lepidoptera_Visitation_rate,HB_Visitation_rate,Bee_Visitation_rate,Coleoptera_Visitation_rate,Diptera_Visitation_rate,Mean_Homospecific,Mean_Heterospecific)
TVUHfunction <- as.data.frame(TVUHfunction)
TVUHfunction <- TVUHfunction[,-1]
chart.Correlation(TVUHfunction, histogram=TRUE, pch=19)
