

require(devtools)
library(tidyverse)
library(DataCombine)
library(vegan)
library(betapart)

### grups funcionals rocka
grupsfuncionals <- read.table("dades/grups funcionals rocka.txt",header=T) %>%
  select(Parcela,Functional_group_Rocka,Codi_planta,Frequencia) %>%
  group_by(Parcela,Functional_group_Rocka,Codi_planta) %>% 
  summarise(Abundance=sum(Frequencia)) %>%
  complete(Parcela,Codi_planta) %>%
  distinct() %>% 
  filter(.,Codi_planta =="ROF" | Codi_planta =="TVUF" | Codi_planta =="TVUH")

## beta-div FUnctional Groups ROF
taulacomposicioFG_ROF <- grupsfuncionals %>%
  filter(.,Codi_planta =="ROF") %>%
  spread(Functional_group_Rocka, Abundance) 
taulacomposicioFG_ROF[is.na(taulacomposicioFG_ROF)] <- 0
taulacomposicioFG_ROF <- as.data.frame(taulacomposicioFG_ROF)
taulacomposicioFG_ROF <- taulacomposicioFG_ROF[-29,-(1:2)]

betadivFG_ROF<-bray.part(taulacomposicioFG_ROF)

## beta-div FUnctional Groups TVUF
taulacomposicioFG_TVUF <- grupsfuncionals %>%
  filter(.,Codi_planta =="TVUF") %>%
  spread(Functional_group_Rocka, Abundance) 
taulacomposicioFG_TVUF[is.na(taulacomposicioFG_TVUF)] <- 0
taulacomposicioFG_TVUF <- as.data.frame(taulacomposicioFG_TVUF)
taulacomposicioFG_TVUF <- taulacomposicioFG_TVUF[,-(1:2)]

betadivFG_TVUF<-bray.part(taulacomposicioFG_TVUF)

## beta-div FUnctional Groups TVUH
taulacomposicioFG_TVUH <- grupsfuncionals %>%
  filter(.,Codi_planta =="TVUH") %>%
  spread(Functional_group_Rocka, Abundance) 
taulacomposicioFG_TVUH[is.na(taulacomposicioFG_TVUH)] <- 0
taulacomposicioFG_TVUH <- as.data.frame(taulacomposicioFG_TVUH)
taulacomposicioFG_TVUH <- taulacomposicioFG_TVUH[-c(7,18,23,25),-(1:2)]
rownames(taulacomposicioFG_TVUH) <- c(1:36)

betadivFG_TVUH<-bray.part(taulacomposicioFG_TVUH)


## nombre grups funcionals
numerogrupsfuncionals <- read.table("dades/grups funcionals rocka.txt",header=T) %>%
  group_by(Parcela,Codi_planta) %>% 
  summarise(Functional_groups=n_distinct(Functional_group_Rocka)) %>%
  complete(Parcela,Codi_planta) %>%
  distinct() %>% 
  filter(.,Codi_planta =="ROF" | Codi_planta =="TVUF" | Codi_planta =="TVUH")
names(numerogrupsfuncionals) <- c("Plot","Species","Functional_group_Rocka")

betadivFG_TVUH$bray[betadivFG_TVUH$bray == NaN] <- 0


ROF <- filter(dataanalysis, Species =="ROF" & Plot != 29) 
TVUF <- filter(dataanalysis, Species =="TVUF")
TVUH <- filter(dataanalysis, Species =="TVUH" & Plot != 7 & Plot != 23 & Plot != 18 & Plot != 25)

#### mean pollen deposition

rofpollen  <- dist(ROF$Mean_pollen)
tvufpollen <- dist(TVUF$Mean_pollen)
tvuhpollen <- dist(TVUH$Mean_pollen)

mantel(betadivFG_ROF$bray, rofpollen, method = "pearson", permutations = 999, na.rm = FALSE)
mantel(betadivFG_TVUF$bray, tvufpollen, method = "pearson", permutations = 999, na.rm = FALSE)
mantel(betadivFG_TVUH$bray, tvuhpollen, method = "pearson", permutations = 999, na.rm = FALSE)


#### proportion heterospecific pollen

rofproportionhomosp  <- dist(ROF$Proportion_Homospecific)
tvufproportionhomosp <- dist(TVUF$Proportion_Homospecific)
tvuhproportionhomosp <- dist(TVUH$Proportion_Homospecific)

mantel(betadivFG_ROF$bray, rofproportionhomosp, method = "pearson", permutations = 999, na.rm = FALSE)
mantel(betadivFG_TVUF$bray, tvufproportionhomosp, method = "pearson", permutations = 999, na.rm = FALSE)
mantel(betadivFG_TVUH$bray, tvuhproportionhomosp, method = "pearson", permutations = 999, na.rm = FALSE)

