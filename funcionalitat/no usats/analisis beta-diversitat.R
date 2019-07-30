
source("funcionalitat/netejar dades polinitzadors.R")

######### composicio especies

##  ROF
composicioROF <- censos %>%
  filter(., Species == "ROF") %>%
  spread(Pollinator, Abundance) 
composicioROF[is.na(composicioROF)] <- 0
composicioROF <- composicioROF[,-(1:2)]
composicioROF<-bray.part(composicioROF)

##  TVUF
composicioTVUF <- censos %>%
  dplyr::filter(., Species == "TVUF") %>%
  spread(Pollinator, Abundance) 
composicioTVUF[is.na(composicioTVUF)] <- 0
composicioTVUF <- composicioTVUF[,-(1:2)]
composicioTVUF<-bray.part(composicioTVUF)

##  TVUH
composicioTVUH <- (dplyr::filter(censos, Species == "TVUH")) %>%
  spread(Pollinator, Abundance)
composicioTVUH[is.na(composicioTVUH)] <- 0
composicioTVUH <- composicioTVUH[,-(1:2)]
composicioTVUH <-bray.part(composicioTVUH)


ROF <- filter(pollen, Species =="ROF" & Plot != 29) 
TVUF <- filter(pollen, Species =="TVUF")
TVUH <- filter(pollen, Species =="TVUH" & Plot != 7 & Plot != 23 & Plot != 18 & Plot != 25)

#### Mean pollen

Mean_pollenROFdist <- dist(ROF$Mean_pollen)
Mean_pollenTVUFdist <- dist(TVUF$Mean_pollen)
Mean_pollenTVUHdist <- dist(TVUH$Mean_pollen)

mantel(composicioROF$bray, Mean_pollenROFdist, method = "pearson", permutations = 999, na.rm = FALSE)
mantel(composicioTVUF$bray, Mean_pollenTVUFdist, method = "pearson", permutations = 999, na.rm = FALSE)
mantel(composicioTVUH$bray, Mean_pollenTVUHdist, method = "pearson", permutations = 999, na.rm = FALSE)

#### Ratio homosp-heterosp

Ratio_Heterosp_HomospdistTVUF <- dist(TVUF$Ratio_Heterosp_Homosp)
Ratio_Heterosp_HomospdistTVUH <- dist(TVUH$Ratio_Heterosp_Homosp)
Ratio_Heterosp_HomospdistROF <- dist(ROF$Ratio_Heterosp_Homosp)

mantel(composicioROF$bray, Ratio_Heterosp_HomospdistROF, method = "pearson", permutations = 999, na.rm = FALSE)
mantel(composicioTVUF$bray, Ratio_Heterosp_HomospdistTVUF, method = "pearson", permutations = 999, na.rm = FALSE)
mantel(composicioTVUH$bray, Ratio_Heterosp_HomospdistTVUH, method = "pearson", permutations = 999, na.rm = FALSE)






############################  Functional groups

## beta-div FUnctional Groups ROF
taulacomposicioFG_ROF <- grupsfuncionals %>%
  filter(.,Codi_planta =="ROF") %>%
  spread(Functional_group_Rocka, Abundance) 
taulacomposicioFG_ROF[is.na(taulacomposicioFG_ROF)] <- 0
taulacomposicioFG_ROF <- as.data.frame(taulacomposicioFG_ROF)
taulacomposicioFG_ROF <- taulacomposicioFG_ROF[-29,-(1:2)]



## beta-div FUnctional Groups TVUF
taulacomposicioFG_TVUF <- grupsfuncionals %>%
  filter(.,Codi_planta =="TVUF") %>%
  spread(Functional_group_Rocka, Abundance) 
taulacomposicioFG_TVUF[is.na(taulacomposicioFG_TVUF)] <- 0
taulacomposicioFG_TVUF <- as.data.frame(taulacomposicioFG_TVUF)
taulacomposicioFG_TVUF <- taulacomposicioFG_TVUF[,-(1:2)]


## beta-div FUnctional Groups TVUH
taulacomposicioFG_TVUH <- grupsfuncionals %>%
  filter(.,Codi_planta =="TVUH") %>%
  spread(Functional_group_Rocka, Abundance) 
taulacomposicioFG_TVUH[is.na(taulacomposicioFG_TVUH)] <- 0
taulacomposicioFG_TVUH <- as.data.frame(taulacomposicioFG_TVUH)
taulacomposicioFG_TVUH <- taulacomposicioFG_TVUH[-c(7,18,23,25),-(1:2)]
rownames(taulacomposicioFG_TVUH) <- c(1:36)


betadivFG_ROF<-bray.part(taulacomposicioFG_ROF)
betadivFG_TVUF<-bray.part(taulacomposicioFG_TVUF)
betadivFG_TVUH<-bray.part(taulacomposicioFG_TVUH)
betadivFG_TVUH$bray[betadivFG_TVUH$bray == NaN] <- 0


#### mean pollen deposition

mantel(betadivFG_ROF$bray, Mean_pollenROFdist, method = "pearson", permutations = 999, na.rm = FALSE)
mantel(betadivFG_TVUF$bray, Mean_pollenTVUFdist, method = "pearson", permutations = 999, na.rm = FALSE)
mantel(betadivFG_TVUH$bray, Mean_pollenTVUHdist, method = "pearson", permutations = 999, na.rm = FALSE)


#### proportion heterospecific pollen

rofproportionhomosp  <- dist(ROF$Proportion_Homospecific)
tvufproportionhomosp <- dist(TVUF$Proportion_Homospecific)
tvuhproportionhomosp <- dist(TVUH$Proportion_Homospecific)

mantel(betadivFG_ROF$bray, rofproportionhomosp, method = "pearson", permutations = 999, na.rm = FALSE)
mantel(betadivFG_TVUF$bray, tvufproportionhomosp, method = "pearson", permutations = 999, na.rm = FALSE)
mantel(betadivFG_TVUH$bray, tvuhproportionhomosp, method = "pearson", permutations = 999, na.rm = FALSE)



