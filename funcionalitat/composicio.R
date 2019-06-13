
source("funcionalitat/netejar dades.R")

######### composicio

##  ROF
diversitatROF <- censos %>%
  filter(., Species == "ROF") %>%
  spread(Pollinator, Abundance) 
diversitatROF[is.na(diversitatROF)] <- 0
diversitatROF <- diversitatROF[,-(1:2)]
composicioROF<-bray.part(diversitatROF)

##  TVUF
diversitatTVUF <- censos %>%
  dplyr::filter(., Species == "TVUF") %>%
  spread(Pollinator, Abundance) 
diversitatTVUF[is.na(diversitatTVUF)] <- 0
diversitatTVUF <- diversitatTVUF[,-(1:2)]
composicioTVUF<-bray.part(diversitatTVUF)

##  TVUH
diversitatTVUH <- (dplyr::filter(censos, Species == "TVUH")) %>%
  spread(Pollinator, Abundance)
diversitatTVUH[is.na(diversitatTVUH)] <- 0
diversitatTVUH <- diversitatTVUH[,-(1:2)]
composicioTVUH <-bray.part(diversitatTVUH)


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
