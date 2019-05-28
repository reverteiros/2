
######### composicio

##  ROF
diversitatROF <- censos %>%
  dplyr::filter(., Species == "ROF") %>%
  spread(Pollinator, Abundance) %>%
  InsertRow(., NewRow = numeric(81),RowNum = 29)
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
  spread(Pollinator, Abundance) %>%
  InsertRow(., NewRow = numeric(57),RowNum = 7)%>%
  InsertRow(., NewRow = numeric(57),RowNum = 18)%>%
  InsertRow(., NewRow = numeric(57),RowNum = 23)%>%
  InsertRow(., NewRow = numeric(57),RowNum = 25)
diversitatTVUH[is.na(diversitatTVUH)] <- 0
diversitatTVUH <- diversitatTVUH[,-(1:2)]
composicioTVUH <-bray.part(diversitatTVUH)

Ratio_Heterosp_HomospdistTVUF <- dist(TVUF$Ratio_Heterosp_Homosp)
Ratio_Heterosp_HomospdistTVUH <- dist(TVUH$Ratio_Heterosp_Homosp)
Ratio_Heterosp_HomospdistROF <- dist(ROF$Ratio_Heterosp_Homosp)
Mean_pollenROFdist <- dist(ROF$Mean_pollen)
Mean_pollenTVUFdist <- dist(TVUF$Mean_pollen)
Mean_pollenTVUHdist <- dist(TVUH$Mean_pollen)

mantel(composicioTVUF$bray, Ratio_Heterosp_HomospdistTVUF, method = "pearson", permutations = 999, na.rm = FALSE)
mantel(composicioTVUH$bray, Ratio_Heterosp_HomospdistTVUH, method = "pearson", permutations = 999, na.rm = FALSE)
mantel(composicioROF$bray, Ratio_Heterosp_HomospdistROF, method = "pearson", permutations = 999, na.rm = FALSE)

mantel(composicioROF$bray, Mean_pollenROFdist, method = "pearson", permutations = 999, na.rm = FALSE)
mantel(composicioTVUF$bray, Mean_pollenTVUFdist, method = "pearson", permutations = 999, na.rm = FALSE)
mantel(composicioTVUH$bray, Mean_pollenTVUHdist, method = "pearson", permutations = 999, na.rm = FALSE)



