
ROF <- filter(dataanalysis, Species =="ROF" & Plot != 29) 

TVUF <- filter(dataanalysis, Species =="TVUF")

TVUH <- filter(dataanalysis, Species =="TVUH" & Plot != 7)



rofpollen  <- dist(ROF$Mean_pollen)
tvufpollen <- dist(TVUF$Mean_pollen)
tvuhpollen <- dist(TVUH$Mean_pollen)

betadivFG_TVUH$bray[betadivFG_TVUH$bray == NaN] <- 0


mantel(betadivFG_ROF$bray, rofpollen, method = "pearson", permutations = 999, na.rm = FALSE)
mantel(betadivFG_TVUF$bray, tvufpollen, method = "pearson", permutations = 999, na.rm = FALSE)
mantel(betadivFG_TVUH$bray, tvuhpollen, method = "pearson", permutations = 999, na.rm = FALSE)
