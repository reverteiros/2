
library(bipartite)
require(devtools)
library(tidyverse)
library(DataCombine)
library(vegan)

a<-read.table("dades/censos.txt",header=T)

flors <- read.table("dades/flors quantitatiu separant thymus morfs.txt",header=T) %>%
  gather(.,"Codi_planta","Abundance")
flors$Plot = c(1:40)



plot1 <- a[a$Parcela == 1,]%>%
  spread(Nom_definitiu, Frequencia) 
flors1 <- flors[flors$Plot == 1,] %>%
  select(-Plot)
plot1 <- plot1 %>% left_join(flors1,by="Codi_planta")
plot1[is.na(plot1)] <- 0
plot1 <- as.data.frame(plot1)
rownames(plot1) <- plot1$Codi_planta
plot1 <- plot1[,-(1:2)]
plot1 <- plot1*1000/plot1$Abundance
plot1 <- as.matrix(plot1) 
w1 <- plot1[,-(ncol(plot1))]
w1TVU <- w1[c("TVUF", "TVUH"),] 
i <- (colSums(w1TVU, na.rm=T) != 0)
w1TVUnonzero <- w1TVU[, i] 
subweb1 <- w1[, colnames(w1TVUnonzero)]


plot2 <- a[a$Parcela == 2,]%>%
  spread(Nom_definitiu, Frequencia) 
flors2 <- flors[flors$Plot == 2,] %>%
  select(-Plot)
plot2 <- plot2 %>% left_join(flors2,by="Codi_planta")
plot2[is.na(plot2)] <- 0
plot2 <- as.data.frame(plot2)
rownames(plot2) <- plot2$Codi_planta
plot2 <- plot2[,-(1:2)]
plot2 <- plot2*1000/plot2$Abundance
plot2 <- as.matrix(plot2) 
w2 <- plot2[,-(ncol(plot2))]
w2TVU <- w2[c("TVUF", "TVUH"),] 
i <- (colSums(w2TVU, na.rm=T) != 0)
w2TVUnonzero <- w2TVU[, i] 
subweb2 <- w2[, colnames(w2TVUnonzero)]


plot3 <- a[a$Parcela == 3,]%>%
  spread(Nom_definitiu, Frequencia) 
flors3 <- flors[flors$Plot == 3,] %>%
  select(-Plot)
plot3 <- plot3 %>% left_join(flors3,by="Codi_planta")
plot3[is.na(plot3)] <- 0
plot3 <- as.data.frame(plot3)
rownames(plot3) <- plot3$Codi_planta
plot3 <- plot3[,-(1:2)]
plot3 <- plot3*1000/plot3$Abundance
plot3 <- as.matrix(plot3) 
w3 <- plot3[,-(ncol(plot3))]
w3TVU <- w3[c("TVUF", "TVUH"),] 
i <- (colSums(w3TVU, na.rm=T) != 0)
w3TVUnonzero <- w3TVU[, i] 
subweb3 <- w3[, colnames(w3TVUnonzero)]


plot4 <- a[a$Parcela == 4,]%>%
  spread(Nom_definitiu, Frequencia) 
flors4 <- flors[flors$Plot == 4,] %>%
  select(-Plot)
plot4 <- plot4 %>% left_join(flors4,by="Codi_planta")
plot4[is.na(plot4)] <- 0
plot4 <- as.data.frame(plot4)
rownames(plot4) <- plot4$Codi_planta
plot4 <- plot4[,-(1:2)]
plot4 <- plot4*1000/plot4$Abundance
plot4 <- as.matrix(plot4) 
w4 <- plot4[,-(ncol(plot4))]
w4TVU <- w4[c("TVUF", "TVUH"),] 
i <- (colSums(w4TVU, na.rm=T) != 0)
w4TVUnonzero <- w4TVU[, i] 
subweb4 <- w4[, colnames(w4TVUnonzero)]


plot5 <- a[a$Parcela == 5,]%>%
  spread(Nom_definitiu, Frequencia) 
flors5 <- flors[flors$Plot == 5,] %>%
  select(-Plot)
plot5 <- plot5 %>% left_join(flors5,by="Codi_planta")
plot5[is.na(plot5)] <- 0
plot5 <- as.data.frame(plot5)
rownames(plot5) <- plot5$Codi_planta
plot5 <- plot5[,-(1:2)]
plot5 <- plot5*1000/plot5$Abundance
plot5 <- as.matrix(plot5) 
w5 <- plot5[,-(ncol(plot5))]
w5TVU <- w5[c("TVUF", "TVUH"),] 
i <- (colSums(w5TVU, na.rm=T) != 0)
w5TVUnonzero <- w5TVU[, i] 
subweb5 <- w5[, colnames(w5TVUnonzero)]


plot6 <- a[a$Parcela == 6,]%>%
  spread(Nom_definitiu, Frequencia) 
flors6 <- flors[flors$Plot == 6,] %>%
  select(-Plot)
plot6 <- plot6 %>% left_join(flors6,by="Codi_planta")
plot6[is.na(plot6)] <- 0
plot6 <- as.data.frame(plot6)
rownames(plot6) <- plot6$Codi_planta
plot6 <- plot6[,-(1:2)]
plot6 <- plot6*1000/plot6$Abundance
plot6 <- as.matrix(plot6) 
w6 <- plot6[,-(ncol(plot6))]
w6TVU <- w6[c("TVUF", "TVUH"),] 
i <- (colSums(w6TVU, na.rm=T) != 0)
w6TVUnonzero <- w6TVU[, i] 
subweb6 <- w6[, colnames(w6TVUnonzero)]


plot7 <- a[a$Parcela == 7,]%>%
  spread(Nom_definitiu, Frequencia) 
flors7 <- flors[flors$Plot == 7,] %>%
  select(-Plot)
plot7 <- plot7 %>% left_join(flors7,by="Codi_planta")
plot7[is.na(plot7)] <- 0
plot7 <- as.data.frame(plot7)
rownames(plot7) <- plot7$Codi_planta
plot7 <- plot7[,-(1:2)]
plot7 <- plot7*1000/plot7$Abundance
plot7 <- as.matrix(plot7) 
w7 <- plot7[,-(ncol(plot7))]
w7TVU <- w7["TVUF",] 
subweb7 <- w7[, c("Adela_aldrovandella","Apis","Calliphoridae_3","Empididae","Hylaeus_garrulus","Tropinota_squalida")]


plot8 <- a[a$Parcela == 8,]%>%
  spread(Nom_definitiu, Frequencia) 
flors8 <- flors[flors$Plot == 8,] %>%
  select(-Plot)
plot8 <- plot8 %>% left_join(flors8,by="Codi_planta")
plot8[is.na(plot8)] <- 0
plot8 <- as.data.frame(plot8)
rownames(plot8) <- plot8$Codi_planta
plot8 <- plot8[,-(1:2)]
plot8 <- plot8*1000/plot8$Abundance
plot8 <- as.matrix(plot8) 
w8 <- plot8[,-(ncol(plot8))]
w8TVU <- w8[c("TVUF", "TVUH"),] 
i <- (colSums(w8TVU, na.rm=T) != 0)
w8TVUnonzero <- w8TVU[, i] 
subweb8 <- w8[, colnames(w8TVUnonzero)]


plot9 <- a[a$Parcela == 9,]%>%
  spread(Nom_definitiu, Frequencia) 
flors9 <- flors[flors$Plot == 9,] %>%
  select(-Plot)
plot9 <- plot9 %>% left_join(flors9,by="Codi_planta")
plot9[is.na(plot9)] <- 0
plot9 <- as.data.frame(plot9)
rownames(plot9) <- plot9$Codi_planta
plot9 <- plot9[,-(1:2)]
plot9 <- plot9*1000/plot9$Abundance
plot9 <- as.matrix(plot9) 
w9 <- plot9[,-(ncol(plot9))]
w9TVU <- w9[c("TVUF", "TVUH"),] 
i <- (colSums(w9TVU, na.rm=T) != 0)
w9TVUnonzero <- w9TVU[, i] 
subweb9 <- w9[, colnames(w9TVUnonzero)]


plot10 <- a[a$Parcela == 10,]%>%
  spread(Nom_definitiu, Frequencia) 
flors10 <- flors[flors$Plot == 10,] %>%
  select(-Plot)
plot10 <- plot10 %>% left_join(flors10,by="Codi_planta")
plot10[is.na(plot10)] <- 0
plot10 <- as.data.frame(plot10)
rownames(plot10) <- plot10$Codi_planta
plot10 <- plot10[,-(1:2)]
plot10 <- plot10*1000/plot10$Abundance
plot10 <- as.matrix(plot10) 
w10 <- plot10[,-(ncol(plot10))]
w10TVU <- w10[c("TVUF", "TVUH"),] 
i <- (colSums(w10TVU, na.rm=T) != 0)
w10TVUnonzero <- w10TVU[, i] 
subweb10 <- as.data.frame(w10[, c("Andrena_djelfensis")])
names(subweb10) <- c("Andrena_djelfensis")


plot11 <- a[a$Parcela == 11,]%>%
  spread(Nom_definitiu, Frequencia) 
flors11 <- flors[flors$Plot == 11,] %>%
  select(-Plot)
plot11 <- plot11 %>% left_join(flors11,by="Codi_planta")
plot11[is.na(plot11)] <- 0
plot11 <- as.data.frame(plot11)
rownames(plot11) <- plot11$Codi_planta
plot11 <- plot11[,-(1:2)]
plot11 <- plot11*1000/plot11$Abundance
plot11 <- as.matrix(plot11) 
w11 <- plot11[,-(ncol(plot11))]
w11TVU <- w11[c("TVUF", "TVUH"),] 
i <- (colSums(w11TVU, na.rm=T) != 0)
w11TVUnonzero <- w11TVU[, i] 
subweb11 <- w11[, colnames(w11TVUnonzero)]


plot12 <- a[a$Parcela == 12,]%>%
  spread(Nom_definitiu, Frequencia) 
flors12 <- flors[flors$Plot == 12,] %>%
  select(-Plot)
plot12 <- plot12 %>% left_join(flors12,by="Codi_planta")
plot12[is.na(plot12)] <- 0
plot12 <- as.data.frame(plot12)
rownames(plot12) <- plot12$Codi_planta
plot12 <- plot12[,-(1:2)]
plot12 <- plot12*1000/plot12$Abundance
plot12 <- as.matrix(plot12) 
w12 <- plot12[,-(ncol(plot12))]
w12TVU <- w12[c("TVUF", "TVUH"),] 
i <- (colSums(w12TVU, na.rm=T) != 0)
w12TVUnonzero <- w12TVU[, i] 
subweb12 <- w12[, colnames(w12TVUnonzero)]


plot13 <- a[a$Parcela == 13,]%>%
  spread(Nom_definitiu, Frequencia) 
flors13 <- flors[flors$Plot == 13,] %>%
  select(-Plot)
plot13 <- plot13 %>% left_join(flors13,by="Codi_planta")
plot13[is.na(plot13)] <- 0
plot13 <- as.data.frame(plot13)
rownames(plot13) <- plot13$Codi_planta
plot13 <- plot13[,-(1:2)]
plot13 <- plot13*1000/plot13$Abundance
plot13 <- as.matrix(plot13) 
w13 <- plot13[,-(ncol(plot13))]
w13TVU <- w13[c("TVUF", "TVUH"),] 
i <- (colSums(w13TVU, na.rm=T) != 0)
w13TVUnonzero <- w13TVU[, i] 
subweb13 <- w13[, colnames(w13TVUnonzero)]


plot14 <- a[a$Parcela == 14,]%>%
  spread(Nom_definitiu, Frequencia) 
flors14 <- flors[flors$Plot == 14,] %>%
  select(-Plot)
plot14 <- plot14 %>% left_join(flors14,by="Codi_planta")
plot14[is.na(plot14)] <- 0
plot14 <- as.data.frame(plot14)
rownames(plot14) <- plot14$Codi_planta
plot14 <- plot14[,-(1:2)]
plot14 <- plot14*1000/plot14$Abundance
plot14 <- as.matrix(plot14) 
w14 <- plot14[,-(ncol(plot14))]
w14TVU <- w14[c("TVUF", "TVUH"),] 
i <- (colSums(w14TVU, na.rm=T) != 0)
w14TVUnonzero <- w14TVU[, i] 
subweb14 <- w14[, colnames(w14TVUnonzero)]


plot15 <- a[a$Parcela == 15,]%>%
  spread(Nom_definitiu, Frequencia) 
flors15 <- flors[flors$Plot == 15,] %>%
  select(-Plot)
plot15 <- plot15 %>% left_join(flors15,by="Codi_planta")
plot15[is.na(plot15)] <- 0
plot15 <- as.data.frame(plot15)
rownames(plot15) <- plot15$Codi_planta
plot15 <- plot15[,-(1:2)]
plot15 <- plot15*1000/plot15$Abundance
plot15 <- as.matrix(plot15) 
w15 <- plot15[,-(ncol(plot15))]
w15TVU <- w15[c("TVUF", "TVUH"),] 
i <- (colSums(w15TVU, na.rm=T) != 0)
w15TVUnonzero <- w15TVU[, i] 
subweb15 <- w15[, colnames(w15TVUnonzero)]


plot16 <- a[a$Parcela == 16,]%>%
  spread(Nom_definitiu, Frequencia) 
flors16 <- flors[flors$Plot == 16,] %>%
  select(-Plot)
plot16 <- plot16 %>% left_join(flors16,by="Codi_planta")
plot16[is.na(plot16)] <- 0
plot16 <- as.data.frame(plot16)
rownames(plot16) <- plot16$Codi_planta
plot16 <- plot16[,-(1:2)]
plot16 <- plot16*1000/plot16$Abundance
plot16 <- as.matrix(plot16) 
w16 <- plot16[,-(ncol(plot16))]
w16TVU <- w16[c("TVUF", "TVUH"),] 
i <- (colSums(w16TVU, na.rm=T) != 0)
w16TVUnonzero <- w16TVU[, i] 
subweb16 <- w16[, colnames(w16TVUnonzero)]


plot17 <- a[a$Parcela == 17,]%>%
  spread(Nom_definitiu, Frequencia) 
flors17 <- flors[flors$Plot == 17,] %>%
  select(-Plot)
plot17 <- plot17 %>% left_join(flors17,by="Codi_planta")
plot17[is.na(plot17)] <- 0
plot17 <- as.data.frame(plot17)
rownames(plot17) <- plot17$Codi_planta
plot17 <- plot17[,-(1:2)]
plot17 <- plot17*1000/plot17$Abundance
plot17 <- as.matrix(plot17) 
w17 <- plot17[,-(ncol(plot17))]
w17TVU <- w17[c("TVUF", "TVUH"),] 
i <- (colSums(w17TVU, na.rm=T) != 0)
w17TVUnonzero <- w17TVU[, i] 
subweb17 <- w17[, colnames(w17TVUnonzero)]


plot18 <- a[a$Parcela == 18,]%>%
  spread(Nom_definitiu, Frequencia) 
flors18 <- flors[flors$Plot == 18,] %>%
  select(-Plot)
plot18 <- plot18 %>% left_join(flors18,by="Codi_planta")
plot18[is.na(plot18)] <- 0
plot18 <- as.data.frame(plot18)
rownames(plot18) <- plot18$Codi_planta
plot18 <- plot18[,-(1:2)]
plot18 <- plot18*1000/plot18$Abundance
plot18 <- as.matrix(plot18) 
w18 <- plot18[,-(ncol(plot18))]
w18TVU <- w18[("TVUF"),] 
subweb18 <- w18[, c("Braconidae","Euodynerus_bidentoides","Gasteruption_1","Hylaeus_garrulus","Hylaeus_hyalinatus","Hylaeus_sp_1")]


plot19 <- a[a$Parcela == 19,]%>%
  spread(Nom_definitiu, Frequencia) 
flors19 <- flors[flors$Plot == 19,] %>%
  select(-Plot)
plot19 <- plot19 %>% left_join(flors19,by="Codi_planta")
plot19[is.na(plot19)] <- 0
plot19 <- as.data.frame(plot19)
rownames(plot19) <- plot19$Codi_planta
plot19 <- plot19[,-(1:2)]
plot19 <- plot19*1000/plot19$Abundance
plot19 <- as.matrix(plot19) 
w19 <- plot19[,-(ncol(plot19))]
w19TVU <- w19[c("TVUF", "TVUH"),] 
i <- (colSums(w19TVU, na.rm=T) != 0)
w19TVUnonzero <- w19TVU[, i] 
subweb19 <- w19[, colnames(w19TVUnonzero)]


plot20 <- a[a$Parcela == 20,]%>%
  spread(Nom_definitiu, Frequencia) 
flors20 <- flors[flors$Plot == 20,] %>%
  select(-Plot)
plot20 <- plot20 %>% left_join(flors20,by="Codi_planta")
plot20[is.na(plot20)] <- 0
plot20 <- as.data.frame(plot20)
rownames(plot20) <- plot20$Codi_planta
plot20 <- plot20[,-(1:2)]
plot20 <- plot20*1000/plot20$Abundance
plot20 <- as.matrix(plot20) 
w20 <- plot20[,-(ncol(plot20))]
w20TVU <- w20[c("TVUF", "TVUH"),] 
i <- (colSums(w20TVU, na.rm=T) != 0)
w20TVUnonzero <- w20TVU[, i] 
subweb20 <- w20[, colnames(w20TVUnonzero)]


plot21 <- a[a$Parcela == 21,]%>%
  spread(Nom_definitiu, Frequencia) 
flors21 <- flors[flors$Plot == 21,] %>%
  select(-Plot)
plot21 <- plot21 %>% left_join(flors21,by="Codi_planta")
plot21[is.na(plot21)] <- 0
plot21 <- as.data.frame(plot21)
rownames(plot21) <- plot21$Codi_planta
plot21 <- plot21[,-(1:2)]
plot21 <- plot21*1000/plot21$Abundance
plot21 <- as.matrix(plot21) 
w21 <- plot21[,-(ncol(plot21))]
w21TVU <- w21[c("TVUF", "TVUH"),] 
i <- (colSums(w21TVU, na.rm=T) != 0)
w21TVUnonzero <- w21TVU[, i] 
subweb21 <- w21[, colnames(w21TVUnonzero)]


plot22 <- a[a$Parcela == 22,]%>%
  spread(Nom_definitiu, Frequencia) 
flors22 <- flors[flors$Plot == 22,] %>%
  select(-Plot)
plot22 <- plot22 %>% left_join(flors22,by="Codi_planta")
plot22[is.na(plot22)] <- 0
plot22 <- as.data.frame(plot22)
rownames(plot22) <- plot22$Codi_planta
plot22 <- plot22[,-(1:2)]
plot22 <- plot22*1000/plot22$Abundance
plot22 <- as.matrix(plot22) 
w22 <- plot22[,-(ncol(plot22))]
w22TVU <- w22[c("TVUF", "TVUH"),] 
i <- (colSums(w22TVU, na.rm=T) != 0)
w22TVUnonzero <- w22TVU[, i] 
subweb22 <- w22[, colnames(w22TVUnonzero)]


plot23 <- a[a$Parcela == 23,]%>%
  spread(Nom_definitiu, Frequencia) 
flors23 <- flors[flors$Plot == 23,] %>%
  select(-Plot)
plot23 <- plot23 %>% left_join(flors23,by="Codi_planta")
plot23[is.na(plot23)] <- 0
plot23 <- as.data.frame(plot23)
rownames(plot23) <- plot23$Codi_planta
plot23 <- plot23[,-(1:2)]
plot23 <- plot23*1000/plot23$Abundance
plot23 <- as.matrix(plot23) 
w23 <- plot23[,-(ncol(plot23))]
w23TVU <- w23[("TVUF"),] 
subweb23 <- w23[, c("Apis","Empididae","Lasioglossum_transitorium_planulum","Pseudophilotes_panoptes")]


plot24 <- a[a$Parcela == 24,]%>%
  spread(Nom_definitiu, Frequencia) 
flors24 <- flors[flors$Plot == 24,] %>%
  select(-Plot)
plot24 <- plot24 %>% left_join(flors24,by="Codi_planta")
plot24[is.na(plot24)] <- 0
plot24 <- as.data.frame(plot24)
rownames(plot24) <- plot24$Codi_planta
plot24 <- plot24[,-(1:2)]
plot24 <- plot24*1000/plot24$Abundance
plot24 <- as.matrix(plot24) 
w24 <- plot24[,-(ncol(plot24))]
w24TVU <- w24[c("TVUF", "TVUH"),] 
i <- (colSums(w24TVU, na.rm=T) != 0)
w24TVUnonzero <- w24TVU[, i] 
subweb24 <- w24[, colnames(w24TVUnonzero)]


plot25 <- a[a$Parcela == 25,]%>%
  spread(Nom_definitiu, Frequencia) 
flors25 <- flors[flors$Plot == 25,] %>%
  select(-Plot)
plot25 <- plot25 %>% left_join(flors25,by="Codi_planta")
plot25[is.na(plot25)] <- 0
plot25 <- as.data.frame(plot25)
rownames(plot25) <- plot25$Codi_planta
plot25 <- plot25[,-(1:2)]
plot25 <- plot25*1000/plot25$Abundance
plot25 <- as.matrix(plot25) 
w25 <- plot25[,-(ncol(plot25))]
w25TVU <- w25[("TVUF"),] 
subweb25 <- as.data.frame(w25[, c("Apis")])
names(subweb25) <- c("Apis")

plot26 <- a[a$Parcela == 26,]%>%
  spread(Nom_definitiu, Frequencia) 
flors26 <- flors[flors$Plot == 26,] %>%
  select(-Plot)
plot26 <- plot26 %>% left_join(flors26,by="Codi_planta")
plot26[is.na(plot26)] <- 0
plot26 <- as.data.frame(plot26)
rownames(plot26) <- plot26$Codi_planta
plot26 <- plot26[,-(1:2)]
plot26 <- plot26*1000/plot26$Abundance
plot26 <- as.matrix(plot26) 
w26 <- plot26[,-(ncol(plot26))]
w26TVU <- w26[c("TVUF", "TVUH"),] 
i <- (colSums(w26TVU, na.rm=T) != 0)
w26TVUnonzero <- w26TVU[, i] 
subweb26 <- w26[, colnames(w26TVUnonzero)]


plot27 <- a[a$Parcela == 27,]%>%
  spread(Nom_definitiu, Frequencia) 
flors27 <- flors[flors$Plot == 27,] %>%
  select(-Plot)
plot27 <- plot27 %>% left_join(flors27,by="Codi_planta")
plot27[is.na(plot27)] <- 0
plot27 <- as.data.frame(plot27)
rownames(plot27) <- plot27$Codi_planta
plot27 <- plot27[,-(1:2)]
plot27 <- plot27*1000/plot27$Abundance
plot27 <- as.matrix(plot27) 
w27 <- plot27[,-(ncol(plot27))]
w27TVU <- w27[c("TVUF", "TVUH"),] 
i <- (colSums(w27TVU, na.rm=T) != 0)
w27TVUnonzero <- w27TVU[, i] 
subweb27 <- w27[, colnames(w27TVUnonzero)]


plot28 <- a[a$Parcela == 28,]%>%
  spread(Nom_definitiu, Frequencia) 
flors28 <- flors[flors$Plot == 28,] %>%
  select(-Plot)
plot28 <- plot28 %>% left_join(flors28,by="Codi_planta")
plot28[is.na(plot28)] <- 0
plot28 <- as.data.frame(plot28)
rownames(plot28) <- plot28$Codi_planta
plot28 <- plot28[,-(1:2)]
plot28 <- plot28*1000/plot28$Abundance
plot28 <- as.matrix(plot28) 
w28 <- plot28[,-(ncol(plot28))]
w28TVU <- w28[c("TVUF", "TVUH"),] 
i <- (colSums(w28TVU, na.rm=T) != 0)
w28TVUnonzero <- w28TVU[, i] 
subweb28 <- w28[, colnames(w28TVUnonzero)]


plot29 <- a[a$Parcela == 29,]%>%
  spread(Nom_definitiu, Frequencia) 
flors29 <- flors[flors$Plot == 29,] %>%
  select(-Plot)
plot29 <- plot29 %>% left_join(flors29,by="Codi_planta")
plot29[is.na(plot29)] <- 0
plot29 <- as.data.frame(plot29)
rownames(plot29) <- plot29$Codi_planta
plot29 <- plot29[,-(1:2)]
plot29 <- plot29*1000/plot29$Abundance
plot29 <- as.matrix(plot29) 
w29 <- plot29[,-(ncol(plot29))]
w29TVU <- w29[c("TVUF", "TVUH"),] 
i <- (colSums(w29TVU, na.rm=T) != 0)
w29TVUnonzero <- w29TVU[, i] 
subweb29 <- w29[, colnames(w29TVUnonzero)]


plot30 <- a[a$Parcela == 30,]%>%
  spread(Nom_definitiu, Frequencia) 
flors30 <- flors[flors$Plot == 30,] %>%
  select(-Plot)
plot30 <- plot30 %>% left_join(flors30,by="Codi_planta")
plot30[is.na(plot30)] <- 0
plot30 <- as.data.frame(plot30)
rownames(plot30) <- plot30$Codi_planta
plot30 <- plot30[,-(1:2)]
plot30 <- plot30*1000/plot30$Abundance
plot30 <- as.matrix(plot30) 
w30 <- plot30[,-(ncol(plot30))]
w30TVU <- w30[c("TVUF", "TVUH"),] 
i <- (colSums(w30TVU, na.rm=T) != 0)
w30TVUnonzero <- w30TVU[, i] 
subweb30 <- w30[, colnames(w30TVUnonzero)]


plot31 <- a[a$Parcela == 31,]%>%
  spread(Nom_definitiu, Frequencia) 
flors31 <- flors[flors$Plot == 31,] %>%
  select(-Plot)
plot31 <- plot31 %>% left_join(flors31,by="Codi_planta")
plot31[is.na(plot31)] <- 0
plot31 <- as.data.frame(plot31)
rownames(plot31) <- plot31$Codi_planta
plot31 <- plot31[,-(1:2)]
plot31 <- plot31*1000/plot31$Abundance
plot31 <- as.matrix(plot31) 
w31 <- plot31[,-(ncol(plot31))]
w31TVU <- w31[c("TVUF", "TVUH"),] 
i <- (colSums(w31TVU, na.rm=T) != 0)
w31TVUnonzero <- w31TVU[, i] 
subweb31 <- w31[, colnames(w31TVUnonzero)]


plot32 <- a[a$Parcela == 32,]%>%
  spread(Nom_definitiu, Frequencia) 
flors32 <- flors[flors$Plot == 32,] %>%
  select(-Plot)
plot32 <- plot32 %>% left_join(flors32,by="Codi_planta")
plot32[is.na(plot32)] <- 0
plot32 <- as.data.frame(plot32)
rownames(plot32) <- plot32$Codi_planta
plot32 <- plot32[,-(1:2)]
plot32 <- plot32*1000/plot32$Abundance
plot32 <- as.matrix(plot32) 
w32 <- plot32[,-(ncol(plot32))]
w32TVU <- w32[c("TVUF", "TVUH"),] 
i <- (colSums(w32TVU, na.rm=T) != 0)
w32TVUnonzero <- w32TVU[, i] 
subweb32 <- w32[, colnames(w32TVUnonzero)]


plot33 <- a[a$Parcela == 33,]%>%
  spread(Nom_definitiu, Frequencia) 
flors33 <- flors[flors$Plot == 33,] %>%
  select(-Plot)
plot33 <- plot33 %>% left_join(flors33,by="Codi_planta")
plot33[is.na(plot33)] <- 0
plot33 <- as.data.frame(plot33)
rownames(plot33) <- plot33$Codi_planta
plot33 <- plot33[,-(1:2)]
plot33 <- plot33*1000/plot33$Abundance
plot33 <- as.matrix(plot33) 
w33 <- plot33[,-(ncol(plot33))]
w33TVU <- w33[c("TVUF", "TVUH"),] 
i <- (colSums(w33TVU, na.rm=T) != 0)
w33TVUnonzero <- w33TVU[, i] 
subweb33 <- w33[, colnames(w33TVUnonzero)]


plot34 <- a[a$Parcela == 34,]%>%
  spread(Nom_definitiu, Frequencia) 
flors34 <- flors[flors$Plot == 34,] %>%
  select(-Plot)
plot34 <- plot34 %>% left_join(flors34,by="Codi_planta")
plot34[is.na(plot34)] <- 0
plot34 <- as.data.frame(plot34)
rownames(plot34) <- plot34$Codi_planta
plot34 <- plot34[,-(1:2)]
plot34 <- plot34*1000/plot34$Abundance
plot34 <- as.matrix(plot34) 
w34 <- plot34[,-(ncol(plot34))]
w34TVU <- w34[c("TVUF", "TVUH"),] 
i <- (colSums(w34TVU, na.rm=T) != 0)
w34TVUnonzero <- w34TVU[, i] 
subweb34 <- w34[, colnames(w34TVUnonzero)]


plot35 <- a[a$Parcela == 35,]%>%
  spread(Nom_definitiu, Frequencia) 
flors35 <- flors[flors$Plot == 35,] %>%
  select(-Plot)
plot35 <- plot35 %>% left_join(flors35,by="Codi_planta")
plot35[is.na(plot35)] <- 0
plot35 <- as.data.frame(plot35)
rownames(plot35) <- plot35$Codi_planta
plot35 <- plot35[,-(1:2)]
plot35 <- plot35*1000/plot35$Abundance
plot35 <- as.matrix(plot35) 
w35 <- plot35[,-(ncol(plot35))]
w35TVU <- w35[c("TVUF", "TVUH"),] 
i <- (colSums(w35TVU, na.rm=T) != 0)
w35TVUnonzero <- w35TVU[, i] 
subweb35 <- w35[, colnames(w35TVUnonzero)]


plot36 <- a[a$Parcela == 36,]%>%
  spread(Nom_definitiu, Frequencia) 
flors36 <- flors[flors$Plot == 36,] %>%
  select(-Plot)
plot36 <- plot36 %>% left_join(flors36,by="Codi_planta")
plot36[is.na(plot36)] <- 0
plot36 <- as.data.frame(plot36)
rownames(plot36) <- plot36$Codi_planta
plot36 <- plot36[,-(1:2)]
plot36 <- plot36*1000/plot36$Abundance
plot36 <- as.matrix(plot36) 
w36 <- plot36[,-(ncol(plot36))]
w36TVU <- w36[c("TVUF", "TVUH"),] 
i <- (colSums(w36TVU, na.rm=T) != 0)
w36TVUnonzero <- w36TVU[, i] 
subweb36 <- w36[, colnames(w36TVUnonzero)]


plot37 <- a[a$Parcela == 37,]%>%
  spread(Nom_definitiu, Frequencia) 
flors37 <- flors[flors$Plot == 37,] %>%
  select(-Plot)
plot37 <- plot37 %>% left_join(flors37,by="Codi_planta")
plot37[is.na(plot37)] <- 0
plot37 <- as.data.frame(plot37)
rownames(plot37) <- plot37$Codi_planta
plot37 <- plot37[,-(1:2)]
plot37 <- plot37*1000/plot37$Abundance
plot37 <- as.matrix(plot37) 
w37 <- plot37[,-(ncol(plot37))]
w37TVU <- w37[c("TVUF", "TVUH"),] 
i <- (colSums(w37TVU, na.rm=T) != 0)
w37TVUnonzero <- w37TVU[, i] 
subweb37 <- w37[, colnames(w37TVUnonzero)]


plot38 <- a[a$Parcela == 38,]%>%
  spread(Nom_definitiu, Frequencia) 
flors38 <- flors[flors$Plot == 38,] %>%
  select(-Plot)
plot38 <- plot38 %>% left_join(flors38,by="Codi_planta")
plot38[is.na(plot38)] <- 0
plot38 <- as.data.frame(plot38)
rownames(plot38) <- plot38$Codi_planta
plot38 <- plot38[,-(1:2)]
plot38 <- plot38*1000/plot38$Abundance
plot38 <- as.matrix(plot38) 
w38 <- plot38[,-(ncol(plot38))]
w38TVU <- w38[c("TVUF", "TVUH"),] 
i <- (colSums(w38TVU, na.rm=T) != 0)
w38TVUnonzero <- w38TVU[, i] 
subweb38 <- w38[, colnames(w38TVUnonzero)]


plot39 <- a[a$Parcela == 39,]%>%
  spread(Nom_definitiu, Frequencia) 
flors39 <- flors[flors$Plot == 39,] %>%
  select(-Plot)
plot39 <- plot39 %>% left_join(flors39,by="Codi_planta")
plot39[is.na(plot39)] <- 0
plot39 <- as.data.frame(plot39)
rownames(plot39) <- plot39$Codi_planta
plot39 <- plot39[,-(1:2)]
plot39 <- plot39*1000/plot39$Abundance
plot39 <- as.matrix(plot39) 
w39 <- plot39[,-(ncol(plot39))]
w39TVU <- w39[c("TVUF", "TVUH"),] 
i <- (colSums(w39TVU, na.rm=T) != 0)
w39TVUnonzero <- w39TVU[, i] 
subweb39 <- w39[, colnames(w39TVUnonzero)]


plot40 <- a[a$Parcela == 40,]%>%
  spread(Nom_definitiu, Frequencia) 
flors40 <- flors[flors$Plot == 40,] %>%
  select(-Plot)
plot40 <- plot40 %>% left_join(flors40,by="Codi_planta")
plot40[is.na(plot40)] <- 0
plot40 <- as.data.frame(plot40)
rownames(plot40) <- plot40$Codi_planta
plot40 <- plot40[,-(1:2)]
plot40 <- plot40*1000/plot40$Abundance
plot40 <- as.matrix(plot40) 
w40 <- plot40[,-(ncol(plot40))]
w40TVU <- w40[c("TVUF", "TVUH"),] 
i <- (colSums(w40TVU, na.rm=T) != 0)
w40TVUnonzero <- w40TVU[, i] 
subweb40 <- w40[, colnames(w40TVUnonzero)]






networkmetrics<-matrix(0,40,3)

colnames(networkmetrics)<-c("H2", "generality","vulnerability")


b<-networklevel(w1, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[1,]<-b

b<-networklevel(w2, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[2,]<-b

b<-networklevel(w3, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[3,]<-b

b<-networklevel(w4, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[4,]<-b

b<-networklevel(w5, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[5,]<-b

b<-networklevel(w6, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[6,]<-b

b<-networklevel(w7, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[7,]<-b

b<-networklevel(w8, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[8,]<-b

b<-networklevel(w9, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[9,]<-b

b<-networklevel(w10, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[10,]<-b

b<-networklevel(w11, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[11,]<-b

b<-networklevel(w12, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[12,]<-b

b<-networklevel(w13, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[13,]<-b

b<-networklevel(w14, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[14,]<-b

b<-networklevel(w15, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[15,]<-b

b<-networklevel(w16, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[16,]<-b

b<-networklevel(w17, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[17,]<-b

b<-networklevel(w18, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[18,]<-b

b<-networklevel(w19, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[19,]<-b

b<-networklevel(w20, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[20,]<-b

b<-networklevel(w21, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[21,]<-b

b<-networklevel(w22, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[22,]<-b

b<-networklevel(w23, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[23,]<-b

b<-networklevel(w24, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[24,]<-b

b<-networklevel(w25, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[25,]<-b

b<-networklevel(w26, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[26,]<-b

b<-networklevel(w27, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[27,]<-b

b<-networklevel(w28, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[28,]<-b

b<-networklevel(w29, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[29,]<-b

b<-networklevel(w30, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[30,]<-b

b<-networklevel(w31, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[31,]<-b

b<-networklevel(w32, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[32,]<-b

b<-networklevel(w33, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[33,]<-b

b<-networklevel(w34, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[34,]<-b

b<-networklevel(w35, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[35,]<-b

b<-networklevel(w36, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[36,]<-b

b<-networklevel(w37, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[37,]<-b

b<-networklevel(w38, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[38,]<-b

b<-networklevel(w39, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[39,]<-b

b<-networklevel(w40, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[40,]<-b


networkmetrics <- as.data.frame(networkmetrics)

networkmetrics$Plot <- c(1:40)

networkmetrics <- networkmetrics %>%
  select(H2,generality,Plot)





networkmetricsTVU<-matrix(0,40,3)

colnames(networkmetricsTVU)<-c("H2TVU", "generalityTVU","vulnerabilityTVU")


b<-networklevel(subweb1, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[1,]<-b

b<-networklevel(subweb2, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[2,]<-b

b<-networklevel(subweb3, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[3,]<-b

b<-networklevel(subweb4, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[4,]<-b

b<-networklevel(subweb5, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[5,]<-b

b<-networklevel(subweb6, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[6,]<-b

b<-networklevel(subweb7, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[7,]<-b

b<-networklevel(subweb8, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[8,]<-b

b<-networklevel(subweb9, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[9,]<-b

b<-networklevel(subweb10, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[10,]<-b

b<-networklevel(subweb11, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[11,]<-b

b<-networklevel(subweb12, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[12,]<-b

b<-networklevel(subweb13, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[13,]<-b

b<-networklevel(subweb14, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[14,]<-b

b<-networklevel(subweb15, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[15,]<-b

b<-networklevel(subweb16, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[16,]<-b

b<-networklevel(subweb17, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[17,]<-b

b<-networklevel(subweb18, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[18,]<-b

b<-networklevel(subweb19, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[19,]<-b

b<-networklevel(subweb20, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[20,]<-b

b<-networklevel(subweb21, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[21,]<-b

b<-networklevel(subweb22, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[22,]<-b

b<-networklevel(subweb23, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[23,]<-b

b<-networklevel(subweb24, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[24,]<-b

b<-networklevel(subweb25, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[25,]<-c(0,3,0)

b<-networklevel(subweb26, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[26,]<-b

b<-networklevel(subweb27, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[27,]<-b

b<-networklevel(subweb28, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[28,]<-b

b<-networklevel(subweb29, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[29,]<-b

b<-networklevel(subweb30, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[30,]<-b

b<-networklevel(subweb31, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[31,]<-b

b<-networklevel(subweb32, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[32,]<-b

b<-networklevel(subweb33, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[33,]<-b

b<-networklevel(subweb34, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[34,]<-b

b<-networklevel(subweb35, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[35,]<-b

b<-networklevel(subweb36, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[36,]<-b

b<-networklevel(subweb37, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[37,]<-b

b<-networklevel(subweb38, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[38,]<-b

b<-networklevel(subweb39, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[39,]<-b

b<-networklevel(subweb40, index=c("H2","generality"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetricsTVU[40,]<-b


networkmetricsTVU <- as.data.frame(networkmetricsTVU)

networkmetricsTVU$Plot <- c(1:40)

networkmetricsTVU <- networkmetricsTVU %>%
  select(generalityTVU,Plot)



