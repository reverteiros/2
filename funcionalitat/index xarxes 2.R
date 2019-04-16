

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






networkmetrics<-matrix(0,40,2)

colnames(networkmetrics)<-c("H2", "Shannon_diversity")


b<-networklevel(w1, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[1,]<-b

b<-networklevel(w2, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[2,]<-b

b<-networklevel(w3, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[3,]<-b

b<-networklevel(w4, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[4,]<-b

b<-networklevel(w5, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[5,]<-b

b<-networklevel(w6, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[6,]<-b

b<-networklevel(w7, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[7,]<-b

b<-networklevel(w8, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[8,]<-b

b<-networklevel(w9, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[9,]<-b

b<-networklevel(w10, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[10,]<-b

b<-networklevel(w11, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[11,]<-b

b<-networklevel(w12, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[12,]<-b

b<-networklevel(w13, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[13,]<-b

b<-networklevel(w14, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[14,]<-b

b<-networklevel(w15, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[15,]<-b

b<-networklevel(w16, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[16,]<-b

b<-networklevel(w17, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[17,]<-b

b<-networklevel(w18, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[18,]<-b

b<-networklevel(w19, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[19,]<-b

b<-networklevel(w20, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[20,]<-b

b<-networklevel(w21, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[21,]<-b

b<-networklevel(w22, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[22,]<-b

b<-networklevel(w23, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[23,]<-b

b<-networklevel(w24, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[24,]<-b

b<-networklevel(w25, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[25,]<-b

b<-networklevel(w26, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[26,]<-b

b<-networklevel(w27, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[27,]<-b

b<-networklevel(w28, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[28,]<-b

b<-networklevel(w29, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[29,]<-b

b<-networklevel(w30, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[30,]<-b

b<-networklevel(w31, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[31,]<-b

b<-networklevel(w32, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[32,]<-b

b<-networklevel(w33, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[33,]<-b

b<-networklevel(w34, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[34,]<-b

b<-networklevel(w35, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[35,]<-b

b<-networklevel(w36, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[36,]<-b

b<-networklevel(w37, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[37,]<-b

b<-networklevel(w38, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[38,]<-b

b<-networklevel(w39, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[39,]<-b

b<-networklevel(w40, index=c("H2","Shannon diversity"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[40,]<-b


networkmetrics <- as.data.frame(networkmetrics)

networkmetrics$Plot <- c(1:40)

networkmetrics <- networkmetrics %>%
  select(H2,Shannon_diversity,Plot)


library(tidyverse)

######## d'
aa1 <- specieslevel(w1, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH") %>%
  mutate(Plot = 1)

aa2 <- specieslevel(w2, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 2)

aa3 <- specieslevel(w3, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 3)

aa4 <- specieslevel(w4, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 4)

aa5 <- specieslevel(w5, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 5)

aa6 <- specieslevel(w6, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 6)

aa7 <- specieslevel(w7, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 7)

aa8 <- specieslevel(w8, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 8)

aa9 <- specieslevel(w9, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 9)

aa10 <- specieslevel(w10, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 10)

aa11 <- specieslevel(w11, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 11)

aa12 <- specieslevel(w12, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 12)

aa13 <- specieslevel(w13, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 13)

aa14 <- specieslevel(w14, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 14)

aa15 <- specieslevel(w15, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 15)

aa16 <- specieslevel(w16, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 16)

aa17 <- specieslevel(w17, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 17)

aa18 <- specieslevel(w18, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 18)

aa19 <- specieslevel(w19, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 19)

aa20 <- specieslevel(w20, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 20)

aa21 <- specieslevel(w21, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 21)

aa22 <- specieslevel(w22, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 22)

aa23 <- specieslevel(w23, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 23)

aa24 <- specieslevel(w24, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 24)

aa25 <- specieslevel(w25, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 25)

aa26 <- specieslevel(w26, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 26)

aa27 <- specieslevel(w27, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 27)

aa28 <- specieslevel(w28, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 28)

aa29 <- specieslevel(w29, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 29)

aa30 <- specieslevel(w30, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 30)

aa31 <- specieslevel(w31, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 31)

aa32 <- specieslevel(w32, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 32)

aa33 <- specieslevel(w33, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 33)

aa34 <- specieslevel(w34, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 34)

aa35 <- specieslevel(w35, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 35)

aa36 <- specieslevel(w36, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 36)

aa37 <- specieslevel(w37, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 37)

aa38 <- specieslevel(w38, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 38)

aa39 <- specieslevel(w39, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 39)

aa40 <- specieslevel(w40, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 40)

dprime <- bind_rows(aa1,aa2,aa3,aa4,aa5,aa6,aa7,aa8,aa9,aa10,aa11,aa12,aa13,aa14,aa15,aa16,aa17,aa18,aa19,aa20,aa21,aa22,aa23,aa24,aa25,aa26,aa27,aa28,aa29,aa30,aa31,aa32,aa33,aa34,aa35,aa36,aa37,aa38,aa39,aa40) %>%
  group_by(Plot, Species) 


####### closeness

a1 <- specieslevel(w1, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 1)

a2 <- specieslevel(w2, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 2)

a3 <- specieslevel(w3, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 3)

a4 <- specieslevel(w4, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 4)

a5 <- specieslevel(w5, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 5)

a6 <- specieslevel(w6, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 6)

a7 <- specieslevel(w7, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 7)

a8 <- specieslevel(w8, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 8)

a9 <- specieslevel(w9, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 9)

a10 <- specieslevel(w10, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 10)

a11 <- specieslevel(w11, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 11)

a12 <- specieslevel(w12, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 12)

a13 <- specieslevel(w13, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 13)

a14 <- specieslevel(w14, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 14)

a15 <- specieslevel(w15, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 15)

a16 <- specieslevel(w16, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 16)

a17 <- specieslevel(w17, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 17)

a18 <- specieslevel(w18, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 18)

a19 <- specieslevel(w19, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 19)

a20 <- specieslevel(w20, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 20)

a21 <- specieslevel(w21, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 21)

a22 <- specieslevel(w22, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 22)

a23 <- specieslevel(w23, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 23)

a24 <- specieslevel(w24, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 24)

a25 <- specieslevel(w25, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 25)

a26 <- specieslevel(w26, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 26)

a27 <- specieslevel(w27, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 27)

a28 <- specieslevel(w28, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 28)

a29 <- specieslevel(w29, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 29)

a30 <- specieslevel(w30, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 30)

a31 <- specieslevel(w31, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 31)

a32 <- specieslevel(w32, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 32)

a33 <- specieslevel(w33, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 33)

a34 <- specieslevel(w34, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 34)

a35 <- specieslevel(w35, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 35)

a36 <- specieslevel(w36, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 36)

a37 <- specieslevel(w37, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 37)

a38 <- specieslevel(w38, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 38)

a39 <- specieslevel(w39, index="closeness", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  select(., Species,weighted.closeness) %>%
  filter(., Species == "ROF" | Species == "TVUF"| Species == "TVUH")%>%
  mutate(Plot = 39)


closenesss <- bind_rows(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32,a33,a34,a35,a36,a37,a38,a39)





####### apis d'

a1 <- specieslevel(w1, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 1)

a2 <- specieslevel(w2, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 2)

a3 <- specieslevel(w3, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 3)

a4 <- specieslevel(w4, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 4)

a5 <- specieslevel(w5, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 5)

a6 <- specieslevel(w6, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 6)

a7 <- specieslevel(w7, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 7)

a8 <- specieslevel(w8, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 8)

a9 <- specieslevel(w9, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 9)

a10 <- specieslevel(w10, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 10)

a11 <- specieslevel(w11, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 11)

a12 <- specieslevel(w12, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 12)

a13 <- specieslevel(w13, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 13)

a14 <- specieslevel(w14, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 14)

a15 <- specieslevel(w15, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 15)

a16 <- specieslevel(w16, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 16)

a17 <- specieslevel(w17, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 17)

a18 <- specieslevel(w18, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 18)

a19 <- specieslevel(w19, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 19)

a20 <- specieslevel(w20, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 20)

a21 <- specieslevel(w21, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 21)

a22 <- specieslevel(w22, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 22)

a23 <- specieslevel(w23, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 23)

a24 <- specieslevel(w24, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 24)

a25 <- specieslevel(w25, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 25)

a26 <- specieslevel(w26, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 26)

a27 <- specieslevel(w27, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 27)

a28 <- specieslevel(w28, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 28)

a29 <- specieslevel(w29, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 29)

a30 <- specieslevel(w30, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 30)

a31 <- specieslevel(w31, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 31)

a32 <- specieslevel(w32, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 32)

a33 <- specieslevel(w33, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 33)

a34 <- specieslevel(w34, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 34)

a35 <- specieslevel(w35, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 35)

a36 <- specieslevel(w36, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 36)

a37 <- specieslevel(w37, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 37)

a38 <- specieslevel(w38, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 38)

a39 <- specieslevel(w39, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 39)

a40 <- specieslevel(w40, index="d", level="higher") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "Apis")%>%
  mutate(Plot = 40)

apis <- bind_rows(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,a31,a32,a33,a34,a35,a36,a37,a38,a39,a40) %>%
  select(d,Plot)

names(apis) <- c("d_apis","Plot") 





