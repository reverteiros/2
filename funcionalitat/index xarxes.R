
library(bipartite)

a<-read.table("dades/censos.txt",header=T)

plot1 <- a[a$Parcela == 1,]
plot2 <- a[a$Parcela == 2,]
plot3 <- a[a$Parcela == 3,]
plot4 <- a[a$Parcela == 4,]
plot5 <- a[a$Parcela == 5,]
plot6 <- a[a$Parcela == 6,]
plot7 <- a[a$Parcela == 7,]
plot8 <- a[a$Parcela == 8,]
plot9 <- a[a$Parcela == 9,]
plot10 <- a[a$Parcela == 10,]
plot11 <- a[a$Parcela == 11,]
plot12 <- a[a$Parcela == 12,]
plot13 <- a[a$Parcela == 13,]
plot14 <- a[a$Parcela == 14,]
plot15 <- a[a$Parcela == 15,]
plot16 <- a[a$Parcela == 16,]
plot17 <- a[a$Parcela == 17,]
plot18 <- a[a$Parcela == 18,]
plot19 <- a[a$Parcela == 19,]
plot20 <- a[a$Parcela == 20,]
plot21 <- a[a$Parcela == 21,]
plot22 <- a[a$Parcela == 22,]
plot23 <- a[a$Parcela == 23,]
plot24 <- a[a$Parcela == 24,]
plot25 <- a[a$Parcela == 25,]
plot26 <- a[a$Parcela == 26,]
plot27 <- a[a$Parcela == 27,]
plot28 <- a[a$Parcela == 28,]
plot29 <- a[a$Parcela == 29,]
plot30 <- a[a$Parcela == 30,]
plot31 <- a[a$Parcela == 31,]
plot32 <- a[a$Parcela == 32,]
plot33 <- a[a$Parcela == 33,]
plot34 <- a[a$Parcela == 34,]
plot35 <- a[a$Parcela == 35,]
plot36 <- a[a$Parcela == 36,]
plot37 <- a[a$Parcela == 37,]
plot38 <- a[a$Parcela == 38,]
plot39 <- a[a$Parcela == 39,]
plot40 <- a[a$Parcela == 40,]



w1<-data.frame(frame2webs(plot1, varnames= c("Codi_planta", "Nom_definitiu", "Parcela","Frequencia"), type.out="list", emptylist="F"))
w2<-data.frame(frame2webs(plot2, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w3<-data.frame(frame2webs(plot3, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w4<-data.frame(frame2webs(plot4, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w5<-data.frame(frame2webs(plot5, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w6<-data.frame(frame2webs(plot6, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w7<-data.frame(frame2webs(plot7, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w8<-data.frame(frame2webs(plot8, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w9<-data.frame(frame2webs(plot9, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w10<-data.frame(frame2webs(plot10, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w11<-data.frame(frame2webs(plot11, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w12<-data.frame(frame2webs(plot12, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w13<-data.frame(frame2webs(plot13, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w14<-data.frame(frame2webs(plot14, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w15<-data.frame(frame2webs(plot15, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w16<-data.frame(frame2webs(plot16, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w17<-data.frame(frame2webs(plot17, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w18<-data.frame(frame2webs(plot18, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w19<-data.frame(frame2webs(plot19, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w20<-data.frame(frame2webs(plot20, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w21<-data.frame(frame2webs(plot21, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w22<-data.frame(frame2webs(plot22, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w23<-data.frame(frame2webs(plot23, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w24<-data.frame(frame2webs(plot24, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w25<-data.frame(frame2webs(plot25, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w26<-data.frame(frame2webs(plot26, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w27<-data.frame(frame2webs(plot27, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w28<-data.frame(frame2webs(plot28, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w29<-data.frame(frame2webs(plot29, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w30<-data.frame(frame2webs(plot30, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w31<-data.frame(frame2webs(plot31, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w32<-data.frame(frame2webs(plot32, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w33<-data.frame(frame2webs(plot33, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w34<-data.frame(frame2webs(plot34, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w35<-data.frame(frame2webs(plot35, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w36<-data.frame(frame2webs(plot36, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w37<-data.frame(frame2webs(plot37, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w38<-data.frame(frame2webs(plot38, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w39<-data.frame(frame2webs(plot39, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
w40<-data.frame(frame2webs(plot40, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))


networkmetrics<-matrix(0,40,6)

colnames(networkmetrics)<-c("Connectance", "Nestedness",  "H2", "Shannon_diversity", "Generality", "Vulnerability")


b<-networklevel(w1, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[1,]<-b

b<-networklevel(w2, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[2,]<-b

b<-networklevel(w3, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[3,]<-b

b<-networklevel(w4, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[4,]<-b

b<-networklevel(w5, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[5,]<-b

b<-networklevel(w6, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[6,]<-b

b<-networklevel(w7, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[7,]<-b

b<-networklevel(w8, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[8,]<-b

b<-networklevel(w9, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[9,]<-b

b<-networklevel(w10, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[10,]<-b

b<-networklevel(w11, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[11,]<-b

b<-networklevel(w12, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[12,]<-b

b<-networklevel(w13, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[13,]<-b

b<-networklevel(w14, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[14,]<-b

b<-networklevel(w15, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[15,]<-b

b<-networklevel(w16, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[16,]<-b

b<-networklevel(w17, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[17,]<-b

b<-networklevel(w18, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[18,]<-b

b<-networklevel(w19, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[19,]<-b

b<-networklevel(w20, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[20,]<-b

b<-networklevel(w21, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[21,]<-b

b<-networklevel(w22, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[22,]<-b

b<-networklevel(w23, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[23,]<-b

b<-networklevel(w24, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[24,]<-b

b<-networklevel(w25, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[25,]<-b

b<-networklevel(w26, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[26,]<-b

b<-networklevel(w27, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[27,]<-b

b<-networklevel(w28, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[28,]<-b

b<-networklevel(w29, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[29,]<-b

b<-networklevel(w30, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[30,]<-b

b<-networklevel(w31, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[31,]<-b

b<-networklevel(w32, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[32,]<-b

b<-networklevel(w33, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[33,]<-b

b<-networklevel(w34, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[34,]<-b

b<-networklevel(w35, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[35,]<-b

b<-networklevel(w36, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[36,]<-b

b<-networklevel(w37, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[37,]<-b

b<-networklevel(w38, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[38,]<-b

b<-networklevel(w39, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[39,]<-b

b<-networklevel(w40, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
networkmetrics[40,]<-b


networkmetrics <- as.data.frame(networkmetrics)

networkmetrics$Plot <- c(1:40)

networkmetrics$dROF <- NA
networkmetrics$dTVUF <- NA
networkmetrics$dTVUH <- NA

######## ROF
a <- specieslevel(w1, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[1] <- a

a <- specieslevel(w2, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[2] <- a

a <- specieslevel(w3, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[3] <- a

a <- specieslevel(w4, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[4] <- a

a <- specieslevel(w5, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[5] <- a

a <- specieslevel(w6, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[6] <- a

a <- specieslevel(w7, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[7] <- a

a <- specieslevel(w8, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[8] <- a

a <- specieslevel(w9, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[9] <- a

a <- specieslevel(w10, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[10] <- a

a <- specieslevel(w11, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[11] <- a

a <- specieslevel(w12, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[12] <- a

a <- specieslevel(w13, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[13] <- a

a <- specieslevel(w14, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[14] <- a

a <- specieslevel(w15, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[15] <- a

a <- specieslevel(w16, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[16] <- a

a <- specieslevel(w17, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[17] <- a

a <- specieslevel(w18, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[18] <- a

a <- specieslevel(w19, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[19] <- a

a <- specieslevel(w20, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[20] <- a

a <- specieslevel(w21, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[21] <- a

a <- specieslevel(w22, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[22] <- a

a <- specieslevel(w23, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[23] <- a

a <- specieslevel(w24, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[24] <- a

a <- specieslevel(w25, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[25] <- a

a <- specieslevel(w26, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[26] <- a

a <- specieslevel(w27, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[27] <- a

a <- specieslevel(w28, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[28] <- a

a <- specieslevel(w29, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[29] <- a

a <- specieslevel(w30, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[30] <- a

a <- specieslevel(w31, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[31] <- a

a <- specieslevel(w32, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[32] <- a

a <- specieslevel(w33, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[33] <- a

a <- specieslevel(w34, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[34] <- a

a <- specieslevel(w35, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[35] <- a

a <- specieslevel(w36, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[36] <- a

a <- specieslevel(w37, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[37] <- a

a <- specieslevel(w38, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[38] <- a

a <- specieslevel(w39, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[39] <- a

a <- specieslevel(w40, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "ROF") %>%
  select(.,d)
networkmetrics$dROF[40] <- a


######## TVUF
a <- specieslevel(w1, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[1] <- a

a <- specieslevel(w2, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[2] <- a

a <- specieslevel(w3, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[3] <- a

a <- specieslevel(w4, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[4] <- a

a <- specieslevel(w5, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[5] <- a

a <- specieslevel(w6, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[6] <- a

a <- specieslevel(w7, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[7] <- a

a <- specieslevel(w8, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[8] <- a

a <- specieslevel(w9, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[9] <- a

a <- specieslevel(w10, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[10] <- a

a <- specieslevel(w11, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[11] <- a

a <- specieslevel(w12, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[12] <- a

a <- specieslevel(w13, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[13] <- a

a <- specieslevel(w14, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[14] <- a

a <- specieslevel(w15, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[15] <- a

a <- specieslevel(w16, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[16] <- a

a <- specieslevel(w17, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[17] <- a

a <- specieslevel(w18, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[18] <- a

a <- specieslevel(w19, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[19] <- a

a <- specieslevel(w20, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[20] <- a

a <- specieslevel(w21, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[21] <- a

a <- specieslevel(w22, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[22] <- a

a <- specieslevel(w23, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[23] <- a

a <- specieslevel(w24, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[24] <- a

a <- specieslevel(w25, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[25] <- a

a <- specieslevel(w26, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[26] <- a

a <- specieslevel(w27, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[27] <- a

a <- specieslevel(w28, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[28] <- a

a <- specieslevel(w29, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[29] <- a

a <- specieslevel(w30, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[30] <- a

a <- specieslevel(w31, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[31] <- a

a <- specieslevel(w32, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[32] <- a

a <- specieslevel(w33, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[33] <- a

a <- specieslevel(w34, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[34] <- a

a <- specieslevel(w35, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[35] <- a

a <- specieslevel(w36, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[36] <- a

a <- specieslevel(w37, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[37] <- a

a <- specieslevel(w38, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[38] <- a

a <- specieslevel(w39, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[39] <- a

a <- specieslevel(w40, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUF") %>%
  select(.,d)
networkmetrics$dTVUF[40] <- a



######## TVUH
a <- specieslevel(w1, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[1] <- a

a <- specieslevel(w2, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[2] <- a

a <- specieslevel(w3, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[3] <- a

a <- specieslevel(w4, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[4] <- a

a <- specieslevel(w5, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[5] <- a

a <- specieslevel(w6, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[6] <- a

a <- specieslevel(w7, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[7] <- a

a <- specieslevel(w8, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[8] <- a

a <- specieslevel(w9, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[9] <- a

a <- specieslevel(w10, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[10] <- a

a <- specieslevel(w11, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[11] <- a

a <- specieslevel(w12, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[12] <- a

a <- specieslevel(w13, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[13] <- a

a <- specieslevel(w14, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[14] <- a

a <- specieslevel(w15, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[15] <- a

a <- specieslevel(w16, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[16] <- a

a <- specieslevel(w17, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[17] <- a

a <- specieslevel(w18, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[18] <- a

a <- specieslevel(w19, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[19] <- a

a <- specieslevel(w20, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[20] <- a

a <- specieslevel(w21, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[21] <- a

a <- specieslevel(w22, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[22] <- a

a <- specieslevel(w23, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[23] <- a

a <- specieslevel(w24, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[24] <- a

a <- specieslevel(w25, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[25] <- a

a <- specieslevel(w26, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[26] <- a

a <- specieslevel(w27, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[27] <- a



a <- specieslevel(w28, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[28] <- a

a <- specieslevel(w29, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[29] <- a

a <- specieslevel(w30, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[30] <- a

a <- specieslevel(w31, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[31] <- a

a <- specieslevel(w32, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[32] <- a

a <- specieslevel(w33, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[33] <- a

a <- specieslevel(w34, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[34] <- a

a <- specieslevel(w35, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[35] <- a

a <- specieslevel(w36, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[36] <- a

a <- specieslevel(w37, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[37] <- a

a <- specieslevel(w38, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[38] <- a

a <- specieslevel(w39, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[39] <- a

a <- specieslevel(w40, index="d", level="lower") %>%
  mutate(Species = rownames(.)) %>%
  filter(., Species == "TVUH") %>%
  select(.,d)
networkmetrics$dTVUH[40] <- a


networkmetrics$dROF[29] <- NA
networkmetrics$dTVUH[25]<- NA
networkmetrics$dTVUH[18]<- NA
networkmetrics$dTVUH[23]<- NA
networkmetrics$dTVUH[7]<- NA

networkmetrics <- networkmetrics %>%
  select(Nestedness,H2,Shannon_diversity,Plot,dROF,dTVUF,dTVUH)
