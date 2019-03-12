
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


# ######## APIS
# a <- specieslevel(w1, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X1.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[1] <- a
# 
# a <- specieslevel(w2, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X2.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[2] <- a
# 
# a <- specieslevel(w3, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X3.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[3] <- a
# 
# a <- specieslevel(w4, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X4.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[4] <- a
# 
# a <- specieslevel(w5, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X5.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[5] <- a
# 
# a <- specieslevel(w6, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X6.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[6] <- a
# 
# a <- specieslevel(w7, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X7.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[7] <- a
# 
# a <- specieslevel(w8, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X8.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[8] <- a
# 
# a <- specieslevel(w9, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X9.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[9] <- a
# 
# a <- specieslevel(w10, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X10.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[10] <- a
# 
# a <- specieslevel(w11, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X11.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[11] <- a
# 
# a <- specieslevel(w12, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X12.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[12] <- a
# 
# a <- specieslevel(w13, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X13.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[13] <- a
# 
# a <- specieslevel(w14, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X14.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[14] <- a
# 
# a <- specieslevel(w15, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X15.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[15] <- a
# 
# a <- specieslevel(w16, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X16.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[16] <- a
# 
# a <- specieslevel(w17, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X17.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[17] <- a
# 
# a <- specieslevel(w18, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X18.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[18] <- a
# 
# a <- specieslevel(w19, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X19.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[19] <- a
# 
# a <- specieslevel(w20, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X20.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[20] <- a
# 
# a <- specieslevel(w21, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X21.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[21] <- a
# 
# a <- specieslevel(w22, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X22.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[22] <- a
# 
# a <- specieslevel(w23, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X23.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[23] <- a
# 
# a <- specieslevel(w24, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X24.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[24] <- a
# 
# a <- specieslevel(w25, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X25.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[25] <- a
# 
# a <- specieslevel(w26, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X26.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[26] <- a
# 
# a <- specieslevel(w27, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X27.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[27] <- a
# 
# 
# 
# a <- specieslevel(w28, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X28.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[28] <- a
# 
# a <- specieslevel(w29, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X29.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[29] <- a
# 
# a <- specieslevel(w30, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X30.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[30] <- a
# 
# a <- specieslevel(w31, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X31.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[31] <- a
# 
# a <- specieslevel(w32, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X32.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[32] <- a
# 
# a <- specieslevel(w33, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X33.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[33] <- a
# 
# a <- specieslevel(w34, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X34.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[34] <- a
# 
# a <- specieslevel(w35, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X35.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[35] <- a
# 
# a <- specieslevel(w36, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X36.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[36] <- a
# 
# a <- specieslevel(w37, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X37.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[37] <- a
# 
# a <- specieslevel(w38, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X38.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[38] <- a
# 
# a <- specieslevel(w39, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X39.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[39] <- a
# 
# a <- specieslevel(w40, index="d", level="higher") %>%
#   mutate(Species = rownames(.)) %>%
#   filter(., Species == "X40.Apis") %>%
#   select(.,d)
# networkmetrics$dApis[40] <- a
# 
# 
