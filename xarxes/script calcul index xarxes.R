
library(bipartite)

a<-read.table("censos.txt",header=T)

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

index<-matrix(0,40,6)

index.names<-c("Connectance", "Weighted NODF",  "H2", "Shannon diversity", "Generality", "Vulnerability")

colnames(index)<-index.names

names(index)
b<-networklevel(taula1, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[1,]<-b

b<-networklevel(taula2, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[2,]<-b

b<-networklevel(taula3, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[3,]<-b

b<-networklevel(taula4, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[4,]<-b

b<-networklevel(taula5, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[5,]<-b

b<-networklevel(taula6, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[6,]<-b

b<-networklevel(taula7, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[7,]<-b

b<-networklevel(taula8, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[8,]<-b

b<-networklevel(taula9, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[9,]<-b

b<-networklevel(taula10, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[10,]<-b

b<-networklevel(taula11, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[11,]<-b

b<-networklevel(taula12, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[12,]<-b

b<-networklevel(taula13, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[13,]<-b

b<-networklevel(taula14, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[14,]<-b

b<-networklevel(taula15, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[15,]<-b

b<-networklevel(taula16, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[16,]<-b

b<-networklevel(taula17, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[17,]<-b

b<-networklevel(taula18, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[18,]<-b

b<-networklevel(taula19, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[19,]<-b

b<-networklevel(taula20, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[20,]<-b

b<-networklevel(taula21, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[21,]<-b

b<-networklevel(taula22, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[22,]<-b

b<-networklevel(taula23, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[23,]<-b

b<-networklevel(taula24, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[24,]<-b

b<-networklevel(taula25, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[25,]<-b

b<-networklevel(taula26, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[26,]<-b

b<-networklevel(taula27, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[27,]<-b

b<-networklevel(taula28, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[28,]<-b

b<-networklevel(taula29, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[29,]<-b

b<-networklevel(taula30, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[30,]<-b

b<-networklevel(taula31, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[31,]<-b

b<-networklevel(taula32, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[32,]<-b

b<-networklevel(taula33, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[33,]<-b

b<-networklevel(taula34, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[34,]<-b

b<-networklevel(taula35, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[35,]<-b

b<-networklevel(taula36, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[36,]<-b

b<-networklevel(taula37, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[37,]<-b

b<-networklevel(taula38, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[38,]<-b

b<-networklevel(taula39, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[39,]<-b

b<-networklevel(taula40, index=c("connectance","weighted NODF","H2","Shannon diversity","generality","vulnerability"),level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[40,]<-b



write.csv(index,"C:/Users/rosa/Desktop/metriques xarxa.csv")

index<-matrix(0,40,2)

index.names<-c("Number of species HL", "Number of species LL")

llista

b<-networklevel(w1, index="connectance", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[1,]<-b

colnames(index)<-index.names

w1<-data.frame(frame2webs(plot1, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[1,]<-b

w1<-data.frame(frame2webs(plot2, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[2,]<-b

w1<-data.frame(frame2webs(plot3, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[3,]<-b

w1<-data.frame(frame2webs(plot4, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[4,]<-b

w1<-data.frame(frame2webs(plot5, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[5,]<-b

w1<-data.frame(frame2webs(plot6, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[6,]<-b

w1<-data.frame(frame2webs(plot7, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[7,]<-b

w1<-data.frame(frame2webs(plot8, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[8,]<-b

w1<-data.frame(frame2webs(plot9, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[9,]<-b

w1<-data.frame(frame2webs(plot10, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[10,]<-b

w1<-data.frame(frame2webs(plot11, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[11,]<-b

w1<-data.frame(frame2webs(plot12, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[12,]<-b

w1<-data.frame(frame2webs(plot13, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[13,]<-b

w1<-data.frame(frame2webs(plot14, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[14,]<-b

w1<-data.frame(frame2webs(plot15, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[15,]<-b

w1<-data.frame(frame2webs(plot16, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[16,]<-b

w1<-data.frame(frame2webs(plot17, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[17,]<-b

w1<-data.frame(frame2webs(plot18, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[18,]<-b

w1<-data.frame(frame2webs(plot19, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[19,]<-b

w1<-data.frame(frame2webs(plot20, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[20,]<-b

w1<-data.frame(frame2webs(plot21, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[21,]<-b

w1<-data.frame(frame2webs(plot22, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[22,]<-b

w1<-data.frame(frame2webs(plot23, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[23,]<-b

w1<-data.frame(frame2webs(plot24, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[24,]<-b

w1<-data.frame(frame2webs(plot25, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[25,]<-b

w1<-data.frame(frame2webs(plot26, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[26,]<-b

w1<-data.frame(frame2webs(plot27, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[27,]<-b

w1<-data.frame(frame2webs(plot28, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[28,]<-b

w1<-data.frame(frame2webs(plot29, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[29,]<-b

w1<-data.frame(frame2webs(plot30, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[30,]<-b

w1<-data.frame(frame2webs(plot31, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[31,]<-b

w1<-data.frame(frame2webs(plot32, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[32,]<-b

w1<-data.frame(frame2webs(plot33, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[33,]<-b

w1<-data.frame(frame2webs(plot34, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[34,]<-b

w1<-data.frame(frame2webs(plot35, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[35,]<-b

w1<-data.frame(frame2webs(plot36, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[36,]<-b

w1<-data.frame(frame2webs(plot37, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[37,]<-b

w1<-data.frame(frame2webs(plot38, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[38,]<-b

w1<-data.frame(frame2webs(plot39, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="number of species", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[39,]<-b

w1<-data.frame(frame2webs(plot40, varnames= c("Codi_planta", "Nom_definitiu", "Parcela", "Frequencia"), type.out="list", emptylist="F"))
b<-networklevel(w1, index="generality", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)
index[40,]<-b
