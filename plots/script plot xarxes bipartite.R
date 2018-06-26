

library(bipartite)
a<-read.table("censos2.txt",header=T)

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

w1<-data.frame(frame2webs(plot40, varnames= c("Planta", "Bitxo", "Parcela", "Frequencia"), type.out="list", emptylist="F"))

plotweb(w1, method="cca", text.rot=45)
