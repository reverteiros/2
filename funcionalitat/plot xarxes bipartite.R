

library(bipartite)

a<-read.table("dades/censos.txt",header=T)

plot1 <- a[a$Parcela == 1,]

w1<-data.frame(frame2webs(plot40, varnames= c("Planta", "Bitxo", "Parcela", "Frequencia"), type.out="list", emptylist="F"))

plotweb(w1, method="cca", text.rot=45)
