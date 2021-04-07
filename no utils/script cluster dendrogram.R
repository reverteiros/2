
setwd("D:/Usuarios/s.reverte/OneDrive - CREAF/TESI/DADES/dades")

pollinators<-read.table("bitxos quantitatiu.txt",header=T)

plants<-read.table("flors quantitatiu.txt", header=T)

tpollinators<-t(pollinators)

tplants<-t(plants)

# Ward Hierarchical Clustering with Bootstrapped p values

library(pvclust)

fit <- pvclust(tplants, method.hclust="ward",method.dist="euclidean")

plot(fit) 

pvrect(fit, alpha=.95)
