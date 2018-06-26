
setwd("D:/Usuarios/s.reverte/OneDrive - CREAF/TESI/DADES/dades")

library (bipartite)

consensus_modularity<-function(datos,iteraciones=1,plot=FALSE,pasos=NULL)
{
  names.plants<-rownames(datos)
  names.animals<-colnames(datos)
  matriz<-datos ; colnames(matriz)<-c(1:ncol(datos)) ; rownames(matriz)<-c(1:nrow(datos))
  a<-matrix(0,ncol=ncol(matriz),nrow=ncol(matriz))
  p<-matrix(0,ncol=nrow(matriz),nrow=nrow(matriz))
  int<-matrix(0,ncol=ncol(matriz),nrow=nrow(matriz))
  likeli<-vector(length=iteraciones)
  info.modul<-as.data.frame(matrix(ncol=5,nrow=iteraciones)) # Matriz de informacion de cada iteracion
  colnames(info.modul)<-c("iteracion","Likelihood","num.modulos","z","significance")
  nulls<-nullmodel(datos,N=50,method=1) ## Se puede modificar aqui el numero de modelos nulos
  modules.nulls<-sapply(nulls,computeModules)
  like.nulls<-sapply(modules.nulls,function(x) x@likelihood)
  pdf("salida.pdf")
  for (i in 1:iteraciones) # LOOP ITERACIONES
  {
    info.modul[i,1]<-i
    modular.object<-computeModules(matriz,steps=pasos)
    info.modul[i,2]<-modular.object@likelihood
    mod<-listModuleInformation(modular.object)
    info.modul[i,3]<-length(mod[[2]])
    info.modul[i,4]<-(modular.object@likelihood-mean(like.nulls))/sd(like.nulls)
    if (isTRUE (info.modul[i,4]>2)){info.modul[i,5]<-"*"}
    else {info.modul[i,5]<-"ns"}
    plotModuleWeb(modular.object,labsize=0.4)
    save(modular.object,file=sprintf("outcome.iteration_%d",i))
    #sprintf("outcome.iteration_%d",i)<-modular.object
    save(modular.object,file="outcome.iteration_%d",append=i)
    for (j in 1:length(mod[[2]])) # LOOP PARA CADA MODULO
    { 
      module.plants<-as.numeric(mod[[2]][[j]][[1]])
      module.pol<-as.numeric(mod[[2]][[j]][[2]]) 
      for (m in 1:length(module.plants)) # LOOP MODULO J PLANTAS
      {
        for (w in m:length(module.plants))
        {
          if(isTRUE(m==w)){diag(p)[module.plants[m]]<-diag(p)[module.plants[m]]+1}
          else
          {
            p[module.plants[m],module.plants[w]]<-p[module.plants[m],module.plants[w]]+1
            p[module.plants[w],module.plants[m]]<-p[module.plants[w],module.plants[m]]+1
          }
        }
      }
      for (q in 1:length(module.pol)) # LOOP MODULO J ANIMALES
      {
        for (u in q:length(module.pol))
        {
          if(isTRUE(q==u)){diag(a)[module.pol[q]]<-diag(a)[module.pol[q]]+1}
          else
          {
            a[module.pol[q],module.pol[u]]<-a[module.pol[q],module.pol[u]]+1
            a[module.pol[u],module.pol[q]]<-a[module.pol[u],module.pol[q]]+1
          }
        }
      }
      for (o in 1:length(module.plants)) # LOOP MODULO J PLANTAS-ANIMALES
      {
        for (h in 1:length(module.pol))
        {
          int[module.plants[o],module.pol[h]]<-int[module.plants[o],module.pol[h]]+1
        }
      }
      save(p,file="plants")
      save(a,file="animals")
      save(int,file="interaccion")
      save(info.modul,file="info")
    }
  }
  int<-as.data.frame(int)
  colnames(a)<-names.animals ; rownames(a)<-names.animals
  colnames(p)<-names.plants ; rownames(p)<-names.plants
  colnames(int)<-names.animals ; rownames(int)<-names.plants
  salida<-list("plants"=p,"animals"=a,"interaction"=int,"info"=info.modul)
  dev.off()
  if (isTRUE(plot==TRUE))
  {
    dev.new()
    plotModuleWeb(computeModules(int),labsize=0.3)
    dev.new() ; par(mfrow=c(2,1))
    plot(hclust(as.dist(1-p/iteraciones),method="average"),main="distance tree for plants")
    plot(hclust(as.dist(1-a/iteraciones),method="average"),main="distance tree for animals")
    cat("Consensus modules and distances trees plotted")
  }
  return(salida)
}


ery.poll<-read.table ("metaweb.txt", header=T) #Non-collapsed matrix
ery.poll<-ery.poll[,-1]
ery.poll=as.matrix(ery.poll)
output<-consensus_modularity(ery.poll,50, plot=TRUE, pasos=1E6)
##################################iteraciones########steps

## se puede cambiar el numero de steps para que el algoritmo sea más rápido, aunque no menos 
## de 1E6 (si tratas con un número de muestras sobre las 50-100)
## o se pueden hacer menos iteraciones


aaa <- load("info")
names(aaa)

info.modul
max(info.modul$Likelihood)
## esto es para sacar cual es la iteracion con una verosimilitud mayor por lo tanto más fiable