
setwd("D:/Usuarios/s.reverte/OneDrive - CREAF/TESI/DADES/dades")

## Shared species

shared  <-  function(w1, w2){
  sp1 = list(bottom=rownames(w1),top=colnames(w1),all=unique(c(colnames(w1),rownames(w1))))
  sp2 = list(bottom=rownames(w2),top=colnames(w2),all=unique(c(colnames(w2),rownames(w2))))
  # Common species
  Csp = sp1$all[sp1$all %in% sp2$all]
  CLsp = sp1$bottom[sp1$bottom %in% sp2$bottom]
  CUsp = sp1$top[sp1$top %in% sp2$top]
  return(list(Plants = length(CLsp), Pollinators = length(CUsp), Species = length(Csp)))
}

shared.dist <-  function(W){
  dplants = matrix(NA,ncol=length(W),nrow=length(W))
  colnames(dplants) = names(W)
  rownames(dplants) = names(W)
  dpolls = dplants
  dspecies = dplants
  for(i in c(1:(length(W)-1))){
    for(j in c((i+1):(length(W)))){
      partition = shared(W[[i]],W[[j]])
      dplants[j,i]		= partition$Plants
      dpolls[j,i]		  = partition$Pollinators
      dspecies[j,i]		= partition$Species
    }
  }
  distances = list(Plants=dplants, Pollinators=dpolls, Species = dspecies) 
  distances = lapply(distances,as.dist)
  return(distances)
}

## Sum of species

allspecies  <-  function(w1, w2){
  Lo = unique(c(rownames(w2),rownames(w1)))
  Up = unique(c(colnames(w2),colnames(w1)))
  All = unique(c(rownames(w2),rownames(w1), colnames(w2),colnames(w1)))
  return(list(Plants = length(Lo), Pollinators = length(Up), Species = length(All)))
}

allspecies.dist <-  function(W){
  dplants = matrix(NA,ncol=length(W),nrow=length(W))
  colnames(dplants) = names(W)
  rownames(dplants) = names(W)
  dpolls = dplants
  dspecies = dplants
  for(i in c(1:(length(W)-1))){
    for(j in c((i+1):(length(W)))){
      partition = allspecies(W[[i]],W[[j]])
      dplants[j,i]		= partition$Plants
      dpolls[j,i]		  = partition$Pollinators
      dspecies[j,i]		= partition$Species
    }
  }
  distances = list(Plants=dplants, Pollinators=dpolls, Species = dspecies) 
  distances = lapply(distances,as.dist)
  return(distances)
}

tot <- allspecies.dist(llistaaa)
compartides <- shared.dist(llistaaa)

names(compartides)

plantes <- compartides$Plants/tot$Plants
polls <- compartides$Pollinators/tot$Pollinators
overalls <- compartides$Species/tot$Species

bitxos <- read.table("bitxos qualitatiu.txt",header=T)

plantesdist <- data.frame(t(combn(rownames(bitxos),2)), as.numeric(plantes))
names(plantesdist) <- c("Plot", "Plot", "plantes")

pollssdist <- data.frame(t(combn(rownames(bitxos),2)), as.numeric(polls))
names(pollssdist) <- c("Plot", "Plot", "pollssdist")

overallsdist <- data.frame(t(combn(rownames(bitxos),2)), as.numeric(overalls))
names(overallsdist) <- c("Plot", "Plot", "overalls")

hist(plantesdist$plantes,main="Proporció de plantes compartides",xlab="mitjana = 0.51")
hist(pollssdist$pollssdist,main="Proporció de pol·linitzadors compartits",xlab="mitjana = 0.15")
hist(overallsdist$overalls,main="Proporció d'espècies compartides (plantes i bitxos junt)",xlab="mitjana = 0.23")



plantes <- compartides$Plants
polls <- compartides$Pollinators
overalls <- compartides$Species

plantesdist <- data.frame(t(combn(rownames(bitxos),2)), as.numeric(plantes))
names(plantesdist) <- c("Plot", "Plot", "plantes")

pollssdist <- data.frame(t(combn(rownames(bitxos),2)), as.numeric(polls))
names(pollssdist) <- c("Plot", "Plot", "pollssdist")

overallsdist <- data.frame(t(combn(rownames(bitxos),2)), as.numeric(overalls))
names(overallsdist) <- c("Plot", "Plot", "overalls")

hist(plantesdist$plantes,main="Proporció de plantes compartides",xlab="mitjana = ")
hist(pollssdist$pollssdist,main="Proporció de pol·linitzadors compartits",xlab="mitjana = ")
hist(overallsdist$overalls,main="Proporció d'espècies compartides (plantes i bitxos junt)",xlab="mitjana = ")

plantes <- tot$Plants
polls <- tot$Pollinators
overalls <- tot$Species


plantesdist <- data.frame(t(combn(rownames(bitxos),2)), as.numeric(plantes))
names(plantesdist) <- c("Plot", "Plot", "plantes")

pollssdist <- data.frame(t(combn(rownames(bitxos),2)), as.numeric(polls))
names(pollssdist) <- c("Plot", "Plot", "pollssdist")

overallsdist <- data.frame(t(combn(rownames(bitxos),2)), as.numeric(overalls))
names(overallsdist) <- c("Plot", "Plot", "overalls")

hist(plantesdist$plantes,main="Proporció de plantes compartides",xlab="mitjana = ")
hist(pollssdist$pollssdist,main="Proporció de pol·linitzadors compartits",xlab="mitjana = ")
hist(overallsdist$overalls,main="Proporció d'espècies compartides (plantes i bitxos junt)",xlab="mitjana = ")

mean(overallsdist$overalls)

taula1<-read.table("Xarxes/taula1.txt", header=T,colnames(1))
taula2<-read.table("Xarxes/taula2.txt", header=T,colnames(1))
taula3<-read.table("Xarxes/taula3.txt", header=T,colnames(1))
taula4<-read.table("Xarxes/taula4.txt", header=T,colnames(1))
taula5<-read.table("Xarxes/taula5.txt", header=T,colnames(1))
taula6<-read.table("Xarxes/taula6.txt", header=T,colnames(1))
taula7<-read.table("Xarxes/taula7.txt", header=T,colnames(1))
taula8<-read.table("Xarxes/taula8.txt", header=T,colnames(1))
taula9<-read.table("Xarxes/taula9.txt", header=T,colnames(1))
taula10<-read.table("Xarxes/taula10.txt", header=T,colnames(1))
taula11<-read.table("Xarxes/taula11.txt", header=T,colnames(1))
taula12<-read.table("Xarxes/taula12.txt", header=T,colnames(1))
taula13<-read.table("Xarxes/taula13.txt", header=T,colnames(1))
taula14<-read.table("Xarxes/taula14.txt", header=T,colnames(1))
taula15<-read.table("Xarxes/taula15.txt", header=T,colnames(1))
taula16<-read.table("Xarxes/taula16.txt", header=T,colnames(1))
taula17<-read.table("Xarxes/taula17.txt", header=T,colnames(1))
taula18<-read.table("Xarxes/taula18.txt", header=T,colnames(1))
taula19<-read.table("Xarxes/taula19.txt", header=T,colnames(1))
taula20<-read.table("Xarxes/taula20.txt", header=T,colnames(1))
taula21<-read.table("Xarxes/taula21.txt", header=T,colnames(1))
taula22<-read.table("Xarxes/taula22.txt", header=T,colnames(1))
taula23<-read.table("Xarxes/taula23.txt", header=T,colnames(1))
taula24<-read.table("Xarxes/taula24.txt", header=T,colnames(1))
taula25<-read.table("Xarxes/taula25.txt", header=T,colnames(1))
taula26<-read.table("Xarxes/taula26.txt", header=T,colnames(1))
taula27<-read.table("Xarxes/taula27.txt", header=T,colnames(1))
taula28<-read.table("Xarxes/taula28.txt", header=T,colnames(1))
taula29<-read.table("Xarxes/taula29.txt", header=T,colnames(1))
taula30<-read.table("Xarxes/taula30.txt", header=T,colnames(1))
taula31<-read.table("Xarxes/taula31.txt", header=T,colnames(1))
taula32<-read.table("Xarxes/taula32.txt", header=T,colnames(1))
taula33<-read.table("Xarxes/taula33.txt", header=T,colnames(1))
taula34<-read.table("Xarxes/taula34.txt", header=T,colnames(1))
taula35<-read.table("Xarxes/taula35.txt", header=T,colnames(1))
taula36<-read.table("Xarxes/taula36.txt", header=T,colnames(1))
taula37<-read.table("Xarxes/taula37.txt", header=T,colnames(1))
taula38<-read.table("Xarxes/taula38.txt", header=T,colnames(1))
taula39<-read.table("Xarxes/taula39.txt", header=T,colnames(1))
taula40<-read.table("Xarxes/taula40.txt", header=T,colnames(1))


t1<-as.matrix(taula1)
t2<-as.matrix(taula2)
t3<-as.matrix(taula3)
t4<-as.matrix(taula4)
t5<-as.matrix(taula5)
t6<-as.matrix(taula6)
t7<-as.matrix(taula7)
t8<-as.matrix(taula8)
t9<-as.matrix(taula9)
t10<-as.matrix(taula10)
t11<-as.matrix(taula11)
t12<-as.matrix(taula12)
t13<-as.matrix(taula13)
t14<-as.matrix(taula14)
t15<-as.matrix(taula15)
t16<-as.matrix(taula16)
t17<-as.matrix(taula17)
t18<-as.matrix(taula18)
t19<-as.matrix(taula19)
t20<-as.matrix(taula20)
t21<-as.matrix(taula21)
t22<-as.matrix(taula22)
t23<-as.matrix(taula23)
t24<-as.matrix(taula24)
t25<-as.matrix(taula25)
t26<-as.matrix(taula26)
t27<-as.matrix(taula27)
t28<-as.matrix(taula28)
t29<-as.matrix(taula29)
t30<-as.matrix(taula30)
t31<-as.matrix(taula31)
t32<-as.matrix(taula32)
t33<-as.matrix(taula33)
t34<-as.matrix(taula34)
t35<-as.matrix(taula35)
t36<-as.matrix(taula36)
t37<-as.matrix(taula37)
t38<-as.matrix(taula38)
t39<-as.matrix(taula39)
t40<-as.matrix(taula40)

llistaaa<-list(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,
               t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,
               t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40)