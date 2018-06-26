
setwd("D:/Usuarios/s.reverte/OneDrive - CREAF/TESI/DADES/dades/Xarxes")

taula1<-read.table("taula1.txt", header=T,colnames(1))
taula2<-read.table("taula2.txt", header=T,colnames(1))

t1<-as.matrix(taula1)
t2<-as.matrix(taula2)

qualitative.subnetwork  <-  function(w1, w2){
  sp1 = list(top=rownames(w1),bottom=colnames(w1),all=unique(c(colnames(w1),rownames(w1))))
  sp2 = list(top=rownames(w2),bottom=colnames(w2),all=unique(c(colnames(w2),rownames(w2))))
  # Common species
  Csp = sp1$all[sp1$all %in% sp2$all]
  CUsp = sp1$top[sp1$top %in% sp2$top]
  CLsp = sp1$bottom[sp1$bottom %in% sp2$bottom]
  wCom = w1[CUsp,CLsp]
  return(wCom)
}

quantitative.subnetwork  <-  function(w1, w2){
  sp1 = list(top=rownames(w1),bottom=colnames(w1),all=unique(c(colnames(w1),rownames(w1))))
  sp2 = list(top=rownames(w2),bottom=colnames(w2),all=unique(c(colnames(w2),rownames(w2))))
  # Common species
  Csp = sp1$all[sp1$all %in% sp2$all]
  CUsp = sp1$top[sp1$top %in% sp2$top]
  CLsp = sp1$bottom[sp1$bottom %in% sp2$bottom]
  w1Com = w1[CUsp,CLsp]
  w2Com = w2[CUsp,CLsp]
  net<-w1Com-w2Com
  if (net[net>0]) w2Com[w2Com] else w1Com[w1Com]
  return(net)
}##### no funciona!!
