
setwd("D:/Usuarios/s.reverte/OneDrive - CREAF/TESI/DADES/dades")

library(vegan)
library(SpatialTools)
library(betapart)

set.seed(2)

# Llegir bases de dades
database2 <- read.table("Database3.txt",header=T)
senseapis<-read.table("bitxos quantitatiu sense apis.txt",header=T)
flors<-read.table("flors quantitatiu.txt",header=T)

# Distancia geografica
x <- as.matrix(database2[,7-8])
c<-dist1(x)
d<-c*100
d.dist<-as.dist(d)
geographic <- as.matrix(d.dist)

# Beta-diversitat
quantitative.plants<-bray.part(flors)
quantitative.senseapis<-bray.part(senseapis)
betaplants <- as.matrix(quantitative.plants$bray)
betasenseapis <- as.matrix(quantitative.senseapis$bray)

# Apis i temperatura
apisratedist <- dist(database2$Honeybeerate)
apisrate <- as.matrix(apisratedist)
tempdist <- dist(database2$T_Max)
temperature <- as.matrix(tempdist)

flowers <- list(flowerrichness, flowerabundance,betaplants)
apisrateplants=list(apisrate, betaplants)
apisrategeo <- list(apisrate, geographic)
apisplants=list(apis, betaplants)
plantsgeographic=list(betaplants, geographic)
tot=list(apisrate, temperature, betaplants)

MMRR(betasenseapis, apisrateplants, nperm=999)
MMRR(betasenseapis, apisplants, nperm=999)
MMRR(betasenseapis, plantsgeographic, nperm=999)
MMRR(betasenseapis, apisrategeo, nperm=999)

MMRR(betapollinators, apisrateplants, nperm=999)
MMRR(betapollinators, apisplants, nperm=999)
MMRR(betapollinators, plantsgeographic, nperm=999)
MMRR(betapollinators, apisrategeo, nperm=999)

MMRR(betasenseapis, tot, nperm=999)
MMRR(betapollinators, tot, nperm=999)

MMRR(betaplants, flowers, nperm=999)
MMRR(betasenseapis, flowers, nperm=999)
MMRR(betapollinators, flowers, nperm=999)

MMRR(apisrate, plantsgeographic, nperm=999)



# MMRR performs Multiple Matrix Regression with Randomization analysis
# Y is a dependent distance matrix
# X is a list of independent distance matrices (with optional names)

MMRR<-function(Y,X,nperm=999){
  #compute regression coefficients and test statistics
  nrowsY<-nrow(Y)
  y<-unfold(Y)
  if(is.null(names(X)))names(X)<-paste("X",1:length(X),sep="")
  Xmats<-sapply(X,unfold)
  fit<-lm(y~Xmats)
  coeffs<-fit$coefficients
  summ<-summary(fit)
  r.squared<-summ$r.squared
  tstat<-summ$coefficients[,"t value"]
  Fstat<-summ$fstatistic[1]
  tprob<-rep(1,length(tstat))
  Fprob<-1
  
  #perform permutations
  for(i in 1:nperm){
    rand<-sample(1:nrowsY)
    Yperm<-Y[rand,rand]
    yperm<-unfold(Yperm)
    fit<-lm(yperm~Xmats)
    summ<-summary(fit)
    Fprob<-Fprob+as.numeric(summ$fstatistic[1]>=Fstat)
    tprob<-tprob+as.numeric(abs(summ$coefficients[,"t value"])>=abs(tstat))
  }
  
  #return values
  tp<-tprob/(nperm+1)
  Fp<-Fprob/(nperm+1)
  names(r.squared)<-"r.squared"
  names(coeffs)<-c("Intercept",names(X))
  names(tstat)<-paste(c("Intercept",names(X)),"(t)",sep="")
  names(tp)<-paste(c("Intercept",names(X)),"(p)",sep="")
  names(Fstat)<-"F-statistic"
  names(Fp)<-"F p-value"
  return(list(r.squared=r.squared,
              coefficients=coeffs,
              tstatistic=tstat,
              tpvalue=tp,
              Fstatistic=Fstat,
              Fpvalue=Fp))
}

# unfold converts the lower diagonal elements of a matrix into a vector
# unfold is called by MMRR

unfold<-function(X){
  x<-vector()
  for(i in 2:nrow(X)) x<-c(x,X[i,1:i-1])
  return(x)
}