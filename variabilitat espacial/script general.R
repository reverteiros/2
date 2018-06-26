
#Dades--------------------

setwd("D:/Usuarios/s.reverte/OneDrive - CREAF/TESI/DADES/dades")
database2 <- read.table("morphs.txt",header=T)

tvuh <- read.table("tvuh.txt",header=T)
tvuh <- read.table("tvuh2.txt",header=T)
tvuf <- read.table("tvuf.txt",header=T)

tvuf2 <- tvuf[,-2]
tvuf3 <- tvuf2[,-1]
tvuh2 <- tvuh[,-1]
tvuhbray<-bray.part(tvuh2)
tvufbray<-bray.part(tvuf3)

mantel.test(x,y, nperm = 999)

x <- as.dist(tvuhbray$bray)
y <- as.dist(tvufbray$bray)

library(sp)
library(ape)
library(raster)
library(ggmap)
library(mapr)
library(rgbif)
library(dismo)
library(mapplots)
library(tidyr)
library(dplyr)
library(betapart)
library(vegan)
library(SpatialTools)
library(ggplot2)


# Comunitat vegetal---------------------------

## Bubble plot amb les esp?cies principals
plants <- read.table("plantes.txt", header=T)
plants$Plot <- c(1:40)
plants$X <- database2$X
plants$Y <- database2$Y

plants1 <- data.frame(plants[,4:24])
plants$Other_flowers <- apply(X = plants1,1,FUN = sum)
plants <- plants[,-(4:24)]

plants <- plants %>% 
  gather(species, abundance, c("ROF","TVU","OTHERS")) %>%
  arrange(Plot, X, Y, species)

xlim <- c(1.82,1.95)
ylim <- c(41.26,41.31)
xyz <- make.xyz(plants$X,plants$Y,plants$abundance,plants$species)
col <- c('red','green','blue')
basemap(xlim, ylim,bg='white')
draw.pie(xyz$x, xyz$y, xyz$z, radius = 0.005, col=col)
legend.pie(1.93,41.26,labels=unique(plants$species), radius=0.005, bty="n", col=col,cex=0.8, label.dist=1.3)

## Mantel correlogram

# Mantel correlogram amb la beta-diversitat quantitativa de flors amb Bray-Curtis:

pollinators<-read.table("flors quantitatiu.txt",header=T)

d <- as.matrix(database2[,11:12])
c<-dist1(d)
e<-c*100
d.dist<-as.dist(e)

quantitative.pollinators<-bray.part(pollinators)

mite.correlog <- mantel.correlog(quantitative.pollinators$bray, D.geo=d.dist, nperm=999)
summary(mite.correlog)


# Comunitat de polinitzadors--------------------------

## Exploraci? de les dades

bitxos <- read.table("bitxos qualitatiu.txt",header=T)
tbitxos <- t(bitxos)
plots <- apply(tbitxos,1,sum)
plots2 <- as.data.frame(plots)
plotsacagar <- data.frame(plots2$plots)
plotsacagar$species <- rownames(plots2)
names(plotsacagar) <- c("Plots.present","Species")
orderedplots <- plotsacagar[ order(-plotsacagar[,1]), ]


# Quantes esp?cies de bitxos estan presents en quantes parcel?les (fila superior nombre de parcel?les, fila inferior nombre d'esp?cies de bitxos). ?s a dir, 95 esp?cies de bitxos apareixen nom?s a 1 parcel?la, 20 esp?cies apareixen a 2 parcel?les, i successivament. En total tenim 170 esp?cies i 3577 individus.
table(orderedplots[,1])

# Nombre de parcel?les presents de cada esp?cie de bitxo (ordenades)
orderedplots$Plots.present

# Hi ha 28 esp?cies presents a 5 parcel?les o m?s. S?n aquestes:
rownames(orderedplots) <- c(1:length(orderedplots$Plots.present))
orderedplots[1:28,1:2]

## Bubble plot
plants <- read.table("bitxos quantitatiu.txt", header=T)
plants$Plot <- c(1:40)
plants$X <- database2$X
plants$Y <- database2$Y
plants2 <- plants %>% 
gather(species, abundance, 1:(ncol(plants)-3)) %>%
arrange(Plot, X, Y, species)

xlim <- c(1.82,1.95)
ylim <- c(41.26,41.31)
xyz <- make.xyz(plants2$X,plants2$Y,plants2$abundance,plants2$species)
col <- rainbow(24)
basemap(xlim, ylim,bg='white')
draw.pie(xyz$x, xyz$y, xyz$z, radius = 0.005, col=col)

## Bubble plots sense esp?cies que surten nom?s a una parcel?la
  
plants <- read.table("bitxos quantitatiu sense singletons.txt", header=T)
plants$Plot <- c(1:40)
plants$X <- database2$X
plants$Y <- database2$Y
plants2 <- plants %>% 
gather(species, abundance, 1:(ncol(plants)-3)) %>%
arrange(Plot, X, Y, species)

xlim <- c(1.82,1.95)
ylim <- c(41.26,41.31)
xyz <- make.xyz(plants2$X,plants2$Y,plants2$abundance,plants2$species)
col <- rainbow(24)
basemap(xlim, ylim)
draw.pie(xyz$x, xyz$y, xyz$z, radius = 0.005, col=col)

## Bubble plots sense apis

library(RColorBrewer)
darkcols <- brewer.pal(12, "Paired")


plants <- read.table("bitxos quantitatiu sense apis.txt", header=T)
plants$Plot <- c(1:40)
plants$X <- database2$X
plants$Y <- database2$Y

plants2 <- plants %>% 
gather(species, abundance, 1:(ncol(plants)-3)) %>%
arrange(Plot, X, Y, species)

xlim <- c(1.82,1.95)
ylim <- c(41.26,41.31)
xyz <- make.xyz(plants2$X,plants2$Y,plants2$abundance,plants2$species)

basemap(xlim, ylim,bg='white')
draw.pie(xyz$x, xyz$y, xyz$z, radius = 0.005, col=darkcols)

## Bubble plots comunitat pol?linitzadors sense apis i sense esp?cies que surten nom?s a una parcel?la

plants <- read.table("bitxos quantitatiu sense apis sense singletons.txt", header=T)
plants$Plot <- c(1:40)
plants$X <- database2$X
plants$Y <- database2$Y

plants2 <- plants %>% 
gather(species, abundance, 1:(ncol(plants)-3)) %>%
arrange(Plot, X, Y, species)

xlim <- c(1.82,1.95)
ylim <- c(41.26,41.31)
xyz <- make.xyz(plants2$X,plants2$Y,plants2$abundance,plants2$species)
col <- rainbow(24)
basemap(xlim, ylim)
draw.pie(xyz$x, xyz$y, xyz$z, radius = 0.005, col=col)


## Distrubicio espacial apis--------------------------
  
# Abund?ncia apis (nombre d'individus registrats per parcel?la)

coordinates(database2)<-c("X","Y")
crs(database2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
plot(gmap(database2, type = "satellite"))
points(Mercator(database2), col = "red", pch = 20, cex = database2$Honeybees/20)

# Autocorrelaci? espacial abund?ncia apis

zone.dists <- as.matrix(dist(cbind(database2$X, database2$Y)))
zone.dists.inv <- 1/zone.dists
diag(zone.dists.inv) <- 0

Moran.I(database2$Honeybees, zone.dists.inv)

# Taxa de visites d'Apis (nombre d'individus per cada 1000 flors)

database2$Honeybee_visit_rate <- database2$Honeybees/database2$OVERALL_Flowers*1000
plot(gmap(database2, type = "satellite"))
points(Mercator(database2), col = "red", pch = 20, cex = database2$Honeybee_visit_rate*1.2)

# Autocorrelaci? espacial taxa de visites apis

zone.dists <- as.matrix(dist(cbind(database2$X, database2$Y)))
zone.dists.inv <- 1/zone.dists
diag(zone.dists.inv) <- 0

Moran.I(database2$Honeybee_visit_rate, zone.dists.inv)

?Moran.I
database2$Honeybee_proportion <- database2$Honeybees/(database2$Honeybees+database2$Solitary)

# Proporci? d'Apis respecte el total de pol?linitzadors. El rang de la proporci? d'Apis dins les comunitats est? entre `r min(database2$Honeybee_proportion)` i `r max(database2$Honeybee_proportion)`.

plot(gmap(database2, type = "satellite"))
points(Mercator(database2), col = "red", pch = 20, cex = database2$Honeybee_proportion*10)

a <- Moran.I(database2$Honeybee_proportion, zone.dists.inv)


# Autocorrelaci? espacial proporci? d'Apis: `r a$p.value`


## Distribucio espacial resta de pol?linitzadors-----------------------

# Abund?ncia altres polinitzadors

plot(gmap(database2, type = "satellite"))
points(Mercator(database2), col = "red", pch = 20, cex = database2$Solitary/8)


# Autocorrelaci? espacial abund?ncia altres pol?linitzadors

zone.dists <- as.matrix(dist(cbind(database2$X, database2$Y)))
zone.dists.inv <- 1/zone.dists
diag(zone.dists.inv) <- 0

Moran.I(database2$Solitary, zone.dists.inv)

# Autocorrelaci? espacial taxa de visites altres pol?linitzadors

database2$Solitary_visit_rate <- database2$Solitary/database2$OVERALL_Flowers*1000
zone.dists <- as.matrix(dist(cbind(database2$X, database2$Y)))
zone.dists.inv <- 1/zone.dists
diag(zone.dists.inv) <- 0

Moran.I(database2$Solitary_visit_rate, zone.dists.inv)


## Corbes de rang abundancia-----------------

# Corba de rang-abund?ncia pol?linitzadors (amb apis)

bitxos <- read.table("bitxos quantitatiu.txt",header=T)
tbitxos <- t(bitxos)
plots <- apply(tbitxos,1,sum)
plots2 <- as.data.frame(plots)
plotsacagar <- data.frame(plots2$plots)
plotsacagar$species <- rownames(plots2)
names(plotsacagar) <- c("Abundance","Species")
orderedplots <- plotsacagar[ order(-plotsacagar[,1]), ]
barplot(orderedplots$Abundance)


# Corba de rang-abund?ncia pol?linitzadors (sense apis)

bitxos <- read.table("bitxos quantitatiu sense apis.txt",header=T)
tbitxos <- t(bitxos)
plots <- apply(tbitxos,1,sum)
plots2 <- as.data.frame(plots)
plotsacagar <- data.frame(plots2$plots)
plotsacagar$species <- rownames(plots2)
names(plotsacagar) <- c("Abundance","Species")
orderedplots <- plotsacagar[ order(-plotsacagar[,1]), ]
barplot(orderedplots$Abundance)


# Relacio entre comunitat animal i vegetal------------------

# Abund?ncia d'insectes - relaci? amb abund?ncia i riquesa de flors

mod1 <- lm(Pollinator_count ~  OVERALL_Flowers + flower_richness, data = database2 ) 
summary(mod1)
plot(Pollinator_count ~  OVERALL_Flowers, data = database2 )
plot(Pollinator_count ~  flower_richness, data = database2 )

# Riquesa d'insectes - relaci? amb abund?ncia i riquesa de flors

mod1 <- lm(Number.of.species.HL ~  OVERALL_Flowers + flower_richness, data = database2 ) 
summary(mod1)
plot(Number.of.species.HL ~  OVERALL_Flowers, data = database2 )
plot(Number.of.species.HL ~  flower_richness, data = database2 )



## Beta-diversitat de pol?linitzadors--------------------------

# Qualitatiu
pollinators<-read.table("bitxos qualitatiu.txt",header=T)
senseapis<-read.table("bitxos qualitatiu sense apis.txt",header=T)
plants<-read.table("flors qualitatiu.txt", header=T)

pollinators.beta.div<-beta.pair(pollinators, index.family="sorensen")
senseapis.beta.div <- beta.pair(senseapis, index.family="sorensen")
plants.beta.div<-beta.pair(plants, index.family="sorensen")

mean(pollinators.beta.div$beta.sor)
sd(pollinators.beta.div$beta.sor)

database2$visitrate <- database2$Pollinator_count/database2$OVERALL_Flowers*1000
mean(database2$visitrate)
sd(database2$visitrate)/mean(database2$visitrate)

# Mantel correlogram nestedness:

mite.correlog <- mantel.correlog(pollinators.beta.div$beta.sne, D.geo=d.dist, nperm=999)
mite.correlog  
mite.correlog <- mantel.correlog(senseapis.beta.div$beta.sne, D.geo=d.dist, nperm=999)
mite.correlog  
mite.correlog <- mantel.correlog(plants.beta.div$beta.sne, D.geo=d.dist, nperm=999)
mite.correlog  

# Mantel correlogram turnover:

mite.correlog <- mantel.correlog(pollinators.beta.div$beta.sim, D.geo=d.dist, nperm=999)
mite.correlog  
mite.correlog <- mantel.correlog(senseapis.beta.div$beta.sim, D.geo=d.dist, nperm=999)
mite.correlog  
mite.correlog <- mantel.correlog(plants.beta.div$beta.sim, D.geo=d.dist, nperm=999)
mite.correlog  

# Quantitatiu
# Mantel correlogram amb la beta-diversitat quantitativa d'insectes amb Bray-Curtis:

pollinators<-read.table("bitxos quantitatiu sense apis.txt",header=T)

quantitative.pollinators<-bray.part(pollinators)

mite.correlog <- mantel.correlog(quantitative.pollinators$bray, D.geo=d.dist, nperm=999)
mite.correlog  


# Mantel correlogram amb la beta-diversitat quantitativa d'insectes sense apis amb Bray-Curtis:

senseapis<-read.table("bitxos quantitatiu sense apis.txt",header=T)

quantitative.senseapis<-bray.part(senseapis)

mite.correlog <- mantel.correlog(quantitative.senseapis$bray, D.geo=d.dist, nperm=999)
mite.correlog  


# Mantel correlogram amb la beta-diversitat quantitativa d'insectes amb Bray-Curtis eliminant les esp?cies que nom?s surten a una parcel?la:

pollinators<-read.table("bitxos quantitatiu sense singletons.txt",header=T)

quantitative.pollinators<-bray.part(pollinators)

mite.correlog <- mantel.correlog(quantitative.pollinators$bray, D.geo=d.dist, nperm=999)
mite.correlog  


# Mantel correlogram amb la beta-diversitat quantitativa d'insectes sense apis amb Bray-Curtis eliminant les esp?cies que nom?s surten a una parcel?la:

pollinators<-read.table("bitxos quantitatiu sense apis sense singletons.txt",header=T)

quantitative.pollinators<-bray.part(pollinators)

mite.correlog <- mantel.correlog(quantitative.pollinators$bray, D.geo=d.dist, nperm=999)
mite.correlog    


# Interaccions-----------------------

# Corba de rang-abund?ncia interaccions (amb apis)

bitxos <- read.table("interaccions quantitatiu.txt",header=T)
tbitxos <- t(bitxos)
plots <- apply(tbitxos,1,sum)
plots2 <- as.data.frame(plots)
plotsacagar <- data.frame(plots2$plots)
plotsacagar$species <- rownames(plots2)
names(plotsacagar) <- c("Abundance","Species")
orderedplots <- plotsacagar[ order(-plotsacagar[,1]), ]
barplot(orderedplots$Abundance)
rownames(orderedplots) <- c(1:325)

# Corba de rang-abund?ncia interaccions (sense apis). La interacci? m?s abundant ?s Bibi?nid-Roman? amb 100, la que ve al darrera ?s Lasioglossum transitorium-Rosmarinus amb 36

bitxos <- read.table("interaccions quantitatiu sense apis.txt",header=T)
tbitxos <- t(bitxos)
plots <- apply(tbitxos,1,sum)
plots2 <- as.data.frame(plots)
plotsacagar <- data.frame(plots2$plots)
plotsacagar$species <- rownames(plots2)
names(plotsacagar) <- c("Abundance","Species")
orderedplots <- plotsacagar[ order(-plotsacagar[,1]), ]
barplot(orderedplots$Abundance)


## Beta-diversitat d'interaccions 

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

B15 = function(pm) with(pm,{(b+c)/(2*a+b+c)})
B_3 = function(pm) with(pm,{2*min(b,c)/(a+b+c)}) 
Brich = function(pm) with(pm,{abs(b-c)/(a+b+c)})

betalink  <-  function(w1, w2, bf=B15, quant = FALSE, calculate_2nd_decomposition = FALSE){
  if(quant == FALSE){
    #need to force matrices to binary  
    w1[w1>1] <- 1
    w2[w2>1] <- 1
    if(calculate_2nd_decomposition == TRUE){ 
      bf = B15
      
    }
    pmb = function(A,B) list(b=sum(!(A %in% B)),c=sum(!(B %in% A)),a=sum(B %in% A))
    sp1 = list(top=rownames(w1),bottom=colnames(w1),all=unique(c(colnames(w1),rownames(w1))))
    sp2 = list(top=rownames(w2),bottom=colnames(w2),all=unique(c(colnames(w2),rownames(w2))))
    beta_U = bf(pmb(sp1$top,sp2$top))
    beta_L = bf(pmb(sp1$bottom,sp2$bottom))
    beta_S = bf(pmb(sp1$all,sp2$all))
    #calculate species beta diversity decompositions
    if(calculate_2nd_decomposition == TRUE){ 
      beta_U_3 = B_3(pmb(sp1$top,sp2$top))
      beta_Urich = Brich(pmb(sp1$top,sp2$top))
      beta_L_3 = B_3(pmb(sp1$bottom,sp2$bottom))
      beta_Lrich = Brich(pmb(sp1$bottom,sp2$bottom))
      beta_S_3 = B_3(pmb(sp1$all,sp2$all))
      beta_Srich = Brich(pmb(sp1$all,sp2$all))
    }
    # Common species
    Csp = sp1$all[sp1$all %in% sp2$all]
    CUsp = sp1$top[sp1$top %in% sp2$top]
    CLsp = sp1$bottom[sp1$bottom %in% sp2$bottom]
    #if((length(CUsp)>0) & (length(CLsp)>0)){ 
    #WHY? You can calculate it the same for 0 overlap!
    w1Con = w1[CUsp,CLsp]
    w2Con = w2[CUsp,CLsp]
    nCon = sum((w1Con == w2Con) & (w1Con == 1))
    pmBos = list(b=sum(w1Con)-nCon,c=sum(w2Con)-nCon,a=nCon)
    pmBwn = list(b=sum(w1)-nCon,c=sum(w2)-nCon,a=nCon)
    beta_OS = bf(pmBos)
    beta_WN = bf(pmBwn)
    if(is.na(beta_OS)) beta_OS = 0
    if(is.na(beta_WN)) beta_WN = 0
    beta_ST = beta_WN - beta_OS
    #calculate link second decomposition
    if(calculate_2nd_decomposition == TRUE){ 
      beta_3 = B_3(pmBwn)
      beta_rich = Brich(pmBwn)
      beta_OS_3 = B_3(pmBos)
      beta_OSrich = Brich(pmBos)    
      #beta_ST_3 = beta_3 - beta_OS
      #beta_STrich = beta_ST - beta_ST_3
      #beta_ST_3 = beta_3 - beta_OS_3
      #beta_STrich = beta_rich - beta_OSrich
      if(is.na(beta_3)) beta_3 = 0
      if(is.na(beta_rich)) beta_rich = 0
      if(is.na(beta_OS_3)) beta_OS_3 = 0
      if(is.na(beta_OSrich)) beta_OSrich = 0
      #if(is.na(beta_ST_3)) beta_ST_3 = 0
      #if(is.na(beta_STrich)) beta_STrich = 0
    }            
    if(beta_WN > 0){
      b_contrib = beta_ST / beta_WN
      #reltive contribs may be added here
    } else {
      b_contrib = 0
    }
    #} else {
    #  beta_WN = 1 #why can you not calculate WN when all species are different?
    #NB makes this change after thinking carefully. I think no overlap implies maximum 
    #beta, not minimum!!
    #NEW addition: beta_WN is already 1 in first instance
    #  beta_OS = 0
    #  beta_ST = 1
    #  b_contrib = 1
    #calculate link second decomposition 
    #  if(calculate_2nd_decomposition == TRUE){ 
    #      beta_3 = NA #when there is no overlap can't say why, right?
    #      beta_rich = NA
    #      beta_OS_3 = 0
    #      beta_OSrich = 0    
    #beta_ST_3 = 0
    #beta_STrich = 0
    #  }            
    #}
    if(calculate_2nd_decomposition == TRUE){
      return(list(U = beta_U, L = beta_L, S = beta_S, OS = beta_OS, 
                  WN = beta_WN, ST = beta_ST, contrib = b_contrib,
                  U_3 = beta_U_3, Urich = beta_Urich, L_3 = beta_L_3, 
                  Lrich = beta_Lrich, S_3 = beta_S_3, Srich = beta_Srich, 
                  WN_3 = beta_3, WNrich = beta_rich, OS_3 = beta_OS_3, 
                  OSrich = beta_OSrich)) #, ST_3 = beta_ST_3,
      #STrich = beta_STrich))
    }else{    
      return(list(U = beta_U, L = beta_L, S = beta_S, OS = beta_OS, 
                  WN = beta_WN, ST = beta_ST, contrib = b_contrib))
    }
  } 
  if(quant == TRUE){
    bf = B15
    # a sum of minima in among-site comparisons; b and c are its sum minus a.
    pmb = function(A,B) list(b= sum(A) - sum(pmin(A, B)), c = sum(B) - sum(pmin(A, B)), a = sum(pmin(A, B)))
    #species level
    rows_ <- unique(c(rownames(w1), rownames(w2)))
    cols_ <- unique(c(colnames(w1), colnames(w2)))  
    empty <- matrix(0, nrow = length(rows_), ncol = length(cols_), dimnames = list(rows_, cols_))
    w1full <- empty
    w2full <- empty
    w1full[rownames(w1), colnames(w1)]  <- w1
    w2full[rownames(w2), colnames(w2)]  <- w2
    sp1 = list(top=rowSums(w1full),bottom=colSums(w1full),all=c(rowSums(w1full),colSums(w1full)))
    sp2 = list(top=rowSums(w2full),bottom=colSums(w2full),all=c(rowSums(w2full),colSums(w2full)))
    beta_U = bf(pmb(sp1$top,sp2$top))
    beta_L = bf(pmb(sp1$bottom,sp2$bottom))
    beta_S = bf(pmb(sp1$all,sp2$all))
    #calculate species beta diversity decompositions
    if(calculate_2nd_decomposition == TRUE){ 
      beta_U_3 = B_3(pmb(sp1$top,sp2$top))
      beta_Urich = Brich(pmb(sp1$top,sp2$top))
      beta_L_3 = B_3(pmb(sp1$bottom,sp2$bottom))
      beta_Lrich = Brich(pmb(sp1$bottom,sp2$bottom))
      beta_S_3 = B_3(pmb(sp1$all,sp2$all))
      beta_Srich = Brich(pmb(sp1$all,sp2$all))
    }
    # Common species
    sp1 = list(top=rownames(w1),bottom=colnames(w1),all=unique(c(colnames(w1),rownames(w1))))
    sp2 = list(top=rownames(w2),bottom=colnames(w2),all=unique(c(colnames(w2),rownames(w2))))
    Csp = sp1$all[sp1$all %in% sp2$all]
    CUsp = sp1$top[sp1$top %in% sp2$top]
    CLsp = sp1$bottom[sp1$bottom %in% sp2$bottom]
    #if((length(CUsp)>0) & (length(CLsp)>0)){ #see above
    w1Con = w1[CUsp,CLsp]
    w2Con = w2[CUsp,CLsp]
    nCon = sum(pmin(w1Con, w2Con))
    nfull = sum(pmin(w1full, w2full))
    #sum((w1Con == w2Con) & (w1Con == 1)) #this wont work with freq data!!!
    pmBos = list(b=sum(w1Con)-nCon,c=sum(w2Con)-nCon,a=nCon)
    #need to use them comparable in size
    pmBwn = list(b=sum(w1full)-nfull,c=sum(w2full)-nfull,a=nfull)
    beta_OS = bf(pmBos)
    beta_WN = bf(pmBwn)
    if(is.na(beta_OS)) beta_OS = 0
    if(is.na(beta_WN)) beta_WN = 0
    beta_ST = beta_WN - beta_OS
    #calculate link second decomposition
    if(calculate_2nd_decomposition == TRUE){ 
      beta_3 = B_3(pmBwn)
      beta_rich = Brich(pmBwn)
      beta_OS_3 = B_3(pmBos)
      beta_OSrich = Brich(pmBos)    
      #beta_ST_3 = beta_3 - beta_OS
      #beta_STrich = beta_ST - beta_ST_3
      #beta_ST_3 = beta_3 - beta_OS_3
      #beta_STrich = beta_rich - beta_OSrich
      if(is.na(beta_3)) beta_3 = 0
      if(is.na(beta_rich)) beta_rich = 0
      if(is.na(beta_OS_3)) beta_OS_3 = 0
      if(is.na(beta_OSrich)) beta_OSrich = 0
      #if(is.na(beta_ST_3)) beta_ST_3 = 0
      #if(is.na(beta_STrich)) beta_STrich = 0
    }            
    if(beta_WN > 0){
      b_contrib = beta_ST / beta_WN
      #reltive contribs may be added here
    } else {
      b_contrib = 0
    }
    #} else {
    # beta_WN = 1 #why can you not calculate WN when all species are different?
    #idem reasoning as before, no overlap, max WN
    #  beta_OS = 0
    #  beta_ST = 1
    #  b_contrib = 1
    #calculate link second decomposition 
    #  if(calculate_2nd_decomposition == TRUE){ 
    #  beta_3 = NA
    #  beta_rich = NA
    #  beta_OS_3 = 0
    #  beta_OSrich = 0    
    #beta_ST_3 = 0
    #beta_STrich = 0
    #}            
    #}
    if(calculate_2nd_decomposition == TRUE){
      return(list(U = beta_U, L = beta_L, S = beta_S, OS = beta_OS, 
                  WN = beta_WN, ST = beta_ST, contrib = b_contrib,
                  U_3 = beta_U_3, Urich = beta_Urich, L_3 = beta_L_3, 
                  Lrich = beta_Lrich, S_3 = beta_S_3, Srich = beta_Srich, 
                  WN_3 = beta_3, WNrich = beta_rich, OS_3 = beta_OS_3, 
                  OSrich = beta_OSrich)) #, ST_3 = beta_ST_3,
      #STrich = beta_STrich))
    }else{    
      return(list(U = beta_U, L = beta_L, S = beta_S, OS = beta_OS, 
                  WN = beta_WN, ST = beta_ST, contrib = b_contrib))
    }
  }
}

betalink.dist <-  function(W, calculate_2nd_decomposition = TRUE, quant = FALSE, ...){
  dWN = matrix(NA,ncol=length(W),nrow=length(W))
  colnames(dWN) = names(W)
  rownames(dWN) = names(W)
  dOS = dWN
  dS = dWN
  dST = dWN
  dContrib = dWN
  #add new metrics
  dU_3 = dWN #this fails when second decomposition is FALSE!!! FIX.
  dUrich = dWN
  dL_3 = dWN
  dLrich = dWN
  dS_3 = dWN
  dSrich = dWN
  dWN_3 = dWN
  dWNrich = dWN
  dOS_3 = dWN
  dOSrich = dWN
  #dST_3 = dWN
  #dSTrich = dWN
  for(i in c(1:(length(W)-1))){
    for(j in c((i+1):(length(W)))){
      partition = betalink(W[[i]],W[[j]], calculate_2nd_decomposition = calculate_2nd_decomposition,
                           quant = quant)
      dWN[j,i]		= partition$WN
      dOS[j,i]		= partition$OS
      dS[j,i]			= partition$S
      dST[j,i]		= partition$ST
      dContrib[j,i]    = partition$contrib
      dU_3[j,i]	    = partition$U_3
      dUrich[j,i]    = partition$Urich
      dL_3[j,i]    = partition$L_3
      dLrich[j,i]    = partition$Lrich
      dS_3[j,i]    = partition$S_3
      dSrich[j,i]    = partition$Srich
      dWN_3[j,i]    = partition$WN_3
      dWNrich[j,i]    = partition$WNrich
      dOS_3[j,i]    = partition$OS_3
      dOSrich[j,i]    = partition$OSrich
      #dST_3[j,i]    = partition$ST_3
      #dSTrich[j,i]    = partition$STrich
    }
  }
  distances = list(WN=dWN, OS=dOS, S=dS, ST=dST, contrib=dContrib,
                   U_3=dU_3, Urich=dUrich, L_3=dL_3, Lrich=dLrich,
                   S_3=dS_3, Srich=dSrich, WN_3=dWN_3, WNrich=dWNrich,
                   OS_3=dOS_3, OSrich=dOSrich) #, ST_3=dST_3, STrich=dSTrich)
  distances = lapply(distances,as.dist)
  return(distances)
}


llistaaa<-list(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,
               t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,
               t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40)

taa<-betalink.dist(llistaaa, quant = T)
faa<-betalink.dist(llistaaa, quant = F)


# Mantel correlogram amb la beta-diversitat quantitativa d'interaccions:

mite.correlog <- mantel.correlog(taa$WN, D.geo=d.dist, nperm=999)
mite.correlog    

# Mantel correlogram amb la beta-diversitat quantitativa d'interaccions, component de rewiring:

mite.correlog <- mantel.correlog(taa$OS, D.geo=d.dist, nperm=999)
mite.correlog    


# Mantel correlogram amb la beta-diversitat quantitativa d'interaccions sense apis:


taula1<-read.table("Xarxes/sense apis/taula1.txt", header=T,colnames(1))
taula2<-read.table("Xarxes/sense apis/taula2.txt", header=T,colnames(1))
taula3<-read.table("Xarxes/sense apis/taula3.txt", header=T,colnames(1))
taula4<-read.table("Xarxes/sense apis/taula4.txt", header=T,colnames(1))
taula5<-read.table("Xarxes/sense apis/taula5.txt", header=T,colnames(1))
taula6<-read.table("Xarxes/sense apis/taula6.txt", header=T,colnames(1))
taula7<-read.table("Xarxes/sense apis/taula7.txt", header=T,colnames(1))
taula8<-read.table("Xarxes/sense apis/taula8.txt", header=T,colnames(1))
taula9<-read.table("Xarxes/sense apis/taula9.txt", header=T,colnames(1))
taula10<-read.table("Xarxes/sense apis/taula10.txt", header=T,colnames(1))
taula11<-read.table("Xarxes/sense apis/taula11.txt", header=T,colnames(1))
taula12<-read.table("Xarxes/sense apis/taula12.txt", header=T,colnames(1))
taula13<-read.table("Xarxes/sense apis/taula13.txt", header=T,colnames(1))
taula14<-read.table("Xarxes/sense apis/taula14.txt", header=T,colnames(1))
taula15<-read.table("Xarxes/sense apis/taula15.txt", header=T,colnames(1))
taula16<-read.table("Xarxes/sense apis/taula16.txt", header=T,colnames(1))
taula17<-read.table("Xarxes/sense apis/taula17.txt", header=T,colnames(1))
taula18<-read.table("Xarxes/sense apis/taula18.txt", header=T,colnames(1))
taula19<-read.table("Xarxes/sense apis/taula19.txt", header=T,colnames(1))
taula20<-read.table("Xarxes/sense apis/taula20.txt", header=T,colnames(1))
taula21<-read.table("Xarxes/sense apis/taula21.txt", header=T,colnames(1))
taula22<-read.table("Xarxes/sense apis/taula22.txt", header=T,colnames(1))
taula23<-read.table("Xarxes/sense apis/taula23.txt", header=T,colnames(1))
taula24<-read.table("Xarxes/sense apis/taula24.txt", header=T,colnames(1))
taula25<-read.table("Xarxes/sense apis/taula25.txt", header=T,colnames(1))
taula26<-read.table("Xarxes/sense apis/taula26.txt", header=T,colnames(1))
taula27<-read.table("Xarxes/sense apis/taula27.txt", header=T,colnames(1))
taula28<-read.table("Xarxes/sense apis/taula28.txt", header=T,colnames(1))
taula29<-read.table("Xarxes/sense apis/taula29.txt", header=T,colnames(1))
taula30<-read.table("Xarxes/sense apis/taula30.txt", header=T,colnames(1))
taula31<-read.table("Xarxes/sense apis/taula31.txt", header=T,colnames(1))
taula32<-read.table("Xarxes/sense apis/taula32.txt", header=T,colnames(1))
taula33<-read.table("Xarxes/sense apis/taula33.txt", header=T,colnames(1))
taula34<-read.table("Xarxes/sense apis/taula34.txt", header=T,colnames(1))
taula35<-read.table("Xarxes/sense apis/taula35.txt", header=T,colnames(1))
taula36<-read.table("Xarxes/sense apis/taula36.txt", header=T,colnames(1))
taula37<-read.table("Xarxes/sense apis/taula37.txt", header=T,colnames(1))
taula38<-read.table("Xarxes/sense apis/taula38.txt", header=T,colnames(1))
taula39<-read.table("Xarxes/sense apis/taula39.txt", header=T,colnames(1))
taula40<-read.table("Xarxes/sense apis/taula40.txt", header=T,colnames(1))


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


llistasa<-list(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,
t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,
t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40)

tsa<-betalink.dist(llistasa, quant = T)
fsa<-betalink.dist(llistasa, quant = F)

mite.correlog <- mantel.correlog(tsa$WN, D.geo=d.dist, nperm=999)
mite.correlog    


# Mantel correlogram amb la beta-diversitat quantitativa d'interaccions sense apis, component rewiring:
  
mite.correlog <- mantel.correlog(tsa$OS, D.geo=d.dist, nperm=999)
mite.correlog    


# Mantel correlogram amb la beta-diversitat quantitativa d'interaccions sense singletons:

taula1<-read.table("Xarxes/sense singletons/taula1.txt", header=T,colnames(1))
taula2<-read.table("Xarxes/sense singletons/taula2.txt", header=T,colnames(1))
taula3<-read.table("Xarxes/sense singletons/taula3.txt", header=T,colnames(1))
taula4<-read.table("Xarxes/sense singletons/taula4.txt", header=T,colnames(1))
taula5<-read.table("Xarxes/sense singletons/taula5.txt", header=T,colnames(1))
taula6<-read.table("Xarxes/sense singletons/taula6.txt", header=T,colnames(1))
taula7<-read.table("Xarxes/sense singletons/taula7.txt", header=T,colnames(1))
taula8<-read.table("Xarxes/sense singletons/taula8.txt", header=T,colnames(1))
taula9<-read.table("Xarxes/sense singletons/taula9.txt", header=T,colnames(1))
taula10<-read.table("Xarxes/sense singletons/taula10.txt", header=T,colnames(1))
taula11<-read.table("Xarxes/sense singletons/taula11.txt", header=T,colnames(1))
taula12<-read.table("Xarxes/sense singletons/taula12.txt", header=T,colnames(1))
taula13<-read.table("Xarxes/sense singletons/taula13.txt", header=T,colnames(1))
taula14<-read.table("Xarxes/sense singletons/taula14.txt", header=T,colnames(1))
taula15<-read.table("Xarxes/sense singletons/taula15.txt", header=T,colnames(1))
taula16<-read.table("Xarxes/sense singletons/taula16.txt", header=T,colnames(1))
taula17<-read.table("Xarxes/sense singletons/taula17.txt", header=T,colnames(1))
taula18<-read.table("Xarxes/sense singletons/taula18.txt", header=T,colnames(1))
taula19<-read.table("Xarxes/sense singletons/taula19.txt", header=T,colnames(1))
taula20<-read.table("Xarxes/sense singletons/taula20.txt", header=T,colnames(1))
taula21<-read.table("Xarxes/sense singletons/taula21.txt", header=T,colnames(1))
taula22<-read.table("Xarxes/sense singletons/taula22.txt", header=T,colnames(1))
taula23<-read.table("Xarxes/sense singletons/taula23.txt", header=T,colnames(1))
taula24<-read.table("Xarxes/sense singletons/taula24.txt", header=T,colnames(1))
taula25<-read.table("Xarxes/sense singletons/taula25.txt", header=T,colnames(1))
taula26<-read.table("Xarxes/sense singletons/taula26.txt", header=T,colnames(1))
taula27<-read.table("Xarxes/sense singletons/taula27.txt", header=T,colnames(1))
taula28<-read.table("Xarxes/sense singletons/taula28.txt", header=T,colnames(1))
taula29<-read.table("Xarxes/sense singletons/taula29.txt", header=T,colnames(1))
taula30<-read.table("Xarxes/sense singletons/taula30.txt", header=T,colnames(1))
taula31<-read.table("Xarxes/sense singletons/taula31.txt", header=T,colnames(1))
taula32<-read.table("Xarxes/sense singletons/taula32.txt", header=T,colnames(1))
taula33<-read.table("Xarxes/sense singletons/taula33.txt", header=T,colnames(1))
taula34<-read.table("Xarxes/sense singletons/taula34.txt", header=T,colnames(1))
taula35<-read.table("Xarxes/sense singletons/taula35.txt", header=T,colnames(1))
taula36<-read.table("Xarxes/sense singletons/taula36.txt", header=T,colnames(1))
taula37<-read.table("Xarxes/sense singletons/taula37.txt", header=T,colnames(1))
taula38<-read.table("Xarxes/sense singletons/taula38.txt", header=T,colnames(1))
taula39<-read.table("Xarxes/sense singletons/taula39.txt", header=T,colnames(1))
taula40<-read.table("Xarxes/sense singletons/taula40.txt", header=T,colnames(1))

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


llistass<-list(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,
t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,
t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40)

tss<-betalink.dist(llistass, quant = T)
fss<-betalink.dist(llistass, quant = F)


mite.correlog <- mantel.correlog(tss$WN, D.geo=d.dist, nperm=999)
mite.correlog    



# Mantel correlogram amb la beta-diversitat quantitativa d'interaccions sense singletons i sense apis:
  
taula1<-read.table("Xarxes/sense singletons/sense apis/taula1.txt", header=T,colnames(1))
taula2<-read.table("Xarxes/sense singletons/sense apis/taula2.txt", header=T,colnames(1))
taula3<-read.table("Xarxes/sense singletons/sense apis/taula3.txt", header=T,colnames(1))
taula4<-read.table("Xarxes/sense singletons/sense apis/taula4.txt", header=T,colnames(1))
taula5<-read.table("Xarxes/sense singletons/sense apis/taula5.txt", header=T,colnames(1))
taula6<-read.table("Xarxes/sense singletons/sense apis/taula6.txt", header=T,colnames(1))
taula7<-read.table("Xarxes/sense singletons/sense apis/taula7.txt", header=T,colnames(1))
taula8<-read.table("Xarxes/sense singletons/sense apis/taula8.txt", header=T,colnames(1))
taula9<-read.table("Xarxes/sense singletons/sense apis/taula9.txt", header=T,colnames(1))
taula10<-read.table("Xarxes/sense singletons/sense apis/taula10.txt", header=T,colnames(1))
taula11<-read.table("Xarxes/sense singletons/sense apis/taula11.txt", header=T,colnames(1))
taula12<-read.table("Xarxes/sense singletons/sense apis/taula12.txt", header=T,colnames(1))
taula13<-read.table("Xarxes/sense singletons/sense apis/taula13.txt", header=T,colnames(1))
taula14<-read.table("Xarxes/sense singletons/sense apis/taula14.txt", header=T,colnames(1))
taula15<-read.table("Xarxes/sense singletons/sense apis/taula15.txt", header=T,colnames(1))
taula16<-read.table("Xarxes/sense singletons/sense apis/taula16.txt", header=T,colnames(1))
taula17<-read.table("Xarxes/sense singletons/sense apis/taula17.txt", header=T,colnames(1))
taula18<-read.table("Xarxes/sense singletons/sense apis/taula18.txt", header=T,colnames(1))
taula19<-read.table("Xarxes/sense singletons/sense apis/taula19.txt", header=T,colnames(1))
taula20<-read.table("Xarxes/sense singletons/sense apis/taula20.txt", header=T,colnames(1))
taula21<-read.table("Xarxes/sense singletons/sense apis/taula21.txt", header=T,colnames(1))
taula22<-read.table("Xarxes/sense singletons/sense apis/taula22.txt", header=T,colnames(1))
taula23<-read.table("Xarxes/sense singletons/sense apis/taula23.txt", header=T,colnames(1))
taula24<-read.table("Xarxes/sense singletons/sense apis/taula24.txt", header=T,colnames(1))
taula25<-read.table("Xarxes/sense singletons/sense apis/taula25.txt", header=T,colnames(1))
taula26<-read.table("Xarxes/sense singletons/sense apis/taula26.txt", header=T,colnames(1))
taula27<-read.table("Xarxes/sense singletons/sense apis/taula27.txt", header=T,colnames(1))
taula28<-read.table("Xarxes/sense singletons/sense apis/taula28.txt", header=T,colnames(1))
taula29<-read.table("Xarxes/sense singletons/sense apis/taula29.txt", header=T,colnames(1))
taula30<-read.table("Xarxes/sense singletons/sense apis/taula30.txt", header=T,colnames(1))
taula31<-read.table("Xarxes/sense singletons/sense apis/taula31.txt", header=T,colnames(1))
taula32<-read.table("Xarxes/sense singletons/sense apis/taula32.txt", header=T,colnames(1))
taula33<-read.table("Xarxes/sense singletons/sense apis/taula33.txt", header=T,colnames(1))
taula34<-read.table("Xarxes/sense singletons/sense apis/taula34.txt", header=T,colnames(1))
taula35<-read.table("Xarxes/sense singletons/sense apis/taula35.txt", header=T,colnames(1))
taula36<-read.table("Xarxes/sense singletons/sense apis/taula36.txt", header=T,colnames(1))
taula37<-read.table("Xarxes/sense singletons/sense apis/taula37.txt", header=T,colnames(1))
taula38<-read.table("Xarxes/sense singletons/sense apis/taula38.txt", header=T,colnames(1))
taula39<-read.table("Xarxes/sense singletons/sense apis/taula39.txt", header=T,colnames(1))
taula40<-read.table("Xarxes/sense singletons/sense apis/taula40.txt", header=T,colnames(1))

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


llistasssa<-list(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,
                 t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,
                 t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40)

tsssa<-betalink.dist(llistasssa, quant = T)
fsssa<-betalink.dist(llistasssa, quant = F)

mite.correlog <- mantel.correlog(tsssa$WN, D.geo=d.dist, nperm=999)
mite.correlog    

# Grafics beta-diversitat distancia-----------------

pollinators<-read.table("bitxos quantitatiu.txt",header=T)
senseapis <- read.table("bitxos quantitatiu sense apis.txt", header=T)
plants<-read.table("flors quantitatiu.txt", header=T)

quantitative.pollinators<-bray.part(pollinators)
quantitative.senseapis<-bray.part(senseapis)
quantitative.plants<-bray.part(plants)

geographicdist <- data.frame(t(combn(rownames(pollinators),2)), as.numeric(d.dist))
names(geographicdist) <- c("Plot", "Plot", "Geographicaldistance")

polldist <- data.frame(t(combn(rownames(pollinators),2)), as.numeric(quantitative.pollinators$bray))
names(polldist) <- c("Plot", "Plot", "Pollinatorsdistance")

senseapisdist <- data.frame(t(combn(rownames(pollinators),2)), as.numeric(quantitative.senseapis$bray))
names(senseapisdist) <- c("Plot", "Plot", "Senseapisdistance")

interaccionsdist <- data.frame(t(combn(rownames(pollinators),2)), as.numeric(taa$WN))
names(interaccionsdist) <- c("Plot", "Plot", "WNdistance")

rewiringdist <- data.frame(t(combn(rownames(pollinators),2)), as.numeric(taa$OS))
names(rewiringdist) <- c("Plot", "Plot", "OSdistance")

interaccionssadist <- data.frame(t(combn(rownames(pollinators),2)), as.numeric(tsa$WN))
names(interaccionssadist) <- c("Plot", "Plot", "WNsadistance")

rewiringsadist <- data.frame(t(combn(rownames(pollinators),2)), as.numeric(tsa$OS))
names(rewiringsadist) <- c("Plot", "Plot", "OSsadistance")

plantdist <- data.frame(t(combn(rownames(pollinators),2)), as.numeric(quantitative.plants$bray))
names(plantdist) <- c("Plot", "Plot", "Plantdistance")


aa<-data.frame(geographicdist$Geographicaldistance, polldist$Pollinatorsdistance,plantdist$Plantdistance, interaccionsdist$WNdistance,rewiringdist$OSdistance)
zz<-c("geographicdist","polldist","plantdist","WNdistance","OSdistance")
names(aa)<-zz

sa<-data.frame(geographicdist$Geographicaldistance, senseapisdist$Senseapisdistance,plantdist$Plantdistance, interaccionssadist$WNsadistance,rewiringsadist$OSsadistance)
names(sa)<-zz

g <- ggplot(aa, aes(x=geographicdist))
g <- g + geom_smooth(aes(y=plantdist), colour="green")+coord_cartesian(ylim = c(0, 1))
g <- g + geom_smooth(aes(y=polldist), colour="blue")+coord_cartesian(ylim = c(0, 1))
g <- g + geom_smooth(aes(y=WNdistance), colour="red")+coord_cartesian(ylim = c(0, 1))
g <- g + geom_smooth(aes(y=OSdistance), colour="orange")+coord_cartesian(ylim = c(0, 1))
g

g <- ggplot(sa, aes(x=geographicdist))
g <- g + geom_smooth(aes(y=plantdist), colour="green")+coord_cartesian(ylim = c(0, 1))
g <- g + geom_smooth(aes(y=polldist), colour="blue")+coord_cartesian(ylim = c(0, 1))
g <- g + geom_smooth(aes(y=WNdistance), colour="red")+coord_cartesian(ylim = c(0, 1))
g <- g + geom_smooth(aes(y=OSdistance), colour="orange")+coord_cartesian(ylim = c(0, 1))
g


# CCA---------------

library(vegan)

names(database2)
names(particio)

particio$Rofapisrate <- particio$Rofapis/database2$OVERALL_Flowers*1000

cor.test(particio$Tvuhapisrate,particio$Tvuhwildrate)
summary(a)

plot(database2$Number.of.species.HL,database2$Honeybeerate)

plantes <- data.frame(database2$ROF_Flowers,database2$TVUF_Flowers,database2$TVUH_Flowers,database2$other_flowers_abundance,database2$flower_richness)
names(plantes) <- c("ROF","TVUF","TVUH","OTHERS","RICH")

particio$Othersapisrate <- particio$OTHERSHoneybees/database2$other_flowers_abundance*1000
particio$Otherswildrate <- particio$OTHERSWild/database2$other_flowers_abundance*1000
particio$Rofapisrate <- particio$ROFHoneybees/database2$ROF_Flowers*1000
particio$Rofwildrate <- particio$ROFWild/database2$ROF_Flowers*1000
particio$Tvufapisrate <- particio$TVUFHoneybees/database2$TVUF_Flowers*1000
particio$Tvufwildrate <- particio$TVUFWild/database2$TVUF_Flowers*1000
particio$Tvuhapisrate <- particio$TVUHHoneybees/database2$TVUH_Flowers*1000
particio$Tvuhwildrate <- particio$TVUHWild/database2$TVUH_Flowers*1000

pollinators <- data.frame(particio$Othersapisrate,particio$Otherswildrate,particio$Rofapisrate,particio$Rofwildrate,particio$Tvufapisrate,particio$Tvufwildrate,particio$Tvuhapisrate,particio$Tvuhwildrate)
names(pollinators) <- c("Othersapisrate","Otherswildrate","Rofapisrate","Rofwildrate","Tvufapisrate","Tvufwildrate","Tvuhapisrate","Tvuhwildrate")
set.seed(2)
x <- CCorA(pollinators, plantes)
biplot(x$corr.X.Cx,x$corr.Y.Cy)



# Mantels------
# Qualitatiu
pollinators<-read.table("bitxos qualitatiu.txt",header=T)
senseapis<-read.table("bitxos qualitatiu sense apis.txt",header=T)
plants<-read.table("flors qualitatiu.txt", header=T)

pollinators.beta.div<-beta.pair(pollinators, index.family="sorensen")
senseapis.beta.div <- beta.pair(senseapis, index.family="sorensen")
plants.beta.div<-beta.pair(plants, index.family="sorensen")

#nestedness
plantnest <- data.frame(t(combn(rownames(pollinators),2)), as.numeric(plants.beta.div$beta.sne))
names(plantnest) <- c("Plot", "Plot", "plantnest")

pollinatorsnest <- data.frame(t(combn(rownames(pollinators),2)), as.numeric(pollinators.beta.div$beta.sne))
names(pollinatorsnest) <- c("Plot", "Plot", "pollinatorsnest")

senseapisnest <- data.frame(t(combn(rownames(pollinators),2)), as.numeric(senseapis.beta.div$beta.sne))
names(senseapisnest) <- c("Plot", "Plot", "senseapisnest")

#turnover
plantturn <- data.frame(t(combn(rownames(pollinators),2)), as.numeric(plants.beta.div$beta.sim))
names(plantturn) <- c("Plot", "Plot", "plantturn")

pollinatorturn <- data.frame(t(combn(rownames(pollinators),2)), as.numeric(pollinators.beta.div$beta.sim))
names(pollinatorturn) <- c("Plot", "Plot", "pollinatorturn")

senseapisturn <- data.frame(t(combn(rownames(pollinators),2)), as.numeric(senseapis.beta.div$beta.sim))
names(senseapisturn) <- c("Plot", "Plot", "senseapisturn")

#beta
plantsbeta <- data.frame(t(combn(rownames(pollinators),2)), as.numeric(plants.beta.div$beta.sor))
names(plantsbeta) <- c("Plot", "Plot", "plantsbeta")

pollinatorsbeta <- data.frame(t(combn(rownames(pollinators),2)), as.numeric(pollinators.beta.div$beta.sor))
names(pollinatorsbeta) <- c("Plot", "Plot", "pollinatorsbeta")

senseapisbeta <- data.frame(t(combn(rownames(pollinators),2)), as.numeric(senseapis.beta.div$beta.sor))
names(senseapisbeta) <- c("Plot", "Plot", "senseapisbeta")

plants1 <- plantsbeta
plants1$plantturn <- plantturn$plantturn
plants1$plantnest <- plantnest$plantnest
plants1$nestprop <- plants1$plantnest/plants1$plantsbeta
plants1$turnprop <- plants1$plantturn/plants1$plantsbeta

polls1 <- pollinatorsbeta
polls1$pollinatorturn <- pollinatorturn$pollinatorturn
polls1$pollinatorsnest <- pollinatorsnest$pollinatorsnest
polls1$nestprop <- polls1$pollinatorsnest/polls1$pollinatorsbeta
polls1$turnprop <- polls1$pollinatorturn/polls1$pollinatorsbeta


s1 <- lm(plants1$plantnest~polls1$pollinatorsnest)
summary(s1)
plot(plants1$plantnest~polls1$pollinatorsnest,xlab="Pollinator nestedness",ylab="Plant nestedness")
abline(s1)

s1 <- lm(plants1$plantturn~polls1$pollinatorturn)
summary(s1)
plot(plants1$plantturn~polls1$pollinatorturn,xlab="Pollinator turnover",ylab="Plant turnover")
abline(s1)

hist(plants1$nestprop,xlab="Proportion of plant nestedness")
hist(plants1$turnprop,xlab="Proportion of plant turnover")

hist(polls1$nestprop,xlab="Proportion of pollinator nestedness")
hist(polls1$turnprop,xlab="Proportion of pollinator turnover")


# Quantitatiu

pollinators<-read.table("bitxos quantitatiu.txt",header=T)
senseapis<-read.table("bitxos quantitatiu sense apis.txt",header=T)
plants<-read.table("flors quantitatiu.txt", header=T)

quantitative.pollinators<-bray.part(pollinators)
quantitative.senseapis<-bray.part(senseapis)
quantitative.plants<-bray.part(plants)

# Transformacions dades

geographicdist <- as.matrix(d.dist)

database2$Honeybee_visit_rate <- database2$Honeybees/database2$OVERALL_Flowers*1000

florabund<-dist(database2$OVERALL_Flowers)
apis <- dist(database2$Honeybees)
nectar <- dist(database2$Nectar)
honeybeerate <- dist(database2$Honeybee_visit_rate)

florsm <- as.matrix(florabund)
pollsnested <- as.matrix(pollinators.beta.div$beta.sne)
senseapis.nested <- as.matrix(senseapis.beta.div$beta.sne)
plantsbray <- as.matrix(quantitative.plants$bray)
honey <- as.matrix(apis)
nectard <- as.matrix(nectar)
honeybeerated <- as.matrix(honeybeerate)
plantsnestedness <- as.matrix(plants.beta.div$beta.sne)
pollsturnover <- as.matrix(pollinators.beta.div$beta.sim)
senseapisturnover <- as.matrix(senseapis.beta.div$beta.sim)
plantturnover <- as.matrix(plants.beta.div$beta.sim)
rewiringquantapis <- as.matrix(taa$OS)
rewiringquantsenseapis <- as.matrix(tsa$OS)
rewiringqualapis <- as.matrix(faa$OS)
rewiringqualsenseapis <- as.matrix(fsa$OS)






# Nestedness amb apis
a1 <- mantel(florsm,pollsnested,permutations = 1999)
a2 <- mantel(nectard,pollsnested,permutations = 1999)
a3 <- mantel(plantsbray,pollsnested,permutations = 1999)
a4 <- mantel(honey,pollsnested,permutations = 1999)
a5 <- mantel(honeybeerated,pollsnested,permutations = 1999)
a6 <- mantel(plantsnestedness,pollsnested,permutations = 1999)
a7 <- mantel(geographicdist,pollsnested,permutations = 1999)

# Nestedness sense apis
b1 <- mantel(florsm,senseapis.nested,permutations = 1999)
b2 <- mantel(nectard,senseapis.nested,permutations = 1999)
b3 <- mantel(plantsbray,senseapis.nested,permutations = 1999)
b4 <- mantel(honey,senseapis.nested,permutations = 1999)
b5 <- mantel(honeybeerated,senseapis.nested,permutations = 1999)
b6 <- mantel(plantsnestedness,senseapis.nested,permutations = 1999)
b7 <- mantel(geographicdist,senseapis.nested,permutations = 1999)

# Turnover amb apis
c1 <- mantel(plantturnover,pollsturnover,permutations = 1999)
c2 <- mantel(plantsbray,pollsturnover,permutations = 1999)
c3 <- mantel(geographicdist,pollsturnover,permutations = 1999)
c4 <- mantel(honeybeerated,pollsturnover,permutations = 1999)
c5 <- mantel(honey,pollsturnover,permutations = 1999)

# Turnover sense apis
d1 <- mantel(plantturnover,senseapisturnover,permutations = 1999)
d2 <- mantel(plantsbray,senseapisturnover,permutations = 1999)
d3 <- mantel(geographicdist,senseapisturnover,permutations = 1999)
d4 <- mantel(honeybeerated,senseapisturnover,permutations = 1999)
d5 <- mantel(honey,senseapisturnover,permutations = 1999)

# Interaccions: Rewiring amb apis
e1 <- mantel(rewiringquantapis,honey,permutations = 1999)
e2 <- mantel(rewiringquantsenseapis,honey,permutations = 1999)
e3 <- mantel(rewiringqualapis,honey,permutations = 1999)
e4 <- mantel(rewiringqualsenseapis,honey,permutations = 1999)
e5 <- mantel(rewiringquantapis,honeybeerated,permutations = 1999)
e6 <- mantel(rewiringquantsenseapis,honeybeerated,permutations = 1999)
e7 <- mantel(rewiringqualapis,honeybeerated,permutations = 1999)
e8 <- mantel(rewiringqualsenseapis,honeybeerated,permutations = 1999)

# Rewiring amb comunitats
f1 <- mantel(rewiringquantapis,pollsturnover,permutations = 1999)
f2 <- mantel(rewiringqualapis,pollsturnover,permutations = 1999)
f3 <- mantel(rewiringquantsenseapis,senseapisturnover,permutations = 1999)
f4 <- mantel(rewiringqualsenseapis,senseapisturnover,permutations = 1999)

f5 <- mantel(rewiringquantapis,pollsnested,permutations = 1999)
f6 <- mantel(rewiringqualapis,pollsnested,permutations = 1999)
f7 <- mantel(rewiringquantsenseapis,senseapis.nested,permutations = 1999)
f8 <- mantel(rewiringqualsenseapis,senseapis.nested,permutations = 1999)

f9 <- mantel(rewiringquantapis,plantsbray,permutations = 1999)
f10 <- mantel(rewiringqualapis,plantsbray,permutations = 1999)
f11 <- mantel(rewiringquantsenseapis,plantsbray,permutations = 1999)
f12 <- mantel(rewiringqualsenseapis,plantsbray,permutations = 1999)


## Rewiring amb comunitats
f1 <- mantel(taa,pollsturnover,permutations = 1999) `r f1$p`  
f2 <- mantel(faa,pollsturnover,permutations = 1999) `r f2$p`  
f3 <- mantel(tsa,senseapisturnover,permutations = 1999)`r f3$p`  
f4 <- mantel(fsa,senseapisturnover,permutations = 1999)`r f4$p`  
f5 <- mantel(taa,plantsbray,permutations = 1999)`r f5$p`  
f6 <- mantel(faa,plantsbray,permutations = 1999)`r f6$p`  
f7 <- mantel(tsa,plantsbray,permutations = 1999)`r f7$p`  
f8 <- mantel(fsa,plantsbray,permutations = 1999)`r f8$p`  



pollinators<-read.table("bitxos qualitatiu.txt",header=T)
pollinatorsnomesabelles <- read.table("bitxos quantitatiu nomes abelles.txt",header=T)
pollinatorsnomesabellessenseapis <- read.table("bitxos quantitatiu nomes abelles sense apis.txt",header=T)

planta<-read.table("abundancia visites per planta.txt",header=T)
planta2<-read.table("abundancia visites per planta sense apis.txt",header=T)


Beeabundance <- apply(X = pollinatorsnomesabelles, 1, FUN = sum)
Beeabundancesenseapis <- apply(X = pollinatorsnomesabellessenseapis, 1, FUN = sum)

database2$Beeabundance <- Beeabundance
database2$Beeabundancesenseapis <- Beeabundancesenseapis

database2$Beevisitrate <- database2$Beeabundance/database2$OVERALL_Flowers*1000
database2$Beevisitratesenseapis <- database2$Beeabundancesenseapis/database2$OVERALL_Flowers*1000
database2$Honeybeerate <- database2$Honeybees/database2$OVERALL_Flowers*1000
database2$solitaryrate <- database2$Solitary/database2$OVERALL_Flowers*1000

database2$ROF_visitssa <- planta2$ROF
database2$TVUF_visitssa <- planta2$TVUF
database2$TVUH_visitssa <- planta2$TVUH

database2$ROF_rate <- planta$ROF/database2$ROF_Flowers*1000
database2$TVUF_rate <- planta$TVUF/database2$TVUF_Flowers*1000
database2$TVUH_rate <- planta$TVUH/database2$TVUH_Flowers*1000

database2$ROF_ratesa <- database2$ROF_visitssa/database2$ROF_Flowers*1000
database2$TVUF_ratesa <- database2$TVUF_visitssa/database2$TVUF_Flowers*1000
database2$TVUH_ratesa <- database2$TVUH_visitssa/database2$TVUH_Flowers*1000

database2$ROF_rateapis <- database2$ROF_rate-database2$ROF_ratesa
database2$TVUF_rateapis <- database2$TVUF_rate-database2$TVUF_ratesa
database2$TVUH_rateapis <- database2$TVUH_rate-database2$TVUH_ratesa

write.table(data3,"D:\\Usuarios\\s.reverte\\OneDrive - CREAF\\TESI\\DADES\\dades\\database2b.txt")



# Rewiring to metaweb----------

total<-read.table("Xarxes/total.txt", header=T,colnames(1))
totalsenseapis<-read.table("Xarxes/total sense apis.txt", header=T,colnames(1))
totalaa<-as.matrix(total)
totalsa<-as.matrix(totalsenseapis)


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


taula1<-read.table("Xarxes/sense apis/taula1.txt", header=T,colnames(1))
taula2<-read.table("Xarxes/sense apis/taula2.txt", header=T,colnames(1))
taula3<-read.table("Xarxes/sense apis/taula3.txt", header=T,colnames(1))
taula4<-read.table("Xarxes/sense apis/taula4.txt", header=T,colnames(1))
taula5<-read.table("Xarxes/sense apis/taula5.txt", header=T,colnames(1))
taula6<-read.table("Xarxes/sense apis/taula6.txt", header=T,colnames(1))
taula7<-read.table("Xarxes/sense apis/taula7.txt", header=T,colnames(1))
taula8<-read.table("Xarxes/sense apis/taula8.txt", header=T,colnames(1))
taula9<-read.table("Xarxes/sense apis/taula9.txt", header=T,colnames(1))
taula10<-read.table("Xarxes/sense apis/taula10.txt", header=T,colnames(1))
taula11<-read.table("Xarxes/sense apis/taula11.txt", header=T,colnames(1))
taula12<-read.table("Xarxes/sense apis/taula12.txt", header=T,colnames(1))
taula13<-read.table("Xarxes/sense apis/taula13.txt", header=T,colnames(1))
taula14<-read.table("Xarxes/sense apis/taula14.txt", header=T,colnames(1))
taula15<-read.table("Xarxes/sense apis/taula15.txt", header=T,colnames(1))
taula16<-read.table("Xarxes/sense apis/taula16.txt", header=T,colnames(1))
taula17<-read.table("Xarxes/sense apis/taula17.txt", header=T,colnames(1))
taula18<-read.table("Xarxes/sense apis/taula18.txt", header=T,colnames(1))
taula19<-read.table("Xarxes/sense apis/taula19.txt", header=T,colnames(1))
taula20<-read.table("Xarxes/sense apis/taula20.txt", header=T,colnames(1))
taula21<-read.table("Xarxes/sense apis/taula21.txt", header=T,colnames(1))
taula22<-read.table("Xarxes/sense apis/taula22.txt", header=T,colnames(1))
taula23<-read.table("Xarxes/sense apis/taula23.txt", header=T,colnames(1))
taula24<-read.table("Xarxes/sense apis/taula24.txt", header=T,colnames(1))
taula25<-read.table("Xarxes/sense apis/taula25.txt", header=T,colnames(1))
taula26<-read.table("Xarxes/sense apis/taula26.txt", header=T,colnames(1))
taula27<-read.table("Xarxes/sense apis/taula27.txt", header=T,colnames(1))
taula28<-read.table("Xarxes/sense apis/taula28.txt", header=T,colnames(1))
taula29<-read.table("Xarxes/sense apis/taula29.txt", header=T,colnames(1))
taula30<-read.table("Xarxes/sense apis/taula30.txt", header=T,colnames(1))
taula31<-read.table("Xarxes/sense apis/taula31.txt", header=T,colnames(1))
taula32<-read.table("Xarxes/sense apis/taula32.txt", header=T,colnames(1))
taula33<-read.table("Xarxes/sense apis/taula33.txt", header=T,colnames(1))
taula34<-read.table("Xarxes/sense apis/taula34.txt", header=T,colnames(1))
taula35<-read.table("Xarxes/sense apis/taula35.txt", header=T,colnames(1))
taula36<-read.table("Xarxes/sense apis/taula36.txt", header=T,colnames(1))
taula37<-read.table("Xarxes/sense apis/taula37.txt", header=T,colnames(1))
taula38<-read.table("Xarxes/sense apis/taula38.txt", header=T,colnames(1))
taula39<-read.table("Xarxes/sense apis/taula39.txt", header=T,colnames(1))
taula40<-read.table("Xarxes/sense apis/taula40.txt", header=T,colnames(1))



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

llistasa<-list(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,
               t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,
               t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40)

distance.to.metaweb<-function (N,M) 
{
  os_prime <- plyr::laply(N, function(x) betalink(x, M)$OS)
  names(os_prime) <- names(N)
  return(os_prime)
}


ambapis<-distance.to.metaweb(llistaaa,totalaa)
mean(senseapis)
sd(senseapis)

senseapis<-distance.to.metaweb(llistasa,totalsa)

# Autocorreñació espacial rewiring amb metaweb

Moran.I(ambapis, zone.dists.inv)
Moran.I(senseapis, zone.dists.inv)

# Regresions entre rewiring i densitat/taxa visita apis

mod1 <- lm(senseapis ~  database2$Honeybees) 
summary(mod1)
plot(senseapis ~  database2$Honeybees)

mod1 <- lm(senseapis ~  database2$Honeybeerate) 
summary(mod1)
plot(senseapis ~  database2$Honeybeerate)

# Efectes beta-diversitat de comunitats sobre rewiring amb metaweb

ambapisdist <- dist(ambapis)
ambapism <- as.matrix(ambapisdist)

senseapisdist <- dist(senseapis)
senseapism <- as.matrix(senseapisdist)



mantel.partial(plantturnover,ambapisdist,fenologia,permutations = 1999)
mantel.partial(florsm,ambapisdist,fenologia,permutations = 1999)
mantel.partial(plantsbray,ambapisdist,fenologia,permutations = 1999)
mantel.partial(honey,ambapisdist,fenologia,permutations = 1999)
mantel.partial(honeybeerated,ambapisdist,fenologia,permutations = 1999)
mantel.partial(plantsnestedness,ambapisdist,fenologia,permutations = 1999)
mantel.partial(pollsnested,ambapisdist,fenologia,permutations = 1999)
mantel.partial(pollsturnover,ambapisdist,fenologia,permutations = 1999)

mantel.partial(plantturnover,senseapism,fenologia,permutations = 1999)
mantel.partial(florsm,senseapism,fenologia,permutations = 1999)
mantel.partial(plantsbray,senseapism,fenologia,permutations = 1999)
mantel.partial(honey,senseapism,fenologia,permutations = 1999)
mantel.partial(honeybeerated,senseapism,fenologia,permutations = 1999)
mantel.partial(plantsnestedness,senseapism,fenologia,permutations = 1999)
mantel.partial(senseapis.nested,senseapism,fenologia,permutations = 1999)
mantel.partial(senseapisturnover,senseapism,fenologia,permutations = 1999)


# Restar efecte data------------

fenologia<-dist(database2$DATA)

quantitative.pollinators<-bray.part(pollinators)
quantitative.senseapis<-bray.part(senseapis)
quantitative.plants<-bray.part(plants)

plantsbray <- as.matrix(quantitative.plants$bray)
senseapisbray <- as.matrix(quantitative.senseapis$bray)
pollinatorsbray <- as.matrix(quantitative.pollinators$bray)

mantel.partial(plantsbray,pollinatorsbray,fenologia,permutations=999)
mantel(plantsbray,pollinatorsbray,permutations=999)

mantel(taa$OS,geographicdist,permutations=9999)
mantel(senseapisbray,geographicdist,permutations=9999)
mantel(pollinatorsbray,geographicdist,permutations=9999)

# Nestedness amb apis
mantel.partial(florsm,pollsnested,fenologia,permutations = 1999)
mantel.partial(nectard,pollsnested,fenologia,permutations = 1999)
mantel.partial(plantsbray,pollsnested,fenologia,permutations = 1999)
mantel.partial(honey,pollsnested,fenologia,permutations = 1999)
mantel.partial(honeybeerated,pollsnested,fenologia,permutations = 1999)
mantel.partial(plantsnestedness,pollsnested,fenologia,permutations = 1999)
mantel.partial(geographicdist,pollsnested,fenologia,permutations = 1999)

# Nestedness sense apis
mantel.partial(florsm,senseapis.nested,fenologia,permutations = 1999)
mantel.partial(nectard,senseapis.nested,fenologia,permutations = 1999)
mantel.partial(plantsbray,senseapis.nested,fenologia,permutations = 1999)
mantel.partial(honey,senseapis.nested,fenologia,permutations = 1999)
mantel.partial(honeybeerated,senseapis.nested,fenologia,permutations = 1999)
mantel.partial(plantsnestedness,senseapis.nested,fenologia,permutations = 1999)
mantel.partial(geographicdist,senseapis.nested,fenologia,permutations = 1999)

# Turnover amb apis
mantel.partial(plantturnover,pollsturnover,fenologia,permutations = 1999)
mantel.partial(plantsbray,pollsturnover,fenologia,permutations = 1999)
mantel.partial(geographicdist,pollsturnover,fenologia,permutations = 1999)
mantel.partial(honeybeerated,pollsturnover,fenologia,permutations = 1999)
mantel.partial(honey,pollsturnover,fenologia,permutations = 1999)

# Turnover sense apis
mantel.partial(plantturnover,senseapisturnover,fenologia,permutations = 1999)
mantel.partial(plantsbray,senseapisturnover,fenologia,permutations = 1999)
mantel.partial(geographicdist,senseapisturnover,fenologia,permutations = 1999)
mantel.partial(honeybeerated,senseapisturnover,fenologia,permutations = 1999)
mantel.partial(honey,senseapisturnover,fenologia,permutations = 1999)

# Interaccions: Rewiring amb apis
mantel.partial(rewiringquantapis,honey,fenologia,permutations = 1999)
mantel.partial(rewiringquantsenseapis,honey,fenologia,permutations = 1999)
mantel.partial(rewiringqualapis,honey,fenologia,permutations = 1999)
mantel.partial(rewiringqualsenseapis,honey,fenologia,permutations = 1999)
mantel.partial(rewiringquantapis,honeybeerated,fenologia,permutations = 1999)
mantel.partial(rewiringquantsenseapis,honeybeerated,fenologia,permutations = 1999)
mantel.partial(rewiringqualapis,honeybeerated,fenologia,permutations = 1999)
mantel.partial(rewiringqualsenseapis,honeybeerated,fenologia,permutations = 1999)

# Rewiring amb comunitats
mantel.partial(rewiringquantapis,pollsturnover,fenologia,permutations = 1999)
mantel.partial(rewiringqualapis,pollsturnover,fenologia,permutations = 1999)
mantel.partial(rewiringquantsenseapis,senseapisturnover,fenologia,permutations = 1999)
mantel.partial(rewiringqualsenseapis,senseapisturnover,fenologia,permutations = 1999)

mantel.partial(rewiringquantapis,pollsnested,fenologia,permutations = 1999)
mantel.partial(rewiringqualapis,pollsnested,fenologia,permutations = 1999)
mantel.partial(rewiringquantsenseapis,senseapis.nested,fenologia,permutations = 1999)
mantel.partial(rewiringqualsenseapis,senseapis.nested,fenologia,permutations = 1999)

mantel.partial(rewiringquantapis,plantsbray,fenologia,permutations = 1999)
mantel.partial(rewiringqualapis,plantsbray,fenologia,permutations = 1999)
mantel.partial(rewiringquantsenseapis,plantsbray,fenologia,permutations = 1999)
mantel.partial(rewiringqualsenseapis,plantsbray,fenologia,permutations = 1999)




