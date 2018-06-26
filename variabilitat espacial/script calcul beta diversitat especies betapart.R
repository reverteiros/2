
setwd("D:/Usuarios/s.reverte/OneDrive - CREAF/TESI/DADES/dades")

library(betapart)

#### Beta-diversitat d'interaccions vs beta-diversitat d'espècies. QUALITATIU

pollinators<-read.table("bitxos qualitatiu sense apis.txt",header=T)
plants<-read.table("flors qualitatiu.txt", header=T)

pollinators.beta.div<-beta.pair(pollinators, index.family="sorensen")
plants.beta.div<-beta.pair(plants, index.family="sorensen")

mOS <- as.matrix(pollinators.beta.div$beta.sim)

Bsor_plants <- data.frame(t(combn(rownames(mOS),2)), as.numeric(plants.beta.div$beta.sor))
names(Bsor_plants) <- c("Plot1", "Plot2", "Bsor_plants")

Bsim_plants <- data.frame(t(combn(rownames(mOS),2)), as.numeric(plants.beta.div$beta.sim))
names(Bsim_plants) <- c("Plot1", "Plot2", "Bsim_plants")

Bsne_plants <- data.frame(t(combn(rownames(mOS),2)), as.numeric(plants.beta.div$beta.sne))
names(Bsne_plants) <- c("Plot1", "Plot2", "Bsne_plants")

Bsor_plants$Bsim_plants <- Bsim_plants$Bsim_plants
Bsor_plants$Bsne_plants <- Bsne_plants$Bsne_plants

#### Beta-diversitat d'interaccions vs beta-diversitat d'espècies. QUANTITATIU

pollinators<-read.table("bitxos quantitatiu.txt",header=T)
sense.apis<-read.table("bitxos quantitatiu sense apis.txt",header=T)
plants<-read.table("flors quantitatiu.txt", header=T)

quantitative.pollinators<-bray.part(pollinators)
quantitative.senseapis<-bray.part(sense.apis)
quantitative.plants<-bray.part(plants)

quant.polls <- data.frame(t(combn(rownames(mOS),2)), as.numeric(quantitative.pollinators$bray))
names(quant.polls) <- c("Plot1", "Plot2", "quant.polls")

quant.senseapis <- data.frame(t(combn(rownames(mOS),2)), as.numeric(quantitative.senseapis$bray))
names(quant.polls) <- c("Plot1", "Plot2", "quant.senseapis")

z <- as.matrix(quantitative.plants$bray)
quant.plants <- data.frame(t(combn(rownames(z),2)), as.numeric(quantitative.plants$bray))
names(quant.plants) <- c("Plot1", "Plot2", "quant.plants")

sd(quant.plants$quant.plants)
