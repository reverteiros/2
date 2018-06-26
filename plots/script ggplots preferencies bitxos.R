

setwd("D:/Usuarios/s.reverte/OneDrive - CREAF/TESI/DADES/dades")

datas <- read.table("proporcions.txt",header=T)
data2 <- read.table("taxes apis wild.txt",header=T)
database2 <- read.table("Database3.txt",header=T)

junt <- merge(datas,data2)

names(database2)

junt$ROFflowers <- database2$ROF_Flowers
junt$TVUflowers <- database2$TVU_Flowers
junt$OTHERflowers <- database2$other_flowers_abundance
junt$rofrate <- junt$ROF_rate_apis+junt$ROF_rate_wild
junt$wildraterofpercent <- junt$ROF_rate_wild/junt$rofrate
junt$apisraterofpercent <- junt$ROF_rate_apis/junt$rofrate
junt$rofpolls <- database2$Honeybees_ROF+database2$Wild_ROF
junt$wildrofpercent <- database2$Wild_ROF/junt$rofpolls
junt$apisrofpercent <- database2$Honeybees_ROF/junt$rofpolls

a <- ggplot(junt, aes(ROFflowers))
a <- a + geom_point(aes(y=apisrofpercent), colour="blue")
a <- a + geom_point(aes(y=wildrofpercent), colour="red")
a <- a + labs(y="")
a

a <- ggplot(junt, aes(ROF))
a <- a + geom_point(aes(y=TVU), colour="blue")
a <- a + ylim(0, 100) + xlim(0, 100)
a <- a + labs(y="%TVU",x="%ROF")
a



library(ggplot2)
library(grid)
library(gridExtra)


a <- ggplot(junt, aes(ROFflowers))
a <- a + geom_smooth(aes(y=Honeybees.rof), colour="blue")
a <- a + geom_point(aes(y=Honeybees.rof), colour="blue")
a <- a + labs(y="")
a <- a + ylim(0, 100)
a

b <- ggplot(junt, aes(OTHERflowers))
b <- b + geom_point(aes(y=Honeybees.others), colour="blue")
b <- b + labs(y="")
b <- b + ylim(0, 100)
b

c <- ggplot(junt, aes(TVUflowers))
c <- c + geom_smooth(aes(y=Honeybees.tvu), colour="blue")
c <- c + geom_point(aes(y=Honeybees.tvu), colour="blue")
c <- c + labs(y="")
c <- c + ylim(0, 100)
c

d <- ggplot(junt, aes(ROFflowers))
d <- d + geom_smooth(aes(y=Solitary.rof), colour="red")
d <- d + geom_point(aes(y=Solitary.rof), colour="red")
d <- d + labs(y="")
d <- d + ylim(0, 100)
d

e <- ggplot(junt, aes(OTHERflowers))
e <- e + geom_point(aes(y=Solitary.others), colour="red")
e <- e + labs(y="")
e <- e + ylim(0, 100)
e

g <- ggplot(junt, aes(TVUflowers))
g <- g + geom_smooth(aes(y=Solitary.tvu), colour="red")
g <- g + geom_point(aes(y=Solitary.tvu), colour="red")
g <- g + labs(y="")
g <- g + ylim(0, 100)
g

grid.arrange(a, c, b, d, g, e, nrow=2, ncol = 3)
grid.arrange(d, g, e, ncol = 3)

a <- ggplot(datas, aes(ROF))
a <- a + geom_smooth(aes(y=Honeybees.rof), colour="blue")
a <- a + geom_point(aes(y=Honeybees.rof), colour="blue")
a <- a + xlim(0, 100) + ylim(0, 100)
a <- a + labs(y="")
a

b <- ggplot(datas, aes(OTHER))
b <- b + geom_point(aes(y=Honeybees.others), colour="blue")
b <- b + xlim(0, 100) + ylim(0, 100)
b <- b + labs(y="")
b

c <- ggplot(datas, aes(TVU))
c <- c + geom_smooth(aes(y=Honeybees.tvu), colour="blue")
c <- c + geom_point(aes(y=Honeybees.tvu), colour="blue")
c <- c + xlim(0, 100) + ylim(0, 100)
c <- c + labs(y="")
c


d <- ggplot(datas, aes(ROF))
d <- d + geom_smooth(aes(y=Solitary.rof), colour="red")
d <- d + geom_point(aes(y=Solitary.rof), colour="red")
d <- d + xlim(0, 100) + ylim(0, 100)
d <- d + labs(y="")
d

e <- ggplot(datas, aes(OTHER))
e <- e + geom_point(aes(y=Solitary.others), colour="red")
e <- e + xlim(0, 100) + ylim(0, 100)
e <- e + labs(y="")
e

g <- ggplot(datas, aes(TVU))
g <- g + geom_smooth(aes(y=Solitary.tvu), colour="red")
g <- g + geom_point(aes(y=Solitary.tvu), colour="red")
g <- g + xlim(0, 100) + ylim(0, 100)
g <- g + labs(y="")
g
