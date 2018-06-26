
setwd("D:/Usuarios/s.reverte/OneDrive - CREAF/TESI/DADES/dades")

database2 <- read.table("Database3.txt",header=T)

names(database2)

database2$No_ROF <- database2$TVU_Flowers+database2$other_flowers_abundance
database2$Wild_No_ROF <- database2$Wild_OTHERS+database2$Wild_TVU
database2$Wild_Rate_No_ROF <- database2$Wild_No_ROF/database2$No_ROF*1000


database2$percent_wild_norof <- 100*(database2$Wild_Rate_No_ROF/(database2$Wild_Rate_No_ROF+database2$Honeybees_TVU_rate+database2$Honeybees_OTHERS_rate))


plot(database2$~database2$Max)
plot(database2$OVERALL_Flowers~database2$Max)
plot(database2$Wild~database2$Max)
plot(database2$Pollinator_species~database2$Max)
plot(database2$Honeybees~database2$Max)
plot(database2$Honeybeerate~database2$Max)
plot(database2$Wildrate~database2$Max)

plot(database2$Pollinator_species~database2$flower_richness)
plot(database2$Wild~database2$OVERALL_Flowers)
plot(database2$Wildrate~database2$OVERALL_Flowers)
plot(database2$Honeybeerate~database2$OVERALL_Flowers)
plot(d$Honeybees_ROF_rate~d$ROF_Flowers)

d$rofpercent <- d$ROF_Flowers/d$OVERALL_Flowers*100
plot(d$Honeybees_ROF_rate~d$rofpercent)

junt <- merge(d,database2)

names(junt)

database2$percent_apis_rof <- 100*database2$Honeybees_ROF_rate/(database2$Wild_ROF_rate+database2$Honeybees_ROF_rate)

database2$percent_wild_rof <- 100*database2$Wild_ROF_rate/(database2$Wild_ROF_rate+database2$Honeybees_ROF_rate)

library(ggplot2)

a <- ggplot(database2, aes(ROF_Flowers))
a <- a + geom_point(aes(y=Honeybees_ROF), colour="blue")
a <- a + geom_point(aes(y=Wild_ROF), colour="red")
a <- a + labs(y="")
a

a <- ggplot(database2, aes(ROF_Flowers))
a <- a + geom_point(aes(y=percent_apis_rof), colour="blue")
a <- a + geom_point(aes(y=percent_wild_rof), colour="red")
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
