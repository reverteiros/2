
setwd("D:/Usuarios/s.reverte/OneDrive - CREAF/TESI/DADES/dades")
dades <- read.table("parelles3.txt",header=T)
bitxos <- dades[,5:174]
rownames(bitxos) <- dades[,3]
rownames(dades) <- dades[,3]

distancia <- dist(dades[,4])
Distance <- data.frame(t(combn(rownames(dades),2)), as.numeric(distancia))
names(Distance) <- c("Plot", "Plot", "Distance")

grup1 <- dades[1:3,]
grup2 <- dades[4:6,]
grup3 <- dades[7:9,]
grup4 <- dades[10:12,]
grup5 <- dades[13:15,]
grup6 <- dades[16:18,]
grup7 <- dades[19:21,]
grup8 <- dades[22:24,]
grup9 <- dades[25:27,]
grup10 <- dades[28:30,]
grup11 <- dades[31:33,]
grup12 <- dades[34:36,]
grup13 <- dades[37:39,]
grup14 <- dades[40:42,]
grup15 <- dades[43:45,]

rownames(grup1) <- grup1[,3]
rownames(grup2) <- grup2[,3]
rownames(grup3) <- grup3[,3]
rownames(grup4) <- grup4[,3]
rownames(grup5) <- grup5[,3]
rownames(grup6) <- grup6[,3]
rownames(grup7) <- grup7[,3]
rownames(grup8) <- grup8[,3]
rownames(grup9) <- grup9[,3]
rownames(grup10) <- grup10[,3]
rownames(grup11) <- grup11[,3]
rownames(grup12) <- grup12[,3]
rownames(grup13) <- grup13[,3]
rownames(grup14) <- grup14[,3]
rownames(grup15) <- grup15[,3]

grup1 <- grup1[,5:174]
grup2 <- grup2[,5:174]
grup3 <- grup3[,5:174]
grup4 <- grup4[,5:174]
grup5 <- grup5[,5:174]
grup6 <- grup6[,5:174]
grup7 <- grup7[,5:174]
grup8 <- grup8[,5:174]
grup9 <- grup9[,5:174]
grup10 <- grup10[,5:174]
grup11 <- grup11[,5:174]
grup12 <- grup12[,5:174]
grup13 <- grup13[,5:174]
grup14 <- grup14[,5:174]
grup15 <- grup15[,5:174]

grup1.b<-bray.part(grup1)
grup2.b<-bray.part(grup2)
grup3.b<-bray.part(grup3)
grup4.b<-bray.part(grup4)
grup5.b<-bray.part(grup5)
grup6.b<-bray.part(grup6)
grup7.b<-bray.part(grup7)
grup8.b<-bray.part(grup8)
grup9.b<-bray.part(grup9)
grup10.b<-bray.part(grup10)
grup11.b<-bray.part(grup11)
grup12.b<-bray.part(grup12)
grup13.b<-bray.part(grup13)
grup14.b<-bray.part(grup14)
grup15.b<-bray.part(grup15)

grup1.a <- data.frame(t(combn(rownames(grup1),2)), as.numeric(grup1.b$bray))
names(grup1.a) <- c("Comb", "Comb", "Bray")
grup2.a <- data.frame(t(combn(rownames(grup2),2)), as.numeric(grup2.b$bray))
names(grup2.a) <- c("Comb", "Comb", "Bray")
grup3.a <- data.frame(t(combn(rownames(grup3),2)), as.numeric(grup3.b$bray))
names(grup3.a) <- c("Comb", "Comb", "Bray")
grup4.a <- data.frame(t(combn(rownames(grup4),2)), as.numeric(grup4.b$bray))
names(grup4.a) <- c("Comb", "Comb", "Bray")
grup5.a <- data.frame(t(combn(rownames(grup5),2)), as.numeric(grup5.b$bray))
names(grup5.a) <- c("Comb", "Comb", "Bray")
grup6.a <- data.frame(t(combn(rownames(grup6),2)), as.numeric(grup6.b$bray))
names(grup6.a) <- c("Comb", "Comb", "Bray")
grup7.a <- data.frame(t(combn(rownames(grup7),2)), as.numeric(grup7.b$bray))
names(grup7.a) <- c("Comb", "Comb", "Bray")
grup8.a <- data.frame(t(combn(rownames(grup8),2)), as.numeric(grup8.b$bray))
names(grup8.a) <- c("Comb", "Comb", "Bray")
grup9.a <- data.frame(t(combn(rownames(grup9),2)), as.numeric(grup9.b$bray))
names(grup9.a) <- c("Comb", "Comb", "Bray")
grup10.a <- data.frame(t(combn(rownames(grup10),2)), as.numeric(grup10.b$bray))
names(grup10.a) <- c("Comb", "Comb", "Bray")
grup11.a <- data.frame(t(combn(rownames(grup11),2)), as.numeric(grup11.b$bray))
names(grup11.a) <- c("Comb", "Comb", "Bray")
grup12.a <- data.frame(t(combn(rownames(grup12),2)), as.numeric(grup12.b$bray))
names(grup12.a) <- c("Comb", "Comb", "Bray")
grup13.a <- data.frame(t(combn(rownames(grup13),2)), as.numeric(grup13.b$bray))
names(grup13.a) <- c("Comb", "Comb", "Bray")
grup14.a <- data.frame(t(combn(rownames(grup14),2)), as.numeric(grup14.b$bray))
names(grup14.a) <- c("Comb", "Comb", "Bray")
grup15.a <- data.frame(t(combn(rownames(grup15),2)), as.numeric(grup15.b$bray))
names(grup15.a) <- c("Comb", "Comb", "Bray")

beta <- rbind(grup1.a, grup2.a, grup3.a, grup4.a, grup5.a, grup6.a, grup7.a, grup8.a, grup9.a, grup10.a, grup11.a, grup12.a, grup13.a, grup14.a, grup15.a)


graphdata <- read.table("GRAFIC3D.txt",header=T)

library(scatterplot3d)

# 1. Source the function
source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
# 2. Empty 3D scatter plot using pch=""
s3d <- scatterplot3d(graphdata$Dist1,graphdata$Dist2,graphdata$Bray, pch = "", grid=FALSE, box=FALSE)
# 3. Add grids
addgrids3d(graphdata$Dist1,graphdata$Dist2,graphdata$Bray, grid = c("xy", "xz", "yz"))
# 4. Add points
s3d$points3d(graphdata$Dist1,graphdata$Dist2,graphdata$Bray, pch = 16)
scatterplot3d(graphdata$Dist1,graphdata$Dist2,graphdata$Bray, pch = 16, type="h")

# 3D scatter plot
s3d <- scatterplot3d(graphdata$Dist2,graphdata$Dist1,graphdata$Bray, type = "h", color = "blue", angle=55, pch = 16)
# Add regression plane
my.lm <- lm(graphdata$Bray ~ graphdata$Dist2 + graphdata$Dist1)
s3d$plane3d(my.lm)
# Add supplementary points
s3d$points3d(seq(10, 20, 2), seq(85, 60, -5), seq(60, 10, -10),
             col = "red", type = "h", pch = 8)
