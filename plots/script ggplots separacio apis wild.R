

setwd("D:/Usuarios/s.reverte/OneDrive - CREAF/TESI/DADES/dades")
setwd("C:/Users/saret/OneDrive - CREAF/TESI/DADES/dades")

database2 <- read.table("Data.txt",header=T)
names(database2)
library(ggplot2)

database2$percent_wild_rof <- database2$Wild_ROF*100/database2$Wild_Abundance
database2$percent_wild_no_rof <- (database2$Wild_OTHERS+database2$Wild_TVU)*100/database2$Wild_Abundance
database2$percent_honeybee_rof <- database2$Honeybees_ROF*100/database2$Honeybee_Abundance
database2$percent_honeybee_no_rof <- (database2$Honeybees_OTHERS+database2$Honeybees_TVU)*100/database2$Honeybee_Abundance

## wild bar plot with error bars

aa <- c("percent_wild_rof","percent_wild_no_rof")
ab <- mean(database2$percent_wild_rof)
ac <- mean(database2$percent_wild_no_rof)
ad <- sd(database2$percent_wild_rof)
ae <- sd(database2$percent_wild_no_rof)

mean1 <- c(ab,ac)
sd1 <- c(ad,ae)

x <- data.frame(aa,mean1,sd1)

ggplot(x) +
  geom_bar( aes(x=aa, y=mean1), stat="identity", fill="blue", alpha=0.7) +
  geom_errorbar( aes(x=aa, ymin=mean1-sd1, ymax=mean1+sd1), width=0.4, colour="black", alpha=0.9, size=1.3) +
  labs(y = "")


## honeybees bar plot with error bars

aa <- c("percent_honeybee_rof","percent_honeybee_no_rof")
ab <- mean(database2$percent_honeybee_rof)
ac <- mean(database2$percent_honeybee_no_rof)
ad <- sd(database2$percent_honeybee_rof)
ae <- sd(database2$percent_honeybee_no_rof)

mean1 <- c(ab,ac)
sd1 <- c(ad,ae)

x <- data.frame(aa,mean1,sd1)

ggplot(x) +
  geom_bar( aes(x=aa, y=mean1), stat="identity", fill="blue", alpha=0.7) +
  geom_errorbar( aes(x=aa, ymin=mean1-sd1, ymax=mean1+sd1), width=0.4, colour="black", alpha=0.9, size=1.3) +
  labs(y = "")


#########################################

g <- ggplot(database2, aes((Honeybee_Visit_Rate)))
g <- g + geom_point(aes(y=(Pollinator_Richness)), colour="red")
g

g <- ggplot(database2, aes((overall_flowers)))
g <- g + geom_point(aes(y=(Wild_Abundance)), colour="red")
g



g <- ggplot(database2, aes((Honeybee_Abundance)))
g <- g + geom_point(aes(y=(Wild_Abundance)), colour="red")
g

g <- ggplot(database2, aes(NO_ROF))
g <- g + geom_point(aes(y=percent_HB_ROF), colour="red")
g <- g + geom_point(aes(y=percent_HB_NO_ROF), colour="blue")
g

g <- ggplot(database2, aes(ROF_Abundance))
g <- g + geom_point(aes(y=Honeybee_Visit_Rate_ROF), colour="red")
g <- g + geom_point(aes(y=HB_Rate_NO_ROF), colour="blue")
g

g <- ggplot(database2, aes(NO_ROF))
g <- g + geom_point(aes(y=Honeybee_Visit_Rate_ROF), colour="red")
g <- g + geom_point(aes(y=HB_Rate_NO_ROF), colour="blue")
g

g <- ggplot(database2, aes(ROF_Abundance))
g <- g + geom_point(aes(y=Honeybee_Visit_Rate_ROF), colour="black")
g <- g + geom_point(aes(y=Wild_Visit_Rate_ROF), colour="orange")
g

g <- ggplot(database2, aes(NO_ROF))
g <- g + geom_point(aes(y=HB_Rate_NO_ROF), colour="black")
g <- g + geom_point(aes(y=WILD_Rate_NO_ROF), colour="orange")
g


g <- ggplot(database2, aes(Honeybee_Visit_Rate))
g <- g + geom_point(aes(y=Wild_Abundance), colour="blue")
g

g <- ggplot(database2, aes(NO_ROF))
g <- g + geom_point(aes(y=Honeybee_Visit_Rate), colour="black")
g <- g + geom_point(aes(y=Wild_Visit_Rate), colour="orange")
g

g <- ggplot(database2, aes(ROF_Abundance))
g <- g + geom_point(aes(y=Wild_Visit_Rate_ROF), colour="red")
g <- g + geom_point(aes(y=WILD_Rate_NO_ROF), colour="blue")
g

g <- ggplot(database2, aes(NO_ROF))
g <- g + geom_point(aes(y=Wild_Visit_Rate_ROF), colour="red")
g <- g + geom_point(aes(y=WILD_Rate_NO_ROF), colour="blue")
g


g <- ggplot(database2, aes(Honeybee_Visit_Rate_ROF))
g <- g + geom_point(aes(y=Wild_Visit_Rate_ROF), colour="red")
g <- g + geom_point(aes(y=WILD_Rate_NO_ROF), colour="blue")
g


g <- ggplot(database2, aes(HB_Rate_NO_ROF))
g <- g + geom_point(aes(y=Wild_Visit_Rate_ROF), colour="red")
g <- g + geom_point(aes(y=WILD_Rate_NO_ROF), colour="blue")
g
