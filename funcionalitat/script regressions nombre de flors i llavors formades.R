
DATA<-read.table("Database.txt",header=T)
DATA

reg<-lm(DATA$Overall_Flowers~DATA$TVUF_Percentage_Avorted)
summary(reg)

plot1<-plot(DATA$Overall_Flowers~DATA$TVUH_Percentage_Avorted, xlab="Percentatge d'òvuls pol·linitzats en Thymus Hermafrodita", ylab="Nombre total de flors a la parcel·la")
abline(reg)


Percs<-matrix(0,40,3)
Percs[,1]<-DATA$TVUF_Flowers/DATA$Overall_Flowers*100
Percs[,2]<-DATA$TVUH_Flowers/DATA$Overall_Flowers*100
Percs[,3]<-DATA$TVUH_Flowers/DATA$TVUF_Flowers


reg<-lm(Percs[,3]~DATA$TVUF_Pollinated_ovules)
summary(reg)

plot1<-plot(DATA$TVUH_Flowers~DATA$TVUF_Pollinated_ovules, xlab="Percentatge d'òvuls pol·linitzats en Thymus Femella", ylab="Nombre de flors de Thymus Hermafrodita")
abline(reg)

