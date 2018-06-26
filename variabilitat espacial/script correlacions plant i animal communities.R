
setwd("D:/Usuarios/s.reverte/Desktop/dades")

dades<-read.table("database.txt",header=T)
dades2<-read.table("bitxos quantitatiu.txt",header=T)
dades$Apis<-dades2$Apis
dades$Wild<-dades$Pollinator_count-dades$Apis
names(data)

OVERALL_Flowers
flower_richness
dades$visit_rate<-dades$Pollinator_count/dades$OVERALL_Flowers*1000
dades$Apis_visit_rate<-dades$Apis/dades$OVERALL_Flowers*1000
dades$wild_visit_rate<-dades$Wild/dades$OVERALL_Flowers*1000

cor.test(dades$Apis,dades$Wild)
cor.test(dades$Apis_visit_rate,dades$wild_visit_rate)
cor.test(dades$TVUH_Flowers,dades$visit_rate)
cor.test(dades$TVUH_Flowers,dades$Apis)
cor.test(dades$TVUH_Flowers,dades$Wild)
cor.test(dades$TVUH_Flowers,dades$Apis_visit_rate)
cor.test(dades$TVUH_Flowers,dades$wild_visit_rate)
