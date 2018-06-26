
datum<-data.frame(fWNdistapis$fWNdistapis,tWNdistapis$tWNdistapis,tOSdistapis$tOSdistapis,fOSdistapis$fOSdistapis,fOSdistsenseapis$fOSdistsenseapis,fWNdistsenseapis$fWNdistsenseapis,tOSdistsenseapis$tOSdistsenseapis,tWNdistsenseapis$tWNdistsenseapis,quant.senseapis$quant.senseapis,quant.plants$quant.plants,fenologiadist$fenologia,geographicdist$geographicdist,quant.polls$quant.polls)

names(datum) <- c("WN.qual.apis","WN.quant.apis","OS.quant.apis","OS.qual.apis","OS.qual.senseapis","WN.qual.senseapis","OS.quant.senseapis","WN.quant.senseapis","quant.senseapis","quant.flors","fenologia","distancia","quant.polls")

head(datum)
write.csv(datum,"D:\\Usuarios\\s.reverte\\Desktop\\dades\\datum.csv")

