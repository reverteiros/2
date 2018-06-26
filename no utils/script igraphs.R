plants1<-levels(plot1$Codi_planta)
pollinators1<-levels(plot1$Nom_definitiu)
actors <- data.frame(name=c(plants1,pollinators1))
relations <- data.frame(Pollinator=plot1$Nom_definitiu,
                        Plant=plot1$Codi_planta,
                        Frequence=plot1$Frequencia)
g1 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants2<-levels(plot2$Codi_planta)
pollinators2<-levels(plot2$Nom_definitiu)
actors <- data.frame(name=c(plants2,pollinators2))
relations <- data.frame(Pollinator=plot2$Nom_definitiu,
                        Plant=plot2$Codi_planta,
                        Frequence=plot2$Frequencia)
g2 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants3<-levels(plot3$Codi_planta)
pollinators3<-levels(plot3$Nom_definitiu)
actors <- data.frame(name=c(plants3,pollinators3))
relations <- data.frame(Pollinator=plot3$Nom_definitiu,
                        Plant=plot3$Codi_planta,
                        Frequence=plot3$Frequencia)
g3 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants4<-levels(plot4$Codi_planta)
pollinators4<-levels(plot4$Nom_definitiu)
actors <- data.frame(name=c(plants4,pollinators4))
relations <- data.frame(Pollinator=plot4$Nom_definitiu,
                        Plant=plot4$Codi_planta,
                        Frequence=plot4$Frequencia)
g4 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants5<-levels(plot5$Codi_planta)
pollinators5<-levels(plot5$Nom_definitiu)
actors <- data.frame(name=c(plants5,pollinators5))
relations <- data.frame(Pollinator=plot5$Nom_definitiu,
                        Plant=plot5$Codi_planta,
                        Frequence=plot5$Frequencia)
g5 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants6<-levels(plot6$Codi_planta)
pollinators6<-levels(plot6$Nom_definitiu)
actors <- data.frame(name=c(plants6,pollinators6))
relations <- data.frame(Pollinator=plot6$Nom_definitiu,
                        Plant=plot6$Codi_planta,
                        Frequence=plot6$Frequencia)
g6 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants7<-levels(plot7$Codi_planta)
pollinators7<-levels(plot7$Nom_definitiu)
actors <- data.frame(name=c(plants7,pollinators7))
relations <- data.frame(Pollinator=plot7$Nom_definitiu,
                        Plant=plot7$Codi_planta,
                        Frequence=plot7$Frequencia)
g7 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants8<-levels(plot8$Codi_planta)
pollinators8<-levels(plot8$Nom_definitiu)
actors <- data.frame(name=c(plants8,pollinators8))
relations <- data.frame(Pollinator=plot8$Nom_definitiu,
                        Plant=plot8$Codi_planta,
                        Frequence=plot8$Frequencia)
g8 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants9<-levels(plot9$Codi_planta)
pollinators9<-levels(plot9$Nom_definitiu)
actors <- data.frame(name=c(plants9,pollinators9))
relations <- data.frame(Pollinator=plot9$Nom_definitiu,
                        Plant=plot9$Codi_planta,
                        Frequence=plot9$Frequencia)
g9 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants10<-levels(plot10$Codi_planta)
pollinators10<-levels(plot10$Nom_definitiu)
actors <- data.frame(name=c(plants10,pollinators10))
relations <- data.frame(Pollinator=plot10$Nom_definitiu,
                        Plant=plot10$Codi_planta,
                        Frequence=plot10$Frequencia)
g10 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants11<-levels(plot11$Codi_planta)
pollinators11<-levels(plot11$Nom_definitiu)
actors <- data.frame(name=c(plants11,pollinators11))
relations <- data.frame(Pollinator=plot11$Nom_definitiu,
                        Plant=plot11$Codi_planta,
                        Frequence=plot11$Frequencia)
g11 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants12<-levels(plot12$Codi_planta)
pollinators12<-levels(plot12$Nom_definitiu)
actors <- data.frame(name=c(plants12,pollinators12))
relations <- data.frame(Pollinator=plot12$Nom_definitiu,
                        Plant=plot12$Codi_planta,
                        Frequence=plot12$Frequencia)
g12 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants13<-levels(plot13$Codi_planta)
pollinators13<-levels(plot13$Nom_definitiu)
actors <- data.frame(name=c(plants13,pollinators13))
relations <- data.frame(Pollinator=plot13$Nom_definitiu,
                        Plant=plot13$Codi_planta,
                        Frequence=plot13$Frequencia)
g13 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants14<-levels(plot14$Codi_planta)
pollinators14<-levels(plot14$Nom_definitiu)
actors <- data.frame(name=c(plants14,pollinators14))
relations <- data.frame(Pollinator=plot14$Nom_definitiu,
                        Plant=plot14$Codi_planta,
                        Frequence=plot14$Frequencia)
g14 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants15<-levels(plot15$Codi_planta)
pollinators15<-levels(plot15$Nom_definitiu)
actors <- data.frame(name=c(plants15,pollinators15))
relations <- data.frame(Pollinator=plot15$Nom_definitiu,
                        Plant=plot15$Codi_planta,
                        Frequence=plot15$Frequencia)
g15 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants16<-levels(plot16$Codi_planta)
pollinators16<-levels(plot16$Nom_definitiu)
actors <- data.frame(name=c(plants16,pollinators16))
relations <- data.frame(Pollinator=plot16$Nom_definitiu,
                        Plant=plot16$Codi_planta,
                        Frequence=plot16$Frequencia)
g16 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants17<-levels(plot17$Codi_planta)
pollinators17<-levels(plot17$Nom_definitiu)
actors <- data.frame(name=c(plants17,pollinators17))
relations <- data.frame(Pollinator=plot17$Nom_definitiu,
                        Plant=plot17$Codi_planta,
                        Frequence=plot17$Frequencia)
g17 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants18<-levels(plot18$Codi_planta)
pollinators18<-levels(plot18$Nom_definitiu)
actors <- data.frame(name=c(plants18,pollinators18))
relations <- data.frame(Pollinator=plot18$Nom_definitiu,
                        Plant=plot18$Codi_planta,
                        Frequence=plot18$Frequencia)
g18 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants19<-levels(plot19$Codi_planta)
pollinators19<-levels(plot19$Nom_definitiu)
actors <- data.frame(name=c(plants19,pollinators19))
relations <- data.frame(Pollinator=plot19$Nom_definitiu,
                        Plant=plot19$Codi_planta,
                        Frequence=plot19$Frequencia)
g19 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants20<-levels(plot20$Codi_planta)
pollinators20<-levels(plot20$Nom_definitiu)
actors <- data.frame(name=c(plants20,pollinators20))
relations <- data.frame(Pollinator=plot20$Nom_definitiu,
                        Plant=plot20$Codi_planta,
                        Frequence=plot20$Frequencia)
g20 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants21<-levels(plot21$Codi_planta)
pollinators21<-levels(plot21$Nom_definitiu)
actors <- data.frame(name=c(plants21,pollinators21))
relations <- data.frame(Pollinator=plot21$Nom_definitiu,
                        Plant=plot21$Codi_planta,
                        Frequence=plot21$Frequencia)
g21 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants22<-levels(plot22$Codi_planta)
pollinators22<-levels(plot22$Nom_definitiu)
actors <- data.frame(name=c(plants22,pollinators22))
relations <- data.frame(Pollinator=plot22$Nom_definitiu,
                        Plant=plot22$Codi_planta,
                        Frequence=plot22$Frequencia)
g22 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants23<-levels(plot23$Codi_planta)
pollinators23<-levels(plot23$Nom_definitiu)
actors <- data.frame(name=c(plants23,pollinators23))
relations <- data.frame(Pollinator=plot23$Nom_definitiu,
                        Plant=plot23$Codi_planta,
                        Frequence=plot23$Frequencia)
g23 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants24<-levels(plot24$Codi_planta)
pollinators24<-levels(plot24$Nom_definitiu)
actors <- data.frame(name=c(plants24,pollinators24))
relations <- data.frame(Pollinator=plot24$Nom_definitiu,
                        Plant=plot24$Codi_planta,
                        Frequence=plot24$Frequencia)
g24 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants25<-levels(plot25$Codi_planta)
pollinators25<-levels(plot25$Nom_definitiu)
actors <- data.frame(name=c(plants25,pollinators25))
relations <- data.frame(Pollinator=plot25$Nom_definitiu,
                        Plant=plot25$Codi_planta,
                        Frequence=plot25$Frequencia)
g25 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants26<-levels(plot26$Codi_planta)
pollinators26<-levels(plot26$Nom_definitiu)
actors <- data.frame(name=c(plants26,pollinators26))
relations <- data.frame(Pollinator=plot26$Nom_definitiu,
                        Plant=plot26$Codi_planta,
                        Frequence=plot26$Frequencia)
g26 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants27<-levels(plot27$Codi_planta)
pollinators27<-levels(plot27$Nom_definitiu)
actors <- data.frame(name=c(plants27,pollinators27))
relations <- data.frame(Pollinator=plot27$Nom_definitiu,
                        Plant=plot27$Codi_planta,
                        Frequence=plot27$Frequencia)
g27 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants28<-levels(plot28$Codi_planta)
pollinators28<-levels(plot28$Nom_definitiu)
actors <- data.frame(name=c(plants28,pollinators28))
relations <- data.frame(Pollinator=plot28$Nom_definitiu,
                        Plant=plot28$Codi_planta,
                        Frequence=plot28$Frequencia)
g28 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants29<-levels(plot29$Codi_planta)
pollinators29<-levels(plot29$Nom_definitiu)
actors <- data.frame(name=c(plants29,pollinators29))
relations <- data.frame(Pollinator=plot29$Nom_definitiu,
                        Plant=plot29$Codi_planta,
                        Frequence=plot29$Frequencia)
g29 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants30<-levels(plot30$Codi_planta)
pollinators30<-levels(plot30$Nom_definitiu)
actors <- data.frame(name=c(plants30,pollinators30))
relations <- data.frame(Pollinator=plot30$Nom_definitiu,
                        Plant=plot30$Codi_planta,
                        Frequence=plot30$Frequencia)
g30 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants31<-levels(plot31$Codi_planta)
pollinators31<-levels(plot31$Nom_definitiu)
actors <- data.frame(name=c(plants31,pollinators31))
relations <- data.frame(Pollinator=plot31$Nom_definitiu,
                        Plant=plot31$Codi_planta,
                        Frequence=plot31$Frequencia)
g31 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants32<-levels(plot32$Codi_planta)
pollinators32<-levels(plot32$Nom_definitiu)
actors <- data.frame(name=c(plants32,pollinators32))
relations <- data.frame(Pollinator=plot32$Nom_definitiu,
                        Plant=plot32$Codi_planta,
                        Frequence=plot32$Frequencia)
g32 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants33<-levels(plot33$Codi_planta)
pollinators33<-levels(plot33$Nom_definitiu)
actors <- data.frame(name=c(plants33,pollinators33))
relations <- data.frame(Pollinator=plot33$Nom_definitiu,
                        Plant=plot33$Codi_planta,
                        Frequence=plot33$Frequencia)
g33 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants34<-levels(plot34$Codi_planta)
pollinators34<-levels(plot34$Nom_definitiu)
actors <- data.frame(name=c(plants34,pollinators34))
relations <- data.frame(Pollinator=plot34$Nom_definitiu,
                        Plant=plot34$Codi_planta,
                        Frequence=plot34$Frequencia)
g34 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants35<-levels(plot35$Codi_planta)
pollinators35<-levels(plot35$Nom_definitiu)
actors <- data.frame(name=c(plants35,pollinators35))
relations <- data.frame(Pollinator=plot35$Nom_definitiu,
                        Plant=plot35$Codi_planta,
                        Frequence=plot35$Frequencia)
g35 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants36<-levels(plot36$Codi_planta)
pollinators36<-levels(plot36$Nom_definitiu)
actors <- data.frame(name=c(plants36,pollinators36))
relations <- data.frame(Pollinator=plot36$Nom_definitiu,
                        Plant=plot36$Codi_planta,
                        Frequence=plot36$Frequencia)
g36 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants37<-levels(plot37$Codi_planta)
pollinators37<-levels(plot37$Nom_definitiu)
actors <- data.frame(name=c(plants37,pollinators37))
relations <- data.frame(Pollinator=plot37$Nom_definitiu,
                        Plant=plot37$Codi_planta,
                        Frequence=plot37$Frequencia)
g37 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants38<-levels(plot38$Codi_planta)
pollinators38<-levels(plot38$Nom_definitiu)
actors <- data.frame(name=c(plants38,pollinators38))
relations <- data.frame(Pollinator=plot38$Nom_definitiu,
                        Plant=plot38$Codi_planta,
                        Frequence=plot38$Frequencia)
g38 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants39<-levels(plot39$Codi_planta)
pollinators39<-levels(plot39$Nom_definitiu)
actors <- data.frame(name=c(plants39,pollinators39))
relations <- data.frame(Pollinator=plot39$Nom_definitiu,
                        Plant=plot39$Codi_planta,
                        Frequence=plot39$Frequencia)
g39 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

plants40<-levels(plot40$Codi_planta)
pollinators40<-levels(plot40$Nom_definitiu)
actors <- data.frame(name=c(plants40,pollinators40))
relations <- data.frame(Pollinator=plot40$Nom_definitiu,
                        Plant=plot40$Codi_planta,
                        Frequence=plot40$Frequencia)
g40 <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)