setwd('/home/gianfranco/Documentos/enviado a sarah 08-set-2015')
casas_cc.csv<-read.csv("Puntos_CerroColorado.csv", header = T)
encuestas_cc.csv<-read.csv("ccolorado_INSPECCIONES_8sep2015.csv", header = T)
formato2_cc.csv<-read.csv("f2_ccolorado_actual.csv", header = T)

#Buscar duplicados en las bases de datos
puntos_uni_rep_cas <- casas_cc.csv[which(duplicated(casas_cc.csv$UNICODE)),1]
aux2<-casas_cc.csv[casas_cc.csv$UNICODE %in% puntos_uni_rep_cas,]
puntos_uni_rep_enc <- encuestas_cc.csv[which(duplicated(encuestas_cc.csv$UNICODE)),1]
aux3_sindup<-encuestas_cc.csv[!encuestas_cc.csv$UNICODE %in% puntos_uni_rep_enc,]
encuestas_cc.csv<-aux3_sindup
#aux3<-encuestas_cc.csv[encuestas_cc.csv$UNICODE %in% puntos_uni_rep_enc,]
#aux3 <- aux3[order(aux3$UNICODE),]

#Junto las primeras dos tablas y lo almaceno en "aux" : CASAS Y ENCUESTAS
aux=merge(casas_cc.csv, encuestas_cc.csv, by=c("UNICODE"), all.x = TRUE)
#Junto con la base de datos que falta:FORMATO2
merge_casas_encuestas_formato2_08sep2015=merge(aux, formato2_cc.csv, by=c("UNICODE"), all.x = TRUE)
write.csv(merge_casas_encuestas_formato2_08sep2015,'merge_casas_encuestas_formato2_08sep2015.csv')
