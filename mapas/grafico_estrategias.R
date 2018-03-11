library("zoom")
#Direccion de la carpeta
setwd('/home/gianfranco/Documentos/proyectos R/mapas')

gps<-read.csv("ASAGPS.csv",sep=";")#Leer el archivo
gps<-subset(gps,LATITUDE<0,select=c(UNICODE:LONGITUDE))#Obtener columnas que me interesan
estrategia<-read.csv("bd_estrategias.csv")# Leer archivo
estrategia<-subset(estrategia,P=1,select=c(P:UNICODE,LOCALIDAD:estrategia))
rociadoIIc<-read.csv("ROCIADO_IICICLO_PARTICIPACION_24set15.csv",sep=";")
rociadoIIc<-subset(rociadoIIc,P=1,select=c(UNICODE,DIA_FR:OBSERVACIONES))
gps_estrategia<-merge(gps,estrategia,by="UNICODE",all.x=T)
gps_estrategia_IIc<-merge(gps_estrategia,rociadoIIc,by="UNICODE",all.x=T)
asa<-gps_estrategia_IIc

with(asa,plot(asa$LONGITUDE,asa$LATITUDE,asp=1,pch=".",xlab="Longitud",ylab="Latitud",main="Rociado II CICLO, ASA, Arequipa"))
#############
#Almacenar los datos
mnzASA.csv<-read.csv("Mz Alto Selva Alebre 2732011.csv",sep=";")
mnzASA.csv<- mnzASA.csv[c("ident","lat","long")]
clusterASA.csv<-read.csv("estrategias.csv")
clusterASA.csv<-clusterASA.csv[c("ident","lat","long","color.1")]
#Colores pa=palegreen1, rf=khaki1, l=lightyellow1, c=magenta
#Dibujamos los cluster
x<-NULL
y<-NULL
for (i in 2:nrow(clusterASA.csv)) {
  if (!is.na(clusterASA.csv[i,2])) {
    x<-c(x,clusterASA.csv[i,3])
    y<-c(y,clusterASA.csv[i,2])
  }
  else{
    polygon(x,y,col = clusterASA.csv[i-1,4])
    x<-NULL
    y<-NULL
  }
}

#Dibujamos las manzanas
#x<-NULL
#y<-NULL
for (i in 2:nrow(mnzASA.csv)) {
  if (!is.na(mnzASA.csv[i,2])) {
    x<-c(x,mnzASA.csv[i,3])
    y<-c(y,mnzASA.csv[i,2])
  }
  else{
    polygon(x,y,col = "gray96")
    x<-NULL
    y<-NULL
  }
}
#############

lines(asa$LONGITUDE,asa$LATITUDE,cex=sqrt(asa$estrategia=="Comm_Leaders")/4,asp=1,type="p",col="yellow",pch=19)
lines(asa$LONGITUDE,asa$LATITUDE,cex=sqrt(asa$estrategia=="GroupLotto")/4,asp=1,type="p",col="orange",pch=19)
lines(asa$LONGITUDE,asa$LATITUDE,cex=sqrt(asa$estrategia=="Adv_Planning")/4,asp=1,type="p",col="green",pch=19)
lines(asa$LONGITUDE,asa$LATITUDE,cex=sqrt(asa$estrategia=="Control")/4,asp=1,type="p",col="pink",pch=19)
lines(asa$LONGITUDE,asa$LATITUDE,cex=sqrt(asa$T==1)/2,asp=1,type="p",col="blue",pch=1)
lines(asa$LONGITUDE,asa$LATITUDE,cex=sqrt(asa$C==1)/4,asp=1,type="p",col="red",pch=19)
lines(asa$LONGITUDE,asa$LATITUDE,cex=sqrt(asa$R==1)/4,asp=1,type="p",col="red",pch=19)
lines(asa$LONGITUDE,asa$LATITUDE,cex=sqrt(asa$DES==1)/4,asp=1,type="p",col="red",pch=19)
lines(asa$LONGITUDE,asa$LATITUDE,cex=sqrt(asa$LV==1)/4,asp=1,type="p",col="red",pch=19)
lines(asa$LONGITUDE,asa$LATITUDE,cex=sqrt(asa$LP==1)/2,asp=1,type="p",col="red",pch=1)
# legend("topright",bty="n",c("Planificacion Avanzada","Rifa Grupal","Lideres","Control","Cerradas,Renuentes,Deshabitadas"),col=c("green","orange","yellow","pink","red"),pch=19)
# legend(-71.522,-16.371,bty="n",c("Tratadas"),col=c("blue"),pch=1)
# dev.print(device=pdf,"
