setwd('/home/gianfranco/Documentos/proyectos R/pruebas')

estrategia<-read.csv("bd_estrategias.csv")#Leer el archivo
rociadoI<-read.csv("Iciclo_ASA.csv")#Leer el archivo
x<- data.frame()
locEst<-c("1.1.7.","1.1.8.","1.1.13.","1.1.14.","1.1.22.","1.1.23.","1.1.25.","1.1.26.","1.1.27.","1.1.29.","1.1.34.","1.1.36.","1.1.39.","1.1.42.","1.1.44.","1.1.45.","1.1.71.","1.1.77.","1.1.78.","1.1.81.","1.1.82.","1.1.83.")
for (i in 1:length(locEst)) {
  y<- rociadoI[grepl(locEst[i],rociadoI$UNICODE, fixed = T),]
  x<-rbind(x,y)
}


y<-setdiff(estrategia$UNICODE,rociadoI$UNICODE)
x<-setdiff(rociadoI$UNICODE,estrategia$UNICODE)
