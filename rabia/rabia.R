
#Ruta
  setwd('/home/gianfranco/Documentos/proyectos R/rabia')
  
#Leer los archivos
  puentes<-read.csv("PUENTES.csv")
  casos_rabia<-read.csv("Casos_Rabia_2015.csv")
  torrenteras<-read.csv("TORRENTERAS.csv")
  
#Almacenando los campos "LONGITUD" y"LATITUD"
  puentes <- puentes[, c("long","lat")]
  casos_rabia <- casos_rabia[, c("long","lat")]
  torrenteras <- torrenteras[, c("ident","long","lat")]
  
#Graficando
  x11()
  #Casos de rabia
    plot(casos_rabia, col = "red", pch=19, cex = 1, cex.main=0.8, ylab="Latitud", 
         main = "Casos de rabia en Arequipa", xlab="Longitud", xlim=c(-71.60,-71.45))
  #Torrenteras
    x<-NULL
    y<-NULL
    n_row <- nrow(torrenteras)+1
    for (i in 2:n_row) {
      if (!is.na(torrenteras[i,2])) {
        x<-c(x,torrenteras[i,2])
        y<-c(y,torrenteras[i,3])
      }
      else{
        lines(x,y,col="blue",pch=19)
        x<-NULL
        y<-NULL
      }
    }
  #Puentes
    points(puentes, col = "darkgreen", pch=2, cex = 1)
    
#Leyenda
  #En la parte inferior izquierda  
    #legend(x=-71.605, y=-16.44, legend = "Torrenteras", col = "blue", lty=1, bty="n")
    #legend(x=-71.596, y=-16.447, bty="n",c("Puentes","Casos de rabia"),col=c("green","red"),pch=c(2,19))
  #En la parte superior derecha
    legend(x=-71.50, y=-16.305, legend = "Torrenteras", col = "blue", lty=1, bty="n")
    legend(x=-71.491, y=-16.311, bty="n",c("Puentes","Casos de rabia"),col=c("darkgreen","red"),pch=c(2,19))
    
#Imprimiendo en pdf
  dev.print(device = pdf,"CasosRabiaAQP.pdf")
  