
x11()
dat <- read.csv(file = "/home/gianfranco/Documentos/github/Participation/merge_participacion/resultados/info_sindupli_c1yc2ASA.csv")

tcoord <- dat[dat$STATUS==1, c("LONGITUD","LATITUD")]
rcoord <- dat[dat$STATUS==2, c("LONGITUD","LATITUD")]
ccoord <- dat[dat$STATUS==3, c("LONGITUD","LATITUD" )]
par(mfrow=c(1,1))

plot(tcoord, col = "green", pch=19, cex = 0.3, cex.main=0.8, ylab="Latitud", main = "Spatial distribution of houses by status", xlab="Longitud")
points(rcoord, col = "red", pch=19, cex = 0.3)
points(ccoord, col = "blue", pch=19, cex = 0.3)

#Dibujamos los cluster
  clusterASA<-read.csv(file = "'/home/gianfranco/Documentos/github/Participation/merge_participacion/plot_asa/cluster_estrategias.csv")
  clusterASA<-clusterASA[c("ident","lat","long")]
  clusterASA$color[grepl("pa",clusterASA$ident)] <- "lightblue"#"firebrick1"
  clusterASA$color[grepl("rg",clusterASA$ident)] <- "orange"
  clusterASA$color[grepl("l",clusterASA$ident)] <- "black"
  clusterASA$color[grepl("c",clusterASA$ident)] <- "hotpink"
  
  #Colores pa=seagreen, rf=orange, l=yellow, c=hotpink
  #Dibujamos los cluster
    x<-NULL
    y<-NULL
    n_row <- nrow(clusterASA)+1
    for (i in 2:n_row) {
      if (!is.na(clusterASA[i,2])) {
        x<-c(x,clusterASA[i,3])
        y<-c(y,clusterASA[i,2])
      }
      else{
        polygon(x,y,border = clusterASA[i-1,4], lwd = 2)
        x<-NULL
        y<-NULL
      }
    }
    
  #Leyenda
    legend(x=-71.5, y=-16.375, legend = c("Tratada","Renuente","Cerrada"), col = c("green", "red","blue"), pch = 19, bty="n")
    legend(x=-71.502, y=-16.378, legend = c("Lotto","Adv.Planning","Leader","Control"), col = c("orange", "lightblue","black","hotpink"), lty=1, bty="n")
  #Imprimir en pdf
    dev.print(device = pdf,"SpatialDistributionofHousesbyStatus.pdf")
    