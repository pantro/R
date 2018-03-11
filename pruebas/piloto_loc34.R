#Autor: Gian Franco
######################
#install.packages (“foreign”)
library(foreign)
setwd('/home/gianfranco/Documentos/proyectos R/pruebas')
#path.expand("~/foo")
x <- read.dbf(path.expand("control/objcolor.dbf"))
y <- read.dbf(path.expand("control/t_ani.dbf"))


#********************************************************
#----------------------- Piloto --------------------
#********************************************************


#Casas que si estan en E pero no en E (siE_noM) 
siE_noM<- merge_ExM[merge_ExM$UNICODE%in%dif_ExM,]
#Quitando localidad 78 que falta digitar
siE_noM_sin78<- siE_noM[!grepl("1.1.78.",siE_noM$UNICODE),]
#Crear un campo OBS_PLANILLON
siE_noM_sin78$OBS_PLANILLON<- unlist(NA)
siE_noM_sin78<- siE_noM_sin78[,c("UNICODE","OBS_PLANILLON")]#Solo campos que nos importan por el momento
#Poner la explicacion para cada vivienda luego de revisar el planillon
siE_noM_sin78["1.1.13.76"==siE_noM_sin78$UNICODE,2]<-"NO VISITADAS" 
siE_noM_sin78["1.1.14.28"==siE_noM_sin78$UNICODE,2]<-"(P)" 
siE_noM_sin78["1.1.14.31"==siE_noM_sin78$UNICODE,2]<-"(P)" 
siE_noM_sin78["1.1.26.123A"==siE_noM_sin78$UNICODE,2]<-"(C)" 
siE_noM_sin78["1.1.26.55A"==siE_noM_sin78$UNICODE,2]<-"(C)" 
siE_noM_sin78["1.1.26.56A"==siE_noM_sin78$UNICODE,2]<-"(V)" 
siE_noM_sin78["1.1.26.56B"==siE_noM_sin78$UNICODE,2]<-"(C)" 
siE_noM_sin78["1.1.26.95A"==siE_noM_sin78$UNICODE,2]<-"(C)" 
siE_noM_sin78["1.1.29.133"==siE_noM_sin78$UNICODE,2]<-"NO VISITADAS" 
siE_noM_sin78["1.1.29.13A"==siE_noM_sin78$UNICODE,2]<-"NO VISITADAS" 
siE_noM_sin78[grepl("1.1.34.",siE_noM_sin78$UNICODE),2]<-"NO VISITADAS" # 39 casas
siE_noM_sin78["1.1.34.285A"==siE_noM_sin78$UNICODE,2]<-"(P)" 
siE_noM_sin78["1.1.34.490"==siE_noM_sin78$UNICODE,2]<-"(R1)"
siE_noM_sin78["1.1.34.491"==siE_noM_sin78$UNICODE,2]<-"(R6)"
siE_noM_sin78["1.1.34.559A"==siE_noM_sin78$UNICODE,2]<-"NO HAY"
siE_noM_sin78["1.1.34.559B"==siE_noM_sin78$UNICODE,2]<-"NO HAY"
siE_noM_sin78["1.1.34.583A"==siE_noM_sin78$UNICODE,2]<-"NO HAY"
siE_noM_sin78["1.1.34.670"==siE_noM_sin78$UNICODE,2]<-"(P)"
siE_noM_sin78["1.1.34.670A"==siE_noM_sin78$UNICODE,2]<-"(P)"
siE_noM_sin78["1.1.36.12"==siE_noM_sin78$UNICODE,2]<-"(P)" 
siE_noM_sin78["1.1.36.13"==siE_noM_sin78$UNICODE,2]<-"(P)" 
siE_noM_sin78["1.1.36.48"==siE_noM_sin78$UNICODE,2]<-"(P)" 
siE_noM_sin78["1.1.36.74"==siE_noM_sin78$UNICODE,2]<-"(P)" 
siE_noM_sin78["1.1.36.76"==siE_noM_sin78$UNICODE,2]<-"(P)" 
siE_noM_sin78["1.1.36.9"==siE_noM_sin78$UNICODE,2]<-"(P)" 
siE_noM_sin78["1.1.39.9A"==siE_noM_sin78$UNICODE,2]<-"NO VISITADAS" 
siE_noM_sin78["1.1.42.162A"==siE_noM_sin78$UNICODE,2]<-"(P)" 
siE_noM_sin78["1.1.42.38A"==siE_noM_sin78$UNICODE,2]<-"NO VISITADAS" 
siE_noM_sin78[grepl("1.1.44.",siE_noM_sin78$UNICODE),2]<-"LOTE VACIO" # 45 casas
siE_noM_sin78["1.1.44.25"==siE_noM_sin78$UNICODE,2]<-"(P)" 
siE_noM_sin78["1.1.44.4"==siE_noM_sin78$UNICODE,2]<-"(P)" 
siE_noM_sin78["1.1.44.6"==siE_noM_sin78$UNICODE,2]<-"(P)" 
siE_noM_sin78["1.1.7.114"==siE_noM_sin78$UNICODE,2]<-"(R2)" 
siE_noM_sin78["1.1.7.123"==siE_noM_sin78$UNICODE,2]<-"(C)"
siE_noM_sin78["1.1.7.127"==siE_noM_sin78$UNICODE,2]<-"(P)" 
siE_noM_sin78["1.1.7.138"==siE_noM_sin78$UNICODE,2]<-"(P)" 
siE_noM_sin78["1.1.7.16"==siE_noM_sin78$UNICODE,2]<-"(C)"
siE_noM_sin78["1.1.7.18"==siE_noM_sin78$UNICODE,2]<-"(D)"
siE_noM_sin78["1.1.7.22"==siE_noM_sin78$UNICODE,2]<-"(C)"
siE_noM_sin78["1.1.7.25"==siE_noM_sin78$UNICODE,2]<-"(C)"
siE_noM_sin78["1.1.7.26"==siE_noM_sin78$UNICODE,2]<-"(C)"
siE_noM_sin78["1.1.7.3"==siE_noM_sin78$UNICODE,2]<-"(P)"
siE_noM_sin78["1.1.7.30"==siE_noM_sin78$UNICODE,2]<-"(C)"
siE_noM_sin78["1.1.7.32A"==siE_noM_sin78$UNICODE,2]<-"(P)"
siE_noM_sin78["1.1.7.33"==siE_noM_sin78$UNICODE,2]<-"(C)"
siE_noM_sin78["1.1.7.33A"==siE_noM_sin78$UNICODE,2]<-"(C)"
siE_noM_sin78["1.1.7.34"==siE_noM_sin78$UNICODE,2]<-"(C)"
siE_noM_sin78["1.1.7.34A"==siE_noM_sin78$UNICODE,2]<-"(C)"
siE_noM_sin78["1.1.7.38"==siE_noM_sin78$UNICODE,2]<-"(C)"
siE_noM_sin78["1.1.7.43"==siE_noM_sin78$UNICODE,2]<-"(C)"
siE_noM_sin78["1.1.7.44"==siE_noM_sin78$UNICODE,2]<-"(R2)"
siE_noM_sin78["1.1.7.45"==siE_noM_sin78$UNICODE,2]<-"(R8)"
siE_noM_sin78["1.1.7.47"==siE_noM_sin78$UNICODE,2]<-"(V)"
siE_noM_sin78["1.1.7.51"==siE_noM_sin78$UNICODE,2]<-"(P)"
siE_noM_sin78["1.1.7.58"==siE_noM_sin78$UNICODE,2]<-"(P)"
siE_noM_sin78["1.1.7.61"==siE_noM_sin78$UNICODE,2]<-"(R2)"
siE_noM_sin78["1.1.7.68"==siE_noM_sin78$UNICODE,2]<-"(C)"
siE_noM_sin78["1.1.7.69"==siE_noM_sin78$UNICODE,2]<-"(C)"
siE_noM_sin78["1.1.7.73"==siE_noM_sin78$UNICODE,2]<-"(P)"
siE_noM_sin78["1.1.7.75"==siE_noM_sin78$UNICODE,2]<-"(P)"
siE_noM_sin78["1.1.7.8"==siE_noM_sin78$UNICODE,2]<-"(P)"
siE_noM_sin78["1.1.7.90"==siE_noM_sin78$UNICODE,2]<-"(C)"
siE_noM_sin78["1.1.7.92"==siE_noM_sin78$UNICODE,2]<-"(C)"
siE_noM_sin78["1.1.7.95"==siE_noM_sin78$UNICODE,2]<-"(V)"
siE_noM_sin78["1.1.77.79A"==siE_noM_sin78$UNICODE,2]<-"NO VISITADAS"
siE_noM_sin78["1.1.77.94A"==siE_noM_sin78$UNICODE,2]<-"NO VISITADAS"
siE_noM_sin78["1.1.77.94B"==siE_noM_sin78$UNICODE,2]<-"NO VISITADAS"
siE_noM_sin78["1.1.81.120"==siE_noM_sin78$UNICODE,2]<-"(C)"
siE_noM_sin78["1.1.81.134"==siE_noM_sin78$UNICODE,2]<-"(C)"
siE_noM_sin78["1.1.81.175"==siE_noM_sin78$UNICODE,2]<-"(C)"
siE_noM_sin78["1.1.81.2"==siE_noM_sin78$UNICODE,2]<-"LOTE VACIO"
siE_noM_sin78["1.1.81.27A"==siE_noM_sin78$UNICODE,2]<-"(C)"
siE_noM_sin78["1.1.81.27B"==siE_noM_sin78$UNICODE,2]<-"NO VISITADAS"
siE_noM_sin78["1.1.81.36"==siE_noM_sin78$UNICODE,2]<-"(P)"
siE_noM_sin78["1.1.81.4"==siE_noM_sin78$UNICODE,2]<-"(R8)"
siE_noM_sin78["1.1.81.51"==siE_noM_sin78$UNICODE,2]<-"(C)"
siE_noM_sin78["1.1.81.6"==siE_noM_sin78$UNICODE,2]<-"(C)"
siE_noM_sin78["1.1.81.61"==siE_noM_sin78$UNICODE,2]<-"(C)"
siE_noM_sin78["1.1.81.66"==siE_noM_sin78$UNICODE,2]<-"(C)"
siE_noM_sin78["1.1.81.7"==siE_noM_sin78$UNICODE,2]<-"(C)"
siE_noM_sin78["1.1.81.7A"==siE_noM_sin78$UNICODE,2]<-"(C)"
siE_noM_sin78["1.1.81.86"==siE_noM_sin78$UNICODE,2]<-"(C)"
siE_noM_sin78["1.1.81.93"==siE_noM_sin78$UNICODE,2]<-"(V)"
siE_noM_sin78["1.1.81.96"==siE_noM_sin78$UNICODE,2]<-"(V)"
siE_noM_sin78["1.1.82.21A"==siE_noM_sin78$UNICODE,2]<-"NO VISITADAS"
siE_noM_sin78["1.1.82.59"==siE_noM_sin78$UNICODE,2]<-"NO VISITADAS"




###################################
#----- Estaregias VS Sensis -----sin aplicar filtro  "675"
####################################
#Difencias entre Estrategia y Sensis
dif_ExS<-setdiff(estrategias$UNICODE,statusSensis$UNICODE)
dif_SxE<-setdiff(statusSensis$UNICODE,estrategias$UNICODE)

#Interseccion
int_ExS<-intersect(estrategias$UNICODE, statusSensis$UNICODE)

#Listas de datos que SI Sensis y NO Estrategias "675"
siS_noE<- statusSensis[statusSensis$UNICODE%in%dif_SxE,]

#Quitando todas las localidades que no deben haber
fallas_siSnoE<- siS_noE
fallas_siSnoE<- fallas_siSnoE[!grepl("1.1.15.",fallas_siSnoE$UNICODE, fixed = TRUE),] #11 datos eliminados
fallas_siSnoE<- fallas_siSnoE[!grepl("1.1.2.",fallas_siSnoE$UNICODE, fixed = TRUE),] #123 datos eliminados
fallas_siSnoE<- fallas_siSnoE[!grepl("1.1.28.",fallas_siSnoE$UNICODE, fixed = TRUE),] #17 datos eliminados
fallas_siSnoE<- fallas_siSnoE[!grepl("1.1.37.",fallas_siSnoE$UNICODE, fixed = TRUE),] #8 datos eliminados
fallas_siSnoE<- fallas_siSnoE[!grepl("1.1.38.",fallas_siSnoE$UNICODE, fixed = TRUE),] #21 datos eliminados
fallas_siSnoE<- fallas_siSnoE[!grepl("1.1.40.",fallas_siSnoE$UNICODE, fixed = TRUE),] #20 datos eliminados
fallas_siSnoE<- fallas_siSnoE[!grepl("1.1.6.",fallas_siSnoE$UNICODE, fixed = TRUE),] #184 datos eliminados
fallas_siSnoE<- fallas_siSnoE[!grepl("1.1.62.",fallas_siSnoE$UNICODE, fixed = TRUE),] #92 datos eliminados
fallas_siSnoE<- fallas_siSnoE[!grepl("1.1.70.",fallas_siSnoE$UNICODE, fixed = TRUE),] #58 datos eliminados
fallas_siSnoE<- fallas_siSnoE[!grepl("1.1.76.",fallas_siSnoE$UNICODE, fixed = TRUE),] #3 datos eliminados
#Quitando los datos mal digitados
fallas_siSnoE<- fallas_siSnoE[!grepl("...",fallas_siSnoE$UNICODE, fixed = TRUE),] #33 datos eliminados
fallas_siSnoE<- fallas_siSnoE[!grepl("1.1..",fallas_siSnoE$UNICODE, fixed = TRUE),] #20 datos eliminados
#Agregando una columna "OBS_PLANILLON" para poner que que paso con esas viviendas
planillon_siSnoE<- fallas_siSnoE
planillon_siSnoE$OBS_PANILLLON<- unlist(NA)
planillon_siSnoE["1.1.13.192"==planillon_siSnoE$UNICODE,3]<-"Revisar, no hay en planillon"
planillon_siSnoE["1.1.13.25?"==planillon_siSnoE$UNICODE,3]<-"Error de digtacion, eliminar, esta duplicado y mal, por que si fue programada"
planillon_siSnoE["1.1.13.27A"==planillon_siSnoE$UNICODE,3]<-"CERRADA planillon y cuantis"
planillon_siSnoE["1.1.13.513"==planillon_siSnoE$UNICODE,3]<-"Revisar, no hay en planillon"
planillon_siSnoE["1.1.13.610"==planillon_siSnoE$UNICODE,3]<-"Revisar, no hay en planillon"
planillon_siSnoE["1.1.13.639"==planillon_siSnoE$UNICODE,3]<-"Revisar, no hay en planillon"
planillon_siSnoE["1.1.13.646"==planillon_siSnoE$UNICODE,3]<-"Revisar, no hay en planillon"
planillon_siSnoE["1.1.23.107A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon"
planillon_siSnoE["1.1.23.124B"==planillon_siSnoE$UNICODE,3]<-"No hay-> planillon"
planillon_siSnoE["1.1.25.49A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.25.59A"==planillon_siSnoE$UNICODE,3]<-"R4-> planillon, Cerrada en cuantis"
planillon_siSnoE["1.1.25.62A"==planillon_siSnoE$UNICODE,3]<-"Cerrada-> planillon"
planillon_siSnoE["1.1.26.142A"==planillon_siSnoE$UNICODE,3]<-"No hay-> planillon"
planillon_siSnoE["1.1.26.28A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.26.50A"==planillon_siSnoE$UNICODE,3]<-"Cerrada-> planillon"
planillon_siSnoE["1.1.26.69A"==planillon_siSnoE$UNICODE,3]<-"No hay-> planillon"
planillon_siSnoE["1.1.26.96A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.27.116A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.27.148A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.27.148B"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.27.205A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.27.224A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.27.276A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.27.27A"==planillon_siSnoE$UNICODE,3]<-"Volver-> planillon"
planillon_siSnoE["1.1.27.282A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.27.42A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.27.525"==planillon_siSnoE$UNICODE,3]<-"Revisar, no hay en planillon"
planillon_siSnoE["1.1.27.79A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.27.84A"==planillon_siSnoE$UNICODE,3]<-"R8 -> planillon, Renuente en cuantis"
planillon_siSnoE["1.1.27.890"==planillon_siSnoE$UNICODE,3]<-"Revisar, no hay en planillon"
planillon_siSnoE["1.1.27.95A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.29.57A"==planillon_siSnoE$UNICODE,3]<-"Cerrada-> planillon"
planillon_siSnoE["1.1.29.6A"==planillon_siSnoE$UNICODE,3]<-"Cerrada-> planillon"
planillon_siSnoE["1.1.34.100A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.34.120A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.34.141A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon"
planillon_siSnoE["1.1.34.159A"==planillon_siSnoE$UNICODE,3]<-"Cerrada-> planillon"
planillon_siSnoE["1.1.34.24B"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.34.256A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.34.259A"==planillon_siSnoE$UNICODE,3]<-"No hay-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.34.266A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.34.295A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.34.2A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.34.309A"==planillon_siSnoE$UNICODE,3]<-"Cerrada-> planillon"
planillon_siSnoE["1.1.34.343B"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.34.38A"==planillon_siSnoE$UNICODE,3]<-"No hay-> planillon"
planillon_siSnoE["1.1.34.431A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Cerrada en cuantis"
planillon_siSnoE["1.1.34.439A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.34.451A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon"
planillon_siSnoE["1.1.34.479A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.34.518A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.34.521A"==planillon_siSnoE$UNICODE,3]<-"No hay-> planillon"
planillon_siSnoE["1.1.34.53A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.34.571A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.34.571B"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Cerrada en cuantis"
planillon_siSnoE["1.1.34.577A"==planillon_siSnoE$UNICODE,3]<-"Volver-> planillon"
planillon_siSnoE["1.1.34.647A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.34.64A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.34.656B"==planillon_siSnoE$UNICODE,3]<-"Cerrada-> planillon"
planillon_siSnoE["1.1.34.714A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.34.739"==planillon_siSnoE$UNICODE,3]<-"No hay-> planillon"
planillon_siSnoE["1.1.36.39A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon"
planillon_siSnoE["1.1.36.6A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.36.7A"==planillon_siSnoE$UNICODE,3]<-"No hay-> planillon"
planillon_siSnoE["1.1.39.135A"==planillon_siSnoE$UNICODE,3]<-"No hay-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.39.1A"==planillon_siSnoE$UNICODE,3]<-"No hay-> planillon"
planillon_siSnoE["1.1.45.37B"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.71.132A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.71.56A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.77.104B"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon"
planillon_siSnoE["1.1.77.137a"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, , ERROR AL DIGITAR Tratada en cuantis"
planillon_siSnoE["1.1.77.29A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.77.38C"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.77.51A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.77.54A"==planillon_siSnoE$UNICODE,3]<-"No hay-> planillon"
planillon_siSnoE["1.1.77.97A"==planillon_siSnoE$UNICODE,3]<-"Programada-> planillon, Tratada en cuantis"
planillon_siSnoE["1.1.82.59B"==planillon_siSnoE$UNICODE,3]<-"No hay-> planillon"
