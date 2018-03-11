##################################################
#
# Autor: Gian Franco
# Fecha: 15/12/2015
#
# MATCH CUANTIS: Match realizado a la primera y segunda digitación de CUANTIS.
#
##################################################

#Llamando a las funciones necesarias para hacer el MATCH
  #Ruta donde se encuentra el archivo "FunctionMatch.R"
  setwd('/home/gianfranco/Documentos/github/Participation/match')
  source("FunctionMatch.R")

#Ruta de las base de datos que vamos a utilizar en el codigo
  setwd('/home/gianfranco/Documentos/github/Participation/match/match_cuantis/bd')
  #setwd('~/Participation/match/bd')

#Leer el archivos
  data_1<-read.csv("Cuantis_participacion - BD.csv")
  data_2<-read.csv("Cuantis_participacion_2da_dig - 2DA. DIG..csv")

#Seleccionando campos importantes para análisis principal báciso
  sel<-c("UNICODE","P","D","L","V","DIA_FR","MES_FR","ANIO_FR","T","C","R","DES","LV","LP","HORA_INI","HORA_FIN")
  data_1<-data_1[,sel]
  data_2<-data_2[,sel]

#Filtrando para que no se esté buscando localidades que no estan en el estudio
  filtro<-function(dat,dentro)
  {
    filtrado<-dat[is.element(dat$L,dentro)==TRUE,]
    return(filtrado)
  }
  dentro<-c(7,8,13,14,22,23,25,26,27,29,34,36,39,42,44,45,71,77,78,81,82,83)
  #data_1
  filtro_data_1<-filtro(data_1,dentro)
  data_1<-filtro_data_1 #307 registros eliminados
  #data_2
  filtro_data_2<-filtro(data_2,dentro)
  data_2<-filtro_data_2 #29 registros eliminados
  
#Uniformizando las columnas que tienen horas
  #Campos que contienen las horas
  colHour<- c("HORA_INI","HORA_FIN")
  #Utilizamos la funcion UNIF_HOUR
  data_1 <- UnifHour(data_1, colHour)
  data_2 <- UnifHour(data_2, colHour)
  
#Cambiando los NA a "0" para un mejor análisis
  #Almacenando los nombres de las columnas que vamos a querer que sus NAs cambien a "0"
  namesCol<-c("T","C","R","DES","LV","LP")
  #Utilizando la funcion ChanceNAxCero
  data_1 <- ChanceNAxCero(data_1, namesCol)
  data_2 <- ChanceNAxCero(data_2, namesCol)
    
#LLamando a la funcion Match
  data_total<- Match(data_1, data_2)

#Almacenando los datos que hicieron el MATCH perfectamente
  result_match<-data_total$perfect
  
#Almacenando los datos problema
  data_problem <- data_total$problem
  
#Ordenar por MES, DIA, VIVIENDA y LOCALIDAD. Luego por LOCALIDAD denuevo, para poder buscar mas facil los errores ya que estaran en la misma forma en que fueron ingresados.
  data_problem <- data_problem[order(data_problem$MES_FR, data_problem$DIA_FR, data_problem$V, data_problem$L),]
  data_problem <- data_problem[order(data_problem$L),]
  
#Exportar en un archivo CSV el resultado del MATCH
  #Ruta donde se almacenan los resultados
  setwd('/home/gianfranco/Documentos/github/Participation/match/match_cuantis/resultados')
  write.csv(data_problem, "datos_problema_match_cuantis.csv")

##################################################
#
# MATCH CUANTIS: FASE 2
#
##################################################
  #Ruta donde se encuentran los archivos para la FASE 2
    setwd('/home/gianfranco/Documentos/github/Participation/match/match_cuantis/correcciones')
  
  #Leer el archivos
    #Los datos corregidos deben tener un campo "ERRORES_DOCUMENTADOS" donde se explica el error que se encontro
      f2_cuantis<-read.csv("datos_problema_match_cuantis_CORREGIDO.csv")
  
  #ELIMINAR DATOS: 
  # - Datos que fueron tachados en las hojas pero fueron ingresados
  # - Datos que no se encuentra sus datos físicos
      f2_cuantis <- f2_cuantis[!grepl("ELIMINAR", f2_cuantis$ERRORES_DOCUMENTADOS, fixed = TRUE),]# Eliminados 19 registros
      f2_cuantis <- f2_cuantis[!grepl("?", f2_cuantis$ERRORES_DOCUMENTADOS, fixed = TRUE),]# Eliminados 30 registros
      
  #Utilizando la funcion MATCH FASE 2
    f2_match_cuantis <- MatchF2(f2_cuantis)
  
  #Almacenamos los datos que hicieron perfectamente el MATCH desde la FASE 1 hasta la FASE 2
    cuantis_perfect <- f2_match_cuantis$perfect
    result_match <- rbind(result_match, cuantis_perfect)
  
  # Almacenamos los datos problema, que son datos que estan en una digitacion y no estan enla otra
  # y tambien los errores que se pudieron cometer al momento de corregir los registros
    cuantis_problem <- f2_match_cuantis$problem
    
  #------------------------------------------------------------------------------------------------------------------------------------
  #Suponemos que todos los códigos que tienen una sola digitación (que estan en una digitacion y no en la otra) estan perfectos, ya que
  #pasaron por la revisión de la persona encargada, es por esto que solo tomaremos como importantes a los datos que tienen duplicados,
  #que para nosotros serán nuestros posibles errores, ya que tambien pueden ser distintas visitas a una misma casa.
  #------------------------------------------------------------------------------------------------------------------------------------
    #Ubicarmos solo a los unicode DUPLICADOS
      aux <- cuantis_problem[which(duplicated(cuantis_problem$UNICODE)),1]
      f2_duplicados<-cuantis_problem[cuantis_problem$UNICODE %in% aux,]
  
  #------------------------------------------------------------------------------------------------------------------------------------
  #Como vemos que los datos que supuestamente tenian que ser errores, de UNICODE duplicado, son distintas visitas a una misma vivienda
  #Entonces aumentamos a nuestra variable general "result_match" estos 229 registros
  #------------------------------------------------------------------------------------------------------------------------------------    
    #Aumentar registros correctos a la variable general, para tener el resultado final de el match, datos totalmente correctos.
      result_match_cuantis <- rbind(result_match, cuantis_problem)
      
  #Aplicando la funcion DataUnique que se encargara de que no haya registros exactamente iguales en el resultado
    result_match_cuantis <- DataUnique(result_match_cuantis)# Se eliminan 14 registros
  
  ##############################################################################################################
  # ----------------- ARREGLANDO ERRORES DE FICHAS DE CUANTIS 02-02-2016 ---------------------------------------
  ##############################################################################################################
    result_match_cuantis$UNICODE <- as.character(result_match_cuantis$UNICODE)
    result_match_cuantis$V <- as.character(result_match_cuantis$V)
    n_row <- nrow(result_match_cuantis)
    #TRATADAS vivienda de dos codigos que no anotaron los cuantis por que entraron por uno y salieron por otro
      result_match_cuantis[n_row+1,] <- c("1.1.22.202",1,1,22,202,26,5,15,1,0,0,0,0,0,"09:58","10:56")
      result_match_cuantis[n_row+2,] <- c("1.1.42.162A",1,1,42,'162A',14,5,15,1,0,0,0,0,0,"10:18","10:43")
      result_match_cuantis[n_row+3,] <- c("1.1.42.167A",1,1,42,'167A',19,5,15,1,0,0,0,0,0,"08:47","09:01")
    #No hay hoja de cuanti dia 18-05-2015
      result_match_cuantis[n_row+4,] <- c("1.1.42.12",1,1,42,12,18,5,15,1,0,0,0,0,0,"08:48",NA)
      result_match_cuantis[n_row+5,] <- c("1.1.42.122",1,1,42,122,18,5,15,1,0,0,0,0,0,"09:26",NA)
      result_match_cuantis[n_row+6,] <- c("1.1.42.142",1,1,42,142,18,5,15,1,0,0,0,0,0,"09:23",NA)
      result_match_cuantis[n_row+7,] <- c("1.1.42.151",1,1,42,151,18,5,15,1,0,0,0,0,0,"08:10",NA)
      result_match_cuantis[n_row+8,] <- c("1.1.42.168",1,1,42,168,18,5,15,1,0,0,0,0,0,"10:38",NA)
      result_match_cuantis[n_row+9,] <- c("1.1.42.18",1,1,42,18,18,5,15,1,0,0,0,0,0,"08:12",NA)
      result_match_cuantis[n_row+10,] <- c("1.1.42.20",1,1,42,20,18,5,15,1,0,0,0,0,0,"11:56",NA)
      result_match_cuantis[n_row+11,] <- c("1.1.42.58",1,1,42,58,18,5,15,1,0,0,0,0,0,"10:34",NA)
      result_match_cuantis[n_row+12,] <- c("1.1.42.15",1,1,42,15,18,5,15,1,0,0,0,0,0,NA,NA)
      result_match_cuantis[n_row+13,] <- c("1.1.42.34",1,1,42,34,18,5,15,1,0,0,0,0,0,NA,NA)
      result_match_cuantis[n_row+14,] <- c("1.1.42.9",1,1,42,9,18,5,15,1,0,0,0,0,0,"08:09",NA)
      result_match_cuantis[n_row+15,] <- c("1.1.42.42",1,1,42,42,18,5,15,1,0,0,0,0,0,NA,NA)
      result_match_cuantis[n_row+16,] <- c("1.1.42.46",1,1,42,15,18,5,15,1,0,0,0,0,0,"11:34",NA)
    #No hay hoja de cuanti dia 20-05-2015
      result_match_cuantis[n_row+17,] <- c("1.1.45.22",1,1,45,22,20,5,15,1,0,0,0,0,0,"11:48",NA)
      result_match_cuantis[n_row+18,] <- c("1.1.45.24",1,1,45,24,20,5,15,1,0,0,0,0,0,"11:25",NA)
      result_match_cuantis[n_row+19,] <- c("1.1.45.26",1,1,45,26,20,5,15,1,0,0,0,0,0,"11:51",NA)
      result_match_cuantis[n_row+20,] <- c("1.1.45.27",1,1,45,27,20,5,15,1,0,0,0,0,0,"11:00",NA)
    #Aumentar en cuantis
      result_match_cuantis[n_row+21,] <- c("1.1.78.23",1,1,78,23,24,8,15,1,0,0,0,0,0,"09:00","09:57")
    #Los cuantis se equivocaron al escribir el codigo en la ficha fisica
      result_match_cuantis[result_match_cuantis$UNICODE=="1.1.42.55" & result_match_cuantis$T==1,c(1,5)] <- c("1.1.42.35",35)
      result_match_cuantis[result_match_cuantis$UNICODE=="1.1.44.6B" & result_match_cuantis$T==1,c(1,5)] <- c("1.1.44.68",68)
  #----------------------------------------------------------------------------------------------------------------------------
      
#Exportar en un archivo CSV el resultado del MATCH
  #Ruta donde se almacenan los resultados
      setwd('/home/gianfranco/Documentos/github/Participation/match/match_cuantis/resultados')
      write.csv(result_match_cuantis, "result_match_cuantis.csv")
      