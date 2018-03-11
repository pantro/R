##################################################
#
# Autor: Gian Franco
# Fecha: 23/12/2015
#
# MATCH SENSIS: Match realizado de la primera y segunda digitación de SENSIS.
#
##################################################

#Llamando a las funciones necesarias para hacer el MATCH
  #Ruta donde se encuentra el archivo "FunctionMatch.R"
  setwd('/home/gianfranco/Documentos/github/Participation/match')
  source("FunctionMatch.R")

#Ruta de las base de datos que vamos a utilizar en el codigo
  setwd('/home/gianfranco/Documentos/github/Participation/match/match_sensis/bd')
  #setwd('~/Participation/match/bd')

#Leer el archivos
  data_1<-read.csv("Sensibilizacion_Participacion - Hoja 1.csv", stringsAsFactors = FALSE)
  data_2<-read.csv("sensibilizacion_2da_dig.xlsx - sensibilizacion_ASA_2daDigitac.csv", stringsAsFactors = FALSE)

#Seleccionando campos importantes para análisis principal báciso
  sel<-c("UNICODE","P","D","L","V","DIA","MES","HORA_VISITA","DIA_PROG","MES_PROG","HORA_PROG","DIA_REPROG","MES_REPROG","HORA_REPROG","MOTIVO_REPROG","RESULTADO")
  
  data_1<-data_1[,sel]
  data_2<-data_2[,sel]

#Filtrando para que no se esté buscando localidades que no estan en el estudio, suponiendo que no se han equivocado al ingresar la LOCALIDAD
  filtro<-function(dat,dentro)
  {
    filtrado<-dat[is.element(dat$L,dentro)==TRUE,]
    return(filtrado)
  }
  dentro<-c(7,8,13,14,22,23,25,26,27,29,34,36,39,42,44,45,71,77,78,81,82,83)
  #data_1
  filtro_data_1<-filtro(data_1,dentro)
  data_1<-filtro_data_1 # eliminados 1034 regiswtros
  #data_2
  filtro_data_2<-filtro(data_2,dentro)
  data_2<-filtro_data_2 # eliminados 0 registros
  
#Uniformizando las columnas que tienen horas
  #Campos que contienen las horas
  colHour<- c("HORA_VISITA","HORA_PROG","HORA_REPROG")
  
  #Utilizamos la funcion UNIF_HOUR
  data_1 <- UnifHour(data_1, colHour)
  data_2 <- UnifHour(data_2, colHour)
  
#Eliminando espacios vacios en la columna RESULTADO
  data_1$RESULTADO <- gsub(" ","",data_1$RESULTADO, fixed = T)
  data_2$RESULTADO <- gsub(" ","",data_2$RESULTADO, fixed = T)

#LLamando a la funcion Match
  data_total<- Match(data_1, data_2)

#Almacenando los datos que hicieron el MATCH perfectamente
  result_match<-data_total$perfect
  
#Almacenando los datos problema
  data_problem <- data_total$problem
  
#Ordenar por MES, DIA, VIVIENDA y LOCALIDAD. Luego por LOCALIDAD denuevo, para poder buscar mas facil los errores ya que estaran en la misma forma en que fueron ingresados.
  data_problem <- data_problem[order(data_problem$MES, data_problem$DIA, data_problem$V, data_problem$L),]
  data_problem <- data_problem[order(data_problem$L),]
  
#Exportar en un archivo CSV el resultado del MATCH
  #Ruta donde se almacenan los resultados
  setwd('/home/gianfranco/Documentos/github/Participation/match/match_sensis/resultados')
  write.csv(data_problem, "datos_problema_match_sensis.csv")
  
##################################################
#
# MATCH SENSIS: FASE 2
#
##################################################
  #Ruta donde se encuentran los archivos para la FASE 2
    setwd('/home/gianfranco/Documentos/github/Participation/match/match_sensis/correcciones')
    
  #Leer el archivos
    #Los datos corregidos deben tener un campo "ERRORES_DOCUMENTADOS" donde se explica el error que se encontro
      f2_sensis<-read.csv("datos_problema_match_sensis_CORREGIDO.csv", stringsAsFactors = FALSE)
  
  #ELIMINAR DATOS: 
    # - Datos que fueron tachados en las hojas pero fueron ingresados
    # - Datos que no se encuentra sus datos físicos
      f2_sensis <- f2_sensis[!grepl("ELIMINAR", f2_sensis$ERRORES_DOCUMENTADOS, fixed = TRUE),]# Eliminados 197 registros
      f2_sensis <- f2_sensis[!grepl("?", f2_sensis$ERRORES_DOCUMENTADOS, fixed = TRUE),]# Eliminados 32 registros
      
  #Utilizando la funcion MATCH FASE 2
    f2_match <- MatchF2(f2_sensis)
  
  #Almacenamos los datos que hicieron perfectamente el MATCH desde la FASE 1 hasta la FASE 2
    sensis_perfect<-f2_match$perfect
    result_match <- rbind(result_match, sensis_perfect)
  
  # Almacenamos los datos problema, que son datos que estan en una digitacion y no estan enla otra
  # y tambien los errores que se pudieron cometer al momento de corregir los registros
    sensis_problem <- f2_match$problem
  
  #------------------------------------------------------------------------------------------------------------------------------------
  #Suponemos que todos los códigos que tienen una sola digitación (que estan en una digitacion y no en la otra) estan perfectos, ya que
  #pasaron por la revisión de la persona encargada, es por esto que solo tomaremos como importantes a los datos que tienen duplicados,
  #que para nosotros serán nuestros posibles errores, ya que tambien pueden ser distintas visitas a una misma casa.
  #------------------------------------------------------------------------------------------------------------------------------------
    #Ubicarmos solo a los unicode DUPLICADOS
      aux <- sensis_problem[which(duplicated(sensis_problem$UNICODE)),1]
      f2_duplicados<-sensis_problem[sensis_problem$UNICODE %in% aux,]
    
  #------------------------------------------------------------------------------------------------------------------------------------
  #Como vemos que los datos que supuestamente tenian que ser errores, de UNICODE duplicado, son distintas visitas a una misma vivienda
  #Entonces aumentamos a nuestra variable general "result_match" estos 229 registros
  #------------------------------------------------------------------------------------------------------------------------------------    
    #Aumentar registros correctos a la variable general, para tener el resultado final de el match, datos totalmente correctos.
      result_match_sensis <- rbind(result_match, sensis_problem)
    
    #Aplicando la funcion DataUnique que se encargara de que no haya registros exactamente iguales en el resultado
      result_match_sensis <- DataUnique(result_match_sensis)# Se eliminan 6 registros
      
##############################################################################################################
# ----------------- ARREGLANDO ERRORES DE FICHAS DE CUANTIS - Corregido el dia 11-02-2016 ---------------------------------------
##############################################################################################################
  result_match_sensis$UNICODE <- as.character(result_match_sensis$UNICODE)
  n_row <- nrow(result_match_sensis)
  #No hay fichas de sensis pero esta en el planillon
    result_match_sensis[n_row+1,] <- c("1.1.34.50A",1,1,34,"50A",19,5,"11:00",20,5,"11:00", NA, NA, NA, NA,"RP")
    result_match_sensis[n_row+2,] <- c("1.1.34.50",1,1,34,50,19,5,"11:00",20,5,"11:00", NA, NA, NA, NA,"RP")
    result_match_sensis[n_row+3,] <- c("1.1.42.42",1,1,42,42,18,3,"09:47",NA,NA,NA, NA, NA, NA, NA,"P")
    result_match_sensis[n_row+4,] <- c("1.1.42.46",1,1,42,46,18,3,"10:36",18,3,"11:30", NA, NA, NA, NA,"P")
    
  #Actualizando la informacion de los "LV" que no tienen fichas ni en CUANTIS ni en SENSIS.
    #Son 43 registros
      result_match_sensis[n_row+5,] <- c("1.1.44.73",1,1,44,73,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")  
      result_match_sensis[n_row+6,] <- c("1.1.44.26",1,1,44,26,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+7,] <- c("1.1.44.27",1,1,44,27,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+8,] <- c("1.1.44.28",1,1,44,28,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+9,] <- c("1.1.44.29",1,1,44,29,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+10,] <- c("1.1.44.30",1,1,44,30,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+11,] <- c("1.1.44.31",1,1,44,31,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+12,] <- c("1.1.44.32",1,1,44,32,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+13,] <- c("1.1.44.33",1,1,44,33,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+14,] <- c("1.1.44.34",1,1,44,34,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+15,] <- c("1.1.44.35",1,1,44,35,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+16,] <- c("1.1.44.36",1,1,44,36,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+17,] <- c("1.1.44.37",1,1,44,37,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+18,] <- c("1.1.44.38",1,1,44,38,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+19,] <- c("1.1.44.39",1,1,44,39,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+20,] <- c("1.1.44.40",1,1,44,40,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+21,] <- c("1.1.44.41",1,1,44,41,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+22,] <- c("1.1.44.42",1,1,44,42,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+23,] <- c("1.1.44.43",1,1,44,43,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+24,] <- c("1.1.44.44",1,1,44,44,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+25,] <- c("1.1.44.45",1,1,44,45,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+26,] <- c("1.1.44.46",1,1,44,46,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+27,] <- c("1.1.44.47",1,1,44,47,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+28,] <- c("1.1.44.48",1,1,44,48,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+29,] <- c("1.1.44.49",1,1,44,49,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+30,] <- c("1.1.44.50",1,1,44,50,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+31,] <- c("1.1.44.51",1,1,44,51,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+32,] <- c("1.1.44.56",1,1,44,56,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+33,] <- c("1.1.44.57",1,1,44,57,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+34,] <- c("1.1.44.58",1,1,44,58,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+35,] <- c("1.1.44.59",1,1,44,59,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+36,] <- c("1.1.44.60",1,1,44,60,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+37,] <- c("1.1.44.61",1,1,44,61,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+38,] <- c("1.1.44.62",1,1,44,62,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+39,] <- c("1.1.44.63",1,1,44,63,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+40,] <- c("1.1.44.64",1,1,44,64,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+41,] <- c("1.1.44.65",1,1,44,65,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+42,] <- c("1.1.44.66",1,1,44,66,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+43,] <- c("1.1.44.67",1,1,44,67,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+44,] <- c("1.1.44.69",1,1,44,69,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+45,] <- c("1.1.44.70",1,1,44,70,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+46,] <- c("1.1.44.71",1,1,44,71,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      result_match_sensis[n_row+47,] <- c("1.1.44.72",1,1,44,72,NA,NA,NA,NA,NA,NA, NA, NA, NA, NA,"LV")
      
      
#------------------------------------------------------------------------------------------------------------
      
  #Exportar en un archivo CSV el resultado del MATCH
    #Ruta donde se almacenan los resultados
      setwd('/home/gianfranco/Documentos/github/Participation/match/match_sensis/resultados')
      write.csv(result_match_sensis, "result_match_sensis.csv")
        
        