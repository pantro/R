##################################################
#
# Autor: Gian Franco
#
# FUNCIONES MATCH: Funciones utilizadas al realizar los MATCH con cada base de datos.
#
##################################################

#-------------------------------------------------------------------
#============ Funcion MATCH =========================================
# Requerimientos:
#   - Las bases de datos deben contener los mismo nombres de sus columnas
#--------------------------------------------------------------------
Match<-function(data_1,data_2) {
  #Esta funcion retorna un data frame con los datos problema, donde se encuentran los errores de difgitación 
  #y también aquellos registros que estan en una digitación y no estan en la otra.
  #
  #ARGS
  # data_1 = Primera digitación
  # data_2 = Segunda digitación
  #
  #RETURNS
  # problem = datos problemas
  # perfect = Son los datos que realizaron perfectamente el MATCH
  #
  
  #Eliminar los DUPLICADOS exactamente iguales en todas sus columnas
  data_1<- unique(data_1)
  data_2<- unique(data_2)
  
  #Agregar un campo para reconocer a que digitacion pertenece
  data_1$DIG_1<- unlist(1)
  data_2$DIG_2<- unlist(1)
  
  #Mantenemos todas las filas de ambas bases, y las que coinciden exactamente igual, en todas sus columnas, aparecen una sola vez. (Full outer join)
  full_outer_join<- merge(data_1, data_2, all = TRUE)
  
  #Obtenemos solo las columnas que tienen "NA" en DIG_1 o DIG_2:
  #   - Son las filas que estan en una base de datos y no en la otra
  #   - Son aquellas filas que NO coinciden en todas sus columnas
  data_problem<- full_outer_join[is.na(full_outer_join$DIG_1) | is.na(full_outer_join$DIG_2),]
  
  #Almacenando los datos que hicieron perfectamente el match
  data_perfect <- full_outer_join[!(is.na(full_outer_join$DIG_1) | is.na(full_outer_join$DIG_2)),]

  return(list(problem=data_problem, perfect=data_perfect))
}
#=========================================================================

#-------------------------------------------------------------------
#============ Funcion MATCH FASE 2=========================================
#--------------------------------------------------------------------
MatchF2<-function(data_problem_corrected) {
  # Esta función se encargará de recibir los datos problema dados por la funcion MATCH pero ya corregidos.   
  # Separará los datos de la primera digitación y la segunda digitación para volver a utilizar la función MATCH
  # y poder comprobar que el encargado de hacer las correcciones no haya cometido errores.
  # Los datos corregidos deben tener un campo "ERRORES_DOCUMENTADOS" donde se explica el error que se encontro
  #
  #ARGS
  # data_problem_corrected = Datos problemas ya corregidos
  #
  #RETURNS
  # problem = datos problemas
  # perfect = Son los datos que realizaron perfectamente el MATCH
  #
  #Separar los que fueron realizados en la primera digitación de los que fueron realizados en la segunda digitación
  f2_DIG1 <- data_problem_corrected[is.na(data_problem_corrected$DIG_2),]
  f2_DIG2 <- data_problem_corrected[is.na(data_problem_corrected$DIG_1),]
  
  #Eliminar las columnas DIG_1, DIG_2 y el campo de ERRORES_DOCUMENTADOS, para que al momento de hacer el MATCH denuevo no nos generen errores
  n_col <- length(data_problem_corrected)
  f2_DIG1 <- f2_DIG1[,-c(n_col-2,n_col-1,n_col)]
  f2_DIG2 <- f2_DIG2[,-c(n_col-2,n_col-1,n_col)]
  
  #Utilizar la FUNCION MATCH
  f2_match <- Match(f2_DIG1, f2_DIG2)
  
  #Almacenando los datos que hicieron perfectamente el MATCH en la FASE 2, que aumenta columnas a la variable que ingresamos "result"
  f2_match_perfect <- f2_match$perfect
  
  #Almacenando los datos que son problema al realizar MATCH FASE 2, estos son datos que estan en una digitacion y no estan enla otra
  #y tambien los errores que se pudieron cometer al momento de corregir los registros.
  f2_match_problem <- f2_match$problem
  
  return(list(problem=f2_match_problem, perfect=f2_match_perfect))
}
#=========================================================================

#-------------------------------------------------------------------
#============ Funcion MATCH FASE 2=========================================
#--------------------------------------------------------------------
DataUnique<-function(data) {
  # Esta función se encargará de recibir todos los datos resultados del match.
  # Eliminara los campos de primera digitacion y segunda digitacion.
  # Devolvera los datos unicos, evitando tener registros completamente iguales en el resultado final
  #
  #ARGS
  # data = Datos resultados del match final
  #
  #RETURNS
  # dataUnique = Retorna los registros unicos, evitando tener registros iguales.
  #
  #Elimina las columnas de primera digitacion y segunda digitacion.
  data <- subset( data, select = -c(DIG_1, DIG_2))
  
  #Escoger solo registros unicos
  DataUnique <- unique(data)
  
  return(DataUnique)
}
#=========================================================================


#----------------------------------------------------------------------------------------------
#====================  Funcion "UNIF_HOUR"  ====================================================
#----------------------------------------------------------------------------------------------
UnifHour<- function(data,colHour){
  #Esta funcion se encarga de UNIFORMIZAR las horas
  #
  #Args
  # data = Data.frame que contiene el campo de horas que queremos uniformizar
  # colHour = Arreglo de los nombres de las columnas que contienen las horas que deseamos uniformizar
  #
  #RETURNS
  # data = Data frame ya con los campos de las horas uniformizados
  #
  n_col<-length(colHour)
  for (i in 1:n_col) {
    data[,colHour[i]] <- strftime(strptime(data[,colHour[i]], format="%H:%M"),"%H:%M") 
  }
  
  return(data)
}
#================================================================================================

#----------------------------------------------------------------------------------------------
#====================  Funcion "ChanceNAxCero"  ====================================================
#----------------------------------------------------------------------------------------------
ChanceNAxCero<- function(data,namesCol){
  #Esta funcion se encarga de cambiar los NAs por "0"
  #
  #Args
  # data = Data frame con el cual vamos a trabajar
  # namesCol = Arreglo de los nombres de las columnas que contienen los NAs que deseamos cambiar por "0"
  #
  #RETURNS
  # data = Data frame ya con los campos NAs ya cambiados a "0"
  #
  n_col<-length(namesCol)
  for (i in 1:n_col) {
    data[is.na(data[,namesCol[i]]),namesCol[i]]<-0
  }
  
  return(data)
}
#================================================================================================  
