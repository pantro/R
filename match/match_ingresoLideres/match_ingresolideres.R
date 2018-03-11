##################################################
#
# Autor: Gian Franco
# Fecha: 15/12/2015
#
# MATCH INGRESO LIDERES: Match realizado de la primera y segunda digitación de INGRESO LIDERES.
#
##################################################

#Llamando a las funciones necesarias para hacer el MATCH
  #Ruta donde se encuentra el archivo "FunctionMatch.R"
  setwd('/home/gianfranco/Documentos/github/Participation/match')
  source("FunctionMatch.R")

#Ruta de las base de datos que vamos a utilizar en el codigo
  setwd('/home/gianfranco/Documentos/github/Participation/match/match_ingresoLideres/bd')
  #setwd('~/Participation/match/bd')

#Leer el archivos
  data_1<-read.csv("BD_ingresolideres_1D LIDERES.csv")
  data_2<-read.csv("BD_ingresolideres_2D LIDERES.csv")
  
#Seleccionando campos importantes para análisis principal báciso
  sel<-c("UNICODE","P","D","L","V","CLUSTER","LETRA", "CODIGO_LIDER",  "DIA_F",  "MES_F",  "ANIO_F",  "NOMBRES",  "PARTICIPA",  "TALLA_POLO",  "SEXO",  "EDAD",  "GRADO_INSTRUCCION",  "OCUPACION",  "MOTIVO_ACEPTA_RECHAZO",  "MOTIVO_ACEPTA",  "MOTIVO_RECHAZO",  "CAPACITACION",  "NIVEL_ENTUSIASMO",  "FECHA_CUESTIONARIO",  "EXPERIENCIA",  "EXP_POSITIVA",  "EXP_DIFICIL",  "TIEMPO_VISITAS",  "MAS_AYUDAR_VISITAS",  "OTRO_VISITAS",  "FALTO_RESPONDER_COMENTARIOS",  "FALTO_INFO_CHAGAS",  "FALTO_PREPARA_FUMIGACION",  "FALTO_INFO_CHIRIS",  "FALTO_SOCIODRAMA",  "FALTO_EXPLICAR_ENF_CHAGAS",  "FALTO_PREPARATIVO",  "FALTO_PREGUNTAS",  "FALTO_TIEMPO_MATERIALESS",  "CALIFICA_CAPACITACION",  "VOLVER_PARTICIPAR")
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
  data_1<-filtro_data_1
  #data_2
  filtro_data_2<-filtro(data_2,dentro)
  data_2<-filtro_data_2

#LLamando a la funcion Match
  data_total<- Match(data_1, data_2)
  
#Almacenando los datos que hicieron el MATCH perfectamente
  result_match<-data_total$perfect
  
#Almacenando los datos problema
  data_problem <- data_total$problem
  
#Exportar en un archivo CSV el resultado del MATCH
  #Ruta donde se almacenan los resultados
  setwd('/home/gianfranco/Documentos/github/Participation/match/match_ingresoLideres/resultados')
  write.csv(data_problem, "datos_problema_match_ingresolideres.csv")
  
##################################################
#
# MATCH INGRESO LIDERES: FASE 2
#
##################################################
  #Ruta donde se encuentran los archivos para la FASE 2
    setwd('/home/gianfranco/Documentos/github/Participation/match/match_ingresoLideres/correcciones')
  
  #Leer el archivos
    #Los datos corregidos deben tener un campo "ERRORES_DOCUMENTADOS" donde se explica el error que se encontro
    f2_ingresoLideres<-read.csv("datos_problema_match_ingresolideres_CORREGIDO.csv")
    
  #ELIMINAR
    f2_ingresoLideres <- f2_ingresoLideres[!grepl("ELIMINAR",f2_ingresoLideres$ERRORES_DOCUMENTADOS, fixed = TRUE),]
    
  #Utilizando la funcion MATCH FASE 2
    f2_match <- MatchF2(f2_ingresoLideres)
  
  #Almacenamos los datos que hicieron perfectamente el MATCH desde la FASE 1 hasta la FASE 2
    result_match <- rbind(result_match,f2_match$perfect)
  
  # Almacenamos los datos problema, que son datos que estan en una digitacion y no estan enla otra
  # y tambien los errores que se pudieron cometer al momento de corregir los registros
    ingresoLideres_problem <- f2_match$problem
  
  #------------------------------------------------------------------------------------------------------------------------------------
  # La variable "ingresoLideres_problem" salio "0" es por eso que todos el resultado esta en la variable "result_match"
  #------------------------------------------------------------------------------------------------------------------------------------
    
  #Exportar en un archivo CSV el resultado del MATCH
    #Ruta donde se almacenan los resultados
      setwd('/home/gianfranco/Documentos/github/Participation/match/match_ingresoLideres/resultados')
      write.csv(result_match, "result_match_ingresoLideres.csv")
      