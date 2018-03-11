##################################################
#
# Autor: Gian Franco
# Fecha: 15/12/2015
#
# MATCH LIDERES: Match realizado de la primera y segunda digitación de LIDERES BD.
#
##################################################

  #Llamando a las funciones necesarias para hacer el MATCH
    #Ruta donde se encuentra el archivo "FunctionMatch.R"
    setwd('/home/gianfranco/Documentos/github/Participation/match')
    source("FunctionMatch.R")
  
  #Ruta de las base de datos que vamos a utilizar en el codigo
    setwd('/home/gianfranco/Documentos/github/Participation/match/match_lideres/bd')
    #setwd('~/Participation/match/bd')
  
  #Leer el archivos
    data_1<-read.csv("BD_CROQUIS - BD.csv")
    data_2<-read.csv("BD_CROQUIS_LIDERES2D.xlsx - BD.csv")
    
  #Seleccionando campos importantes para análisis principal báciso
    sel<-c("UNICODE","P","D","L","V","CLUSTER","CODIGO_LIDER1","CODIGO_LIDER2","DIA_1ra_visita","MES_1ra_visita","Resultado_1ra_visita","OBSERVACION1.","DIA_2da_visita","Mes_2da_visita","Resultado_2da_visita","OBBERVACION2.","Resultado_Final")
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
    setwd('/home/gianfranco/Documentos/github/Participation/match/match_lideres/resultados')
    write.csv(data_problem, "datos_problema_match_lideres.csv")
    
##################################################
#
# MATCH CROQUIS LIDERES: FASE 2
#
##################################################
  #Ruta donde se encuentran los archivos para la FASE 2
    setwd('/home/gianfranco/Documentos/github/Participation/match/match_lideres/correcciones')
  
  #Leer el archivos
  #Los datos corregidos deben tener un campo "ERRORES_DOCUMENTADOS" donde se explica el error que se encontro
    f2_match<-read.csv("datos_problema_match_lideres_CORREGIDO.csv")
  
  #ELIMINAR DATOS: 
  # - Datos que fueron tachados en las hojas pero fueron ingresados
  # - Datos que no se encuentra sus datos físicos
    f2_match <- f2_match[!grepl("ELIMINAR", f2_match$ERRORES_DOCUMENTADOS, fixed = TRUE),]# se eliminan 9 registros
  
  #Utilizando la funcion MATCH FASE 2
    f2_match <- MatchF2(f2_match)
  
  #Almacenamos los datos que hicieron perfectamente el MATCH desde la FASE 1 hasta la FASE 2
    lideres_perfect <- f2_match$perfect
    result_match <- rbind(result_match, lideres_perfect)
  
  
  # Almacenamos los datos problema, que son datos que estan en una digitacion y no estan enla otra
  # y tambien los errores que se pudieron cometer al momento de corregir los registros
    lideres_problem <- f2_match$problem
  
  #------------------------------------------------------------------------------------------------------------------------------------
  #Suponemos que todos los códigos que tienen una sola digitación (que estan en una digitacion y no en la otra) estan perfectos, ya que
  #pasaron por la revisión de la persona encargada, es por esto que solo tomaremos como importantes a los datos que tienen duplicados,
  #que para nosotros serán nuestros posibles errores, ya que tambien pueden ser distintas visitas a una misma casa.
  #------------------------------------------------------------------------------------------------------------------------------------
    #Ubicarmos solo a los unicode DUPLICADOS
      aux <- lideres_problem[which(duplicated(lideres_problem$UNICODE)),1]
      f2_duplicados<-lideres_problem[lideres_problem$UNICODE %in% aux,]
      
  #------------------------------------------------------------------------------------------------------------------------------------
  #Como vemos que los datos que supuestamente tenian que ser errores, de UNICODE duplicado, son distintas visitas a una misma vivienda
  #Entonces aumentamos a nuestra variable general "result_match" estos 229 registros
  #------------------------------------------------------------------------------------------------------------------------------------    
    #Aumentar registros correctos a la variable general, para tener el resultado final de el match, datos totalmente correctos.
      result_match_lideres <- rbind(result_match, lideres_problem)
    
    #Aplicando la funcion DataUnique que se encargara de que no haya registros exactamente iguales en el resultado
      result_match_lideres <- DataUnique(result_match_lideres)# Ningun registro eliminado
      
  #Exportar en un archivo CSV el resultado del MATCH
  #Ruta donde se almacenan los resultados
    setwd('/home/gianfranco/Documentos/github/Participation/match/match_lideres/resultados')
    write.csv(result_match_lideres, "result_match_lideres.csv")
    