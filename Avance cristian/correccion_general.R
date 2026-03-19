#######################################################---
# 1. CONFIGURACIÓN E INSTALACIÓN DE LIBRERÍAS----
#######################################################---
rm(list = ls()) 
options(warn = -1) 

# Verificación de librerías
paquetes <- c("sf", "dplyr", "leaflet", "viridis", "xml2", "purrr")
instalados <- paquetes %in% installed.packages()
if (any(!instalados)) {
  install.packages(paquetes[!instalados])
}

library(sf)
library(dplyr)
library(leaflet)
library(viridis)
library(xml2)
library(purrr)
library(geosphere)

# RUTAS DE ARCHIVOS
kml_dir <- "D:/github_UPCH/rutas_vancan/GPS VANCAN 2023 JLByR/GPS VANCAN 2023 JLByR/"
csv_path <- file.path(kml_dir, "Mz_JLByR_08mar2024.csv")

# PARÁMETROS
UMBRAL_KMH <- 9.0 # Límite de velocidad humana

#######################################################---
# 2. MOTOR DE EXTRACCIÓN (Funciones Base)----
#######################################################---

# A. Función para dibujar las manzanas del distrito como contexto en el mapa
procesar_manzanas <- function(ruta_csv) {
  if (!file.exists(ruta_csv)) return(NULL)
  
  # Leemos el archivo y limpiamos coordenadas vacías
  pol_raw <- read.csv(ruta_csv, sep = ";", stringsAsFactors = FALSE)
  pol_raw <- pol_raw %>% mutate(poly_id = cumsum(is.na(lat) | lat == "" | lat == "0"))
  
  pol_clean <- pol_raw %>%
    filter(!(is.na(lat) | lat == "" | lat == "0")) %>%
    mutate(lat = as.numeric(gsub(",", ".", lat)), long = as.numeric(gsub(",", ".", long))) %>%
    filter(!is.na(lat) & !is.na(long))
  if(nrow(pol_clean) == 0) return(NULL)
  
  # Convertimos las coordenadas en polígonos espaciales (manzanas)
  return(pol_clean %>% st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
           group_by(poly_id) %>% summarise(geometry = st_cast(st_combine(geometry), "POLYGON"), .groups = "drop"))
}

# B. Función principal: Analiza UN solo archivo KML, limpia el ruido y extrae la ruta real
procesar_archivo_individual <- function(ruta_completa, nombre_archivo) {
  
  # 1. Leemos el archivo KML y extraemos los tiempos y las coordenadas en bruto
  kml <- read_xml(ruta_completa)
  ns <- xml_ns(kml)
  
  txt_time <- xml_text(xml_find_all(kml, ".//kml:when", ns))
  txt_coord <- xml_text(xml_find_all(kml, ".//gx:coord", ns))
  
  list_coord <- strsplit(txt_coord, " ")
  if (length(list_coord) == 0) return(NULL)
  mat_coord <- do.call(rbind, list_coord)
  
  n_max <- max(length(txt_time), nrow(mat_coord))
  
  # Aseguramos que los datos tengan el mismo tamaño llenando con espacios vacíos si falta algo
  final_times <- rep(NA, n_max)
  final_lon <- rep(NA, n_max)
  final_lat <- rep(NA, n_max)
  
  if (length(txt_time) > 0) final_times[1:length(txt_time)] <- txt_time
  if (nrow(mat_coord) > 0) {
    final_lon[1:nrow(mat_coord)] <- as.numeric(mat_coord[,1])
    final_lat[1:nrow(mat_coord)] <- as.numeric(mat_coord[,2])
  }
  
  # 2. Creamos una tabla inicial con los datos en bruto
  df <- data.frame(
    ARCHIVO = nombre_archivo,
    ID_Original = 1:n_max, 
    Time_Raw = final_times, 
    Longitude = final_lon, 
    Latitude = final_lat, 
    stringsAsFactors = FALSE
  )
  
  # Procesamiento y ordenamiento
  df_proc <- df %>%
    mutate(Time = as.POSIXct(Time_Raw, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")) %>%
    arrange(Time)
  
  # CORRECCIÓN HORA PERÚ
  attr(df_proc$Time, "tzone") <- "America/Lima"
  
  # ==========================================================
  # === ELIMINACIÓN DE RUIDO (LÓGICA SECUENCIAL) ===
  # Esta es la parte más importante. Evalúa punto por punto.
  # Si detecta un punto falso (un salto de GPS), lo ignora y 
  # calcula la distancia desde el ÚLTIMO PUNTO REAL CONOCIDO.
  # ==========================================================
  
  # 1. Inicializamos columnas vacías para el cálculo
  n_rows <- nrow(df_proc)
  df_proc$delta_sec <- 0
  df_proc$dist_m <- 0
  df_proc$velocidad_kmh <- 0
  df_proc$ESTADO <- "OK"
  df_proc$DETALLE <- "Validado"
  
  # Evaluamos la primera fila manualmente
  if (is.na(df_proc$Time[1])) {
    df_proc$ESTADO[1] <- "ERROR ESTRUCTURA"
    df_proc$DETALLE[1] <- "Punto Fantasma"
  } else if (is.na(df_proc$Longitude[1]) || is.na(df_proc$Latitude[1])) {
    df_proc$ESTADO[1] <- "ERROR COORDENADA"
    df_proc$DETALLE[1] <- "Coordenada Vacía"
  } else {
    df_proc$DETALLE[1] <- "Punto Inicial"
  }
  
  if (n_rows > 1) {
    # Variable crucial: Memoria del último índice válido
    last_valid_idx <- 1
    
    for (i in 2:n_rows) {
      
      # Filtros básicos: descartar si no hay tiempo o coordenadas
      if (is.na(df_proc$Time[i])) {
        df_proc$ESTADO[i] <- "ERROR ESTRUCTURA"
        df_proc$DETALLE[i] <- "Punto Fantasma"
        next
      }
      if (is.na(df_proc$Longitude[i]) || is.na(df_proc$Latitude[i])) {
        df_proc$ESTADO[i] <- "ERROR COORDENADA"
        df_proc$DETALLE[i] <- "Coordenada Vacía"
        next
      }
      
      # Calcular tiempo respecto al ÚLTIMO PUNTO VÁLIDO
      t_current <- df_proc$Time[i]
      t_last <- df_proc$Time[last_valid_idx]
      
      delta_t <- as.numeric(difftime(t_current, t_last, units = "secs"))
      df_proc$delta_sec[i] <- delta_t
      
      # Si el tiempo es 0, es un registro duplicado por error del aparato
      if (delta_t == 0) {
        df_proc$ESTADO[i] <- "DUPLICADO"
        df_proc$DETALLE[i] <- "Registro duplicado"
        next
      }
      
      # Calculamos la distancia real (en metros) usando la curvatura de la Tierra
      dist_m <- geosphere::distHaversine(
        c(df_proc$Longitude[last_valid_idx], df_proc$Latitude[last_valid_idx]),
        c(df_proc$Longitude[i], df_proc$Latitude[i])
      )
      df_proc$dist_m[i] <- dist_m
      
      # Calculamos a qué velocidad se movió para llegar a esa distancia
      vel_kmh <- (dist_m / delta_t) * 3.6
      df_proc$velocidad_kmh[i] <- vel_kmh
      
      # Lógica Antirrebote: Evaluar y decidir si actualizamos la memoria
      if (vel_kmh > UMBRAL_KMH) {
        
        # Si la velocidad es irreal (ej: > 9 km/h), marcamos como error.
        # CRUCIAL: No actualizamos 'last_valid_idx'. Así, el siguiente punto
        # se comparará con el punto bueno anterior al salto, cortando la cadena de error.
        df_proc$ESTADO[i] <- "RUIDO GPS (SALTO)"
        df_proc$DETALLE[i] <- paste0("Exceso Vel: ", round(vel_kmh, 1), " km/h")
        # OJO: Aquí NO actualizamos last_valid_idx.
        # El próximo ciclo del bucle seguirá comparando contra el punto anterior al salto.
      } else {
        # ¡Punto bueno! Actualizamos la memoria al índice actual
        last_valid_idx <- i
      }
    }
  }
  
  return(df_proc)
}

#######################################################---
# 3. EJECUCIÓN MASIVA (PROCESAR TODOS LOS KML)----
#######################################################---
# Buscar archivos
archivos_kml <- list.files(kml_dir, pattern = "\\.kml$", full.names = TRUE)
nombres_kml <- list.files(kml_dir, pattern = "\\.kml$", full.names = FALSE)

# Opcional: Filtrar el archivo GENERAL para no duplicar
indices <- !grepl("GENERAL", nombres_kml)
archivos_kml <- archivos_kml[indices]
nombres_kml <- nombres_kml[indices]

cat(paste(">>> Se encontraron", length(archivos_kml), "archivos para procesar.\n"))

# Bucle de procesamiento
lista_datos_completos <- list()

for (i in seq_along(archivos_kml)) {
  cat(paste("Procesando:", nombres_kml[i], "...\n"))
  tryCatch({
    res <- procesar_archivo_individual(archivos_kml[i], nombres_kml[i])
    if (!is.null(res)) {
      lista_datos_completos[[i]] <- res
    }
  }, error = function(e) {
    cat(paste("Error leyendo archivo:", nombres_kml[i], "\n"))
  })
}

# Unir todo en una sola Gran Tabla
DF_MAESTRO <- bind_rows(lista_datos_completos)

#######################################################---
# 4. GENERACIÓN DEL MAPA MAESTRO----
#######################################################---
cat(">>> Generando Mapa Maestro Multicapa...\n")

m <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron)

# Capa de Manzanas
pol_sf <- procesar_manzanas(csv_path)
if (!is.null(pol_sf)) {
  m <- m %>% addPolygons(data = pol_sf, group = "Contexto (Manzanas)", color = "#444", weight = 1, fillOpacity = 0.1)
}

# BUCLE PARA DIBUJAR CADA ARCHIVO
nombres_unicos <- unique(DF_MAESTRO$ARCHIVO)
# Generar colores únicos
colores <- viridis(length(nombres_unicos), option = "D") 

for (i in seq_along(nombres_unicos)) {
  nombre_archivo <- nombres_unicos[i]
  
  # FILTRO ESTRICTO PARA EL MAPA
  # Aquí estaba el error: Debemos asegurarnos de que no haya NAs en lat/long
  data_archivo <- DF_MAESTRO %>% 
    filter(ARCHIVO == nombre_archivo) %>%
    filter(ESTADO == "OK" & !is.na(Time)) %>%
    filter(!is.na(Longitude) & !is.na(Latitude)) %>% # <--- CORRECCIÓN CRÍTICA
    mutate(Secuencia = row_number())
  
  if (nrow(data_archivo) > 1) {
    # Convertir a espacial
    sf_linea <- data_archivo %>% 
      st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
      summarise(do_union = FALSE) %>% st_cast("LINESTRING")
    
    sf_puntos <- data_archivo %>% 
      st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
    
    # Agregar al mapa por capas
    m <- m %>%
      addPolylines(
        data = sf_linea, 
        color = colores[i], 
        weight = 3, 
        opacity = 0.8,
        group = nombre_archivo
      ) %>%
      addCircleMarkers(
        data = sf_puntos,
        radius = 4,
        color = colores[i],
        stroke = FALSE, fillOpacity = 0.8,
        popup = ~paste0(
          "<b>Archivo:</b> ", ARCHIVO, "<br>",
          "<b>Punto:</b> ", Secuencia, "<br>",
          "<b>Hora (PE):</b> ", format(Time, "%H:%M:%S"), "<br>",
          "<b>Velocidad:</b> ", round(velocidad_kmh, 1), " km/h"
        ),
        group = nombre_archivo
      )
  }
}

# Control de Capas
m <- m %>%
  addLayersControl(
    overlayGroups = c("Contexto (Manzanas)", nombres_unicos),
    options = layersControlOptions(collapsed = FALSE)
  )

print(m)

#######################################################---
# 5. TABLAS DE AUDITORÍA CONSOLIDADA----
#######################################################---
cat(">>> Generando tablas maestras...\n")

# TABLA A: ERRORES DE TODOS LOS ARCHIVOS
df_errores_global <- DF_MAESTRO %>%
  filter(ESTADO != "OK") %>%
  select(ARCHIVO, ID_Original, Time, velocidad_kmh, ESTADO, DETALLE)

if(nrow(df_errores_global) > 0) {
  View(df_errores_global, title = "⚠️ ERRORES (TODOS)")
}

# TABLA B: DATA COMPLETA CONSOLIDADA
View(DF_MAESTRO %>% select(ARCHIVO, ID_Original, Time, Longitude, Latitude, velocidad_kmh, ESTADO, DETALLE), 
     title = "📋 AUDITORÍA GLOBAL")

cat(">>> Proceso finalizado exitosamente.\n")
