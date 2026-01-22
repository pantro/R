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

# RUTAS DE ARCHIVOS
kml_dir <- "D:/github_UPCH/rutas_vancan/GPS VANCAN 2023 JLByR/GPS VANCAN 2023 JLByR/"
csv_path <- file.path(kml_dir, "Mz_JLByR_08mar2024.csv")

# PARÁMETROS
UMBRAL_KMH <- 9.0 # Límite de velocidad humana

#######################################################---
# 2. MOTOR DE EXTRACCIÓN (Funciones Base)----
#######################################################---

# A. Procesar Manzanas
procesar_manzanas <- function(ruta_csv) {
  if (!file.exists(ruta_csv)) return(NULL)
  pol_raw <- read.csv(ruta_csv, sep = ";", stringsAsFactors = FALSE)
  pol_raw <- pol_raw %>% mutate(poly_id = cumsum(is.na(lat) | lat == "" | lat == "0"))
  pol_clean <- pol_raw %>%
    filter(!(is.na(lat) | lat == "" | lat == "0")) %>%
    mutate(lat = as.numeric(gsub(",", ".", lat)), long = as.numeric(gsub(",", ".", long))) %>%
    filter(!is.na(lat) & !is.na(long))
  if(nrow(pol_clean) == 0) return(NULL)
  return(pol_clean %>% st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
           group_by(poly_id) %>% summarise(geometry = st_cast(st_combine(geometry), "POLYGON"), .groups = "drop"))
}

# B. Extracción Forense (Individual)
procesar_archivo_individual <- function(ruta_completa, nombre_archivo) {
  kml <- read_xml(ruta_completa)
  ns <- xml_ns(kml)
  
  txt_time <- xml_text(xml_find_all(kml, ".//kml:when", ns))
  txt_coord <- xml_text(xml_find_all(kml, ".//gx:coord", ns))
  
  list_coord <- strsplit(txt_coord, " ")
  if (length(list_coord) == 0) return(NULL)
  mat_coord <- do.call(rbind, list_coord)
  
  n_max <- max(length(txt_time), nrow(mat_coord))
  
  final_times <- rep(NA, n_max)
  final_lon <- rep(NA, n_max)
  final_lat <- rep(NA, n_max)
  
  if (length(txt_time) > 0) final_times[1:length(txt_time)] <- txt_time
  if (nrow(mat_coord) > 0) {
    final_lon[1:nrow(mat_coord)] <- as.numeric(mat_coord[,1])
    final_lat[1:nrow(mat_coord)] <- as.numeric(mat_coord[,2])
  }
  
  # Crear Dataframe Base
  df <- data.frame(
    ARCHIVO = nombre_archivo,
    ID_Original = 1:n_max, 
    Time_Raw = final_times, 
    Longitude = final_lon, 
    Latitude = final_lat, 
    stringsAsFactors = FALSE
  )
  
  # Procesamiento y Lógica de Auditoría
  df_proc <- df %>%
    mutate(Time = as.POSIXct(Time_Raw, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")) %>%
    arrange(Time)
  
  # CORRECCIÓN HORA PERÚ (CRÍTICO)
  attr(df_proc$Time, "tzone") <- "America/Lima"
  
  df_final <- df_proc %>%
    mutate(
      delta_sec = as.numeric(difftime(Time, lag(Time), units = "secs")),
      dist_m = sqrt((Longitude - lag(Longitude))^2 + (Latitude - lag(Latitude))^2) * 111139,
      velocidad_kmh = (dist_m / delta_sec) * 3.6
    ) %>%
    mutate(
      # Clasificación de Estado
      ESTADO = case_when(
        is.na(Time) ~ "ERROR ESTRUCTURA",
        is.na(Longitude) | is.na(Latitude) ~ "ERROR COORDENADA", # Nueva validación
        delta_sec == 0 ~ "DUPLICADO",
        velocidad_kmh > UMBRAL_KMH ~ "RUIDO GPS (SALTO)",
        TRUE ~ "OK"
      ),
      DETALLE = case_when(
        ESTADO == "ERROR ESTRUCTURA" ~ "Punto Fantasma",
        ESTADO == "ERROR COORDENADA" ~ "Coordenada Vacía",
        ESTADO == "DUPLICADO" ~ "Registro duplicado",
        ESTADO == "RUIDO GPS (SALTO)" ~ paste0("Exceso Vel: ", round(velocidad_kmh, 1), " km/h"),
        TRUE ~ "Validado"
      )
    )
  
  return(df_final)
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