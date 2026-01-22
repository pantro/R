#######################################################---
# 1. CONFIGURACIÓN E INSTALACIÓN DE LIBRERÍAS----
#######################################################---
rm(list = ls()) 
options(warn = -1) 

# Verificación de librerías
paquetes <- c("sf", "dplyr", "leaflet", "viridis", "xml2")
instalados <- paquetes %in% installed.packages()
if (any(!instalados)) {
  install.packages(paquetes[!instalados])
}

library(sf)
library(dplyr)
library(leaflet)
library(viridis)
library(xml2)

# RUTAS DE ARCHIVOS (Modificar aquí para nuevos análisis)
kml_dir <- "D:/github_UPCH/rutas_vancan/GPS VANCAN 2023 JLByR/GPS VANCAN 2023 JLByR/"
archivo_objetivo <- "GPS-25  17-06-2023  GRUPO 24.kml"
ruta_kml <- file.path(kml_dir, archivo_objetivo)
csv_path <- file.path(kml_dir, "Mz_JLByR_08mar2024.csv")

# PARÁMETROS DE CALIBRACIÓN
UMBRAL_KMH <- 9.0 # Límite de velocidad para detectar saltos (Ruido)

#######################################################---
# 2. FUNCIONES DE PROCESAMIENTO (MOTORES)----
#######################################################---

# A. Procesamiento de Manzanas (Contexto)
procesar_manzanas <- function(ruta_csv) {
  if (!file.exists(ruta_csv)) return(NULL)
  pol_raw <- read.csv(ruta_csv, sep = ";", stringsAsFactors = FALSE)
  
  # Generar ID único
  pol_raw <- pol_raw %>% mutate(poly_id = cumsum(is.na(lat) | lat == "" | lat == "0"))
  
  # Limpiar
  pol_clean <- pol_raw %>%
    filter(!(is.na(lat) | lat == "" | lat == "0")) %>%
    mutate(lat = as.numeric(gsub(",", ".", lat)), long = as.numeric(gsub(",", ".", long))) %>%
    filter(!is.na(lat) & !is.na(long))
  
  if(nrow(pol_clean) == 0) return(NULL)
  
  return(pol_clean %>% st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
           group_by(poly_id) %>% summarise(geometry = st_cast(st_combine(geometry), "POLYGON"), .groups = "drop"))
}

# B. Extracción Total (Recupera Data Corrupta)
extract_full_data <- function(kml_path) {
  kml <- read_xml(kml_path)
  ns <- xml_ns(kml)
  
  txt_time <- xml_text(xml_find_all(kml, ".//kml:when", ns))
  txt_coord <- xml_text(xml_find_all(kml, ".//gx:coord", ns))
  
  list_coord <- strsplit(txt_coord, " ")
  if (length(list_coord) == 0) return(NULL)
  mat_coord <- do.call(rbind, list_coord)
  
  # Forzar dimensiones máximas para capturar errores
  n_max <- max(length(txt_time), nrow(mat_coord))
  
  final_times <- rep(NA, n_max)
  final_lon <- rep(NA, n_max)
  final_lat <- rep(NA, n_max)
  
  if (length(txt_time) > 0) final_times[1:length(txt_time)] <- txt_time
  if (nrow(mat_coord) > 0) {
    final_lon[1:nrow(mat_coord)] <- as.numeric(mat_coord[,1])
    final_lat[1:nrow(mat_coord)] <- as.numeric(mat_coord[,2])
  }
  
  return(data.frame(ID_Original = 1:n_max, Time_Raw = final_times, Longitude = final_lon, Latitude = final_lat, stringsAsFactors = FALSE))
}

#######################################################---
# 3. ANÁLISIS FORENSE Y CORRECCIÓN HORARIA----
#######################################################---
cat(">>> Analizando datos y corrigiendo zona horaria...\n")

df_total <- extract_full_data(ruta_kml)

df_analisis <- df_total %>%
  # 1. Convertir texto a fecha UTC (Hora Z)
  mutate(Time = as.POSIXct(Time_Raw, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")) %>%
  arrange(Time) 

# 2. *** CORRECCIÓN CRÍTICA DE HORA (PERÚ) ***
# Cambiamos la etiqueta de zona horaria a Lima (UTC-5)
attr(df_analisis$Time, "tzone") <- "America/Lima"

df_analisis <- df_analisis %>%
  mutate(
    # Cálculos Cinemáticos
    delta_sec = as.numeric(difftime(Time, lag(Time), units = "secs")),
    dist_m = sqrt((Longitude - lag(Longitude))^2 + (Latitude - lag(Latitude))^2) * 111139,
    velocidad_kmh = (dist_m / delta_sec) * 3.6
  ) %>%
  mutate(
    # Diagnóstico de Integridad
    ESTADO = case_when(
      is.na(Time) ~ "ERROR ESTRUCTURA",
      delta_sec == 0 ~ "DUPLICADO",
      velocidad_kmh > UMBRAL_KMH ~ "RUIDO GPS (SALTO)",
      TRUE ~ "OK"
    ),
    # Detalle Técnico
    DETALLE = case_when(
      ESTADO == "ERROR ESTRUCTURA" ~ "Punto Fantasma (Sin hora)",
      ESTADO == "DUPLICADO" ~ "Registro duplicado (0 seg)",
      ESTADO == "RUIDO GPS (SALTO)" ~ paste0("Velocidad excesiva: ", round(velocidad_kmh, 1), " km/h"),
      TRUE ~ "Dato validado"
    )
  )

# Separación de Datos
df_mapa <- df_analisis %>% 
  filter(ESTADO == "OK" & !is.na(Time)) %>%
  mutate(Secuencia = row_number())

sf_sucio <- df_total %>% filter(!is.na(Longitude)) %>% st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
linea_sucia <- sf_sucio %>% summarise(do_union = FALSE) %>% st_cast("LINESTRING")

sf_limpio <- df_mapa %>% st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
linea_limpia <- sf_limpio %>% summarise(do_union = FALSE) %>% st_cast("LINESTRING")


#######################################################---
# 4. GENERACIÓN DEL MAPA INTERACTIVO----
#######################################################---
cat(">>> Generando diagrama interactivo...\n")

m <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron)

# Capa Manzanas
pol_sf <- procesar_manzanas(csv_path)
if (!is.null(pol_sf)) m <- m %>% addPolygons(data = pol_sf, group = "Manzanas", color = "#444", weight = 1, fillOpacity = 0.1)

# Paleta de colores
pal <- colorNumeric("viridis", domain = df_mapa$Secuencia)

m <- m %>%
  # 1. RUTA SUCIA (Fondo Rojo)
  addPolylines(data = linea_sucia, color = "red", weight = 1, dashArray = "5,5", opacity = 0.4, 
               group = "Original (Con Ruido)") %>%
  
  # 2. RUTA LIMPIA (Azul)
  addPolylines(data = linea_limpia, color = "#2196F3", weight = 3, opacity = 0.7, 
               group = "Procesada (Limpia)") %>%
  
  # 3. PUNTOS INTERACTIVOS (Con hora corregida)
  addCircleMarkers(
    data = sf_limpio,
    radius = 5,
    fillColor = ~pal(Secuencia),
    color = "black", weight = 1, opacity = 1, fillOpacity = 0.9,
    
    label = ~as.character(Secuencia),
    labelOptions = labelOptions(textOnly = FALSE, style = list("font-weight" = "bold")),
    
    popup = ~paste0(
      "<div style='text-align:center;'>",
      "<b>PUNTO #", Secuencia, "</b><hr>",
      "⏰ <b>Hora (Perú):</b> ", format(Time, "%H:%M:%S"), "<br>", # Aquí usa la hora corregida
      "🏃 <b>Velocidad:</b> ", round(velocidad_kmh, 1), " km/h<br>",
      "📏 <b>Distancia:</b> ", round(dist_m, 1), " m<br>",
      "</div>"
    ),
    group = "Puntos Interactivos"
  ) %>%
  
  addLayersControl(
    overlayGroups = c("Manzanas", "Original (Con Ruido)", "Procesada (Limpia)", "Puntos Interactivos"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend("bottomright", pal = pal, values = df_mapa$Secuencia, title = "Secuencia", opacity = 1) %>%
  addControl(html = '<div style="background: white; padding: 10px; border-radius:5px;"><b>DIAGRAMA DINÁMICO</b></div>', position = "topright")

print(m)


#######################################################---
# 5. TABLAS DE AUDITORÍA----
#######################################################---
cat(">>> Abriendo reportes de auditoría...\n")

# Reporte de Eliminados
df_errores <- df_analisis %>%
  filter(ESTADO != "OK") %>%
  select(ID_Original, Time, Longitude, Latitude, velocidad_kmh, ESTADO, DETALLE)

if(nrow(df_errores) > 0) {
  View(df_errores, title = "⚠️ COMPONENTES DEFECTUOSOS (ELIMINADOS)")
}

# Reporte Total
# Nota: La columna Time aquí ya mostrará la hora peruana en la tabla
View(df_analisis %>% select(ID_Original, Time, velocidad_kmh, ESTADO, DETALLE), 
     title = "📋 AUDITORÍA COMPLETA")

cat(">>> Proceso finalizado.\n")




############################################---

############################################---