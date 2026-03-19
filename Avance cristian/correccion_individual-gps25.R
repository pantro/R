#######################################################---
# 1. CONFIGURACIÓN E INSTALACIÓN DE LIBRERÍAS----
#######################################################---
rm(list = ls()) 
options(warn = -1) 

# Verificación de librerías
paquetes <- c("sf", "dplyr", "leaflet", "viridis", "xml2", "geosphere", "nngeo")
instalados <- paquetes %in% installed.packages()
if (any(!instalados)) {
  install.packages(paquetes[!instalados])
}

library(sf)
sf_use_s2(FALSE) # CRÍTICO: Apaga el motor espacial estricto para evitar errores por vértices duplicados.
library(dplyr)
library(leaflet)
library(viridis)
library(xml2)
library(geosphere)
library(nngeo) # Para ajustar puntos al polígono más cercano (Snapping)

# RUTAS DE ARCHIVOS (Modificar aquí para nuevos análisis)
kml_dir <- "D:/github_UPCH/rutas_vancan/GPS VANCAN 2023 JLByR/GPS VANCAN 2023 JLByR/"
archivo_objetivo <- "GPS-25  17-06-2023  GRUPO 24.kml"
ruta_kml <- file.path(kml_dir, archivo_objetivo)
csv_path <- file.path(kml_dir, "Mz_JLByR_08mar2024.csv")

# PARÁMETROS DE CALIBRACIÓN
UMBRAL_KMH <- 9.0         # Límite de velocidad para detectar saltos (Ruido)
RADIO_AGRUPAMIENTO <- 5.0 # Metros de radio geográfico para agrupar puntos detenidos

#######################################################---
# 2. FUNCIONES DE PROCESAMIENTO Y AGRUPAMIENTO----
#######################################################---

# A. Procesamiento de Manzanas (Contexto)
procesar_manzanas <- function(ruta_csv) {
  if (!file.exists(ruta_csv)) return(NULL)
  pol_raw <- read.csv(ruta_csv, sep = ";", stringsAsFactors = FALSE)
  pol_raw <- pol_raw %>% mutate(poly_id = cumsum(is.na(lat) | lat == "" | lat == "0"))
  pol_clean <- pol_raw %>%
    filter(!(is.na(lat) | lat == "" | lat == "0")) %>%
    mutate(lat = as.numeric(gsub(",", ".", lat)), long = as.numeric(gsub(",", ".", long))) %>%
    filter(!is.na(lat) & !is.na(long))
  if(nrow(pol_clean) == 0) return(NULL)
  
  # Corrección de Sintaxis y Adición de st_make_valid()
  return(
    pol_clean %>% 
      st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
      group_by(poly_id) %>% 
      summarise(geometry = st_cast(st_combine(geometry), "POLYGON"), .groups = "drop") %>%
      st_make_valid() # Asegura que no haya polígonos corruptos
  )
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
  n_max <- max(length(txt_time), nrow(mat_coord))
  
  final_times <- rep(NA, n_max); final_lon <- rep(NA, n_max); final_lat <- rep(NA, n_max)
  if (length(txt_time) > 0) final_times[1:length(txt_time)] <- txt_time
  if (nrow(mat_coord) > 0) {
    final_lon[1:nrow(mat_coord)] <- as.numeric(mat_coord[,1])
    final_lat[1:nrow(mat_coord)] <- as.numeric(mat_coord[,2])
  }
  return(data.frame(ID_Original = 1:n_max, Time_Raw = final_times, Longitude = final_lon, Latitude = final_lat, stringsAsFactors = FALSE))
}

# C. Función para Agrupar Puntos Cercanos (Clustering)
agrupar_puntos <- function(df_puntos, radio) {
  df_agrupado <- df_puntos %>% mutate(Cluster_ID = NA_integer_)
  cluster_actual <- 1
  
  for (i in 1:nrow(df_agrupado)) {
    if (is.na(df_agrupado$Cluster_ID[i])) {
      df_agrupado$Cluster_ID[i] <- cluster_actual
      distancias <- geosphere::distHaversine(
        c(df_agrupado$Longitude[i], df_agrupado$Latitude[i]),
        cbind(df_agrupado$Longitude, df_agrupado$Latitude)
      )
      vecinos_idx <- which(distancias <= radio & is.na(df_agrupado$Cluster_ID))
      if (length(vecinos_idx) > 0) df_agrupado$Cluster_ID[vecinos_idx] <- cluster_actual
      cluster_actual <- cluster_actual + 1
    }
  }
  
  df_final <- df_agrupado %>%
    group_by(Cluster_ID) %>%
    summarise(
      Longitude = mean(Longitude), Latitude = mean(Latitude),
      Time_Min = min(Time), Time_Max = max(Time),
      Puntos_Agrupados = n(), Vel_Promedio = mean(velocidad_kmh, na.rm=TRUE),
      .groups = "drop"
    ) %>%
    arrange(Time_Min) %>% 
    mutate(
      Secuencia_Agrupada = row_number(),
      Progreso_Tiempo = if(n() > 1) (row_number() - 1) / (n() - 1) else 0
    )
  return(df_final)
}

#######################################################---
# 3. ANÁLISIS FORENSE, LIMPIEZA Y AJUSTE ESPACIAL----
#######################################################---
cat(">>> Analizando datos, limpiando ruido y realizando ajuste espacial (Snapping)...\n")

df_total <- extract_full_data(ruta_kml)

df_analisis <- df_total %>%
  mutate(Time = as.POSIXct(Time_Raw, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")) %>%
  arrange(Time) 
attr(df_analisis$Time, "tzone") <- "America/Lima"

# Lógica Antirrebote y Cálculo de Velocidad Real
n_rows <- nrow(df_analisis)
df_analisis$delta_sec <- 0; df_analisis$dist_m <- 0; df_analisis$velocidad_kmh <- 0
df_analisis$ESTADO <- "OK"; df_analisis$DETALLE <- "Validado"

if (n_rows > 0) {
  if (is.na(df_analisis$Time[1])) df_analisis$ESTADO[1] <- "ERROR ESTRUCTURA"
  if (is.na(df_analisis$Longitude[1])) df_analisis$ESTADO[1] <- "ERROR COORDENADA"
  
  last_valid_idx <- 1
  for (i in 2:n_rows) {
    if (is.na(df_analisis$Time[i]) || is.na(df_analisis$Longitude[i])) {
      df_analisis$ESTADO[i] <- "ERROR"; next
    }
    
    t_current <- df_analisis$Time[i]
    t_last <- df_analisis$Time[last_valid_idx]
    delta_t <- as.numeric(difftime(t_current, t_last, units = "secs"))
    df_analisis$delta_sec[i] <- delta_t
    
    if (delta_t == 0) { df_analisis$ESTADO[i] <- "DUPLICADO"; next }
    
    dist_m <- geosphere::distHaversine(
      c(df_analisis$Longitude[last_valid_idx], df_analisis$Latitude[last_valid_idx]),
      c(df_analisis$Longitude[i], df_analisis$Latitude[i])
    )
    df_analisis$dist_m[i] <- dist_m
    vel_kmh <- (dist_m / delta_t) * 3.6
    df_analisis$velocidad_kmh[i] <- vel_kmh
    
    if (vel_kmh > UMBRAL_KMH) {
      df_analisis$ESTADO[i] <- "RUIDO GPS (SALTO)"
      df_analisis$DETALLE[i] <- paste0("Vel. Excesiva: ", round(vel_kmh, 1), " km/h")
    } else {
      last_valid_idx <- i
    }
  }
}

# Nos quedamos solo con los puntos limpios
df_mapa <- df_analisis %>% filter(ESTADO == "OK" & !is.na(Time))

# === NUEVA LÓGICA ESPACIAL: HUMANIZAR RUTA (AJUSTAR A MANZANAS) ===
pol_sf <- procesar_manzanas(csv_path)

if (!is.null(pol_sf)) {
  # Convertimos los puntos a formato espacial
  puntos_sf <- df_mapa %>% st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
  
  # Detectamos qué puntos caen FUERA de los polígonos
  # st_intersects devuelve una lista. Si está vacía (length 0), está fuera.
  intersecciones <- st_intersects(puntos_sf, pol_sf)
  df_mapa$Ubicacion <- sapply(intersecciones, function(x) ifelse(length(x) == 0, "FUERA DE CUADRA", "DENTRO DE CUADRA"))
  
  # --- LÓGICA DE SNAPPING (Ajuste al Borde) ---
  # Separamos los puntos rebeldes
  puntos_rebeldes_idx <- which(df_mapa$Ubicacion == "FUERA DE CUADRA")
  
  if(length(puntos_rebeldes_idx) > 0) {
    puntos_fuera_sf <- puntos_sf[puntos_rebeldes_idx, ]
    
    # st_nearest_points encuentra la línea más corta entre el punto y el polígono más cercano
    lineas_mas_cortas <- st_nearest_points(puntos_fuera_sf, st_union(pol_sf))
    
    # Extraemos las coordenadas del extremo de esa línea que toca el polígono (el borde)
    # st_cast("POINT") convierte la línea en dos puntos. Nos quedamos con el segundo (el del borde)
    puntos_borde_sf <- st_cast(lineas_mas_cortas, "POINT")[seq(2, length(lineas_mas_cortas) * 2, 2)]
    
    # Reemplazamos las coordenadas originales por las nuevas "Ajustadas" en el dataframe original
    coords_ajustadas <- st_coordinates(puntos_borde_sf)
    df_mapa$Longitude[puntos_rebeldes_idx] <- coords_ajustadas[, 1]
    df_mapa$Latitude[puntos_rebeldes_idx] <- coords_ajustadas[, 2]
    
    # Actualizamos el estado para la auditoría
    df_mapa$DETALLE[puntos_rebeldes_idx] <- "Ajustado a Vereda (Snapping)"
  }
}

# Agrupamos los puntos (Clustering) usando los PUNTOS YA AJUSTADOS
df_cluster <- agrupar_puntos(df_mapa, RADIO_AGRUPAMIENTO)


#######################################################---
# 4. GENERACIÓN DEL MAPA INTERACTIVO----
#######################################################---
cat(">>> Generando diagrama interactivo con capas profesionales...\n")

m <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron)

# Capa Manzanas
if (!is.null(pol_sf)) m <- m %>% addPolygons(data = pol_sf, group = "Manzanas", color = "#444", weight = 1, fillOpacity = 0.1)

# Paleta Temporal (De Morado a Amarillo)
paleta_temporal <- colorNumeric(palette = "viridis", domain = c(0, 1))

# === 0. CAPA COMPARATIVA: TODOS LOS PUNTOS CRUDOS (OCULTA POR DEFECTO) ===
df_raw <- df_analisis %>% filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  mutate(
    color_raw = ifelse(ESTADO == "OK", "#BDBDBD", "#E53935"),
    radio_raw = ifelse(ESTADO == "OK", 3, 5),
    popup_crudo = paste0(
      "<div style='text-align:left; background-color:", ifelse(ESTADO == "OK", "#f9f9f9", "#ffebee"), "; padding:8px; border-radius:5px; border: 1px solid ", ifelse(ESTADO == "OK", "#ccc", "#ffcdd2"), ";'>",
      "<div style='text-align:center;'><b>PUNTO ORIGINAL #", ID_Original, "</b></div><hr style='margin:5px 0;'>",
      "⏰ <b>Hora:</b> ", ifelse(is.na(Time), "Desconocida", format(Time, "%H:%M:%S")), "<br>",
      "📏 <b>Salto de Distancia:</b> ", round(dist_m, 1), " metros<br>",
      "🏃 <b>Vel. Calculada:</b> ", round(velocidad_kmh, 1), " km/h<br>",
      "⚠️ <b>Estado:</b> <span style='color:", ifelse(ESTADO == "OK", "green", "red"), "; font-weight:bold;'>", ESTADO, "</span><br>",
      "📝 <b>Motivo:</b> ", DETALLE,
      "</div>"
    )
  )

linea_sucia <- df_raw %>% st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% summarise(do_union = FALSE) %>% st_cast("LINESTRING")

m <- m %>% 
  addPolylines(data = linea_sucia, color = "red", weight = 1, dashArray = "4,4", opacity = 0.4, group = "Puntos Crudos (Con Ruido)") %>%
  addCircleMarkers(
    data = df_raw, lng = ~Longitude, lat = ~Latitude,
    radius = ~radio_raw, fillColor = ~color_raw, color = "black", weight = 1, opacity = 1, fillOpacity = 0.8,
    popup = ~popup_crudo, group = "Puntos Crudos (Con Ruido)"
  )

# === 0.5 NUEVA CAPA: DESVÍOS (PUNTOS FUERA DE CUADRA) ===
# Pintaremos de naranja oscuro los puntos que originalmente cayeron fuera y fueron arrastrados al borde.
if ("Ubicacion" %in% names(df_mapa)) {
  df_desvios <- df_mapa %>% filter(Ubicacion == "FUERA DE CUADRA")
  
  if(nrow(df_desvios) > 0) {
    m <- m %>% addCircleMarkers(
      data = df_desvios, lng = ~Longitude, lat = ~Latitude,
      radius = 6, fillColor = "#FF9800", color = "black", weight = 2, opacity = 1, fillOpacity = 0.9,
      popup = ~paste0("<div style='text-align:center;'><b>📍 DESVÍO DETECTADO</b><hr>Hora: ", format(Time, "%H:%M:%S"), "<br>Ajustado al borde de la manzana más cercana.</div>"),
      group = "Desvíos Detectados (Snapping)"
    )
  }
}


# === 1. DIBUJO DE LÍNEAS (Ruta Trazada Ajustada) ===
for (i in 1:(nrow(df_cluster) - 1)) {
  m <- m %>% addPolylines(
    lng = c(df_cluster$Longitude[i], df_cluster$Longitude[i + 1]),
    lat = c(df_cluster$Latitude[i], df_cluster$Latitude[i + 1]),
    color = paleta_temporal(df_cluster$Progreso_Tiempo[i]), 
    weight = 3, opacity = 0.8, group = "Ruta Trazada Humanizada"
  )
}

# === 2. SEPARACIÓN INTELIGENTE DE PUNTOS AJUSTADOS ===
df_unicos <- df_cluster %>% filter(Puntos_Agrupados == 1) %>%
  mutate(popup_html = paste0("<div style='text-align:center;'><b>PUNTO NORMAL #", Secuencia_Agrupada, "</b><hr>⏰ <b>Hora:</b> ", format(Time_Min, "%H:%M:%S"), "<br>🏃 <b>Velocidad:</b> ", round(Vel_Promedio, 1), " km/h</div>"))

df_grupos <- df_cluster %>% filter(Puntos_Agrupados > 1) %>%
  mutate(
    radio_dinamico = 5 + (Puntos_Agrupados - 1) * 0.8, 
    popup_html = paste0("<div style='text-align:center; background-color:#f9f9f9; padding:5px;'><b>🎯 ZONA DE CONCENTRACIÓN #", Secuencia_Agrupada, "</b><hr>📍 <b>Puntos agrupados:</b> ", Puntos_Agrupados, " registros<br>📥 <b>Llegada:</b> ", format(Time_Min, "%H:%M:%S"), "<br>📤 <b>Salida:</b> ", format(Time_Max, "%H:%M:%S"), "<br>🏃 <b>Vel. Promedio:</b> ", round(Vel_Promedio, 1), " km/h</div>")
  )

if(nrow(df_unicos) > 0) m <- m %>% addCircleMarkers(data = df_unicos, lng = ~Longitude, lat = ~Latitude, radius = 4, fillColor = ~paleta_temporal(Progreso_Tiempo), color = "black", weight = 1, opacity = 1, fillOpacity = 0.9, popup = ~popup_html, group = "Ruta Trazada Humanizada")
if(nrow(df_grupos) > 0) m <- m %>% addCircles(data = df_grupos, lng = ~Longitude, lat = ~Latitude, radius = ~radio_dinamico, fillColor = ~paleta_temporal(Progreso_Tiempo), color = "black", weight = 1, opacity = 1, fillOpacity = 0.75, popup = ~popup_html, group = "Ruta Trazada Humanizada")

# === NUEVA LEYENDA ===
leyenda_html <- "
<div style='padding: 8px 10px; background: white; border-radius: 5px; box-shadow: 0 0 10px rgba(0,0,0,0.2); font-family: Arial, sans-serif;'>
  <strong style='color: #555; font-size: 14px;'>Progresión temporal</strong><br/>
  <div style='background: linear-gradient(to right, #440154, #21918c, #fde725); width: 220px; height: 12px; margin-top: 8px; border-radius: 2px;'></div>
  <div style='display: flex; justify-content: space-between; font-size: 12px; margin-top: 4px; color: #666;'>
    <span>Inicio</span>
    <span>Medio</span>
    <span>Fin</span>
  </div>
</div>
"

# === CONTROLES FINALES ===
m <- m %>%
  addLayersControl(
    overlayGroups = c("Manzanas", "Ruta Trazada Humanizada", "Desvíos Detectados (Snapping)", "Puntos Crudos (Con Ruido)"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup(c("Puntos Crudos (Con Ruido)", "Desvíos Detectados (Snapping)")) %>% # <-- APAGAMOS CAPAS SECUNDARIAS
  addControl(html = leyenda_html, position = "bottomright") %>%
  addControl(html = paste0('<div style="background: white; padding: 10px; border-radius:5px; font-family: Arial;"><b>DIAGRAMA DINÁMICO</b><br><small style="color: gray;">Archivo: ', archivo_objetivo, '</small></div>'), position = "topright")

print(m)

#######################################################---
# 5. TABLAS DE AUDITORÍA----
#######################################################---
cat(">>> Abriendo reportes de auditoría...\n")

df_errores <- df_analisis %>% filter(ESTADO != "OK") %>% select(ID_Original, Time, Longitude, Latitude, velocidad_kmh, ESTADO, DETALLE)
if(nrow(df_errores) > 0) View(df_errores, title = "⚠️ COMPONENTES DEFECTUOSOS (ELIMINADOS)")

if ("Ubicacion" %in% names(df_mapa)) {
  View(df_mapa %>% select(ID_Original, Time, Longitude, Latitude, velocidad_kmh, Ubicacion, DETALLE), title = "📋 AUDITORÍA RUTA HUMANIZADA")
} else {
  View(df_mapa %>% select(ID_Original, Time, Longitude, Latitude, velocidad_kmh, DETALLE), title = "📋 AUDITORÍA RUTA HUMANIZADA")
}

cat(">>> Proceso finalizado.\n")