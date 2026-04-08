#######################################################---
# SHINY APP: AUDITORÍA DE CAMPO (PUNTOS CRUDOS GENERAL)
# Lectura desde Excel (.xlsx) y KML General
#######################################################---
rm(list = ls())
options(warn = -1)

# 1. CARGA DE LIBRERÍAS (Añadimos readxl para Excel)
paquetes <- c("shiny", "leaflet", "dplyr", "readr", "xml2", "readxl")
instalados <- paquetes %in% installed.packages()
if(any(!instalados)) install.packages(paquetes[!instalados])

library(shiny)
library(leaflet)   
library(dplyr)     
library(readr)     
library(xml2)      
library(readxl)    # <-- NUEVA LIBRERÍA PARA EXCEL

cat("==================================================\n")
cat("📊 INICIANDO CARGA DE DATOS GENERALES (EXCEL Y KML)...\n")
cat("==================================================\n")

# =======================================================
# A. RUTAS LOCALES
# =======================================================
ruta_base <- "D:/github_UPCH/R/R/data_vacunacion/2025/JLBYR_2025"

# Actualizamos los nombres de los archivos generales
ruta_excel_completa <- file.path(ruta_base, "surveys_2025-10-07.xlsx")
ruta_kml_completa <- file.path(ruta_base, "GPS_VANCAN_2025_GENERAL.kml")

# =======================================================
# B. ETL: APLICATIVO (EXCEL - NARANJA)
# =======================================================
cat(">>> 1. Procesando Excel General...\n")
if(!file.exists(ruta_excel_completa)) stop("❌ No se encontró el archivo Excel.")

# Leemos el Excel directamente
df_app_raw <- read_excel(ruta_excel_completa)
colnames(df_app_raw) <- tolower(colnames(df_app_raw))

# Aplicamos el filtro maestro para purificar coordenadas
df_app_limpio <- df_app_raw %>%
  select(block_id, date, lat, long) %>%
  mutate(
    lat = as.numeric(gsub(",", ".", as.character(lat))),
    long = as.numeric(gsub(",", ".", as.character(long))),
    date_clean = as.POSIXct(as.character(date), format="%Y-%m-%dT%H:%M:%OS", tz="UTC")
  ) %>%
  filter(!is.na(long) & !is.na(lat) & long != 0 & lat != 0)

attr(df_app_limpio$date_clean, "tzone") <- "America/Lima"
cat("    [OK] Encuestas Generales Válidas:", nrow(df_app_limpio), "\n")

# =======================================================
# C. ETL: RUTAS GPS (KML - MORADO)
# =======================================================
cat(">>> 2. Procesando KML General...\n")
if(!file.exists(ruta_kml_completa)) stop("❌ No se encontró el KML.")

kml_doc <- read_xml(ruta_kml_completa)
ns <- xml_ns(kml_doc)
tramos <- xml_find_all(kml_doc, ".//gx:Track", ns)

lista_puntos_gps <- list()
for (i in seq_along(tramos)) {
  tramo_actual <- tramos[[i]]
  tiempos_raw <- xml_text(xml_find_all(tramo_actual, "./kml:when", ns))
  coords_raw <- xml_text(xml_find_all(tramo_actual, "./gx:coord", ns))
  if (length(tiempos_raw) == length(coords_raw) && length(coords_raw) > 0) {
    mat_coord <- do.call(rbind, strsplit(coords_raw, " "))
    lista_puntos_gps[[i]] <- data.frame(
      TIME = tiempos_raw, LONG = as.numeric(mat_coord[,1]), LAT = as.numeric(mat_coord[,2]),
      stringsAsFactors = FALSE
    )
  }
}

df_gps_limpio <- bind_rows(lista_puntos_gps) %>%
  filter(!is.na(LONG) & !is.na(LAT) & LONG != 0 & LAT != 0) %>%
  mutate(TIME_FORMAT = as.POSIXct(TIME, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC"))

attr(df_gps_limpio$TIME_FORMAT, "tzone") <- "America/Lima"
cat("    [OK] Puntos GPS Generales Válidos:", nrow(df_gps_limpio), "\n")

# =======================================================
# D. CÁLCULO DE LÍMITES
# =======================================================
cat(">>> 3. Calculando límites espaciales...\n")
lon_min_final <- min(c(min(df_app_limpio$long), min(df_gps_limpio$LONG)))
lon_max_final <- max(c(max(df_app_limpio$long), max(df_gps_limpio$LONG)))
lat_min_final <- min(c(min(df_app_limpio$lat), min(df_gps_limpio$LAT)))
lat_max_final <- max(c(max(df_app_limpio$lat), max(df_gps_limpio$LAT)))

# =======================================================
# E. SHINY APP
# =======================================================
cat(">>> 4. Levantando Shiny App...\n")
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%;margin:0;padding:0;}"),
  leafletOutput("mapa_interactivo", width = "100%", height = "100vh"),
  
  absolutePanel(
    top = 20, left = 60, draggable = TRUE, width = 340,
    wellPanel(
      h4("📊 Auditoría Espacial General 2025"),
      hr(),
      p(HTML(paste0("<b style='color:#751dc3;'>🛰️ Ruta GPS (Morado):</b> ", nrow(df_gps_limpio), " puntos"))),
      p(HTML(paste0("<b style='color:#e68102;'>📱 Encuestas (Naranja):</b> ", nrow(df_app_limpio), " puntos"))),
      hr(),
      p("✅ Mostrando toda la base general en vista de puntos crudos.")
    )
  )
)

server <- function(input, output, session) {
  output$mapa_interactivo <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      fitBounds(lng1 = lon_min_final, lat1 = lat_min_final, lng2 = lon_max_final, lat2 = lat_max_final) %>%
      
      addProviderTiles(providers$CartoDB.Positron, group = "Mapa Gris (Limpio)") %>%
      addProviderTiles(providers$OpenStreetMap, group = "Mapa Calles") %>%
      
      # 1. PUNTOS GPS (MORADOS)
      addCircleMarkers(data = df_gps_limpio, lng = ~LONG, lat = ~LAT,
                       radius = 2, fillColor = "#751dc3", color = "#751dc3", weight = 0, fillOpacity = 0.4,
                       group = "GPS Ruta General (Morado)",
                       popup = ~paste("<b>Hora GPS:</b>", format(TIME_FORMAT, "%H:%M:%S"))) %>%
      
      # 2. PUNTOS APLICATIVO (NARANJAS)
      addCircleMarkers(data = df_app_limpio, lng = ~long, lat = ~lat,
                       radius = 4, fillColor = "#e68102", color = "white", weight = 1, fillOpacity = 0.9,
                       group = "Encuestas Generales (Naranja)",
                       popup = ~paste("<div style='font-family: Arial; font-size: 11px;'>",
                                      "<b style='color:#e68102;'>Encuesta Registrada</b><hr style='margin: 3px 0;'>",
                                      "<b>BLOCK_ID:</b>", block_id, "<br>",
                                      "<b>Hora Local:</b>", format(date_clean, "%H:%M:%S"),
                                      "</div>")) %>%
      
      addLayersControl(baseGroups = c("Mapa Gris (Limpio)", "Mapa Calles"),
                       overlayGroups = c("GPS Ruta General (Morado)", "Encuestas Generales (Naranja)"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      
      addLegend(position = "bottomright", colors = c("#751dc3", "#e68102"), 
                labels = c("Ruta GPS (Morado)", "Encuestas (Naranja)"),
                title = "Auditoría Espacial General")
  })
}

shinyApp(ui, server)