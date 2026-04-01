#######################################################---
# DIAGNÓSTICO AISLADO: MAPA DE RUTA GPS (KML)
# Objetivo: Validar la integridad del trazado espacial
#######################################################---
rm(list = ls())
options(warn = -1)

# 1. LIBRERÍAS
paquetes <- c("shiny", "leaflet", "dplyr", "xml2")
instalados <- paquetes %in% installed.packages()
if(any(!instalados)) install.packages(paquetes[!instalados])

library(shiny)
library(leaflet)   
library(dplyr)     
library(xml2)      

cat("==================================================\n")
cat("🛰️ INICIANDO DIAGNÓSTICO AISLADO DEL KML...\n")
cat("==================================================\n")

# =======================================================
# A. LECTURA DEL ARCHIVO LOCAL (KML)
# =======================================================
ruta_kml <- "D:/github_UPCH/R/R/data_vacunacion/2025/JLBYR_2025/GPS-61 22-06-2025 GRUPO 01.kml"

if(!file.exists(ruta_kml)) stop("❌ No se encontró el KML.")

cat(">>> 1. Procesando KML de Ruta...\n")
kml_doc <- read_xml(ruta_kml)
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
      Longitude = as.numeric(mat_coord[,1]), 
      Latitude = as.numeric(mat_coord[,2]),
      stringsAsFactors = FALSE
    )
  }
}

# Consolidamos y blindamos matemáticamente
df_gps_limpio <- bind_rows(lista_puntos_gps) %>%
  filter(!is.na(Longitude) & !is.na(Latitude) & Longitude != 0 & Latitude != 0)

cat("    [OK]", nrow(df_gps_limpio), "puntos de ruta válidos extraídos.\n")

# Calculamos los límites exactos (Bounding Box) del GPS
lon_min <- min(df_gps_limpio$Longitude, na.rm = TRUE)
lon_max <- max(df_gps_limpio$Longitude, na.rm = TRUE)
lat_min <- min(df_gps_limpio$Latitude, na.rm = TRUE)
lat_max <- max(df_gps_limpio$Latitude, na.rm = TRUE)

cat("    [LÍMITES GPS] Lon:", lon_min, "a", lon_max, "| Lat:", lat_min, "a", lat_max, "\n")

# =======================================================
# B. SHINY APP (UI & SERVER)
# =======================================================
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%;margin:0;padding:0;}"),
  
  leafletOutput("mapa_interactivo", width = "100%", height = "100vh"),
  
  absolutePanel(
    top = 20, left = 60, draggable = TRUE, width = 300,
    wellPanel(
      h4("🛰️ Diagnóstico de GPS"),
      hr(),
      HTML(paste0("<b style='color:blue;'>Puntos de ruta trazados:</b> ", nrow(df_gps_limpio))),
      hr(),
      helpText("Si ves este panel y el mapa base, el sistema está estable.")
    )
  )
)

server <- function(input, output, session) {
  
  output$mapa_interactivo <- renderLeaflet({
    # Verificamos que tengamos puntos válidos antes de dibujar
    if(nrow(df_gps_limpio) > 0) {
      leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
        
        # Encuadre automático usando los límites del GPS
        fitBounds(lng1 = lon_min, lat1 = lat_min, lng2 = lon_max, lat2 = lat_max) %>%
        
        addProviderTiles(providers$CartoDB.Positron) %>%
        
        # Trazamos los puntos en azul
        addCircleMarkers(
          data = df_gps_limpio, lng = ~Longitude, lat = ~Latitude,
          radius = 3, fillColor = "blue", color = "blue", 
          weight = 0, fillOpacity = 0.5, group = "Ruta GPS"
        )
    } else {
      # Fallback si por alguna razón no extrajo nada
      leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(-71.52, -16.43, 13)
    }
  })
}

shinyApp(ui, server)