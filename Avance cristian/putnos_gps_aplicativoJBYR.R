#######################################################---
# DIAGNÓSTICO AISLADO: MAPA DE APLICATIVO (CSV)
# CORRECCIÓN DE DECIMALES Y FILTRO ESTRICTO
#######################################################---
rm(list = ls())
options(warn = -1)

# 1. LIBRERÍAS
paquetes <- c("shiny", "leaflet", "dplyr", "readr")
instalados <- paquetes %in% installed.packages()
if(any(!instalados)) install.packages(paquetes[!instalados])

library(shiny)
library(leaflet)   
library(dplyr)     
library(readr)     

cat("==================================================\n")
cat("📱 INICIANDO DIAGNÓSTICO AISLADO DEL APLICATIVO...\n")
cat("==================================================\n")

# =======================================================
# A. LECTURA Y LIMPIEZA EXTREMA DEL CSV LOCAL
# =======================================================
ruta_csv <- "D:/github_UPCH/R/R/data_vacunacion/2025/JLBYR_2025/Data_JLBYR_Cluster03_2025.csv"

if(!file.exists(ruta_csv)) stop("❌ No se encontró el CSV.")

cat(">>> 1. Procesando CSV (Extrayendo solo variables clave)...\n")

# Leemos el archivo crudo. Usamos delimitador explícito.
df_app_raw <- read_delim(ruta_csv, delim = ";", show_col_types = FALSE)

# Estandarizamos los nombres de las columnas a minúsculas por si acaso
colnames(df_app_raw) <- tolower(colnames(df_app_raw))

# EL FILTRO MAESTRO: Solo block_id, date, lat, long
df_app_limpio <- df_app_raw %>%
  select(block_id, date, lat, long) %>%
  mutate(
    # TRUCO SENIOR: Convertimos a texto, forzamos el punto como decimal, y pasamos a numérico
    lat = as.numeric(gsub(",", ".", as.character(lat))),
    long = as.numeric(gsub(",", ".", as.character(long))),
    # Limpiamos la fecha
    date_clean = as.POSIXct(as.character(date), format="%Y-%m-%dT%H:%M:%OS", tz="UTC")
  ) %>%
  # Rechazamos cualquier cosa que no tenga coordenada o sea 0
  filter(!is.na(long) & !is.na(lat) & long != 0 & lat != 0)

cat("    [OK]", nrow(df_app_limpio), "encuestas válidas listas para graficar.\n")

# Calculamos los límites para que la cámara haga zoom automático
lon_min <- min(df_app_limpio$long, na.rm = TRUE)
lon_max <- max(df_app_limpio$long, na.rm = TRUE)
lat_min <- min(df_app_limpio$lat, na.rm = TRUE)
lat_max <- max(df_app_limpio$lat, na.rm = TRUE)

cat(">>> 2. Levantando Mapa Interactivo...\n")

# =======================================================
# B. SHINY APP (UI & SERVER)
# =======================================================
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%;margin:0;padding:0;}"),
  
  leafletOutput("mapa_interactivo", width = "100%", height = "100vh"),
  
  absolutePanel(
    top = 20, left = 60, draggable = TRUE, width = 300,
    wellPanel(
      h4("📱 Datos del Aplicativo (Puros)"),
      hr(),
      HTML(paste0("<b style='color:purple;'>Encuestas graficadas:</b> ", nrow(df_app_limpio))),
      hr(),
      checkboxInput("usar_cluster", "Agrupar (Clusters)", value = TRUE)
    )
  )
)

server <- function(input, output, session) {
  
  output$mapa_interactivo <- renderLeaflet({
    m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      # La cámara volará automáticamente a encerrar tus datos
      fitBounds(lng1 = lon_min, lat1 = lat_min, lng2 = lon_max, lat2 = lat_max) %>%
      addProviderTiles(providers$CartoDB.Positron) 
    m
  })
  
  observe({
    proxy <- leafletProxy("mapa_interactivo") %>% clearGroup("Encuestas")
    
    if (input$usar_cluster) {
      proxy %>% addCircleMarkers(
        data = df_app_limpio, lng = ~long, lat = ~lat,
        radius = 5, fillColor = "purple", color = "black", weight = 1, fillOpacity = 0.8,
        clusterOptions = markerClusterOptions(),
        group = "Encuestas",
        popup = ~paste("<b>ID:</b>", block_id, "<br><b>Hora:</b>", format(date_clean, "%H:%M:%S"))
      )
    } else {
      proxy %>% addCircleMarkers(
        data = df_app_limpio, lng = ~long, lat = ~lat,
        radius = 4, fillColor = "purple", color = "white", weight = 1, fillOpacity = 0.9,
        group = "Encuestas",
        popup = ~paste("<b>ID:</b>", block_id, "<br><b>Hora:</b>", format(date_clean, "%H:%M:%S"))
      )
    }
  })
}

shinyApp(ui, server)