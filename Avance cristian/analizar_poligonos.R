#######################################################---
# HERRAMIENTA DE AUDITORÍA: VISOR DE CATASTROS Y POLÍGONOS
# Permite visualizar y descartar archivos CSV y KML
#######################################################---
rm(list = ls())
options(warn = -1)

library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# Apagamos el motor estricto para evitar errores al leer los polígonos
sf_use_s2(FALSE) 

# =======================================================
# 1. RUTA EXACTA A TU CARPETA DE POLÍGONOS
# =======================================================
ruta_poligonos <- "D:/github_UPCH/R/R/data_vacunacion/2025/cluster_30_31/poligonos/Localities"

# Leer absolutamente todos los archivos CSV y KML de la carpeta
archivos_disponibles <- list.files(ruta_poligonos, pattern = "\\.(csv|kml)$", ignore.case = TRUE)

if(length(archivos_disponibles) == 0) {
  stop(paste("❌ No se encontraron archivos CSV o KML en la ruta:", ruta_poligonos))
}

# =======================================================
# 2. FUNCIÓN DE LECTURA (Soporta CSV de UPCH y KML estándar)
# =======================================================
procesar_archivo_espacial <- function(ruta_completa) {
  ext <- tolower(tools::file_ext(ruta_completa))
  
  # Si es tu formato CSV clásico
  if (ext == "csv") {
    pol_raw <- read.csv(ruta_completa, sep = ";", stringsAsFactors = FALSE)
    pol_raw <- pol_raw %>% mutate(poly_id = cumsum(is.na(lat) | lat == "" | lat == "0"))
    pol_clean <- pol_raw %>%
      filter(!(is.na(lat) | lat == "" | lat == "0")) %>%
      mutate(lat = as.numeric(gsub(",", ".", lat)), long = as.numeric(gsub(",", ".", long))) %>%
      filter(!is.na(lat) & !is.na(long))
    
    if(nrow(pol_clean) == 0) return(NULL)
    
    pol_sf <- pol_clean %>% 
      st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
      group_by(poly_id) %>% 
      summarise(geometry = st_cast(st_combine(geometry), "POLYGON"), .groups = "drop") %>%
      st_make_valid()
    
    return(pol_sf)
    
    # Si tienes archivos KML de Google Earth en la carpeta
  } else if (ext == "kml") {
    pol_sf <- tryCatch({
      st_read(ruta_completa, quiet = TRUE) %>% st_zm() %>% st_make_valid()
    }, error = function(e) return(NULL))
    return(pol_sf)
  }
  return(NULL)
}

# =======================================================
# 3. INTERFAZ DE USUARIO (SHINY)
# =======================================================
ui <- fluidPage(
  titlePanel("Validador Visual de Catastros (Manzanas)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Control de Archivos"),
      selectInput("archivo_sel", "Selecciona un archivo de la carpeta:", 
                  choices = archivos_disponibles),
      hr(),
      helpText("Instrucciones:"),
      helpText("1. Selecciona un archivo de la lista."),
      helpText("2. El mapa te llevará automáticamente a la zona."),
      helpText("3. Verifica si los bloques cubren la zona de trabajo que necesitas."),
      width = 3
    ),
    
    mainPanel(
      leafletOutput("mapa_poligonos", height = "85vh"),
      width = 9
    )
  )
)

# =======================================================
# 4. LÓGICA DEL SERVIDOR
# =======================================================
server <- function(input, output, session) {
  
  # Leer el polígono dinámicamente cuando el usuario cambia el menú
  poligono_reactivo <- reactive({
    req(input$archivo_sel)
    ruta_completa <- file.path(ruta_poligonos, input$archivo_sel)
    procesar_archivo_espacial(ruta_completa)
  })
  
  # Dibujar el mapa
  output$mapa_poligonos <- renderLeaflet({
    m <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron)
    
    pol_sf <- poligono_reactivo()
    
    if (!is.null(pol_sf) && nrow(pol_sf) > 0) {
      # Obtener los límites para centrar la cámara
      bbox <- st_bbox(pol_sf)
      
      m <- m %>% 
        addPolygons(data = pol_sf, 
                    fillColor = "#3388ff", 
                    fillOpacity = 0.4, 
                    color = "black", 
                    weight = 1.5,
                    popup = "Bloque de Manzana") %>%
        fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
    } else {
      # Si el archivo está vacío o dañado, centrar en Arequipa genérico
      m <- m %>% setView(lng = -71.53, lat = -16.40, zoom = 12) %>%
        addPopups(lng = -71.53, lat = -16.40, "Archivo vacío o formato no soportado")
    }
    
    m
  })
}

shinyApp(ui, server)