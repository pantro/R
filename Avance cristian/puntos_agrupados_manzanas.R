#######################################################---
# 1. CONFIGURACIÓN E INSTALACIÓN DE LIBRERÍAS----
#######################################################---
rm(list = ls()) 
options(warn = -1) 

paquetes <- c("shiny", "sf", "dplyr", "leaflet", "viridis", "xml2", "geosphere")
instalados <- paquetes %in% installed.packages()
if (any(!instalados)) {
  install.packages(paquetes[!instalados])
}

library(shiny)      # Framework para aplicaciones web interactivas en R
library(sf)         # Análisis espacial (Spatial Features)
sf_use_s2(FALSE)    # Desactiva el motor esférico S2 para tolerar polígonos geométricamente imperfectos
library(dplyr)      # Manipulación y transformación de dataframes
library(leaflet)    # Renderizado de mapas interactivos
library(viridis)    # Paletas de colores perceptualmente uniformes
library(xml2)       # Lectura y parseo de archivos KML/XML
library(geosphere)  # Cálculos de distancias geográficas reales (fórmula Haversine)

# DEFINICIÓN DE RUTAS Y PARÁMETROS GLOBALES
kml_dir <- "D:/github_UPCH/rutas_vancan/GPS VANCAN 2023 JLByR/GPS VANCAN 2023 JLByR/"
archivo_objetivo <- "GPS-25  17-06-2023  GRUPO 24.kml"
ruta_kml <- file.path(kml_dir, archivo_objetivo)
csv_path <- file.path(kml_dir, "Mz_JLByR_08mar2024.csv")

# LÓGICA BIOMECÁNICA: Velocidad máxima humana realista en trabajo de campo
VELOCIDAD_MAX_MS <- 1.5  
UMBRAL_KMH <- VELOCIDAD_MAX_MS * 3.6 

#######################################################---
# 2. FUNCIONES BASE Y PRE-PROCESAMIENTO DE DATOS----
#######################################################---

# TRANSFORMA CSV EN POLÍGONOS ESPACIALES
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
           group_by(poly_id) %>% summarise(geometry = st_cast(st_combine(geometry), "POLYGON"), .groups = "drop") %>%
           st_make_valid()) 
}

# EXTRAE COORDENADAS Y MARCAS DE TIEMPO DEL KML
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

# ALGORITMO DE FUSIÓN ESPACIO-TEMPORAL CON LÍMITES
agrupar_puntos_secuencial <- function(df_puntos, radio, max_puntos) {
  if(nrow(df_puntos) == 0) return(NULL)
  df_puntos <- df_puntos %>% arrange(Time) # Orden cronológico estricto
  
  cluster_id <- 1
  current_lon <- df_puntos$Longitude[1]
  current_lat <- df_puntos$Latitude[1]
  time_min <- df_puntos$Time[1]
  time_max <- df_puntos$Time[1]
  puntos_count <- 1
  
  ids_list <- as.character(df_puntos$ID_Original[1])
  dist_acumulada <- 0
  vel_anterior_kmh <- df_puntos$velocidad_kmh[1]
  vel_anterior_ms <- df_puntos$velocidad_ms[1]
  dist_next <- df_puntos$dist_next[1]
  
  resultados <- list()
  
  if(nrow(df_puntos) > 1) {
    for (i in 2:nrow(df_puntos)) {
      distancia <- geosphere::distHaversine(
        c(current_lon, current_lat), c(df_puntos$Longitude[i], df_puntos$Latitude[i])
      )
      
      # Evalúa si cumple los criterios de unión (proximidad y límite de densidad)
      if (distancia <= radio && puntos_count < max_puntos) {
        current_lon <- (current_lon + df_puntos$Longitude[i]) / 2 # Nuevo centroide
        current_lat <- (current_lat + df_puntos$Latitude[i]) / 2
        time_max <- df_puntos$Time[i]
        puntos_count <- puntos_count + 1
        ids_list <- paste(ids_list, df_puntos$ID_Original[i], sep = ", ")
        dist_acumulada <- dist_acumulada + df_puntos$dist_m[i]
      } else {
        # Almacena el grupo terminado
        resultados[[cluster_id]] <- data.frame(
          Cluster_ID = cluster_id, Longitude = current_lon, Latitude = current_lat,
          Time_Min = time_min, Time_Max = time_max, Puntos_Agrupados = puntos_count,
          IDs_Unidos = ids_list, Distancia_Envuelta = dist_acumulada, 
          Vel_Ant_KMH = vel_anterior_kmh, Vel_Ant_MS = vel_anterior_ms, Dist_Next = dist_next,
          stringsAsFactors = FALSE
        )
        # Inicializa el siguiente grupo
        cluster_id <- cluster_id + 1
        current_lon <- df_puntos$Longitude[i]
        current_lat <- df_puntos$Latitude[i]
        time_min <- df_puntos$Time[i]
        time_max <- df_puntos$Time[i]
        puntos_count <- 1
        ids_list <- as.character(df_puntos$ID_Original[i])
        dist_acumulada <- 0
        vel_anterior_kmh <- df_puntos$velocidad_kmh[i]
        vel_anterior_ms <- df_puntos$velocidad_ms[i]
        dist_next <- df_puntos$dist_next[i]
      }
    }
  }
  
  resultados[[cluster_id]] <- data.frame(
    Cluster_ID = cluster_id, Longitude = current_lon, Latitude = current_lat,
    Time_Min = time_min, Time_Max = time_max, Puntos_Agrupados = puntos_count,
    IDs_Unidos = ids_list, Distancia_Envuelta = dist_acumulada, 
    Vel_Ant_KMH = vel_anterior_kmh, Vel_Ant_MS = vel_anterior_ms, Dist_Next = dist_next,
    stringsAsFactors = FALSE
  )
  
  # Calcula el porcentaje de completitud de la ruta para la paleta de colores
  df_final <- bind_rows(resultados) %>%
    mutate(
      Secuencia_Agrupada = row_number(),
      Progreso_Tiempo = if(n() > 1) (row_number() - 1) / (n() - 1) else 0
    )
  return(df_final)
}

cat(">>> Limpiando y preparando datos base con Filtro Biomecánico...\n")
pol_sf <- procesar_manzanas(csv_path)
df_total <- extract_full_data(ruta_kml)
df_analisis <- df_total %>% mutate(Time = as.POSIXct(Time_Raw, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")) %>% arrange(Time) 
attr(df_analisis$Time, "tzone") <- "America/Lima"

# EJECUCIÓN DEL FILTRO CINEMÁTICO Y BIOMECÁNICO
if (nrow(df_analisis) > 0) {
  df_analisis$delta_sec <- 0; df_analisis$dist_m <- 0; df_analisis$velocidad_ms <- 0; df_analisis$velocidad_kmh <- 0; 
  df_analisis$ESTADO <- "OK"
  df_analisis$DETALLE <- "Validado"
  
  df_analisis$ESTADO[is.na(df_analisis$Time) | is.na(df_analisis$Longitude)] <- "ERROR COORDENADA"
  df_analisis$DETALLE[is.na(df_analisis$Time) | is.na(df_analisis$Longitude)] <- "Sin datos GPS"
  
  last_valid_idx <- 1
  for (i in 2:nrow(df_analisis)) {
    if (df_analisis$ESTADO[i] != "OK") next
    
    delta_t <- as.numeric(difftime(df_analisis$Time[i], df_analisis$Time[last_valid_idx], units = "secs"))
    if (delta_t == 0) { 
      df_analisis$ESTADO[i] <- "DUPLICADO"
      df_analisis$DETALLE[i] <- "Registro doble en el mismo segundo"
      next 
    }
    
    dist_m <- geosphere::distHaversine(
      c(df_analisis$Longitude[last_valid_idx], df_analisis$Latitude[last_valid_idx]),
      c(df_analisis$Longitude[i], df_analisis$Latitude[i])
    )
    
    vel_ms <- dist_m / delta_t
    vel_kmh <- vel_ms * 3.6
    
    df_analisis$dist_m[i] <- dist_m
    df_analisis$velocidad_ms[i] <- vel_ms
    df_analisis$velocidad_kmh[i] <- vel_kmh
    
    # Exclusión de datos basados en umbral de factibilidad física
    if (vel_kmh > UMBRAL_KMH) { 
      df_analisis$ESTADO[i] <- "RUIDO GPS (SALTO)" 
      df_analisis$DETALLE[i] <- paste0("Salto irreal: ", round(dist_m, 1), "m en ", delta_t, " seg. (", round(vel_ms, 1), " m/s)")
    } else { 
      last_valid_idx <- i 
    }
  }
}

# Consolidación de dataset útil
df_mapa <- df_analisis %>% filter(ESTADO == "OK" & !is.na(Time)) %>%
  mutate(
    dist_next = geosphere::distHaversine(
      cbind(Longitude, Latitude),
      cbind(lead(Longitude), lead(Latitude))
    )
  )

# GENERACIÓN DE ESTRUCTURAS HTML PARA POPUPS DE MAPA
df_raw <- df_analisis %>% filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  mutate(
    color_raw = ifelse(ESTADO == "OK", "#BDBDBD", "#E53935"),
    radio_raw = ifelse(ESTADO == "OK", 3, 5),
    popup_crudo = paste0(
      "<div style='text-align:left; background-color:", ifelse(ESTADO == "OK", "#f9f9f9", "#ffebee"), "; padding:8px; border-radius:5px; border: 1px solid ", ifelse(ESTADO == "OK", "#ccc", "#ffcdd2"), ";'>",
      "<div style='text-align:center;'><b>Punto Original: ", ID_Original, "</b></div><hr style='margin:5px 0;'>",
      "Hora: ", ifelse(is.na(Time), "Desconocida", format(Time, "%H:%M:%S")), "<br>",
      "Distancia de salto: ", round(dist_m, 1), " metros<br>",
      "Velocidad calculada: ", round(velocidad_ms, 1), " m/s<br>",
      "Estado: <span style='color:", ifelse(ESTADO == "OK", "green", "red"), "; font-weight:bold;'>", ESTADO, "</span><br>",
      "Motivo: ", DETALLE,
      "</div>"
    )
  )

linea_sucia <- df_raw %>% st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% summarise(do_union = FALSE) %>% st_cast("LINESTRING")


#######################################################---
# 3. INTERFAZ DE USUARIO (UI) - SHINY CON PESTAÑAS----
#######################################################---
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%;margin:0;padding:0;}"),
  
  tabsetPanel(
    # --- PISTA DE VISUALIZACIÓN PRINCIPAL ---
    tabPanel("Mapa Interactivo",
             leafletOutput("mapa_interactivo", width = "100%", height = "95vh"),
             absolutePanel(
               top = 60, left = 60, draggable = TRUE, width = 320,
               wellPanel(
                 h4("Panel Dinamico"), hr(),
                 sliderInput("slider_radio", "Distancia de Fusion (metros):", min = 1, max = 20, value = 5, step = 1),
                 sliderInput("slider_puntos", "Limite maximo de puntos por fusion:", min = 1, max = 15, value = 15, step = 1),
                 helpText("Ajusta la capacidad matematica del agrupamiento en tiempo real.")
               )
             )
    ),
    
    # --- MÓDULOS DE AUDITORÍA Y TRAZABILIDAD ---
    tabPanel("Auditoria: Puntos Correctos",
             div(style="padding: 30px; background-color: #f9f9f9; height: 95vh; overflow-y: auto;",
                 h3("Registro de Puntos Validos (Filtrados)"),
                 p("Esta tabla contiene unicamente los datos que superaron la limpieza del Filtro Biomecanico."),
                 dataTableOutput("tabla_correctos")
             )
    ),
    
    tabPanel("Auditoria: Ruido y Errores",
             div(style="padding: 30px; background-color: #ffebee; height: 95vh; overflow-y: auto;",
                 h3("Registro de Puntos Eliminados"),
                 p("Lista detallada del ruido GPS eliminado, con la evidencia matematica de su rechazo."),
                 dataTableOutput("tabla_errores")
             )
    )
  )
)

#######################################################---
# 4. SERVIDOR (LÓGICA EN TIEMPO REAL Y REACTIVIDAD)----
#######################################################---
server <- function(input, output, session) {
  
  paleta_temporal <- colorNumeric(palette = "viridis", domain = c(0, 1))
  
  # RENDERIZADO INICIAL DEL MAPA Y CAPAS ESTÁTICAS
  output$mapa_interactivo <- renderLeaflet({
    m <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron)
    
    if (!is.null(pol_sf)) {
      m <- m %>% addPolygons(data = pol_sf, color = "#2c3e50", weight = 2, fillColor = "#bdc3c7", fillOpacity = 0.25, group = "Manzanas")
    }
    
    # Capa Forense (Oculta por defecto)
    m <- m %>% 
      addPolylines(data = linea_sucia, color = "red", weight = 1, dashArray = "4,4", opacity = 0.4, group = "Puntos Crudos (Con Ruido)") %>%
      addCircleMarkers(
        data = df_raw, lng = ~Longitude, lat = ~Latitude,
        radius = ~radio_raw, fillColor = ~color_raw, color = "black", weight = 1, opacity = 1, fillOpacity = 0.8,
        popup = ~popup_crudo, group = "Puntos Crudos (Con Ruido)"
      )
    
    leyenda_html <- "<div style='padding: 8px 10px; background: white; border-radius: 5px; box-shadow: 0 0 10px rgba(0,0,0,0.2); font-family: Arial, sans-serif;'><strong style='color: #555; font-size: 14px;'>Progresion temporal</strong><br/><div style='background: linear-gradient(to right, #440154, #21918c, #fde725); width: 220px; height: 12px; margin-top: 8px; border-radius: 2px;'></div><div style='display: flex; justify-content: space-between; font-size: 12px; margin-top: 4px; color: #666;'><span>Inicio</span><span>Medio</span><span>Fin</span></div></div>"
    
    m <- m %>% addControl(html = leyenda_html, position = "bottomright") %>%
      addLayersControl(
        overlayGroups = c("Manzanas", "Ruta Trazada", "Puntos y Agrupaciones", "Puntos Crudos (Con Ruido)"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup("Puntos Crudos (Con Ruido)")
    m
  })
  
  # OBSERVADOR REACTIVO: Se activa con cambios en los sliders
  observe({
    df_cluster <- agrupar_puntos_secuencial(df_mapa, input$slider_radio, input$slider_puntos)
    req(df_cluster)
    df_filtrado <- df_cluster
    
    # Preparación de popups limpios (sin íconos)
    df_unicos <- df_filtrado %>% filter(Puntos_Agrupados == 1) %>% 
      mutate(popup = paste0("<div style='text-align:center;'><b>Punto: ", IDs_Unidos, "</b><hr>Hora: ", format(Time_Min, "%H:%M:%S"), "<br>Velocidad: ", round(Vel_Ant_MS, 1), " m/s<br>Dist. al sgte: ", ifelse(is.na(Dist_Next), "Fin de ruta", paste0(round(Dist_Next, 1), " m")), "</div>"))
    
    df_grupos <- df_filtrado %>% filter(Puntos_Agrupados > 1) %>% 
      mutate(radio_calculado = Puntos_Agrupados * 2, popup = paste0("<div style='text-align:center; background-color:#fff3e0; padding:5px; border-radius:5px;'><b>Fusion: ", Secuencia_Agrupada, "</b><hr>Puntos unidos: ", IDs_Unidos, "<br>Distancia agrupada: ", round(Distancia_Envuelta, 1), " m<br>Llegada: ", format(Time_Min, "%H:%M:%S"), "<br>Salida: ", format(Time_Max, "%H:%M:%S"), "</div>"))
    
    # Limpieza visual previa al redibujado
    proxy <- leafletProxy("mapa_interactivo") %>% clearGroup("Ruta Trazada") %>% clearGroup("Puntos y Agrupaciones")
    
    # Renderizado Dinámico
    if(nrow(df_cluster) > 1){
      for (i in 1:(nrow(df_cluster) - 1)) {
        proxy <- proxy %>% addPolylines(lng = c(df_cluster$Longitude[i], df_cluster$Longitude[i + 1]), lat = c(df_cluster$Latitude[i], df_cluster$Latitude[i + 1]), color = paleta_temporal(df_cluster$Progreso_Tiempo[i]), weight = 3, opacity = 0.8, group = "Ruta Trazada")
      }
    }
    
    if(nrow(df_unicos) > 0) proxy <- proxy %>% addCircleMarkers(data = df_unicos, lng = ~Longitude, lat = ~Latitude, radius = 4, fillColor = ~paleta_temporal(Progreso_Tiempo), color = "black", weight = 1, opacity = 1, fillOpacity = 0.9, popup = ~popup, group = "Puntos y Agrupaciones")
    if(nrow(df_grupos) > 0) proxy <- proxy %>% addCircles(data = df_grupos, lng = ~Longitude, lat = ~Latitude, radius = ~radio_calculado, fillColor = "#FF9800", color = "#E65100", weight = 2, opacity = 1, fillOpacity = 0.85, popup = ~popup, group = "Puntos y Agrupaciones")
  })
  
  # RENDERIZADO DE TABLAS FORENSES (Configuradas con formateo de hora purgado)
  output$tabla_correctos <- renderDataTable({
    df_analisis %>%
      filter(ESTADO == "OK") %>%
      mutate(
        Time = format(Time, "%H:%M:%S"), # Extrae sólo la hora
        velocidad_ms = round(velocidad_ms, 2)
      ) %>%
      select(ID_Original, Time, Longitude, Latitude, velocidad_ms, DETALLE) %>%
      rename(`ID Punto` = ID_Original, `Hora` = Time, `Longitud` = Longitude, `Latitud` = Latitude, `Velocidad (m/s)` = velocidad_ms, `Observacion` = DETALLE)
  }, options = list(pageLength = 15, language = list(url = '//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json')))
  
  output$tabla_errores <- renderDataTable({
    df_analisis %>%
      filter(ESTADO != "OK") %>%
      mutate(
        Time = format(Time, "%H:%M:%S"), # Extrae sólo la hora
        dist_m = round(dist_m, 2), 
        velocidad_ms = round(velocidad_ms, 2)
      ) %>%
      select(ID_Original, Time, dist_m, velocidad_ms, ESTADO, DETALLE) %>%
      rename(`ID Punto` = ID_Original, `Hora Error` = Time, `Salto (Metros)` = dist_m, `Velocidad Irreal (m/s)` = velocidad_ms, `Diagnostico` = ESTADO, `Causa Matematica` = DETALLE)
  }, options = list(pageLength = 15, language = list(url = '//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json')))
  
}

shinyApp(ui, server)