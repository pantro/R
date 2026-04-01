#######################################################---
# SHINY APP: DASHBOARD FORENSE (VP21)
# Explicación Paso a Paso: Ruido, Snapping y Clustering
#######################################################---
rm(list = ls())
options(warn = -1)

# 1. CARGA DE LIBRERÍAS
paquetes <- c("shiny", "leaflet", "dplyr", "readr", "xml2", "readxl", "DT", "sf", "ggplot2", "geosphere")
instalados <- paquetes %in% installed.packages()
if(any(!instalados)) install.packages(paquetes[!instalados])

library(shiny)
library(leaflet)   
library(dplyr)     
library(readr)     
library(xml2)      
library(readxl)
library(DT) 
library(sf)        
library(ggplot2)   
library(geosphere) 

# CRÍTICO: Apagamos el motor espacial estricto para evitar errores de geometría
sf_use_s2(FALSE) 

cat("==================================================\n")
cat("🎯 INICIANDO DASHBOARD: VP21 (CLUSTER 15)...\n")
cat("==================================================\n")

# =======================================================
# A. FUNCIONES BASE (Procesamiento y Matemáticas)
# =======================================================
calcular_distancia <- function(lon1, lat1, lon2, lat2) {
  rad <- pi / 180
  a1 <- lat1 * rad; a2 <- lat2 * rad
  b1 <- lon1 * rad; b2 <- lon2 * rad
  dlon <- b2 - b1; dlat <- a2 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(a2) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  return(6378137 * c) 
}

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

# SACAR A LA CALLE (SNAPPING) - Modificado para guardar la distancia original
sacar_a_la_calle <- function(df, lon_col, lat_col, poligonos) {
  df$Ubicacion <- "En la calle (Original)" 
  df$lon_orig <- df[[lon_col]]
  df$lat_orig <- df[[lat_col]]
  df$dist_ajuste_m <- 0
  
  if(is.null(poligonos) || nrow(poligonos) == 0 || nrow(df) == 0) return(df)
  
  pts <- st_as_sf(df, coords = c(lon_col, lat_col), crs = 4326)
  intersecciones <- st_intersects(pts, poligonos)
  
  for(i in seq_len(nrow(df))) {
    if(length(intersecciones[[i]]) > 0) {
      block_idx <- intersecciones[[i]][1]
      borde <- st_cast(poligonos[block_idx, ], "MULTILINESTRING")
      linea <- st_nearest_points(pts[i,], borde)
      pt_calle <- st_cast(linea, "POINT")[2] 
      coords <- st_coordinates(pt_calle)
      
      df[i, lon_col] <- coords[1, 1]
      df[i, lat_col] <- coords[1, 2]
      df$Ubicacion[i] <- "Ajustado a vereda"
      df$dist_ajuste_m[i] <- calcular_distancia(df$lon_orig[i], df$lat_orig[i], coords[1,1], coords[1,2])
    }
  }
  return(df)
}

# ALGORITMO: FUSIÓN DE PUNTOS SECUENCIALES (Clustering por 2 variables)
agrupar_puntos_secuencial <- function(df_puntos, radio_metros, max_puntos) {
  if(nrow(df_puntos) == 0) return(NULL)
  df_agrupado <- df_puntos %>% arrange(date_clean) %>% mutate(Cluster_ID = NA_integer_)
  
  cluster_actual <- 1
  puntos_en_cluster <- 0
  centro_lon <- df_agrupado$long[1]
  centro_lat <- df_agrupado$lat[1]
  
  for (i in 1:nrow(df_agrupado)) {
    if (puntos_en_cluster == 0) {
      df_agrupado$Cluster_ID[i] <- cluster_actual
      puntos_en_cluster <- 1
      centro_lon <- df_agrupado$long[i]; centro_lat <- df_agrupado$lat[i]
    } else {
      # Comprobamos distancia respecto al centro del cluster actual
      dist_al_centro <- calcular_distancia(centro_lon, centro_lat, df_agrupado$long[i], df_agrupado$lat[i])
      
      if (dist_al_centro <= radio_metros && puntos_en_cluster < max_puntos) {
        df_agrupado$Cluster_ID[i] <- cluster_actual
        puntos_en_cluster <- puntos_en_cluster + 1
      } else {
        # Cerramos grupo e iniciamos uno nuevo
        cluster_actual <- cluster_actual + 1
        df_agrupado$Cluster_ID[i] <- cluster_actual
        puntos_en_cluster <- 1
        centro_lon <- df_agrupado$long[i]; centro_lat <- df_agrupado$lat[i]
      }
    }
  }
  
  df_final <- df_agrupado %>%
    group_by(Cluster_ID) %>%
    summarise(
      long = mean(long), lat = mean(lat),
      Hora_Inicio = min(date_clean), Hora_Fin = max(date_clean),
      Puntos_Agrupados = n(),
      perros_vacunados = sum(perros_vacunados, na.rm = TRUE),
      Registros_Unidos = paste(orden_vacunacion, collapse=", "),
      .groups = "drop"
    ) %>%
    arrange(Hora_Inicio) %>% 
    mutate(orden_vacunacion = row_number())
  return(df_final)
}

# =======================================================
# B. RUTAS LOCALES
# =======================================================
ruta_base <- "D:/github_UPCH/R/R/data_vacunacion/2025/JLBYR_2025"
ruta_excel_completa <- file.path(ruta_base, "surveys_2025-10-07.xlsx")
ruta_kml_completa <- file.path(ruta_base, "GPS-53 22-06-2024 GRUPO 01.kml") 
ruta_manzanas_csv <- "D:/github_UPCH/R/R/data_vacunacion/2025/Loc_cluster_15_11may2023.csv" 

# =======================================================
# C. ETL 1: APLICATIVO (EXCEL)
# =======================================================
cat(">>> 1. Procesando App...\n")
df_app_raw <- read_excel(ruta_excel_completa)
colnames(df_app_raw) <- tolower(colnames(df_app_raw))
if(!"number_dog_vaccinated" %in% colnames(df_app_raw)) df_app_raw$number_dog_vaccinated <- 1

df_app_vp21 <- df_app_raw %>%
  select(block_id, cluster, user_app, date, lat, long, number_dog_vaccinated) %>%
  mutate(
    lat = as.numeric(gsub(",", ".", as.character(lat))), long = as.numeric(gsub(",", ".", as.character(long))),
    date_clean = as.POSIXct(as.character(date), format="%Y-%m-%dT%H:%M:%OS", tz="America/Lima"),
    cluster_std = toupper(gsub(" ", "", as.character(cluster))), user_std = toupper(trimws(as.character(user_app))),
    perros_vacunados = as.numeric(number_dog_vaccinated),
    perros_vacunados = ifelse(is.na(perros_vacunados), 1, perros_vacunados)
  ) %>%
  filter(cluster_std == "CLUSTER15" & user_std == "VP21" & as.Date(date_clean, tz="America/Lima") == as.Date("2025-06-22")) %>%
  filter(!is.na(long) & !is.na(lat) & long != 0 & lat != 0) %>% 
  arrange(date_clean) %>%
  mutate(orden_vacunacion = row_number(), hora_dia = format(date_clean, "%H:00"))

attr(df_app_vp21$date_clean, "tzone") <- "America/Lima"

# Métricas Básicas App
total_app_crudo <- nrow(df_app_vp21)
tiempo_app_horas <- as.numeric(difftime(max(df_app_vp21$date_clean), min(df_app_vp21$date_clean), units="hours"))
total_perros_vacunados <- sum(df_app_vp21$perros_vacunados, na.rm = TRUE)
df_grafico_app <- df_app_vp21 %>% group_by(hora_dia) %>% summarise(Total_Perros = sum(perros_vacunados, na.rm = TRUE))

# =======================================================
# D. ETL 2: RUTAS GPS (KML)
# =======================================================
cat(">>> 2. Procesando GPS...\n")
kml_doc <- read_xml(ruta_kml_completa); ns <- xml_ns(kml_doc)
tramos <- xml_find_all(kml_doc, ".//gx:Track", ns)

lista_puntos_gps <- list()
for (i in seq_along(tramos)) {
  tramo_actual <- tramos[[i]]; tiempos_raw <- xml_text(xml_find_all(tramo_actual, "./kml:when", ns))
  coords_raw <- xml_text(xml_find_all(tramo_actual, "./gx:coord", ns))
  if (length(tiempos_raw) == length(coords_raw) && length(coords_raw) > 0) {
    mat_coord <- do.call(rbind, strsplit(coords_raw, " "))
    lista_puntos_gps[[i]] <- data.frame(TIME = tiempos_raw, LONG = as.numeric(mat_coord[,1]), LAT = as.numeric(mat_coord[,2]), stringsAsFactors = FALSE)
  }
}

df_gps_raw <- bind_rows(lista_puntos_gps) %>%
  mutate(TIME_FORMAT = as.POSIXct(TIME, tryFormats = c("%Y-%m-%dT%H:%M:%OSZ", "%Y-%m-%dT%H:%M:%SZ"), tz="UTC")) %>%
  filter(!is.na(LONG) & !is.na(LAT) & !is.na(TIME_FORMAT)) %>%
  filter(as.Date(TIME_FORMAT, tz="America/Lima") == as.Date("2025-06-22")) %>% arrange(TIME_FORMAT)

attr(df_gps_raw$TIME_FORMAT, "tzone") <- "America/Lima"
total_gps_crudo <- nrow(df_gps_raw)

# --- MOTOR BIOMECÁNICO (> 3 m/s) ---
UMBRAL_MS <- 3.0; TIEMPO_GRACIA_SEC <- 3600
if (nrow(df_gps_raw) > 0) {
  v_time <- as.numeric(df_gps_raw$TIME_FORMAT); v_lon <- df_gps_raw$LONG; v_lat <- df_gps_raw$LAT
  v_estado <- rep("OK", nrow(df_gps_raw)); v_vel <- rep(0, nrow(df_gps_raw)); v_dist <- rep(0, nrow(df_gps_raw))
  last_valid_idx <- 1
  for (i in 2:nrow(df_gps_raw)) {
    delta_t <- v_time[i] - v_time[last_valid_idx]
    if (delta_t <= 0) { v_estado[i] <- "DUPLICADO"; next }
    if (delta_t > TIEMPO_GRACIA_SEC) { v_estado[i] <- "OK"; last_valid_idx <- i; next }
    dist_m <- calcular_distancia(v_lon[last_valid_idx], v_lat[last_valid_idx], v_lon[i], v_lat[i])
    vel_ms <- dist_m / delta_t
    v_vel[i] <- vel_ms; v_dist[i] <- dist_m
    if (vel_ms > UMBRAL_MS) { v_estado[i] <- "RUIDO" } else { last_valid_idx <- i }
  }
  df_gps_raw$velocidad_ms <- v_vel; df_gps_raw$distancia_m <- v_dist; df_gps_raw$ESTADO <- v_estado
}

df_gps_validos <- df_gps_raw %>% filter(ESTADO == "OK") %>% mutate(orden_rutina = row_number())
df_gps_ruido <- df_gps_raw %>% filter(ESTADO == "RUIDO")
vel_promedio_trabajador <- mean(df_gps_validos$velocidad_ms[df_gps_validos$velocidad_ms > 0], na.rm = TRUE)

# =======================================================
# E. ETL 3: CATASTRO Y SNAPPING
# =======================================================
cat(">>> 3. Cargando Catastro y Ajustando...\n")
pol_sf <- procesar_manzanas(ruta_manzanas_csv)
lon_rango <- range(c(df_app_vp21$long, df_gps_validos$LONG), na.rm = TRUE)
lat_rango <- range(c(df_app_vp21$lat, df_gps_validos$LAT), na.rm = TRUE)

if(!is.null(pol_sf)) {
  bbox_filtro <- st_bbox(c(xmin = lon_rango[1]-0.005, ymin = lat_rango[1]-0.005, xmax = lon_rango[2]+0.005, ymax = lat_rango[2]+0.005), crs = 4326)
  pol_sf <- st_intersection(pol_sf, st_as_sfc(bbox_filtro))
}

# Aplicar Snapping (Guarda orig y nuevo)
df_app_vp21 <- sacar_a_la_calle(df_app_vp21, "long", "lat", pol_sf)
df_gps_validos <- sacar_a_la_calle(df_gps_validos, "LONG", "LAT", pol_sf)

# Recalcular distancias post-snapping
df_app_vp21 <- df_app_vp21 %>% mutate(lon_ant = lag(long), lat_ant = lag(lat), distancia_m = calcular_distancia(lon_ant, lat_ant, long, lat))
dist_total_app_km <- sum(df_app_vp21$distancia_m, na.rm = TRUE) / 1000

df_gps_validos <- df_gps_validos %>% mutate(lon_ant = lag(LONG), lat_ant = lag(LAT), distancia_m = calcular_distancia(lon_ant, lat_ant, LONG, LAT))
dist_total_gps_km <- sum(df_gps_validos$distancia_m, na.rm = TRUE) / 1000

# Calcular Áreas
area_gps_exacta_m2 <- 0; coords_gps_closed <- NULL
if(nrow(df_gps_validos) >= 3) {
  coords_gps_closed <- rbind(as.matrix(df_gps_validos[, c("LONG", "LAT")]), as.matrix(df_gps_validos[1, c("LONG", "LAT")]))
  area_gps_exacta_m2 <- sum(as.numeric(st_area(st_make_valid(st_sfc(st_polygon(list(coords_gps_closed)), crs = 4326)))))
}

area_app_exacta_m2 <- 0; coords_app_closed <- NULL
if(nrow(df_app_vp21) >= 3) {
  coords_app_closed <- rbind(as.matrix(df_app_vp21[, c("long", "lat")]), as.matrix(df_app_vp21[1, c("long", "lat")]))
  area_app_exacta_m2 <- sum(as.numeric(st_area(st_make_valid(st_sfc(st_polygon(list(coords_app_closed)), crs = 4326)))))
}

# =======================================================
# F. SHINY APP (UI & SERVER)
# =======================================================
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%;margin:0;padding:0;}"),
  
  navbarPage("Ruta de VP21 Yanahuara", id = "main_nav",
             
             tabPanel("️ Auditoría Paso a Paso",
                      leafletOutput("mapa_interactivo", width = "100%", height = "92vh"),
                      
                      absolutePanel(
                        top = 60, left = 20, draggable = TRUE, width = 360,
                        wellPanel(
                          h4("Métricas Crudas Iniciales"),
                          p(HTML(paste("<b> GPS Total:</b>", total_gps_crudo, "| <span style='color:red;'>Eliminados:</span>", nrow(df_gps_ruido)))),
                          p(HTML(paste("<b> App Total:</b>", total_app_crudo))),
                          hr(),
                          
                          h5("Paso 1: Catastro"),
                          checkboxInput("ver_manzanas", "1. Mostrar Base de Manzanas", value = TRUE),
                          hr(),
                          
                          h5("Paso 2: Análisis GPS Satelital"),
                          checkboxInput("ver_gps_orig", "2.a Mostrar GPS (Ubicación Original)", value = FALSE),
                          checkboxInput("ver_gps_snap", HTML("<b style='color:#751dc3;'>2.b Mostrar GPS (Ajustado a Calle)</b>"), value = TRUE),
                          checkboxInput("ver_gps_bad", HTML("<b style='color:#d32f2f;'>2.c Mostrar Ruido Matemático Eliminado</b>"), value = FALSE),
                          hr(),
                          
                          h5("Paso 3: Análisis Aplicativo (Encuestas)"),
                          checkboxInput("ver_app_orig", "3.a Mostrar App (Dentro de casas)", value = FALSE),
                          checkboxInput("ver_app_snap", HTML("<b style='color:#e68102;'>3.b Mostrar App (Ajustado a Calle)</b>"), value = TRUE),
                          
                          conditionalPanel(
                            condition = "input.ver_app_snap == true",
                            div(style="background-color:#fff3e0; padding:10px; border-radius:5px;",
                                h6("Filtro de Agrupamiento Bi-Condicional"),
                                sliderInput("radio_agrupar", "Distancia Máx. (metros):", min = 0, max = 50, value = 0, step = 5),
                                sliderInput("puntos_agrupar", "Puntos Consecutivos Máx.:", min = 2, max = 15, value = 5, step = 1)
                            )
                          ),
                          hr(),
                          
                          h5("Paso 4: Áreas Calculadas"),
                          checkboxInput("ver_area_gps", HTML("<b style='color:#751dc3;'>Ver Área de Ruta GPS</b>"), value = FALSE),
                          checkboxInput("ver_area_app", HTML("<b style='color:#e68102;'>Ver Área de Encuestas App</b>"), value = FALSE)
                        )
                      )
             ),
             
             tabPanel(" Tabla de Ruido (Auditoría)",
                      div(style="padding: 20px;",
                          h3("Registro de Puntos Eliminados (Físicamente imposibles)"),
                          DTOutput("tabla_auditoria")
                      )
             ),
             
             tabPanel(" Dashboard Gerencial",
                      div(style="padding: 20px;",
                          h3("Desempeño y Métricas"),
                          fluidRow(
                            column(4,
                                   wellPanel(
                                     h4(" Desempeño"),
                                     p(HTML(paste("<b>Perros Vacunados:</b>", total_perros_vacunados))),
                                     p(HTML(paste("<b>Velocidad Promedio:</b>", round(vel_promedio_trabajador, 2), "m/s"))),
                                     p(HTML(paste("<b>Tiempo Trabajado:</b>", round(tiempo_app_horas, 2), "horas")))
                                   ),
                                   wellPanel(
                                     h4(" Análisis Espacial"),
                                     p(HTML(paste("<b style='color:#751dc3;'>Área GPS (Real):</b>", round(area_gps_exacta_m2, 2), "m²"))),
                                     p(HTML(paste("<b style='color:#e68102;'>Área App (Registro):</b>", round(area_app_exacta_m2, 2), "m²"))),
                                     p(HTML(paste("<b>Densidad Abarcada:</b>", round(ifelse(area_gps_exacta_m2>0, total_perros_vacunados/area_gps_exacta_m2, 0), 6), "perros / m²")))
                                   )
                            ),
                            column(8,
                                   wellPanel(
                                     h4(" Picos de Producción"),
                                     plotOutput("grafico_horarios", height = "350px")
                                   )
                            )
                          )
                      )
             )
  )
)

server <- function(input, output, session) {
  
  if (nrow(df_gps_validos) > 0) {
    paleta_gradiente_gps <- colorNumeric(palette = c("purple", "green"), domain = df_gps_validos$orden_rutina)
  }
  
  # Gráfico
  output$grafico_horarios <- renderPlot({
    if(nrow(df_grafico_app) > 0){
      ggplot(df_grafico_app, aes(x = hora_dia, y = Total_Perros)) +
        geom_col(fill = "#e68102", color = "black", alpha = 0.8) +
        geom_text(aes(label = Total_Perros), vjust = -0.5, fontface = "bold", size = 5) + 
        theme_minimal() +
        labs(x = "Hora del Día", y = "Nº de Perros Vacunados") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
              axis.title = element_text(size = 14, face = "bold")) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) 
    }
  })
  
  output$tabla_auditoria <- renderDT({
    datatable(df_gps_ruido %>% 
                mutate(Vel = round(velocidad_ms, 2), Salto_m = round(distancia_m, 2),
                       Motivo = paste0("Superó el límite de 3 m/s (", Vel, " m/s)")) %>%
                select(TIME_FORMAT, Salto_m, Vel, Motivo),
              options = list(pageLength = 15, dom = 'ftp'), rownames = FALSE,
              colnames = c("Hora de Falla", "Salto (m)", "Velocidad (m/s)", "Razón Forense"))
  })
  
  # REACTIVO: Agrupamiento Condicional (App)
  datos_app_agrupados <- reactive({
    if (input$radio_agrupar > 0) {
      agrupar_puntos_secuencial(df_app_vp21, input$radio_agrupar, input$puntos_agrupar)
    } else {
      df_app_vp21
    }
  })
  
  # MAPA MAESTRO
  output$mapa_interactivo <- renderLeaflet({
    m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(lon_rango[1], lat_rango[1], lon_rango[2], lat_rango[2])
    
    # Capa 1: Manzanas
    if(!is.null(pol_sf)) {
      m <- m %>% addPolygons(data = pol_sf, fillColor = "#888888", fillOpacity = 0.15, color = "#444444", weight = 1.2, group = "C_Manzanas")
    }
    
    # Capa 2a: GPS Original (Dentro de casas)
    if (nrow(df_gps_validos) > 1) {
      m <- m %>% addCircleMarkers(
        data = df_gps_validos, lng = ~lon_orig, lat = ~lat_orig,
        radius = 3, fillColor = "#a3a3a3", color = "black", weight = 0.5, fillOpacity = 0.5, group = "C_GPS_Orig",
        popup = "Punto GPS crudo (Antes de corrección)"
      )
    }
    
    # Capa 2b: GPS Snapped (En Vereda + Líneas de desplazamiento)
    if (nrow(df_gps_validos) > 1) {
      for(i in 1:(nrow(df_gps_validos) - 1)) {
        m <- m %>% addPolylines(
          lng = c(df_gps_validos$LONG[i], df_gps_validos$LONG[i+1]), lat = c(df_gps_validos$LAT[i], df_gps_validos$LAT[i+1]),
          color = paleta_gradiente_gps(df_gps_validos$orden_rutina[i]), weight = 3, opacity = 0.8, group = "C_GPS_Snap"
        )
      }
      # Líneas mostrando cómo se sacó a la calle
      for(i in 1:nrow(df_gps_validos)) {
        if(df_gps_validos$dist_ajuste_m[i] > 0) {
          m <- m %>% addPolylines(lng = c(df_gps_validos$lon_orig[i], df_gps_validos$LONG[i]), lat = c(df_gps_validos$lat_orig[i], df_gps_validos$LAT[i]),
                                  color = "gray", weight = 1, dashArray = "3,3", group = "C_GPS_Snap")
        }
      }
      
      m <- m %>% addCircleMarkers(
        data = df_gps_validos, lng = ~LONG, lat = ~LAT,
        radius = 3.5, fillColor = ~paleta_gradiente_gps(orden_rutina), color = "black", weight = 0.5, fillOpacity = 1, group = "C_GPS_Snap", 
        popup = ~paste("<b>Paso #</b>", orden_rutina, "<br><b>Hora:</b>", format(TIME_FORMAT, "%H:%M:%S"), "<br><b>Desplazado a calle:</b>", round(dist_ajuste_m, 2), "m")
      ) %>% addLegend(position = "bottomright", pal = paleta_gradiente_gps, values = df_gps_validos$orden_rutina, title = "Ruta GPS (M->V)", opacity = 1)
    }
    
    # Capa 2c: Ruido GPS
    if (nrow(df_gps_ruido) > 0) {
      m <- m %>% addCircleMarkers(
        data = df_gps_ruido, lng = ~LONG, lat = ~LAT,
        radius = 4, fillColor = "#d32f2f", color = "black", weight = 1, fillOpacity = 0.8, group = "C_GPS_Bad",
        popup = ~paste("<b> RUIDO</b><br>", round(velocidad_ms, 2), "m/s")
      )
    }
    
    # Capa 3a: App Original
    if (nrow(df_app_vp21) > 0) {
      m <- m %>% addCircleMarkers(
        data = df_app_vp21, lng = ~lon_orig, lat = ~lat_orig,
        radius = 4, fillColor = "#e6ce8a", color = "black", weight = 0.5, fillOpacity = 0.5, group = "C_App_Orig",
        popup = "Registro en Casa (Crudo)"
      )
    }
    
    # Capas 4: Áreas
    if (!is.null(coords_gps_closed)) {
      m <- m %>% addPolygons(lng = coords_gps_closed[,1], lat = coords_gps_closed[,2], fillColor = "#751dc3", fillOpacity = 0.2, color = "#751dc3", weight = 2, dashArray = "4,4", group = "C_Area_GPS")
    }
    if (!is.null(coords_app_closed)) {
      m <- m %>% addPolygons(lng = coords_app_closed[,1], lat = coords_app_closed[,2], fillColor = "#e68102", fillOpacity = 0.2, color = "#e68102", weight = 2, dashArray = "4,4", group = "C_Area_App")
    }
    
    m
  })
  
  # OBSERVADORES REACTIVOS (Mostrar/Ocultar sin pisarse)
  observe({
    proxy <- leafletProxy("mapa_interactivo")
    if(input$ver_manzanas) proxy %>% showGroup("C_Manzanas") else proxy %>% hideGroup("C_Manzanas")
    if(input$ver_gps_orig) proxy %>% showGroup("C_GPS_Orig") else proxy %>% hideGroup("C_GPS_Orig")
    if(input$ver_gps_snap) proxy %>% showGroup("C_GPS_Snap") else proxy %>% hideGroup("C_GPS_Snap")
    if(input$ver_gps_bad) proxy %>% showGroup("C_GPS_Bad") else proxy %>% hideGroup("C_GPS_Bad")
    if(input$ver_app_orig) proxy %>% showGroup("C_App_Orig") else proxy %>% hideGroup("C_App_Orig")
    if(input$ver_area_gps) proxy %>% showGroup("C_Area_GPS") else proxy %>% hideGroup("C_Area_GPS")
    if(input$ver_area_app) proxy %>% showGroup("C_Area_App") else proxy %>% hideGroup("C_Area_App")
  })
  
  # OBSERVADOR DINÁMICO APP (Snapping + Agrupamiento)
  observe({
    proxy <- leafletProxy("mapa_interactivo") %>% clearGroup("C_App_Snap")
    df_dinamico <- datos_app_agrupados()
    
    if (input$ver_app_snap && nrow(df_dinamico) > 0) {
      if (input$radio_agrupar == 0) {
        # Puntos Simples con línea de desplazamiento
        for(i in 1:nrow(df_dinamico)) {
          if(df_dinamico$dist_ajuste_m[i] > 0) {
            proxy %>% addPolylines(lng = c(df_dinamico$lon_orig[i], df_dinamico$long[i]), lat = c(df_dinamico$lat_orig[i], df_dinamico$lat[i]),
                                   color = "gray", weight = 1, dashArray = "3,3", group = "C_App_Snap")
          }
        }
        proxy %>% addCircleMarkers(
          data = df_dinamico, lng = ~long, lat = ~lat,
          radius = 5, fillColor = "#e68102", color = "white", weight = 1, fillOpacity = 1, group = "C_App_Snap", 
          popup = ~paste("<b>💉 Encuesta #</b>", orden_vacunacion, "<br><b>Hora:</b>", format(date_clean, "%H:%M:%S"), "<br><b>Ajuste Vereda:</b>", round(dist_ajuste_m, 1), "m")
        )
      } else {
        # Agrupados (Bolas grandes)
        proxy %>% addCircleMarkers(
          data = df_dinamico, lng = ~long, lat = ~lat,
          radius = ~ifelse(Puntos_Agrupados == 1, 5, 6 + Puntos_Agrupados), 
          fillColor = ~ifelse(Puntos_Agrupados == 1, "#e68102", "#b35900"), color = "white", weight = 2, fillOpacity = 0.9, group = "C_App_Snap", 
          popup = ~paste("<b> Secuencia Agrupada</b><br><b>Rango:</b>", format(Hora_Inicio, "%H:%M:%S"), "-", format(Hora_Fin, "%H:%M:%S"),
                         "<br><b>Perros Totales:</b>", perros_vacunados, "<br><b>IDs Originales:</b>", Registros_Unidos)
        )
      }
    }
  })
}

shinyApp(ui, server)