#######################################################---
# SHINY APP: DASHBOARD ANALITICO ESPACIAL
# Analisis de Cluster 30/31 (2024) - App vs GPS Masivo
#######################################################---
rm(list = ls())
options(warn = -1)

# 1. CARGA DE LIBRERIAS
paquetes <- c("shiny", "leaflet", "dplyr", "readr", "xml2", "DT", "sf", "ggplot2", "geosphere", "plotly")
instalados <- paquetes %in% installed.packages()
if(any(!instalados)) install.packages(paquetes[!instalados])

library(shiny)
library(leaflet)   
library(dplyr)     
library(readr)     
library(xml2)      
library(DT) 
library(sf)        
library(ggplot2)   
library(geosphere) 
library(plotly)

# Desactivar motor espacial estricto para evitar colapsos geográficos
sf_use_s2(FALSE) 

cat("Iniciando Dashboard Analitico...\n")

# =======================================================
# A. FUNCIONES BASE (Procesamiento y Matematicas)
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
      dist_al_centro <- calcular_distancia(centro_lon, centro_lat, df_agrupado$long[i], df_agrupado$lat[i])
      
      if (dist_al_centro <= radio_metros && puntos_en_cluster < max_puntos) {
        df_agrupado$Cluster_ID[i] <- cluster_actual
        puntos_en_cluster <- puntos_en_cluster + 1
      } else {
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
      Total_Perros_Habitantes = sum(n_dog_house, na.rm = TRUE),
      Vacunados_Fijo = sum(v_2024, na.rm = TRUE),
      Vacunados_Barrido = sum(v_sweep, na.rm = TRUE),
      Registros_Unidos = paste(orden_vacunacion, collapse=", "),
      Promedio_Ajuste_m = mean(dist_ajuste_m, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(Hora_Inicio) %>% 
    mutate(orden_vacunacion = row_number())
  return(df_final)
}

# =======================================================
# B. RUTAS LOCALES EXACTAS
# =======================================================
ruta_base <- "D:/github_UPCH/R/R/data_vacunacion/2025/cluster_30_31"
ruta_app_csv <- file.path(ruta_base, "puntos_usuarios/Auditoria_C31_2024.csv")
ruta_gps_dir <- file.path(ruta_base, "gps")
ruta_poligonos_dir <- file.path(ruta_base, "poligonos") 

# =======================================================
# C. ETL 1: CARGA DE RUTAS GPS MASIVAS
# =======================================================
cat(">>> 1. Procesando Masivamente archivos GPS...\n")
archivos_gps <- list.files(ruta_gps_dir, pattern = "\\.kml$", full.names = TRUE, ignore.case = TRUE)

if(length(archivos_gps) == 0) stop(paste("Error: No se encontraron archivos KML en", ruta_gps_dir))

lista_puntos_gps <- list()
track_id_counter <- 1

for (archivo in archivos_gps) {
  kml_doc <- try(read_xml(archivo), silent = TRUE)
  if (inherits(kml_doc, "try-error")) next
  
  ns <- xml_ns(kml_doc)
  tramos <- xml_find_all(kml_doc, ".//gx:Track", ns)
  
  for (i in seq_along(tramos)) {
    tramo_actual <- tramos[[i]]
    tiempos_raw <- xml_text(xml_find_all(tramo_actual, "./kml:when", ns))
    coords_raw <- xml_text(xml_find_all(tramo_actual, "./gx:coord", ns))
    if (length(tiempos_raw) == length(coords_raw) && length(coords_raw) > 0) {
      mat_coord <- do.call(rbind, strsplit(coords_raw, " "))
      lista_puntos_gps[[length(lista_puntos_gps) + 1]] <- data.frame(
        TRACK_ID = track_id_counter,
        TIME = tiempos_raw, 
        LONG = as.numeric(mat_coord[,1]), 
        LAT = as.numeric(mat_coord[,2]), 
        stringsAsFactors = FALSE
      )
      track_id_counter <- track_id_counter + 1
    }
  }
}

df_gps_raw <- bind_rows(lista_puntos_gps) %>%
  mutate(TIME_FORMAT = as.POSIXct(TIME, tryFormats = c("%Y-%m-%dT%H:%M:%OSZ", "%Y-%m-%dT%H:%M:%SZ"), tz="UTC")) %>%
  filter(!is.na(LONG) & !is.na(LAT) & !is.na(TIME_FORMAT)) %>% 
  arrange(TRACK_ID, TIME_FORMAT)

attr(df_gps_raw$TIME_FORMAT, "tzone") <- "America/Lima"
total_gps_crudo <- nrow(df_gps_raw)

# Filtro de velocidad (> 3 m/s) para los GPS 
UMBRAL_MS <- 3.0; TIEMPO_GRACIA_SEC <- 3600
v_time <- as.numeric(df_gps_raw$TIME_FORMAT); v_lon <- df_gps_raw$LONG; v_lat <- df_gps_raw$LAT
v_estado <- rep("OK", nrow(df_gps_raw)); v_vel <- rep(0, nrow(df_gps_raw)); v_dist <- rep(0, nrow(df_gps_raw))
last_valid_idx <- 1

if(nrow(df_gps_raw) > 1) {
  for (i in 2:nrow(df_gps_raw)) {
    if (df_gps_raw$TRACK_ID[i] != df_gps_raw$TRACK_ID[last_valid_idx]) {
      last_valid_idx <- i
      next
    }
    delta_t <- v_time[i] - v_time[last_valid_idx]
    if (delta_t <= 0) { v_estado[i] <- "DUPLICADO"; next }
    if (delta_t > TIEMPO_GRACIA_SEC) { v_estado[i] <- "OK"; last_valid_idx <- i; next }
    
    dist_m <- calcular_distancia(v_lon[last_valid_idx], v_lat[last_valid_idx], v_lon[i], v_lat[i])
    vel_ms <- dist_m / delta_t
    v_vel[i] <- vel_ms; v_dist[i] <- dist_m
    
    if (vel_ms > UMBRAL_MS) { v_estado[i] <- "RUIDO" } else { last_valid_idx <- i }
  }
}
df_gps_raw$velocidad_ms <- v_vel; df_gps_raw$distancia_m <- v_dist; df_gps_raw$ESTADO <- v_estado

df_gps_validos <- df_gps_raw %>% filter(ESTADO == "OK") %>% mutate(orden_rutina = row_number())
df_gps_ruido <- df_gps_raw %>% filter(ESTADO == "RUIDO")
vel_promedio_trabajador <- mean(df_gps_validos$velocidad_ms[df_gps_validos$velocidad_ms > 0], na.rm = TRUE)

# =======================================================
# D. ETL 2: CARGA DE APLICATIVO (CSV Ya filtrado)
# =======================================================
cat(">>> 2. Procesando App desde CSV...\n")
if(!file.exists(ruta_app_csv)) stop(paste("Falta el archivo CSV en:", ruta_app_csv))

df_app_vp21 <- read.csv(ruta_app_csv, stringsAsFactors = FALSE) %>%
  filter(!is.na(long) & !is.na(lat) & !is.na(date_clean)) %>%
  mutate(
    date_clean = as.POSIXct(date_clean, tz="UTC"),
    n_dog_house = as.numeric(number_dog_house),
    v_2024 = as.numeric(number_dog_vaccinated_2024),
    v_sweep = as.numeric(number_dog_vaccinated_sweep)
  ) %>%
  arrange(date_clean) %>%
  mutate(
    orden_vacunacion = row_number(), 
    hora_dia = format(date_clean, "%H:00", tz="America/Lima")
  )

attr(df_app_vp21$date_clean, "tzone") <- "America/Lima"

# Calcular velocidad para la App
df_app_vp21 <- df_app_vp21 %>%
  mutate(
    lon_ant = lag(long), 
    lat_ant = lag(lat), 
    tiempo_ant = lag(date_clean),
    distancia_m_app = calcular_distancia(lon_ant, lat_ant, long, lat),
    delta_t_app = as.numeric(difftime(date_clean, tiempo_ant, units="secs")),
    velocidad_ms = ifelse(!is.na(delta_t_app) & delta_t_app > 0, distancia_m_app / delta_t_app, 0)
  )

# Metricas App
total_app_crudo <- nrow(df_app_vp21)
total_fijo <- sum(df_app_vp21$v_2024, na.rm = TRUE)
total_barrido <- sum(df_app_vp21$v_sweep, na.rm = TRUE)
total_perros_vacunados <- total_fijo + total_barrido
total_perros_casa <- sum(df_app_vp21$n_dog_house, na.rm = TRUE)
total_casas_utiles <- sum(df_app_vp21$type_house_std %in% c("P", "DOG_ANOTHER_AREA") & (df_app_vp21$raise_dog_house == "SI" | df_app_vp21$raise_dog_house == "SÍ"), na.rm = TRUE)
promedio_perros <- ifelse(total_casas_utiles > 0, total_perros_vacunados / total_casas_utiles, 0)
tiempo_app_horas <- as.numeric(difftime(max(df_app_vp21$date_clean), min(df_app_vp21$date_clean), units="hours"))

# Graficos
df_grafico_fijo <- df_app_vp21 %>% group_by(hora_dia) %>% summarise(Total = sum(v_2024, na.rm = TRUE)) %>% mutate(Tipo = "Punto Fijo")
df_grafico_barrido <- df_app_vp21 %>% group_by(hora_dia) %>% summarise(Total = sum(v_sweep, na.rm = TRUE)) %>% mutate(Tipo = "Barrido")
df_grafico_gps <- df_gps_validos %>% group_by(hora_dia = format(TIME_FORMAT, "%H:00", tz="America/Lima")) %>% summarise(Total = n()) %>% mutate(Tipo = "Puntos GPS")
df_grafico <- bind_rows(df_grafico_fijo, df_grafico_barrido, df_grafico_gps)

# =======================================================
# E. ETL 3: CATASTRO, SNAPPING Y AREAS
# =======================================================
cat(">>> 3. Cargando Catastro y Ajustando...\n")
archivos_poligonos <- list.files(ruta_poligonos_dir, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)

lista_poligonos <- list()
for(archivo_pol in archivos_poligonos) {
  pol_temp <- procesar_manzanas(archivo_pol)
  if(!is.null(pol_temp)) lista_poligonos[[length(lista_poligonos) + 1]] <- pol_temp
}

pol_sf <- if(length(lista_poligonos) > 0) bind_rows(lista_poligonos) else NULL

lon_rango <- range(c(df_app_vp21$long, df_gps_validos$LONG), na.rm = TRUE)
lat_rango <- range(c(df_app_vp21$lat, df_gps_validos$LAT), na.rm = TRUE)
bbox_valido <- all(is.finite(lon_rango)) && all(is.finite(lat_rango))

if(!is.null(pol_sf) && bbox_valido) {
  bbox_filtro <- st_bbox(c(xmin = lon_rango[1]-0.005, ymin = lat_rango[1]-0.005, xmax = lon_rango[2]+0.005, ymax = lat_rango[2]+0.005), crs = 4326)
  pol_sf <- st_intersection(pol_sf, st_as_sfc(bbox_filtro))
}

# Aplicar Snapping
df_app_vp21 <- sacar_a_la_calle(df_app_vp21, "long", "lat", pol_sf)
df_gps_validos <- sacar_a_la_calle(df_gps_validos, "LONG", "LAT", pol_sf)

# Areas Punto a Punto
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

# Areas Manzanas (OPTIMIZADO: Evita colapso inflando los poligonos levemente para st_intersects)
area_manzanas_gps_m2 <- 0; tocadas_gps_sf <- NULL
if(!is.null(pol_sf) && nrow(df_gps_validos) > 0) {
  pts_gps_snap <- st_as_sf(df_gps_validos, coords = c("LONG", "LAT"), crs = 4326)
  pol_inflado <- st_buffer(pol_sf, dist = 0.0001) # Inflado seguro en grados
  idx_gps <- st_intersects(pol_inflado, pts_gps_snap)
  tocadas_gps_sf <- pol_sf[lengths(idx_gps) > 0, ]
  area_manzanas_gps_m2 <- sum(as.numeric(st_area(tocadas_gps_sf)))
}

area_manzanas_app_m2 <- 0; tocadas_app_sf <- NULL
if(!is.null(pol_sf) && nrow(df_app_vp21) > 0) {
  pts_app_snap <- st_as_sf(df_app_vp21, coords = c("long", "lat"), crs = 4326)
  pol_inflado <- st_buffer(pol_sf, dist = 0.0001) # Inflado seguro en grados
  idx_app <- st_intersects(pol_inflado, pts_app_snap)
  tocadas_app_sf <- pol_sf[lengths(idx_app) > 0, ]
  area_manzanas_app_m2 <- sum(as.numeric(st_area(tocadas_app_sf)))
}

# =======================================================
# F. INTERFAZ DE USUARIO Y SERVIDOR (SHINY)
# =======================================================
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%;margin:0;padding:0;}"),
  
  navbarPage("Auditoria de Campo", id = "main_nav",
             
             tabPanel("Auditoria Paso a Paso",
                      leafletOutput("mapa_interactivo", width = "100%", height = "92vh"),
                      
                      absolutePanel(
                        top = 60, left = 20, draggable = TRUE, width = 360,
                        wellPanel(
                          h4("Metricas Iniciales"),
                          p(HTML(paste("<b>GPS Total:</b>", total_gps_crudo, "| <span style='color:red;'>Eliminados:</span>", nrow(df_gps_ruido)))),
                          p(HTML(paste("<b>App Total:</b>", total_app_crudo))),
                          hr(),
                          
                          h5("Paso 1: Catastro"),
                          checkboxInput("ver_manzanas", "1. Mostrar Base de Manzanas", value = TRUE),
                          hr(),
                          
                          h5("Paso 2: Analisis GPS"),
                          checkboxInput("ver_gps_orig", "2.a Mostrar GPS Original", value = FALSE),
                          checkboxInput("ver_gps_snap", "2.b Mostrar GPS Ajustado", value = TRUE),
                          checkboxInput("ver_gps_bad", "2.c Mostrar Ruido Eliminado", value = FALSE),
                          hr(),
                          
                          h5("Paso 3: Analisis Aplicativo"),
                          checkboxInput("ver_app_orig", "3.a Mostrar App Original", value = FALSE),
                          checkboxInput("ver_app_snap", "3.b Mostrar App Ajustada", value = TRUE),
                          
                          conditionalPanel(
                            condition = "input.ver_app_snap == true",
                            div(style="background-color:#f5f5f5; padding:10px; border-radius:5px;",
                                h6("Agrupamiento Espacial"),
                                sliderInput("radio_agrupar", "Distancia Maxima (m):", min = 0, max = 50, value = 0, step = 5),
                                sliderInput("puntos_agrupar", "Puntos Maximos:", min = 2, max = 15, value = 5, step = 1)
                            )
                          ),
                          hr(),
                          
                          h5("Paso 4: Areas Calculadas"),
                          checkboxInput("ver_area_gps", "Area de Ruta GPS (Poligono)", value = FALSE),
                          checkboxInput("ver_area_gps_mz", "Area de Ruta GPS (Manzanas)", value = FALSE),
                          checkboxInput("ver_area_app", "Area Encuestas App (Poligono)", value = FALSE),
                          checkboxInput("ver_area_app_mz", "Area Encuestas App (Manzanas)", value = FALSE)
                        )
                      )
             ),
             
             tabPanel("Tabla de Ruido",
                      div(style="padding: 20px;",
                          h3("Registro de Puntos Eliminados"),
                          DTOutput("tabla_auditoria")
                      )
             ),
             
             tabPanel("Dashboard Gerencial",
                      div(style="padding: 20px;",
                          h3("Desempeno y Metricas"),
                          fluidRow(
                            column(4,
                                   wellPanel(
                                     h4("Desempeno de Campo"),
                                     p(HTML(paste("<b>Total de Casas (Toda la ruta):</b>", total_app_crudo))),
                                     p(HTML(paste("<b>Total Perros:</b>", total_perros_casa))),
                                     p(HTML(paste("<b>Total Perros Vacunados 2024:</b>", total_fijo))),
                                     p(HTML(paste("<b>Total Perros Vacunados en Barrido:</b>", total_barrido))),
                                     p(HTML(paste("<b>Velocidad que tenian los GPS:</b>", round(vel_promedio_trabajador, 2), "m/s")))
                                   ),
                                   wellPanel(
                                     h4("Analisis Espacial"),
                                     p(HTML(paste("<b>Area GPS (Punto a Punto):</b>", round(area_gps_exacta_m2, 2), "m2"))),
                                     p(HTML(paste("<b>Area App (Punto a Punto):</b>", round(area_app_exacta_m2, 2), "m2"))),
                                     p(HTML(paste("<b>Area GPS (Por Manzanas):</b>", round(area_manzanas_gps_m2, 2), "m2"))),
                                     p(HTML(paste("<b>Area App (Por Manzanas):</b>", round(area_manzanas_app_m2, 2), "m2")))
                                   )
                            ),
                            column(8,
                                   wellPanel(
                                     h4("Picos de Produccion"),
                                     plotlyOutput("grafico_horarios", height = "400px")
                                   )
                            )
                          )
                      )
             )
  )
)

server <- function(input, output, session) {
  
  if (nrow(df_gps_validos) > 0) {
    paleta_gradiente_gps <- colorFactor(palette = "Set1", domain = df_gps_validos$TRACK_ID)
  }
  
  output$grafico_horarios <- renderPlotly({
    if(nrow(df_grafico) > 0){
      p <- ggplot(df_grafico, aes(x = hora_dia, y = Total, fill = Tipo, text = paste("Hora:", hora_dia, "<br>Categoria:", Tipo, "<br>Cantidad:", Total))) +
        geom_col(position = "dodge", color = "black", alpha = 0.8) +
        scale_fill_manual(values = c("Punto Fijo" = "#1f77b4", "Barrido" = "#e68102", "Puntos GPS" = "#751dc3")) +
        theme_minimal() +
        labs(x = "Hora", y = "Cantidad") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "top", legend.title = element_blank())
      
      ggplotly(p, tooltip = "text") %>% layout(hovermode = "x unified")
    }
  })
  
  output$tabla_auditoria <- renderDT({
    datatable(df_gps_ruido %>% 
                mutate(Hora_Local = format(TIME_FORMAT, "%Y-%m-%d %H:%M:%S", tz="America/Lima"),
                       Vel = round(velocidad_ms, 2), Salto_m = round(distancia_m, 2),
                       Motivo = paste0("> 3 m/s (", Vel, " m/s)")) %>%
                select(Hora_Local, Salto_m, Vel, Motivo),
              options = list(pageLength = 15, dom = 'ftp'), rownames = FALSE,
              colnames = c("Hora Real (Peru)", "Salto (m)", "Velocidad (m/s)", "Razon"))
  })
  
  datos_app_agrupados <- reactive({
    if (input$radio_agrupar > 0) {
      agrupar_puntos_secuencial(df_app_vp21, input$radio_agrupar, input$puntos_agrupar)
    } else {
      df_app_vp21
    }
  })
  
  output$mapa_interactivo <- renderLeaflet({
    m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron)
    
    if(bbox_valido) {
      m <- m %>% fitBounds(lon_rango[1], lat_rango[1], lon_rango[2], lat_rango[2])
    } else {
      m <- m %>% setView(lng = -71.53, lat = -16.40, zoom = 14)
    }
    
    if(!is.null(pol_sf)) {
      m <- m %>% addPolygons(data = pol_sf, fillColor = "#888888", fillOpacity = 0.15, color = "#444444", weight = 1.2, group = "C_Manzanas")
    }
    
    if (nrow(df_gps_validos) > 1) {
      m <- m %>% addCircleMarkers(
        data = df_gps_validos, lng = ~lon_orig, lat = ~lat_orig,
        radius = 3, fillColor = "#a3a3a3", color = "black", weight = 0.5, fillOpacity = 0.5, group = "C_GPS_Orig",
        popup = "Punto GPS crudo"
      )
      
      for(tid in unique(df_gps_validos$TRACK_ID)) {
        t_data <- df_gps_validos %>% filter(TRACK_ID == tid)
        if(nrow(t_data) > 1) {
          m <- m %>% addPolylines(
            data = t_data, lng = ~LONG, lat = ~LAT,
            color = ~paleta_gradiente_gps(TRACK_ID), weight = 3, opacity = 0.8, group = "C_GPS_Snap"
          )
        }
      }
      
      for(i in 1:nrow(df_gps_validos)) {
        if(df_gps_validos$dist_ajuste_m[i] > 0) {
          m <- m %>% addPolylines(lng = c(df_gps_validos$lon_orig[i], df_gps_validos$LONG[i]), lat = c(df_gps_validos$lat_orig[i], df_gps_validos$LAT[i]),
                                  color = "gray", weight = 1, dashArray = "3,3", group = "C_GPS_Snap")
        }
      }
      
      m <- m %>% addCircleMarkers(
        data = df_gps_validos, lng = ~LONG, lat = ~LAT,
        radius = 3.5, fillColor = ~paleta_gradiente_gps(TRACK_ID), color = "black", weight = 0.5, fillOpacity = 1, group = "C_GPS_Snap", 
        popup = ~paste("<b>Ruta Grupo N°:</b>", TRACK_ID, 
                       "<br><b>Hora:</b>", format(TIME_FORMAT, "%H:%M:%S", tz="America/Lima"), 
                       "<br><b>Velocidad:</b>", round(velocidad_ms, 2), "m/s")
      ) %>% addLegend(position = "bottomright", pal = paleta_gradiente_gps, values = df_gps_validos$TRACK_ID, title = "Rutas de GPS", opacity = 1)
    }
    
    if (nrow(df_gps_ruido) > 0) {
      m <- m %>% addCircleMarkers(
        data = df_gps_ruido, lng = ~LONG, lat = ~LAT,
        radius = 4, fillColor = "#d32f2f", color = "black", weight = 1, fillOpacity = 0.8, group = "C_GPS_Bad",
        popup = ~paste("RUIDO<br>Vel:", round(velocidad_ms, 2), "m/s")
      )
    }
    
    if (nrow(df_app_vp21) > 0) {
      m <- m %>% addCircleMarkers(
        data = df_app_vp21, lng = ~lon_orig, lat = ~lat_orig,
        radius = 4, fillColor = "#e6ce8a", color = "black", weight = 0.5, fillOpacity = 0.5, group = "C_App_Orig",
        popup = "Registro Original"
      )
    }
    
    if (!is.null(coords_gps_closed)) m <- m %>% addPolygons(lng = coords_gps_closed[,1], lat = coords_gps_closed[,2], fillColor = "#751dc3", fillOpacity = 0.2, color = "#751dc3", weight = 2, dashArray = "4,4", group = "C_Area_GPS")
    if (!is.null(tocadas_gps_sf) && nrow(tocadas_gps_sf) > 0) m <- m %>% addPolygons(data = tocadas_gps_sf, fillColor = "#751dc3", fillOpacity = 0.3, color = "#444444", weight = 1.5, group = "C_Area_GPS_Mz")
    
    if (!is.null(coords_app_closed)) m <- m %>% addPolygons(lng = coords_app_closed[,1], lat = coords_app_closed[,2], fillColor = "#e68102", fillOpacity = 0.2, color = "#e68102", weight = 2, dashArray = "4,4", group = "C_Area_App")
    if (!is.null(tocadas_app_sf) && nrow(tocadas_app_sf) > 0) m <- m %>% addPolygons(data = tocadas_app_sf, fillColor = "#e68102", fillOpacity = 0.3, color = "#444444", weight = 1.5, group = "C_Area_App_Mz")
    
    m
  })
  
  observe({
    proxy <- leafletProxy("mapa_interactivo")
    if(input$ver_manzanas) proxy %>% showGroup("C_Manzanas") else proxy %>% hideGroup("C_Manzanas")
    if(input$ver_gps_orig) proxy %>% showGroup("C_GPS_Orig") else proxy %>% hideGroup("C_GPS_Orig")
    if(input$ver_gps_snap) proxy %>% showGroup("C_GPS_Snap") else proxy %>% hideGroup("C_GPS_Snap")
    if(input$ver_gps_bad) proxy %>% showGroup("C_GPS_Bad") else proxy %>% hideGroup("C_GPS_Bad")
    if(input$ver_app_orig) proxy %>% showGroup("C_App_Orig") else proxy %>% hideGroup("C_App_Orig")
    if(input$ver_area_gps) proxy %>% showGroup("C_Area_GPS") else proxy %>% hideGroup("C_Area_GPS")
    if(input$ver_area_gps_mz) proxy %>% showGroup("C_Area_GPS_Mz") else proxy %>% hideGroup("C_Area_GPS_Mz")
    if(input$ver_area_app) proxy %>% showGroup("C_Area_App") else proxy %>% hideGroup("C_Area_App")
    if(input$ver_area_app_mz) proxy %>% showGroup("C_Area_App_Mz") else proxy %>% hideGroup("C_Area_App_Mz")
  })
  
  observe({
    proxy <- leafletProxy("mapa_interactivo") %>% clearGroup("C_App_Snap")
    df_dinamico <- datos_app_agrupados()
    
    if (input$ver_app_snap && nrow(df_dinamico) > 0) {
      if (input$radio_agrupar == 0) {
        for(i in 1:nrow(df_dinamico)) {
          if(df_dinamico$dist_ajuste_m[i] > 0) {
            proxy %>% addPolylines(lng = c(df_dinamico$lon_orig[i], df_dinamico$long[i]), lat = c(df_dinamico$lat_orig[i], df_dinamico$lat[i]),
                                   color = "gray", weight = 1, dashArray = "3,3", group = "C_App_Snap")
          }
        }
        proxy %>% addCircleMarkers(
          data = df_dinamico, lng = ~long, lat = ~lat,
          radius = 5, fillColor = "#e68102", color = "white", weight = 1, fillOpacity = 1, group = "C_App_Snap", 
          popup = ~paste("<b>👤 Usuario:</b>", user_std,
                         "<br><b>🏠 Tipo:</b>", type_house_std,
                         "<br><b>🕒 Hora:</b>", format(date_clean, "%H:%M:%S", tz="America/Lima"), 
                         "<br><b>🏃 Velocidad Previa:</b>", round(velocidad_ms, 2), "m/s",
                         "<br><b>📍 Ajuste a Calle:</b>", round(dist_ajuste_m, 1), "m")
        )
      } else {
        proxy %>% addCircleMarkers(
          data = df_dinamico, lng = ~long, lat = ~lat,
          radius = ~ifelse(Puntos_Agrupados == 1, 5, 6 + Puntos_Agrupados), 
          fillColor = ~ifelse(Puntos_Agrupados == 1, "#e68102", "#b35900"), color = "white", weight = 2, fillOpacity = 0.9, group = "C_App_Snap", 
          popup = ~paste("<b>🎯 Grupo Espacial</b>",
                         "<br><b>Horas:</b>", format(Hora_Inicio, "%H:%M:%S", tz="America/Lima"), "-", format(Hora_Fin, "%H:%M:%S", tz="America/Lima"),
                         "<br><b>Total Perros Habitantes:</b>", Total_Perros_Habitantes, 
                         "<br><b>Vacunados P. Fijo:</b>", Vacunados_Fijo,
                         "<br><b>Vacunados Barrido:</b>", Vacunados_Barrido,
                         "<br><b>IDs de Encuesta:</b>", Registros_Unidos)
        )
      }
    }
  })
}

shinyApp(ui, server)