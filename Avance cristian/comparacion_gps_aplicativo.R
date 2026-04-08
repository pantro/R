#######################################################---
# SHINY APP: DASHBOARD ANALÍTICO ESPACIAL (VP21)
# Recorte de Catastro Inteligente, Áreas Punto a Punto y Gráficos
#######################################################---
rm(list = ls())
options(warn = -1)

# 1. CARGA DE LIBRERÍAS
paquetes <- c("shiny", "leaflet", "dplyr", "readr", "xml2", "readxl", "DT", "sf", "ggplot2")
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

# CRÍTICO: Apagamos el motor espacial para evitar errores de geometría en KMLs pesados
sf_use_s2(FALSE) 

cat("==================================================\n")
cat("🎯 INICIANDO DASHBOARD URBANO: VP21 (CLUSTER 15)...\n")
cat("==================================================\n")

# =======================================================
# A. FUNCIÓN MATEMÁTICA: HAVERSINE Y SNAPPING
# =======================================================
calcular_distancia <- function(lon1, lat1, lon2, lat2) {
  rad <- pi / 180
  a1 <- lat1 * rad; a2 <- lat2 * rad
  b1 <- lon1 * rad; b2 <- lon2 * rad
  dlon <- b2 - b1; dlat <- a2 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(a2) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  return(6378137 * c) # Metros
}

# SACAR A LA CALLE: Previene errores si no hay polígonos
sacar_a_la_calle <- function(df, lon_col, lat_col, poligonos) {
  # Por defecto creamos la columna para evitar colapsos ("Objeto no encontrado")
  df$Ubicacion <- "Sin catastro" 
  
  if(is.null(poligonos) || nrow(poligonos) == 0 || nrow(df) == 0) return(df)
  
  pts <- st_as_sf(df, coords = c(lon_col, lat_col), crs = 4326)
  intersecciones <- st_intersects(pts, poligonos)
  df$Ubicacion <- "En la calle"
  
  for(i in seq_len(nrow(df))) {
    if(length(intersecciones[[i]]) > 0) {
      block_idx <- intersecciones[[i]][1]
      borde <- st_cast(poligonos[block_idx, ], "MULTILINESTRING")
      linea <- st_nearest_points(pts[i,], borde)
      pt_calle <- st_cast(linea, "POINT")[2] 
      coords <- st_coordinates(pt_calle)
      df[i, lon_col] <- coords[1, 1]
      df[i, lat_col] <- coords[1, 2]
      df$Ubicacion[i] <- "Movido a vereda"
    }
  }
  return(df)
}

# =======================================================
# B. RUTAS LOCALES
# =======================================================
ruta_base <- "D:/github_UPCH/R/R/data_vacunacion/2025/JLBYR_2025"
ruta_excel_completa <- file.path(ruta_base, "surveys_2025-10-07.xlsx")
ruta_kml_completa <- file.path(ruta_base, "GPS-53 22-06-2024 GRUPO 01.kml") 
ruta_manzanas_kml <- file.path(ruta_base, "Localidades_Mariscal_Castilla_08may2023", "localidades.kml") 

# =======================================================
# C. ETL 1: APLICATIVO (EXCEL) - FASE DE EXTRACCIÓN
# =======================================================
cat(">>> 1. Procesando App y filtrando NAs...\n")
df_app_raw <- read_excel(ruta_excel_completa)
colnames(df_app_raw) <- tolower(colnames(df_app_raw))
if(!"number_dog_vaccinated" %in% colnames(df_app_raw)) df_app_raw$number_dog_vaccinated <- 1

df_app_vp21 <- df_app_raw %>%
  select(block_id, cluster, user_app, date, lat, long, number_dog_vaccinated) %>%
  mutate(
    lat = as.numeric(gsub(",", ".", as.character(lat))),
    long = as.numeric(gsub(",", ".", as.character(long))),
    date_clean = as.POSIXct(as.character(date), format="%Y-%m-%dT%H:%M:%OS", tz="America/Lima"),
    cluster_std = toupper(gsub(" ", "", as.character(cluster))),
    user_std = toupper(trimws(as.character(user_app))),
    perros_vacunados = as.numeric(number_dog_vaccinated),
    perros_vacunados = ifelse(is.na(perros_vacunados), 1, perros_vacunados)
  ) %>%
  filter(cluster_std == "CLUSTER15" & user_std == "VP21") %>%
  filter(as.Date(date_clean, tz="America/Lima") == as.Date("2025-06-22")) %>%
  filter(!is.na(long) & !is.na(lat) & long != 0 & lat != 0) %>% 
  arrange(date_clean) %>%
  mutate(orden_vacunacion = row_number(), hora_dia = format(date_clean, "%H:00"))

attr(df_app_vp21$date_clean, "tzone") <- "America/Lima"

# =======================================================
# D. ETL 2: RUTAS GPS (KML) - FASE DE EXTRACCIÓN
# =======================================================
cat(">>> 2. Procesando GPS y limpiando ruido...\n")
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
      TIME = tiempos_raw, LONG = as.numeric(mat_coord[,1]), LAT = as.numeric(mat_coord[,2]), stringsAsFactors = FALSE
    )
  }
}

df_gps_raw <- bind_rows(lista_puntos_gps) %>%
  mutate(TIME_FORMAT = as.POSIXct(TIME, tryFormats = c("%Y-%m-%dT%H:%M:%OSZ", "%Y-%m-%dT%H:%M:%SZ"), tz="UTC")) %>%
  filter(!is.na(LONG) & !is.na(LAT) & !is.na(TIME_FORMAT)) %>%
  filter(as.Date(TIME_FORMAT, tz="America/Lima") == as.Date("2025-06-22")) %>%
  arrange(TIME_FORMAT)
attr(df_gps_raw$TIME_FORMAT, "tzone") <- "America/Lima"

# --- MOTOR BIOMECÁNICO (> 3 m/s) ---
UMBRAL_MS <- 3.0 
TIEMPO_GRACIA_SEC <- 3600
if (nrow(df_gps_raw) > 0) {
  v_time <- as.numeric(df_gps_raw$TIME_FORMAT); v_lon <- df_gps_raw$LONG; v_lat <- df_gps_raw$LAT
  v_estado <- rep("OK", nrow(df_gps_raw)); v_vel <- rep(0, nrow(df_gps_raw))
  last_valid_idx <- 1
  for (i in 2:nrow(df_gps_raw)) {
    delta_t <- v_time[i] - v_time[last_valid_idx]
    if (delta_t <= 0) { v_estado[i] <- "DUPLICADO"; next }
    if (delta_t > TIEMPO_GRACIA_SEC) { v_estado[i] <- "OK"; last_valid_idx <- i; next }
    dist_m <- calcular_distancia(v_lon[last_valid_idx], v_lat[last_valid_idx], v_lon[i], v_lat[i])
    vel_ms <- dist_m / delta_t
    v_vel[i] <- vel_ms
    if (vel_ms > UMBRAL_MS) { v_estado[i] <- "RUIDO" } else { last_valid_idx <- i }
  }
  df_gps_raw$velocidad_ms <- v_vel; df_gps_raw$ESTADO <- v_estado
}

df_gps_validos <- df_gps_raw %>% filter(ESTADO == "OK") %>% mutate(orden_rutina = row_number())
df_gps_ruido <- df_gps_raw %>% filter(ESTADO == "RUIDO")

# =======================================================
# E. ETL 3: CATASTRO (RECORTE INTELIGENTE POR BOUNDING BOX)
# =======================================================
cat(">>> 3. Recortando Catastro al tamaño del área de trabajo...\n")
pol_sf <- NULL
lon_rango <- range(c(df_app_vp21$long, df_gps_validos$LONG), na.rm = TRUE)
lat_rango <- range(c(df_app_vp21$lat, df_gps_validos$LAT), na.rm = TRUE)

if(file.exists(ruta_manzanas_kml)) {
  pol_raw <- st_read(ruta_manzanas_kml, quiet = TRUE) %>% st_zm() %>% st_make_valid()
  if(st_crs(pol_raw)$epsg != 4326) pol_raw <- st_transform(pol_raw, 4326)
  
  # MAGIA ESPACIAL: Creamos un marco alrededor de VP21 y borramos el resto del KML
  bbox_filtro <- st_bbox(c(xmin = lon_rango[1]-0.005, ymin = lat_rango[1]-0.005, 
                           xmax = lon_rango[2]+0.005, ymax = lat_rango[2]+0.005), crs = 4326)
  
  pol_sf <- st_intersection(pol_raw, st_as_sfc(bbox_filtro))
} else {
  cat("⚠️ Aviso: No se encontró", ruta_manzanas_kml, "\n")
}

# =======================================================
# F. CÁLCULO DE MÉTRICAS FINALES Y SNAPPING
# =======================================================
cat(">>> 4. Calculando métricas y sacando a la calle...\n")

# --- APP ---
df_app_vp21 <- sacar_a_la_calle(df_app_vp21, "long", "lat", pol_sf)
df_app_vp21 <- df_app_vp21 %>%
  mutate(lon_ant = lag(long), lat_ant = lag(lat), distancia_m = calcular_distancia(lon_ant, lat_ant, long, lat))

dist_total_app_km <- sum(df_app_vp21$distancia_m, na.rm = TRUE) / 1000
tiempo_app_horas <- as.numeric(difftime(max(df_app_vp21$date_clean), min(df_app_vp21$date_clean), units="hours"))
total_perros_vacunados <- sum(df_app_vp21$perros_vacunados, na.rm = TRUE)

# Área Exacta App (Polígono Secuencial Cerrado)
area_app_exacta_m2 <- 0; coords_app_closed <- NULL
if(nrow(df_app_vp21) >= 3) {
  coords_app <- df_app_vp21 %>% select(long, lat) %>% as.matrix()
  coords_app_closed <- rbind(coords_app, coords_app[1, ]) 
  poly_app <- st_polygon(list(coords_app_closed))
  area_app_exacta_m2 <- sum(as.numeric(st_area(st_make_valid(st_sfc(poly_app, crs = 4326)))))
}

# Área Manzanas Tocadas App
area_manzanas_app_m2 <- 0; tocadas_app_sf <- NULL
if(!is.null(pol_sf) && nrow(df_app_vp21) > 0) {
  pts_app_orig <- st_as_sf(df_app_vp21, coords = c("long", "lat"), crs = 4326)
  tocadas_app_sf <- pol_sf[lengths(st_intersects(pol_sf, pts_app_orig)) > 0, ]
  area_manzanas_app_m2 <- sum(as.numeric(st_area(tocadas_app_sf)))
}

# --- GPS ---
df_gps_validos <- sacar_a_la_calle(df_gps_validos, "LONG", "LAT", pol_sf)
df_gps_validos <- df_gps_validos %>%
  mutate(lon_ant = lag(LONG), lat_ant = lag(LAT), distancia_m = calcular_distancia(lon_ant, lat_ant, LONG, LAT))

dist_total_gps_km <- sum(df_gps_validos$distancia_m, na.rm = TRUE) / 1000
tiempo_gps_horas <- as.numeric(difftime(max(df_gps_validos$TIME_FORMAT), min(df_gps_validos$TIME_FORMAT), units="hours"))

# Área Exacta GPS (Polígono Secuencial Cerrado)
area_gps_exacta_m2 <- 0; coords_gps_closed <- NULL
if(nrow(df_gps_validos) >= 3) {
  coords_gps <- df_gps_validos %>% select(LONG, LAT) %>% as.matrix()
  coords_gps_closed <- rbind(coords_gps, coords_gps[1, ])
  poly_gps <- st_polygon(list(coords_gps_closed))
  area_gps_exacta_m2 <- sum(as.numeric(st_area(st_make_valid(st_sfc(poly_gps, crs = 4326)))))
}

# Área Manzanas Tocadas GPS
area_manzanas_gps_m2 <- 0; tocadas_gps_sf <- NULL
if(!is.null(pol_sf) && nrow(df_gps_validos) > 0) {
  pts_gps_orig <- st_as_sf(df_gps_validos, coords = c("LONG", "LAT"), crs = 4326)
  tocadas_gps_sf <- pol_sf[lengths(st_intersects(pol_sf, pts_gps_orig)) > 0, ]
  area_manzanas_gps_m2 <- sum(as.numeric(st_area(tocadas_gps_sf)))
}

densidad_perros_m2 <- ifelse(area_gps_exacta_m2 > 0, total_perros_vacunados / area_gps_exacta_m2, 0)
df_grafico_app <- df_app_vp21 %>% group_by(hora_dia) %>% summarise(Total_Perros = sum(perros_vacunados, na.rm = TRUE))

# =======================================================
# G. SHINY APP (UI & SERVER)
# =======================================================
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%;margin:0;padding:0;}"),
  leafletOutput("mapa_interactivo", width = "100%", height = "100vh"),
  
  # PANEL IZQUIERDO: CONTROLES
  absolutePanel(
    top = 20, left = 60, draggable = TRUE, width = 340,
    wellPanel(
      h4("Tracking: VP21"),
      p("Cluster 15 | 22 Jun 2025"),
      hr(),
      checkboxInput("ver_manzanas", HTML("<b>🏘️ Catastro: Todas las Manzanas</b>"), value = TRUE),
      checkboxInput("ver_manzanas_tocadas", HTML("<b>🔥 Catastro: Manzanas Abarcadas</b>"), value = FALSE),
      hr(),
      checkboxInput("ver_gps_ok", HTML("<b style='color:#751dc3;'>🟣 Rutina GPS (Sacado a la Calle)</b>"), value = TRUE),
      checkboxInput("ver_app", HTML("<b style='color:#e68102;'>🟠 Eventos App (Sacado a la Calle)</b>"), value = TRUE),
      checkboxInput("ver_gps_bad", HTML("<b style='color:#d32f2f;'>🔴 Ver Saltos Eliminados (> 3m/s)</b>"), value = FALSE),
      hr(),
      checkboxInput("ver_area_recorrido", HTML("<b>🕸️ Área Abarcada (Polígono Cerrado)</b>"), value = FALSE),
      hr(),
      checkboxInput("ver_auditoria", HTML("<b>📖 Tabla de Ruido Eliminado</b>"), value = FALSE),
      checkboxInput("ver_paneles", HTML("<b>📊 ABRIR DASHBOARD LATERAL</b>"), value = TRUE)
    )
  ),
  
  # PANEL DERECHO: PESTAÑAS
  conditionalPanel(
    condition = "input.ver_paneles == true",
    absolutePanel(
      top = 20, right = 20, width = 420, draggable = TRUE,
      wellPanel(
        tabsetPanel(
          tabPanel("⚖️ Comparativa",
                   br(),
                   h5("🛰️ Dispositivo GPS (Ruta Real)"),
                   p(paste("🔸 Km Recorridos (Por calles):", round(dist_total_gps_km, 2), "Km")),
                   p(paste("🔸 Tiempo Invertido:", round(tiempo_gps_horas, 2), "Horas")),
                   p(paste("🔸 Área Cerrada (Polígono):", round(area_gps_exacta_m2, 2), "m²")),
                   p(HTML(paste("🔸 <b>Suma Manzanas Tocadas:</b>", round(area_manzanas_gps_m2, 2), "m²"))),
                   hr(),
                   h5("📱 Aplicativo (Registro Efectivo)"),
                   p(paste("🔹 Km Acumulados (Por calles):", round(dist_total_app_km, 2), "Km")),
                   p(paste("🔹 Tiempo Trabajado:", round(tiempo_app_horas, 2), "Horas")),
                   p(paste("🔹 Área Cerrada (Polígono):", round(area_app_exacta_m2, 2), "m²")),
                   p(HTML(paste("🔹 <b>Suma Manzanas Tocadas:</b>", round(area_manzanas_app_m2, 2), "m²")))
          ),
          
          tabPanel("🐕 Analítica Vacunación",
                   br(),
                   h5(paste("Total Perros Vacunados:", total_perros_vacunados)),
                   p(HTML(paste("<b>Densidad de Trabajo:</b>", round(densidad_perros_m2, 5), "perros / m² (En base a ruta GPS)"))),
                   hr(),
                   h5("Picos de Vacunación por Hora"),
                   plotOutput("grafico_horarios", height = "250px")
          )
        )
      )
    )
  ),
  
  # PANEL INFERIOR: AUDITORÍA
  conditionalPanel(
    condition = "input.ver_auditoria == true",
    absolutePanel(
      bottom = 20, right = 20, width = 450, height = 350, draggable = TRUE,
      wellPanel(
        style = "height: 100%; overflow-y: auto; padding: 10px;",
        h4("🛑 Auditoría de Ruido GPS"),
        DTOutput("tabla_auditoria")
      )
    )
  )
)

server <- function(input, output, session) {
  
  if (nrow(df_gps_validos) > 0) {
    paleta_gradiente_gps <- colorNumeric(palette = c("purple", "green"), domain = df_gps_validos$orden_rutina)
  }
  
  # GRÁFICO CON NÚMEROS SOBRE LAS BARRAS
  output$grafico_horarios <- renderPlot({
    if(nrow(df_grafico_app) > 0){
      ggplot(df_grafico_app, aes(x = hora_dia, y = Total_Perros)) +
        geom_col(fill = "#e68102", color = "black", alpha = 0.8) +
        geom_text(aes(label = Total_Perros), vjust = -0.5, fontface = "bold", size = 5) + # Número en la barra
        theme_minimal() +
        labs(x = "Hora del Día", y = "Nº de Perros") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11)) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.25))) # Da espacio para que el número no se corte
    }
  })
  
  output$tabla_auditoria <- renderDT({
    datatable(df_gps_ruido %>% 
                mutate(Vel = round(velocidad_ms, 2), Motivo = paste0("Mayor a 3 m/s (", Vel, ")")) %>%
                select(TIME_FORMAT, Vel, Motivo),
              options = list(pageLength = 4, dom = 'tp'), rownames = FALSE,
              colnames = c("Hora de Falla", "Velocidad m/s", "Filtro Biomecánico"))
  })
  
  output$mapa_interactivo <- renderLeaflet({
    m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(lon_rango[1], lat_rango[1], lon_rango[2], lat_rango[2])
    
    # 0. MANZANAS
    if(!is.null(pol_sf)) {
      m <- m %>% addPolygons(
        data = pol_sf, fillColor = "#cccccc", fillOpacity = 0.2, color = "white", weight = 1, group = "Capa_Manzanas_Base"
      )
      if(!is.null(tocadas_app_sf) && nrow(tocadas_app_sf) > 0) {
        m <- m %>% addPolygons(
          data = tocadas_app_sf, fillColor = "#e68102", fillOpacity = 0.3, color = "#e68102", weight = 2, group = "Capa_Manzanas_Tocadas"
        )
      }
      if(!is.null(tocadas_gps_sf) && nrow(tocadas_gps_sf) > 0) {
        m <- m %>% addPolygons(
          data = tocadas_gps_sf, fillColor = "#751dc3", fillOpacity = 0.3, color = "#751dc3", weight = 2, group = "Capa_Manzanas_Tocadas"
        )
      }
    }
    
    # 1. GPS
    if (nrow(df_gps_validos) > 1) {
      for(i in 1:(nrow(df_gps_validos) - 1)) {
        m <- m %>% addPolylines(
          lng = c(df_gps_validos$LONG[i], df_gps_validos$LONG[i+1]), lat = c(df_gps_validos$LAT[i], df_gps_validos$LAT[i+1]),
          color = paleta_gradiente_gps(df_gps_validos$orden_rutina[i]), weight = 3, opacity = 0.8, group = "Capa_GPS_OK"
        )
      }
      m <- m %>% addCircleMarkers(
        data = df_gps_validos, lng = ~LONG, lat = ~LAT,
        radius = 3, fillColor = ~paleta_gradiente_gps(orden_rutina), color = "black", weight = 0.5, fillOpacity = 1,
        group = "Capa_GPS_OK", popup = ~paste("<b>Paso GPS #</b>", orden_rutina, "<br><b>Hora:</b>", format(TIME_FORMAT, "%H:%M:%S"), "<br><b>Velocidad:</b>", round(velocidad_ms, 2), "m/s", "<br><b>Estado:</b>", Ubicacion)
      ) %>% addLegend(position = "bottomleft", pal = paleta_gradiente_gps, values = df_gps_validos$orden_rutina, title = "Recorrido GPS<br>(Morado -> Verde)", opacity = 1)
    }
    
    # 2. APP
    if (nrow(df_app_vp21) > 0) {
      m <- m %>% addCircleMarkers(
        data = df_app_vp21, lng = ~long, lat = ~lat,
        radius = 6, fillColor = "#e68102", color = "white", weight = 2, fillOpacity = 1,
        group = "Capa_App", popup = ~paste("<b>💉 App #</b>", orden_vacunacion, "<br><b>Hora:</b>", format(date_clean, "%H:%M:%S"), "<br><b>Perros Vacunados:</b>", perros_vacunados, "<br><b>Estado:</b>", Ubicacion)
      )
    }
    
    # 3. RUIDO
    if (nrow(df_gps_ruido) > 0) {
      m <- m %>% addCircleMarkers(
        data = df_gps_ruido, lng = ~LONG, lat = ~LAT,
        radius = 4, fillColor = "#d32f2f", color = "black", weight = 1, fillOpacity = 0.8,
        group = "Capa_GPS_BAD", popup = ~paste("<b style='color:red;'>⚠️ RUIDO</b><br><b>Hora:</b>", format(TIME_FORMAT, "%H:%M:%S"), "<br><b>Velocidad:</b>", round(velocidad_ms, 2), "m/s")
      )
    }
    m
  })
  
  observe({
    if(input$ver_gps_ok) leafletProxy("mapa_interactivo") %>% showGroup("Capa_GPS_OK") else leafletProxy("mapa_interactivo") %>% hideGroup("Capa_GPS_OK")
    if(input$ver_gps_bad) leafletProxy("mapa_interactivo") %>% showGroup("Capa_GPS_BAD") else leafletProxy("mapa_interactivo") %>% hideGroup("Capa_GPS_BAD")
    if(input$ver_app) leafletProxy("mapa_interactivo") %>% showGroup("Capa_App") else leafletProxy("mapa_interactivo") %>% hideGroup("Capa_App")
    
    if(input$ver_manzanas) leafletProxy("mapa_interactivo") %>% showGroup("Capa_Manzanas_Base") else leafletProxy("mapa_interactivo") %>% hideGroup("Capa_Manzanas_Base")
    if(input$ver_manzanas_tocadas) leafletProxy("mapa_interactivo") %>% showGroup("Capa_Manzanas_Tocadas") else leafletProxy("mapa_interactivo") %>% hideGroup("Capa_Manzanas_Tocadas")
    
    proxy <- leafletProxy("mapa_interactivo") %>% clearGroup("Capa_Area_Poly")
    if (input$ver_area_recorrido) {
      if (!is.null(coords_gps_closed)) proxy %>% addPolygons(lng = coords_gps_closed[,1], lat = coords_gps_closed[,2], fillColor = "#751dc3", fillOpacity = 0.15, color = "#751dc3", weight = 2, dashArray = "4,4", group = "Capa_Area_Poly")
      if (!is.null(coords_app_closed)) proxy %>% addPolygons(lng = coords_app_closed[,1], lat = coords_app_closed[,2], fillColor = "#e68102", fillOpacity = 0.15, color = "#e68102", weight = 2, dashArray = "4,4", group = "Capa_Area_Poly")
    }
  })
}

shinyApp(ui, server)