#######################################################---
# SHINY APP: DASHBOARD FORENSE (CIUDAD DE DIOS 2024)
# Análisis de Cluster 18 y 19 - App vs GPS
#######################################################---
rm(list = ls())
options(warn = -1)

# 1. CARGA DE LIBRERÍAS
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

# CRÍTICO: Apagamos el motor espacial estricto para evitar errores de geometría
sf_use_s2(FALSE) 

cat("==================================================\n")
cat("INICIANDO DASHBOARD: CIUDAD DE DIOS (2024)...\n")
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
  
  if("ident" %in% colnames(pol_raw)) {
    pol_clean <- pol_raw %>%
      mutate(
        lat_clean = trimws(as.character(lat)),
        long_clean = trimws(as.character(long))
      ) %>%
      filter(lat_clean != "" & long_clean != "" & !is.na(lat_clean)) %>%
      mutate(
        lat_num = as.numeric(gsub(",", ".", lat_clean)), 
        long_num = as.numeric(gsub(",", ".", long_clean)),
        poly_id = paste0(basename(ruta_csv), "_", ident)
      ) %>%
      filter(!is.na(lat_num) & !is.na(long_num))
  } else {
    pol_clean <- pol_raw %>% 
      mutate(
        lat_clean = trimws(as.character(lat)),
        long_clean = trimws(as.character(long))
      ) %>%
      mutate(poly_id = paste0(basename(ruta_csv), "_", cumsum(is.na(lat_clean) | lat_clean == "" | lat_clean == "0"))) %>%
      filter(!(is.na(lat_clean) | lat_clean == "" | lat_clean == "0")) %>%
      mutate(
        lat_num = as.numeric(gsub(",", ".", lat_clean)), 
        long_num = as.numeric(gsub(",", ".", long_clean))
      ) %>%
      filter(!is.na(lat_num) & !is.na(long_num))
  }
  
  if(nrow(pol_clean) == 0) return(NULL)
  
  pol_sf <- pol_clean %>% 
    st_as_sf(coords = c("long_num", "lat_num"), crs = 4326) %>%
    group_by(poly_id) %>% 
    filter(n() >= 3) %>% 
    summarise(geometry = st_combine(geometry), .groups = "drop") %>%
    st_cast("POLYGON") %>% 
    st_make_valid()
  
  return(pol_sf)
}

sacar_a_la_calle <- function(df, lon_col, lat_col, poligonos, tolerancia_metros = 3) {
  df$Ubicacion <- "En la calle (Original)" 
  df$lon_orig <- df[[lon_col]]
  df$lat_orig <- df[[lat_col]]
  df$lon_snap <- df[[lon_col]]
  df$lat_snap <- df[[lat_col]]
  df$dist_ajuste_m <- 0
  df$dist_a_manzana_m <- 0 
  df$toca_manzana <- "NO" 
  df$estaba_dentro <- FALSE
  
  if(is.null(poligonos) || nrow(poligonos) == 0 || nrow(df) == 0) return(df)
  
  pts_sf <- st_as_sf(df, coords = c(lon_col, lat_col), crs = 4326) %>% st_transform(32719)
  pol_proj <- st_transform(poligonos, 32719)
  
  suppressMessages(suppressWarnings({
    intersecciones <- st_intersects(pts_sf, pol_proj)
    bordes_poligonos <- st_cast(st_geometry(pol_proj), "MULTILINESTRING")
    idx_nearest <- st_nearest_feature(pts_sf, pol_proj)
    
    for(i in seq_len(nrow(df))) {
      is_inside <- length(intersecciones[[i]]) > 0
      
      if(!is.na(idx_nearest[i])) {
        borde <- bordes_poligonos[idx_nearest[i]]
        linea <- st_nearest_points(pts_sf[i,], borde)
        
        coords_proj_borde <- st_coordinates(linea)[2, c("X", "Y")]
        pt_borde_sf <- st_sfc(st_point(coords_proj_borde), crs = 32719) %>% st_transform(4326)
        coords_lonlat <- st_coordinates(pt_borde_sf)
        
        dist_m <- calcular_distancia(df$lon_orig[i], df$lat_orig[i], coords_lonlat[1, "X"], coords_lonlat[1, "Y"])
        
        if(is_inside) {
          df$estaba_dentro[i] <- TRUE
          df$toca_manzana[i] <- "SÍ"
          df[i, lon_col] <- coords_lonlat[1, "X"]
          df[i, lat_col] <- coords_lonlat[1, "Y"]
          df$lon_snap[i] <- coords_lonlat[1, "X"]
          df$lat_snap[i] <- coords_lonlat[1, "Y"]
          df$Ubicacion[i] <- "Ajustado a vereda"
          df$dist_ajuste_m[i] <- dist_m
        } else {
          df$estaba_dentro[i] <- FALSE
          df$dist_a_manzana_m[i] <- dist_m
          if(dist_m <= tolerancia_metros) {
            df$toca_manzana[i] <- "SÍ"
          } else {
            df$toca_manzana[i] <- "NO"
          }
        }
      }
    }
  }))
  return(df)
}

agrupar_puntos_secuencial <- function(df_puntos, radio_metros, max_puntos) {
  if(nrow(df_puntos) == 0) return(NULL)
  df_agrupado <- df_puntos %>% arrange(user_std, date_clean) %>% mutate(Cluster_ID = NA_integer_)
  
  cluster_actual <- 1
  puntos_en_cluster <- 0
  lon_prev <- df_agrupado$long[1]
  lat_prev <- df_agrupado$lat[1]
  user_prev <- df_agrupado$user_std[1]
  
  for (i in 1:nrow(df_agrupado)) {
    if (puntos_en_cluster == 0 || df_agrupado$user_std[i] != user_prev) {
      if(df_agrupado$user_std[i] != user_prev) cluster_actual <- cluster_actual + 1
      df_agrupado$Cluster_ID[i] <- cluster_actual
      puntos_en_cluster <- 1
      lon_prev <- df_agrupado$long[i]
      lat_prev <- df_agrupado$lat[i]
      user_prev <- df_agrupado$user_std[i]
    } else {
      dist_al_previo <- calcular_distancia(lon_prev, lat_prev, df_agrupado$long[i], df_agrupado$lat[i])
      if (dist_al_previo <= radio_metros && puntos_en_cluster < max_puntos) {
        df_agrupado$Cluster_ID[i] <- cluster_actual
        puntos_en_cluster <- puntos_en_cluster + 1
        lon_prev <- df_agrupado$long[i]
        lat_prev <- df_agrupado$lat[i]
      } else {
        cluster_actual <- cluster_actual + 1
        df_agrupado$Cluster_ID[i] <- cluster_actual
        puntos_en_cluster <- 1
        lon_prev <- df_agrupado$long[i]
        lat_prev <- df_agrupado$lat[i]
      }
    }
  }
  
  df_final <- df_agrupado %>%
    group_by(Cluster_ID) %>%
    summarise(
      user_std = first(user_std),
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
# B. RUTAS EXACTAS
# =======================================================
ruta_base <- "D:/github_UPCH/R/R/data_vacunacion/2024/cuidad_dios"
ruta_app_csv <- file.path(ruta_base, "cluster19_2024.csv")

# =======================================================
# C. ETL 1: CARGA DE RUTAS GPS MASIVAS
# =======================================================
cat(">>> 1. Procesando Masivamente archivos GPS...\n")
archivos_gps <- list.files(ruta_base, pattern = "\\.kml$", full.names = TRUE, ignore.case = TRUE)

if(length(archivos_gps) == 0) {
  cat("Advertencia: No se encontraron archivos KML en", ruta_base, "\n")
  df_gps_raw <- data.frame(TRACK_ID = character(), TIME_FORMAT = as.POSIXct(character()), LONG = numeric(), LAT = numeric(), ESTADO = character(), velocidad_ms = numeric(), distancia_m = numeric())
  df_gps_validos <- df_gps_raw
  df_gps_ruido <- df_gps_raw
  vel_promedio_trabajador <- 0
  total_gps_crudo <- 0
  velocidades_por_grupo <- data.frame()
} else {
  lista_puntos_gps <- list()
  
  for (archivo in archivos_gps) {
    kml_doc <- try(read_xml(archivo), silent = TRUE)
    if (inherits(kml_doc, "try-error")) next
    
    nombre_real_gps <- gsub("\\.kml$", "", basename(archivo), ignore.case = TRUE)
    ns <- xml_ns(kml_doc)
    tramos <- xml_find_all(kml_doc, ".//gx:Track", ns)
    
    for (i in seq_along(tramos)) {
      tramo_actual <- tramos[[i]]
      tiempos_raw <- xml_text(xml_find_all(tramo_actual, "./kml:when", ns))
      coords_raw <- xml_text(xml_find_all(tramo_actual, "./gx:coord", ns))
      if (length(tiempos_raw) == length(coords_raw) && length(coords_raw) > 0) {
        mat_coord <- do.call(rbind, strsplit(coords_raw, " "))
        lista_puntos_gps[[length(lista_puntos_gps) + 1]] <- data.frame(
          TRACK_ID = nombre_real_gps, 
          TIME = tiempos_raw, 
          LONG = as.numeric(mat_coord[,1]), 
          LAT = as.numeric(mat_coord[,2]), 
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  df_gps_raw <- bind_rows(lista_puntos_gps) %>%
    mutate(TIME_FORMAT = as.POSIXct(TIME, tryFormats = c("%Y-%m-%dT%H:%M:%OSZ", "%Y-%m-%dT%H:%M:%SZ"), tz="UTC")) %>%
    filter(!is.na(LONG) & !is.na(LAT) & !is.na(TIME_FORMAT)) %>% 
    arrange(TRACK_ID, TIME_FORMAT)
  
  attr(df_gps_raw$TIME_FORMAT, "tzone") <- "America/Lima"
  total_gps_crudo <- nrow(df_gps_raw)
  
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
      
      if (is.na(vel_ms) || is.nan(vel_ms)) {
        v_estado[i] <- "RUIDO"
      } else if (vel_ms > UMBRAL_MS) { 
        v_estado[i] <- "RUIDO" 
      } else { 
        last_valid_idx <- i 
      }
    }
  }
  df_gps_raw$velocidad_ms <- v_vel; df_gps_raw$distancia_m <- v_dist; df_gps_raw$ESTADO <- v_estado
  
  df_gps_validos <- df_gps_raw %>% 
    filter(ESTADO == "OK") %>% 
    group_by(TRACK_ID) %>% 
    arrange(TIME_FORMAT) %>%
    mutate(orden_relativo = row_number()) %>%
    ungroup() %>%
    mutate(orden_rutina = row_number())
  
  df_gps_ruido <- df_gps_raw %>% filter(ESTADO == "RUIDO")
  
  velocidades_por_grupo <- df_gps_validos %>%
    filter(velocidad_ms > 0) %>%
    group_by(TRACK_ID) %>%
    summarise(vel_promedio = round(mean(velocidad_ms, na.rm = TRUE), 2))
  
  vel_promedio_trabajador <- mean(df_gps_validos$velocidad_ms[df_gps_validos$velocidad_ms > 0], na.rm = TRUE)
  if(is.nan(vel_promedio_trabajador)) vel_promedio_trabajador <- 0
}

# =======================================================
# C.2 DATOS MANUALES DE RENDIMIENTO GPS
# =======================================================
df_vacunacion_gps_manual <- data.frame(
  `Equipo GPS (Ruta)` = c(
    "GPS-60 30-11-2024 GRUPO 01", 
    "GPS-56 30-11-2024 GRUPO 03", 
    "GPS-25 30-11-2024 GRUPO 04", 
    "GPS-78 30-11-2024 GRUPO 06", 
    "GPS-80 30-11-2024 GRUPO 07", 
    "GPS-49 30-11-2024 GRUPO 08", 
    "GPS-13 30-11-2024 GRUPO 09"
  ),
  `Perros_Vacunados_GPS` = c(62, 54, 55, 54, 38, 52, 66),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# =======================================================
# D. ETL 2: CARGA DE APLICATIVO
# =======================================================
cat(">>> 2. Procesando App desde CSV...\n")
if(!file.exists(ruta_app_csv)) stop(paste("Falta el archivo CSV en:", ruta_app_csv))

df_app_raw <- read.csv(ruta_app_csv, stringsAsFactors = FALSE)
colnames(df_app_raw) <- tolower(colnames(df_app_raw))

col_usuario <- grep("user|usuario|vp", colnames(df_app_raw), value = TRUE)
if(length(col_usuario) > 0) {
  df_app_raw$user_app <- df_app_raw[[col_usuario[1]]]
} else if ("username" %in% colnames(df_app_raw)) {
  df_app_raw$user_app <- df_app_raw$username
} else {
  df_app_raw$user_app <- "Desconocido"
}

if(!"number_dog_house" %in% colnames(df_app_raw)) df_app_raw$number_dog_house <- 1
if(!"number_dog_vaccinated_2024" %in% colnames(df_app_raw)) df_app_raw$number_dog_vaccinated_2024 <- 0
if(!"number_dog_vaccinated_sweep" %in% colnames(df_app_raw)) df_app_raw$number_dog_vaccinated_sweep <- 0
if(!"type_house" %in% colnames(df_app_raw)) df_app_raw$type_house <- "P"
if(!"raise_dog_house" %in% colnames(df_app_raw)) df_app_raw$raise_dog_house <- "SI"

df_app_vp21 <- df_app_raw %>%
  mutate(
    long = as.numeric(gsub(",", ".", as.character(long))),
    lat = as.numeric(gsub(",", ".", as.character(lat)))
  ) %>%
  filter(!is.na(long) & !is.na(lat)) %>%
  mutate(
    date_str = as.character(if("date" %in% colnames(.)) date else date_clean),
    date_clean = as.POSIXct(gsub("T", " ", substr(date_str, 1, 19)), tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %H:%M:%S", "%d/%m/%Y %H:%M", "%Y-%m-%d"), tz="America/Lima"),
    n_dog_house = as.numeric(number_dog_house),
    v_2024 = as.numeric(number_dog_vaccinated_2024),
    v_sweep = as.numeric(number_dog_vaccinated_sweep),
    user_std = toupper(trimws(as.character(user_app))),
    type_house_std = toupper(trimws(as.character(type_house))),
    rdh = toupper(trimws(as.character(raise_dog_house)))
  ) %>%
  mutate(
    n_dog_house = ifelse(is.na(n_dog_house), 0, n_dog_house),
    v_2024 = ifelse(is.na(v_2024), 0, v_2024),
    v_sweep = ifelse(is.na(v_sweep), 0, v_sweep)
  )

df_app_vp21$date_clean[is.na(df_app_vp21$date_clean)] <- as.POSIXct(Sys.Date(), tz="America/Lima")

attr(df_app_vp21$date_clean, "tzone") <- "America/Lima"

df_app_vp21 <- df_app_vp21 %>%
  arrange(user_std, date_clean) %>%
  group_by(user_std) %>%
  mutate(
    orden_vacunacion = row_number(), 
    hora_dia = format(date_clean, "%H:00 (%I %p)", tz="America/Lima"),
    lon_ant = lag(long), 
    lat_ant = lag(lat), 
    tiempo_ant = lag(date_clean)
  ) %>%
  ungroup() %>%
  mutate(
    distancia_m_app = mapply(function(lo1, la1, lo2, la2) {
      if(is.na(lo1) || is.na(la1)) return(0)
      calcular_distancia(lo1, la1, lo2, la2)
    }, lon_ant, lat_ant, long, lat),
    delta_t_app = as.numeric(difftime(date_clean, tiempo_ant, units="secs")),
    velocidad_ms = ifelse(!is.na(delta_t_app) & delta_t_app > 0, distancia_m_app / delta_t_app, 0)
  )

# Metricas App
total_app_crudo <- nrow(df_app_vp21)
total_fijo <- sum(df_app_vp21$v_2024, na.rm = TRUE)
total_barrido <- sum(df_app_vp21$v_sweep, na.rm = TRUE)
total_perros_vacunados <- total_fijo + total_barrido
total_perros_casa <- sum(df_app_vp21$n_dog_house, na.rm = TRUE)

df_grafico <- df_app_vp21 %>% 
  group_by(hora_dia) %>% 
  summarise(Total = sum(v_sweep, na.rm = TRUE)) %>% 
  mutate(Tipo = "Barrido")

# =======================================================
# E. ETL 3: CATASTRO Y AREAS OPTIMIZADAS
# =======================================================
cat(">>> 3. Cargando Catastro Dinámico...\n")
archivos_poligonos <- list.files(ruta_base, pattern = "^Loc_.*\\.csv$", full.names = TRUE, ignore.case = TRUE)

lista_poligonos <- list()
for(archivo_pol in archivos_poligonos) {
  pol_temp <- procesar_manzanas(archivo_pol)
  if(!is.null(pol_temp)) lista_poligonos[[length(lista_poligonos) + 1]] <- pol_temp
}

pol_sf <- if(length(lista_poligonos) > 0) bind_rows(lista_poligonos) else NULL

lon_rango <- range(c(df_app_vp21$long, df_gps_validos$LONG), na.rm = TRUE)
lat_rango <- range(c(df_app_vp21$lat, df_gps_validos$LAT), na.rm = TRUE)
bbox_valido <- all(is.finite(lon_rango)) && all(is.finite(lat_rango))

cat(">>> Midiendo distancia de puntos a veredas...\n")
df_app_vp21 <- sacar_a_la_calle(df_app_vp21, "long", "lat", pol_sf, tolerancia_metros = 3)

if(nrow(df_gps_validos) > 0) {
  df_gps_validos <- sacar_a_la_calle(df_gps_validos, "LONG", "LAT", pol_sf, tolerancia_metros = 3)
}

cat(">>> Calculando Areas de Impacto...\n")
suppressMessages(suppressWarnings({
  
  area_gps_exacta_m2 <- 0
  poligonos_gps_lista <- list()
  if(nrow(df_gps_validos) > 0) {
    for(tid in unique(df_gps_validos$TRACK_ID)) {
      t_data <- df_gps_validos %>% filter(TRACK_ID == tid)
      if(nrow(t_data) >= 3) {
        pts_sf <- st_as_sf(t_data, coords = c("LONG", "LAT"), crs = 4326)
        hull <- st_convex_hull(st_union(pts_sf))
        area_gps_exacta_m2 <- area_gps_exacta_m2 + as.numeric(st_area(hull))
        poligonos_gps_lista[[length(poligonos_gps_lista) + 1]] <- hull
      }
    }
  }
  
  area_app_exacta_m2 <- 0
  poligonos_app_lista <- list()
  for(usu in unique(df_app_vp21$user_std)) {
    u_data <- df_app_vp21 %>% filter(user_std == usu)
    if(nrow(u_data) >= 3) {
      pts_app_sf <- st_as_sf(u_data, coords = c("long", "lat"), crs = 4326)
      hull_app <- st_convex_hull(st_union(pts_app_sf))
      area_app_exacta_m2 <- area_app_exacta_m2 + as.numeric(st_area(hull_app))
      poligonos_app_lista[[length(poligonos_app_lista) + 1]] <- hull_app
    }
  }
  
  area_manzanas_gps_m2 <- 0; tocadas_gps_sf <- NULL
  if(!is.null(pol_sf) && nrow(df_gps_validos) > 0) {
    df_gps_valido_area <- df_gps_validos %>% filter(toca_manzana == "SÍ")
    if(nrow(df_gps_valido_area) > 0) {
      pts_gps_snap <- st_as_sf(df_gps_valido_area, coords = c("lon_orig", "lat_orig"), crs = 4326) %>% st_transform(32719)
      pol_proj <- st_transform(pol_sf, 32719)
      idx_gps <- unique(unlist(st_is_within_distance(pts_gps_snap, pol_proj, dist = 3.5))) 
      if(length(idx_gps) > 0) {
        tocadas_gps_sf <- pol_sf[idx_gps, ]
        area_manzanas_gps_m2 <- sum(as.numeric(st_area(tocadas_gps_sf)))
      }
    }
  }
  
  area_manzanas_app_m2 <- 0; tocadas_app_sf <- NULL
  if(!is.null(pol_sf) && nrow(df_app_vp21) > 0) {
    df_app_valido_area <- df_app_vp21 %>% filter(toca_manzana == "SÍ")
    if(nrow(df_app_valido_area) > 0) {
      pts_app_snap <- st_as_sf(df_app_valido_area, coords = c("lon_orig", "lat_orig"), crs = 4326) %>% st_transform(32719)
      pol_proj <- st_transform(pol_sf, 32719)
      idx_app <- unique(unlist(st_is_within_distance(pts_app_snap, pol_proj, dist = 3.5)))
      if(length(idx_app) > 0) {
        tocadas_app_sf <- pol_sf[idx_app, ]
        area_manzanas_app_m2 <- sum(as.numeric(st_area(tocadas_app_sf)))
      }
    }
  }
  
  area_manzanas_compartidas_m2 <- 0
  if(!is.null(tocadas_gps_sf) && !is.null(tocadas_app_sf) && nrow(tocadas_gps_sf) > 0 && nrow(tocadas_app_sf) > 0) {
    manzanas_compartidas_sf <- st_intersection(tocadas_gps_sf, tocadas_app_sf)
    if(nrow(manzanas_compartidas_sf) > 0) {
      area_manzanas_compartidas_m2 <- sum(as.numeric(st_area(manzanas_compartidas_sf)))
    }
  }
}))

# =======================================================
# F. INTERFAZ DE USUARIO Y SERVIDOR (SHINY)
# =======================================================
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%;margin:0;padding:0;}
              .dataTables_wrapper { font-size: 14px; }"),
  
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
                                sliderInput("radio_agrupar", "Distancia Maxima (m):", min = 0, max = 1000, value = 0, step = 10),
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
                                     p(HTML(paste("<b>Perros Vacunados Independientemente:</b>", total_fijo))),
                                     p(HTML(paste("<b>Total Perros Vacunados en Barrido:</b>", total_barrido))),
                                     hr(),
                                     h5("Velocidad GPS Promedio por Equipo:"),
                                     if(nrow(velocidades_por_grupo) > 0) HTML(paste0("<ul>", paste0("<li><b>", velocidades_por_grupo$TRACK_ID, ":</b> ", velocidades_por_grupo$vel_promedio, " m/s</li>", collapse = ""), "</ul>")) else p("No hay GPS valido.")
                                   ),
                                   wellPanel(
                                     h4("Analisis Espacial"),
                                     p(HTML(paste("<b>Area GPS (Punto a Punto):</b>", round(area_gps_exacta_m2, 2), "m²"))),
                                     p(HTML(paste("<b>Area App (Punto a Punto):</b>", round(area_app_exacta_m2, 2), "m²"))),
                                     hr(),
                                     p(HTML(paste("<b>Area GPS (Por Manzanas):</b>", round(area_manzanas_gps_m2, 2), "m²"))),
                                     p(HTML(paste("<b>Area App (Por Manzanas):</b>", round(area_manzanas_app_m2, 2), "m²"))),
                                     p(HTML(paste("<span style='color:#751dc3; font-size:16px;'><b>Manzanas Compartidas (App + GPS):</b> ", round(area_manzanas_compartidas_m2, 2), "m²</span>")))
                                   )
                            ),
                            column(8,
                                   wellPanel(
                                     h4("Picos de Produccion (Solo Vacunacion)"),
                                     plotlyOutput("grafico_horarios", height = "350px")
                                   ),
                                   wellPanel(
                                     h3("COMPARATIVA DE RENDIMIENTO: APP vs GPS", style = "text-align: center; margin-bottom: 20px; font-weight: bold;"),
                                     fluidRow(
                                       column(6,
                                              h4("Rendimiento por Encuestador (APP)", style = "color: #e68102; border-bottom: 2px solid #e68102; padding-bottom: 5px;"),
                                              DTOutput("tabla_comparativa")
                                       ),
                                       column(6,
                                              h4("🛰️ Rendimiento por Equipo (GPS)", style = "color: #751dc3; border-bottom: 2px solid #751dc3; padding-bottom: 5px;"),
                                              DTOutput("tabla_comparativa_gps")
                                       )
                                     )
                                   )
                            )
                          )
                      )
             )
  )
)

server <- function(input, output, session) {
  
  if (nrow(df_gps_validos) > 0) {
    paleta_rutas <- colorFactor(palette = "Set1", domain = df_gps_validos$TRACK_ID)
  }
  
  colores_hex <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#A65628", "#F781BF", "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666", "#000000", "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#17becf", "#bcbd22")
  paleta_app <- colorFactor(palette = colores_hex, domain = df_app_vp21$user_std)
  
  output$grafico_horarios <- renderPlotly({
    if(nrow(df_grafico) > 0){
      p <- ggplot(df_grafico, aes(x = hora_dia, y = Total, fill = Tipo, text = paste("Hora:", hora_dia, "<br>Categoria:", Tipo, "<br>Cantidad:", Total))) +
        geom_col(position = "dodge", color = "black", alpha = 0.8) +
        scale_fill_manual(values = c("Barrido" = "#e68102")) +
        theme_minimal() +
        labs(x = "Hora del Día", y = "Cantidad de Perros") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "top", legend.title = element_blank())
      
      ggplotly(p, tooltip = "text") %>% layout(hovermode = "x unified")
    }
  })
  
  # TABLA DE LA APP (SIN EFICIENCIA)
  output$tabla_comparativa <- renderDT({
    req(nrow(df_app_vp21) > 0)
    
    res_app <- df_app_vp21 %>%
      group_by(`Usuario` = user_std) %>%
      summarise(
        `Visitas (Casas)` = n(),
        `Perros Vacunados APP` = sum(v_sweep, na.rm = TRUE)
      ) %>%
      arrange(desc(`Perros Vacunados APP`))
    
    res_app <- res_app %>% mutate(across(everything(), as.character))
    total_visitas <- sum(as.numeric(res_app$`Visitas (Casas)`), na.rm = TRUE)
    total_vacunados <- sum(as.numeric(res_app$`Perros Vacunados APP`), na.rm = TRUE)
    
    res_app <- bind_rows(res_app, data.frame(
      `Usuario` = "TOTAL",
      `Visitas (Casas)` = as.character(total_visitas),
      `Perros Vacunados APP` = as.character(total_vacunados),
      stringsAsFactors = FALSE, check.names = FALSE
    ))
    
    datatable(res_app, 
              options = list(pageLength = 15, dom = 't', scrollX = TRUE), 
              rownames = FALSE, 
              class = 'cell-border stripe hover') %>%
      formatStyle('Usuario', target = 'row', 
                  backgroundColor = styleEqual("TOTAL", "#f9f9f9"),
                  fontWeight = styleEqual("TOTAL", "bold"))
  })
  
  # TABLA DEL GPS (SOLO CON LOS DATOS DE PERROS VACUNADOS)
  output$tabla_comparativa_gps <- renderDT({
    req(nrow(df_vacunacion_gps_manual) > 0)
    
    res_gps <- df_vacunacion_gps_manual %>% arrange(desc(`Perros_Vacunados_GPS`))
    
    res_gps <- res_gps %>% mutate(across(everything(), as.character))
    total_vacunados_gps <- sum(as.numeric(res_gps$`Perros_Vacunados_GPS`), na.rm = TRUE)
    
    res_gps <- bind_rows(res_gps, data.frame(
      `Equipo GPS (Ruta)` = "TOTAL",
      `Perros_Vacunados_GPS` = as.character(total_vacunados_gps),
      stringsAsFactors = FALSE, check.names = FALSE
    ))
    
    datatable(res_gps, 
              options = list(pageLength = 15, dom = 't', scrollX = TRUE), 
              rownames = FALSE, 
              class = 'cell-border stripe hover') %>%
      formatStyle('Equipo GPS (Ruta)', target = 'row', 
                  backgroundColor = styleEqual("TOTAL", "#f9f9f9"),
                  fontWeight = styleEqual("TOTAL", "bold"))
  })
  
  output$tabla_auditoria <- renderDT({
    datatable(df_gps_ruido %>% 
                mutate(Hora_Local = format(TIME_FORMAT, "%Y-%m-%d %I:%M:%S %p", tz="America/Lima"),
                       Vel = round(velocidad_ms, 2), Salto_m = round(distancia_m, 2),
                       Motivo = paste0("> 3 m/s (", Vel, " m/s)")) %>%
                select(Hora_Local, Salto_m, Vel, Motivo),
              options = list(pageLength = 15, dom = 'ftp'), rownames = FALSE,
              colnames = c("Hora Real (Peru)", "Salto (m)", "Velocidad (m/s)", "Razon"))
  })
  
  df_app_reactivo <- reactive({
    df <- df_app_vp21
    df
  })
  
  datos_app_agrupados <- reactive({
    df <- df_app_reactivo()
    if (input$radio_agrupar > 0) {
      agrupar_puntos_secuencial(df, input$radio_agrupar, input$puntos_agrupar)
    } else {
      df
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
        t_data <- df_gps_validos %>% filter(TRACK_ID == tid) %>% arrange(orden_relativo)
        if(nrow(t_data) > 1) {
          m <- m %>% addPolylines(
            data = t_data, lng = ~LONG, lat = ~LAT,
            color = ~paleta_rutas(TRACK_ID), weight = 3, opacity = 0.8, group = "C_GPS_Snap"
          )
        }
      }
      
      for(i in 1:nrow(df_gps_validos)) {
        if(df_gps_validos$estaba_dentro[i]) {
          m <- m %>% addPolylines(lng = c(df_gps_validos$lon_orig[i], df_gps_validos$LONG[i]), lat = c(df_gps_validos$lat_orig[i], df_gps_validos$LAT[i]),
                                  color = "gray", weight = 1, dashArray = "3,3", group = "C_GPS_Snap")
        }
      }
      
      m <- m %>% addCircleMarkers(
        data = df_gps_validos, lng = ~LONG, lat = ~LAT,
        radius = 3.5, fillColor = ~paleta_rutas(TRACK_ID), color = "black", weight = 0.5, fillOpacity = 1, group = "C_GPS_Snap", 
        popup = ~paste("<b>Ruta Grupo:</b>", TRACK_ID, 
                       "<br><b>Orden Relativo:</b>", orden_relativo,
                       "<br><b>Día:</b>", format(TIME_FORMAT, "%Y-%m-%d", tz="America/Lima"), 
                       "<br><b>Hora (Perú):</b>", format(TIME_FORMAT, "%I:%M:%S %p", tz="America/Lima"), 
                       "<br><b>Velocidad Previa:</b>", round(velocidad_ms, 2), "m/s",
                       ifelse(estaba_dentro, 
                              paste0("<br><b>Desplazado hacia afuera:</b> ", round(dist_ajuste_m, 2), " m<br><b>Dentro de manzana:</b> SÍ"), 
                              paste0("<br><b>Distancia a manzana:</b> ", round(dist_a_manzana_m, 2), " m<br><b>Dentro de manzana:</b> ", toca_manzana)))
      ) %>% addLegend(position = "bottomright", pal = paleta_rutas, values = df_gps_validos$TRACK_ID, title = "Equipos GPS", opacity = 1)
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
    
    if (length(poligonos_gps_lista) > 0) {
      for(hull in poligonos_gps_lista) {
        m <- m %>% addPolygons(data = hull, fillColor = "#751dc3", fillOpacity = 0.2, color = "#751dc3", weight = 2, dashArray = "4,4", group = "C_Area_GPS")
      }
    }
    if (!is.null(tocadas_gps_sf) && nrow(tocadas_gps_sf) > 0) m <- m %>% addPolygons(data = tocadas_gps_sf, fillColor = "#751dc3", fillOpacity = 0.3, color = "#444444", weight = 1.5, group = "C_Area_GPS_Mz")
    
    if (length(poligonos_app_lista) > 0) {
      for(hull in poligonos_app_lista) {
        m <- m %>% addPolygons(data = hull, fillColor = "#e68102", fillOpacity = 0.2, color = "#e68102", weight = 2, dashArray = "4,4", group = "C_Area_App")
      }
    }
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
        
        for(usu in unique(df_dinamico$user_std)) {
          u_data <- df_dinamico %>% filter(user_std == usu) %>% arrange(orden_vacunacion)
          if(nrow(u_data) > 1) {
            proxy %>% addPolylines(
              data = u_data, lng = ~long, lat = ~lat,
              color = paleta_app(usu), weight = 2, opacity = 0.8, group = "C_App_Snap"
            )
          }
        }
        
        for(i in 1:nrow(df_dinamico)) {
          if(df_dinamico$estaba_dentro[i]) {
            proxy %>% addPolylines(lng = c(df_dinamico$lon_orig[i], df_dinamico$long[i]), lat = c(df_dinamico$lat_orig[i], df_dinamico$lat[i]),
                                   color = "gray", weight = 1, dashArray = "3,3", group = "C_App_Snap")
          }
        }
        
        proxy %>% addCircleMarkers(
          data = df_dinamico, lng = ~long, lat = ~lat,
          radius = 6, color = "transparent", fillColor = "transparent", weight = 0, fillOpacity = 0.01, 
          group = "C_App_Snap", 
          popup = ~paste0("Usuario: ", user_std,
                          "<br>Tipo: ", type_house_std,
                          "<br>Día: ", format(date_clean, "%Y-%m-%d"), 
                          "<br>Hora: ", format(date_clean, "%I:%M:%S %p", tz="America/Lima"), 
                          "<br>Velocidad Previa: ", round(velocidad_ms, 2), " m/s",
                          ifelse(estaba_dentro, 
                                 paste0("<br>Desplazado hacia afuera: ", round(dist_ajuste_m, 2), " m<br>Dentro de manzana: SÍ"), 
                                 paste0("<br>Distancia a la manzana: ", round(dist_a_manzana_m, 2), " m<br>Dentro de manzana: ", toca_manzana)))
        )
        
        etiquetas_x <- lapply(paleta_app(df_dinamico$user_std), function(col) {
          htmltools::HTML(paste0('<div style="color:', col, '; font-size:14px; font-weight:bold; cursor:pointer; text-shadow: -1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff;">✖</div>'))
        })
        
        proxy %>% addLabelOnlyMarkers(
          data = df_dinamico, lng = ~long, lat = ~lat,
          label = etiquetas_x,
          labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE, 
                                      style = list("padding" = "0px", "background" = "transparent", "border" = "none")),
          group = "C_App_Snap"
        )
        
      } else {
        proxy %>% addCircleMarkers(
          data = df_dinamico, lng = ~long, lat = ~lat,
          radius = ~ifelse(Puntos_Agrupados == 1, 5, 6 + Puntos_Agrupados), 
          fillColor = ~ifelse(Puntos_Agrupados == 1, "#e68102", "#b35900"), color = "white", weight = 2, fillOpacity = 0.9, group = "C_App_Snap", 
          popup = ~paste("<b>Grupo Espacial</b>",
                         "<br><b>Usuario:</b>", user_std,
                         "<br><b>Día:</b>", format(Hora_Inicio, "%Y-%m-%d", tz="America/Lima"), 
                         "<br><b>Rango Horas:</b>", format(Hora_Inicio, "%I:%M:%S %p", tz="America/Lima"), "-", format(Hora_Fin, "%I:%M:%S %p", tz="America/Lima"),
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