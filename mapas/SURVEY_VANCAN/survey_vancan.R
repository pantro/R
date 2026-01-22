library(dplyr)
library(sf)
library(leaflet)
library(viridis)
library(lubridate)

# *********************************************************************
# Cantidad de perros vacunados por kilometros
# *********************************************************************
clean_vaccinated <- function(x) {
  x <- trimws(as.character(x))
  
  x[x %in% c("", "NS")] <- NA
  
  suppressWarnings(as.numeric(x))
}
calc_vaccinated_per_km <- function(df,
                                   lat_col = "LAT",
                                   lon_col = "LONG",
                                   vac_col = "NUMBER_DOG_VACCINATED_2024",
                                   date_col = "DATE") {
  
  # 1. Ordenar por tiempo (ruta real)
  df <- df %>%
    mutate(datetime = as.POSIXct(.data[[date_col]])) %>%
    arrange(datetime)
  
  # 2. Limpiar vacunados (maneja NA y "NS")
  df <- df %>%
    mutate(vaccinated = clean_vaccinated(.data[[vac_col]]))
  
  # 3. Convertir a sf
  sf_points <- st_as_sf(
    df,
    coords = c(lon_col, lat_col),
    crs = 4326,
    remove = FALSE
  )
  
  # 4. Distancia total recorrida
  distances <- st_distance(
    sf_points[-nrow(sf_points), ],
    sf_points[-1, ],
    by_element = TRUE
  )
  
  total_distance_m <- sum(as.numeric(distances), na.rm = TRUE)
  
  # 5. Total de perros vacunados
  total_vaccinated <- sum(sf_points$vaccinated, na.rm = TRUE)
  
  # 6. Media por kilómetro
  vaccinated_per_km <- total_vaccinated / (total_distance_m / 1000)
  
  list(
    total_distance_km = total_distance_m / 1000,
    total_vaccinated = total_vaccinated,
    vaccinated_per_km = vaccinated_per_km
  )
}
# ********************************************************

# *********************************************************************
# Cantidad de perros vacunados por hora
# *********************************************************************
calc_vaccinated_by_hour_range <- function(df,
                                          vac_col = "NUMBER_DOG_VACCINATED_2024",
                                          date_col = "DATE") {
  
  # 1. Preparar datos
  tmp <- df %>%
    mutate(
      vaccinated = clean_vaccinated(.data[[vac_col]]),
      datetime   = as.POSIXct(.data[[date_col]]),
      hour       = hour(datetime),
      hour_range = case_when(
        hour >= 7  & hour < 10 ~ "07-10",
        hour >= 10 & hour < 13 ~ "10-13",
        hour >= 13 & hour < 16 ~ "13-16",
        hour >= 16 & hour < 19 ~ "16-19",
        TRUE                  ~ "OUT_RANGE"
      )
    )
  
  # 2. Sumar vacunados por rango
  sums <- tmp %>%
    group_by(hour_range) %>%
    summarise(total = sum(vaccinated, na.rm = TRUE), .groups = "drop")
  
  # 3. Asegurar que todos los rangos existan
  ranges <- c("07-10", "10-13", "13-16", "16-19", "OUT_RANGE")
  
  sums_complete <- tibble(hour_range = ranges) %>%
    left_join(sums, by = "hour_range") %>%
    mutate(total = ifelse(is.na(total), 0, total))
  
  # 4. Convertir a lista nombrada (formato exacto pedido)
  result <- as.list(sums_complete$total)
  names(result) <- sums_complete$hour_range
  
  return(result)
}
# ********************************************************

# *********************************************************************
# Area cubrida por el recorrido: 
# - Se crea el polígono mínimo convexo que encierra todos los puntos GPS
# *********************************************************************
calc_covered_area <- function(df,
                              lat_col = "LAT",
                              lon_col = "LONG") {
  
  # 1. Convertir a sf
  sf_points <- st_as_sf(
    df,
    coords = c(lon_col, lat_col),
    crs = 4326,
    remove = FALSE
  )
  
  # 2. Proyectar a metros (importante para área)
  sf_points_m <- st_transform(sf_points, 3857)
  
  # 3. Crear polígono (Convex Hull)
  hull <- sf_points_m %>%
    st_union() %>%
    st_convex_hull()
  
  # 4. Calcular área
  area_m2 <- as.numeric(st_area(hull))
  area_km2 <- area_m2 / 1e6
  
  list(
    area_m2 = area_m2,
    area_km2 = area_km2
  )
}
# ********************************************************

#PATH_DATA <- "~/Documentos/GITHUB/RabiesLabPeru/vax_rabies/data/precision_vaccination/surveys_V1_30jun24.csv" # cluster3 y cluster 15
PATH_DATA <- "~/Descargas/surveys_2025-09-23.csv" # fase 2
#PATH_DATA <- "~/Descargas/surveys_fase3_vancan_2025.csv" # fase 3

CLUSTER <- "cluster3"
ANIO <- "2025"
PATH_RESULT <- "~/Documentos/GITHUB/R/mapas/SURVEY_VANCAN/results/"


surveys <- read.csv(PATH_DATA)
# Eliminar datos ingresados por "test.."
surveys <- surveys %>%
  filter(!grepl("test", USER_APP, ignore.case = TRUE))

surveys$DATE <- as.POSIXct(
  surveys$DATE,
  format = "%Y-%m-%dT%H:%M:%OS",
  tz = "UTC"
)
surveys <- surveys[order(surveys$DATE), ]

surveys_cluster <- surveys[surveys$CLUSTER==CLUSTER,]
surveys_cluster_anio <- surveys_cluster[format(surveys_cluster$DATE, "%Y") == ANIO, ]

surveys_cluster_anio <- surveys_cluster_anio %>%
  mutate(
    LAT  = as.numeric(LAT),
    LONG = as.numeric(LONG),
    DATE = as.POSIXct(DATE, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  ) %>%
  filter(!is.na(LAT), !is.na(LONG), !is.na(DATE))

# Usuario unicos
users <- unique(surveys_cluster_anio$USER_APP)

pal_global <- colorNumeric(
  palette = viridis(256),
  domain = c(0, 1)
)

m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron)

########
# Adicionar cuadras de la ciudad
#-----------------------------
pol_raw <- read.csv(
  "~/Descargas/Mz_JLByR_08mar2024.csv",
  sep = ";",
  stringsAsFactors = FALSE
)

# Detectar inicio de polígono (lat y long vacíos)
pol_raw <- pol_raw %>%
  mutate(
    is_start = is.na(lat) | lat == ""
  )

# Crear ID interno acumulado
pol_raw <- pol_raw %>%
  mutate(poly_id = cumsum(is_start))

pol_vertices <- pol_raw %>%
  filter(!is_start) %>%
  mutate(
    lat = as.numeric(lat),
    long = as.numeric(long)
  )

pol_sf <- pol_vertices %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  group_by(ident, poly_id) %>%
  summarise(
    geometry = st_cast(st_combine(geometry), "POLYGON"),
    .groups = "drop"
  )
m <- m %>%
  addPolygons(
    data = pol_sf,
    group = "Polígonos CSV",
    fillColor = "gray",
    fillOpacity = 0.25,
    color = "gray",
    weight = 1
  )
#------------- FIN ADICIONAR POLIGONOS---------------

result_csv <- data.frame()
# =========================
# loop por usuario
# =========================
for (u in users) {
  
  df_user <- surveys_cluster_anio %>%
    filter(USER_APP == u) %>%
    arrange(DATE)
  
  # Si solo tiene un registro continua con el siguiente USER
  if (nrow(df_user) < 2) next
  
  # Poner en el csv el resultados de los kilometros recorridos y perros vacunados
  result_km <- calc_vaccinated_per_km(df_user)
  # Poner las columnas del rango de horas en que se vacunaron a los perros
  result_hours <- calc_vaccinated_by_hour_range(df_user)
  # Contabilizar el area cuadrada
  result_area <- calc_covered_area(df_user)
  
  # Guardar los resultados
  result_csv <- rbind(
    result_csv,
    data.frame(
      cluster = CLUSTER,
      user = u,
      total_distance_km = result_km$total_distance_km,
      total_vaccinated = result_km$total_vaccinated,
      vaccinated_per_km = result_km$vaccinated_per_km,
      `h_07_10` = result_hours$`07-10`,
      `h_10_13` = result_hours$`10-13`,
      `h_13_16` = result_hours$`13-16`,
      `h_16_19` = result_hours$`16-19`,
      OUT_RANGE = result_hours$OUT_RANGE,
      area_m2 = result_area$area_m2,
      area_km2 = result_area$area_km2,
      stringsAsFactors = FALSE
    )
  )
  
  # normalización temporal 0 → 1
  df_user <- df_user %>%
    mutate(t_norm = seq(0, 1, length.out = n()))
  
  # crear segmentos de línea
  segments <- lapply(1:(nrow(df_user) - 1), function(i) {
    data.frame(
      lng = c(df_user$LONG[i], df_user$LONG[i + 1]),
      lat = c(df_user$LAT[i],  df_user$LAT[i + 1]),
      t   = df_user$t_norm[i]
    )
  })
  
  # dibujar segmentos coloreados
  for (seg in segments) {
    m <- m %>%
      addPolylines(
        lng = seg$lng,
        lat = seg$lat,
        color = pal_global(seg$t),
        weight = 4,
        opacity = 1,
        group = u
      )
  }
  
  # color de puntos:
  # rojo si NUMBER_DOG_VACCINATED_2024 > 0
  # caso contrario: color temporal original
  point_colors <- ifelse(
    !is.na(df_user$NUMBER_DOG_VACCINATED_2024) &
      df_user$NUMBER_DOG_VACCINATED_2024 > 0,
    "red",
    pal_global(df_user$t_norm)
  )
  
  # puntos (misma paleta que la ruta)
  m <- m %>%
    addCircleMarkers(
      lng = df_user$LONG,
      lat = df_user$LAT,
      radius = 5,
      stroke = FALSE,
      fillOpacity = 0.9,
      color = point_colors,
      group = u
    )
}

m <- m %>%
  addLayersControl(
    overlayGroups = users,
    options = layersControlOptions(collapsed = FALSE)
  )

m <- m %>%
  addControl(
    html = '
    <div style="
      background: white;
      padding: 10px;
      border-radius: 6px;
      box-shadow: 0 0 5px rgba(0,0,0,0.3);
      font-size: 12px;
    ">
      <div style="font-weight: bold; margin-bottom: 6px;">
        Progresión temporal
      </div>

      <div style="
        width: 160px;
        height: 12px;
        background: linear-gradient(
          to right,
          #440154,
          #472c7a,
          #3b518b,
          #2c718e,
          #21908d,
          #27ad81,
          #5cc863,
          #aadc32,
          #fde725
        );
        margin-bottom: 6px;
      "></div>

      <div style="display: flex; justify-content: space-between;">
        <span>Inicio</span>
        <span>Medio</span>
        <span>Fin</span>
      </div>
    </div>
    ',
    position = "bottomright"
  )

m

# Guardar como HTML
NAME_RESULT <- paste0(PATH_RESULT, ANIO, "/ruta-flutter-", CLUSTER,"-viviendasvacunadas.html")
htmlwidgets::saveWidget(
  m,
  NAME_RESULT,
  selfcontained = TRUE
)



