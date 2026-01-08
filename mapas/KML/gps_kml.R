library(sf)
library(dplyr)
library(leaflet)
library(viridis)
library(xml2)
library(lwgeom)

process_kml <- function(kml_path) {
  kml <- read_xml(kml_path)
  
  ns <- xml_ns(kml)
  
  # Extract times and coordinates
  track_times <- xml_find_all(kml, ".//kml:when", ns)
  
  # For elements in the gx namespace
  track_points <- xml_find_all(kml, ".//gx:coord", ns)
  
  times <- xml_text(track_times)
  coords <- xml_text(track_points)
  
  times_list <- strsplit(times, " ")
  coords_list <- strsplit(coords, " ")
  
  # Convert to matrix and then to data frame
  coords_matrix <- do.call(rbind, coords_list)
  times_matrix <- do.call(rbind, times_list)
  
  
  n_rows_coords = nrow(coords_matrix)
  n_rows_times = nrow(times_matrix)
  
  # Find the minimum number of rows
  min_rows = min(n_rows_coords, n_rows_times)
  
  # Truncate the matrices to have the same number of rows
  coords_matrix_adj = coords_matrix[1:min_rows, ]
  times_matrix_adj = times_matrix[1:min_rows, ]
  
  
  full_matrix <- cbind(coords_matrix_adj, times_matrix_adj)
  df <- as.data.frame(full_matrix, stringsAsFactors = FALSE)
  
  
  
  return(df) # Return the processed DataFrame
}

kml_dir <- "/home/pantro/Descargas/GPS VANCAN 2023 JLByR-selected"

kml_files <- c(
  "GPS-13  11-06-2023  GRUPO 25.kml",
  "GPS-14  11-06-2023  GRUPO 24.kml",
  "GPS-15  11-06-2023  GRUPO 09.kml",
  "GPS-17  10-06-2023  GRUPO 23.kml",
  "GPS-17  11-06-2023  GRUPO 23.kml",
  "GPS-18  10-06-2023  GRUPO 22.kml",
  "GPS-18  11-06-2023  GRUPO 22.kml",
  "GPS-18  24-06-2023  GRUPO 20.kml",
  "GPS-20  10-06-2023  GRUPO 25.kml",
  "GPS-25  17-06-2023  GRUPO 24.kml",
  "GPS-29  17-06-2023  GRUPO 25.kml"
)

kml_paths <- file.path(kml_dir, kml_files)

m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron)

for (i in seq_along(kml_paths)) {
  
  kml_file <- kml_paths[i]
  group_name <- kml_files[i]   # nombre que aparecerá en el checkbox
  
  data <- process_kml(kml_file)
  
  names(data) <- c("Longitude", "Latitude", "Altitude", "Time")
  
  data <- data %>%
    mutate(
      Longitude = as.numeric(Longitude),
      Latitude  = as.numeric(Latitude),
      Altitude  = as.numeric(Altitude),
      Time      = as.POSIXct(Time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )
  
  ruta_sf <- st_as_sf(
    data,
    coords = c("Longitude", "Latitude"),
    crs = 4326,
    remove = FALSE
  )
  
  segmentos <- ruta_sf %>%
    arrange(Time) %>%
    mutate(
      next_lon = lead(Longitude),
      next_lat = lead(Latitude)
    ) %>%
    filter(!is.na(next_lon)) %>%
    rowwise() %>%
    mutate(
      geometry = st_sfc(
        st_linestring(
          matrix(
            c(Longitude, Latitude,
              next_lon, next_lat),
            ncol = 2,
            byrow = TRUE
          )
        ),
        crs = 4326
      )
    ) %>%
    ungroup() %>%
    st_as_sf()
  
  pal <- colorNumeric(
    palette = viridis(256),
    domain  = as.numeric(ruta_sf$Time)
  )
  
  #--------------------------------------------------
  ### UNA SOLA LEYENDA
  pal_global <- colorNumeric(
    palette = viridis(256),
    domain = c(0, 1)
  )
  ruta_sf <- ruta_sf %>%
    mutate(
      time_norm = as.numeric(Time - min(Time)) /
        as.numeric(max(Time) - min(Time))
    )
  
  segmentos <- segmentos %>%
    mutate(
      time_norm = ruta_sf$time_norm[-nrow(ruta_sf)]
    )
  
  color = ~pal_global(time_norm)
  #--------------------------------------------------
  
  m <- m %>%
    addPolylines(
      data = segmentos,
      color = ~pal(as.numeric(Time)),
      weight = 4,
      opacity = 0.9,
      group = group_name
    ) %>%
    addCircleMarkers(
      data = ruta_sf,
      radius = 3,
      color = ~pal(as.numeric(Time)),
      stroke = FALSE,
      fillOpacity = 0.9,
      group = group_name
    )
}

m <- m %>%
  addLayersControl(
    overlayGroups = kml_files,
    options = layersControlOptions(
      collapsed = FALSE
    )
  ) %>%
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

# Guardar como HTML
htmlwidgets::saveWidget(
  m,
  "~/Documentos/GITHUB/R/mapas/KML/ruta_gps_JBYR.html",
  selfcontained = TRUE
)