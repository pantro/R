# DENSIDAD DE PERROS
# Autor: Gian Franco
# 05 feb 2026

#install.packages(c("sf", "dplyr", "ggplot2", "tmap"))
#install.packages("janitor")
library(sf)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(purrr)

# Variables globales
ANIO <- 2024
PATH_DATA <- "~/Descargas/post_vancan_limpio.csv"
PATH_MZ <- "/home/pantro/Descargas/Mz_Cluster-selected"

# Leer datos
survey <- read.csv(PATH_DATA)

# Convertimos la columna "total_can" a numeric para despues poder sumar esos datos
survey <- survey %>%
  mutate(
    total_can = as.numeric(total_can)
  ) %>%
  filter(!is.na(unicode_mz))

# Agrupar por manzana
dogs_mz <- survey %>%
  group_by(cluster, unicode_mz) %>%
  summarise(
    total_dogs = sum(total_can, na.rm = TRUE),
    viviendas = n(),
    .groups = "drop"
  )

# Elimino la columna cluster ya que despues tambien lo tengo de otra base de datos
dogs_mz$cluster <- NULL

# Leer los poligonos de las manzanas
files <- list.files(
  PATH_MZ,
  pattern = "\\.csv$",
  full.names = TRUE
)

# Funcion para detectar separador
detect_delim <- function(file) {
  header <- readLines(file, n = 1)
  
  if (grepl(";", header)) {
    return(";")
  } else if (grepl(",", header)) {
    return(",")
  } else if (grepl("\t", header)) {
    return("\t")
  } else {
    stop(paste("No se pudo detectar el delimitador en", basename(file)))
  }
}

# Funcion para procesar los CSVs
read_cluster_mz <- function(file) {
  
  # 1. cluster desde nombre del archivo
  cluster_id <- stringr::str_extract(basename(file), "Cluster_\\d+") %>%
    stringr::str_remove("Cluster_") %>%
    as.integer()
  
  # 2. detectar delimitador
  delim <- detect_delim(file)
  
  # 3. leer CSV
  df <- readr::read_delim(
    file,
    delim = delim,
    show_col_types = FALSE,
    trim_ws = TRUE
  ) %>%
    janitor::clean_names()
  
  # 4. validar columnas
  required_cols <- c("ident", "lat", "long")
  if (!all(required_cols %in% names(df))) {
    stop(
      paste(
        "Archivo sin columnas esperadas:",
        basename(file),
        "\nColumnas encontradas:",
        paste(names(df), collapse = ", ")
      )
    )
  }
  
  # 5. eliminar filas separadoras
  df <- df %>%
    mutate(
      lat = as.numeric(lat),
      long = as.numeric(long)
    ) %>%
    filter(!is.na(lat), !is.na(long)) %>%
    mutate(
      cluster = cluster_id,
      ident = as.character(ident)
    )
  
  # 6. puntos → sf
  pts <- st_as_sf(
    df,
    coords = c("long", "lat"),
    crs = 4326,
    remove = FALSE
  )
  
  # 7. puntos → polígonos por cuadra
  polys <- pts %>%
    group_by(cluster, ident) %>%
    summarise(
      geometry = st_combine(geometry),
      .groups = "drop"
    ) %>%
    mutate(
      geometry = st_cast(geometry, "POLYGON")
    )
  
  polys
}
#######

#read_cluster_mz <- function(file) {
#  
#  cluster_id <- stringr::str_extract(basename(file), "Cluster_\\d+") %>%
#    stringr::str_remove("Cluster_") %>%
#    as.integer()
#  
#  delim <- detect_delim(file)
#  
#  df <- readr::read_delim(
#    file,
#    delim = delim,
#    show_col_types = FALSE,
#    trim_ws = TRUE
#  ) %>%
#    janitor::clean_names() %>%
#    mutate(
#      lat = as.numeric(lat),
#      long = as.numeric(long)
#    ) %>%
#    filter(!is.na(lat), !is.na(long)) %>%
#    mutate(
#      cluster = cluster_id,
#      ident = as.character(ident)
#    )
#  
#  # 🔴 CONTAR PUNTOS POR CUADRA
#  counts <- df %>%
#    count(cluster, ident, name = "n_points")
#  
#  # quedarse solo con cuadras válidas (>= 3 puntos)
#  df_valid <- df %>%
#    inner_join(
#      counts %>% filter(n_points >= 3),
#      by = c("cluster", "ident")
#    )
#  
#  # si no hay cuadras válidas, salir limpio
#  if (nrow(df_valid) == 0) return(NULL)
#  
#  pts <- st_as_sf(
#    df_valid,
#    coords = c("long", "lat"),
#    crs = 4326,
#    remove = FALSE
#  )
#  
#  polys <- pts %>%
#    group_by(cluster, ident) %>%
#    summarise(
#      geometry = st_combine(geometry),
#      .groups = "drop"
#    ) %>%
#    mutate(
#      geometry = st_cast(geometry, "POLYGON")
#    )
#  
#  polys
#}

#survey_cl1 <- survey[survey$cluster=="Cluster 1",]

# Contruir todas las cuadras desde el archivo csv
manzanas_sf <- purrr::map_dfr(files, read_cluster_mz)

#Como revisar despues cuales cuadras fueron descartadas para verificar que sena ciertas
invalid_mz <- survey %>%
  distinct(unicode_mz) %>%
  anti_join(
    manzanas_sf %>% st_drop_geometry(),
    by = c("unicode_mz" = "ident")
  )

# Unir para crear el mapa
union_mzgps_numdogs <- manzanas_sf %>%
  left_join(
    dogs_mz,
    by = c("ident" = "unicode_mz")
  )

# Crear categorias para el mapa
mapa_final <- union_mzgps_numdogs %>%
  mutate(
    status = case_when(
      is.na(total_dogs) ~ "Not surveyed",
      total_dogs == 0   ~ "Surveyed – no dogs",
      total_dogs > 0    ~ "Surveyed – with dogs"
    ),
    densidad = ifelse(
      total_dogs > 0,
      total_dogs / as.numeric(st_area(st_transform(geometry, 32719))) * 1e6,
      NA
    )
  )

# Crear centroides solo para cuadras encuestadas
centroides <- mapa_final %>%
  filter(!is.na(total_dogs)) %>%
  st_point_on_surface()
# Los centroides se dibujaran en el mapa de un tamaño proporcional a los perros vacunados
centroides <- centroides %>%
  mutate(
    radius = scales::rescale(total_dogs, to = c(4, 20))
  )
centroides <- centroides %>%
  dplyr::mutate(
    label_html = paste0(
      "<div style='font-size:13px;'>",
      " ", ident, "<br>",
      "<b>Total Dogs:</b> ", total_dogs,
      "</div>"
    )
  )
# Total de cuadras del cluster
total_cuadras <- nrow(union_mzgps_numdogs)

# Cuadras en la muestra
cuadras_muestra <- nrow(centroides)

# Total de perros en la muestra
total_perros_muestra <- sum(centroides$total_dogs, na.rm = TRUE)

# Densidad de perros por cuadra (usando muestra)
densidad_perros_cuadra <- total_perros_muestra / cuadras_muestra

# Crear el panel informativo
info_panel <- paste0(
  "<div style='background:white; padding:10px; border-radius:8px;
   box-shadow:0 0 10px rgba(0,0,0,0.2); font-size:14px;'>
   
   Total de cuadras: <b>", total_cuadras, "</b><br>
   Cuadras en muestra: <b>", cuadras_muestra, "</b><br><br>
   
   <b>Densidad perros por cuadra (muestra)</b><br>
   Fórmula: Perros / Cuadras = ", total_perros_muestra, " / ", cuadras_muestra, "<br>
   Resultado: <b>", round(densidad_perros_cuadra, 3), "</b>
   
   </div>"
)


# Se dibujara un mapa donde se pintaran las cuadras de un color gris solo para tenerlo de referencia
# y los puntos dentro de las cuadras seran proporcionales a la cantidad de perros vacunados
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Todas las cuadras
  addPolygons(
    data = mapa_final,
    fillColor = "gray85",
    fillOpacity = 0.5,
    color = "white",
    weight = 0.5
  ) %>%
  
  # Círculos proporcionales
  addCircleMarkers(
    data = centroides,
    lng = ~st_coordinates(geometry)[,1],
    lat = ~st_coordinates(geometry)[,2],
    radius = ~sqrt(total_dogs) * 2,
    fillColor = "red",
    fillOpacity = 0.8,
    color = "white",
    weight = 1,
    label = ~lapply(label_html, htmltools::HTML),
    labelOptions = labelOptions(
      direction = "auto"
    )
  ) %>%
  
  # Panel informativo
  addControl(
    html = info_panel,
    position = "bottomright"
  )
