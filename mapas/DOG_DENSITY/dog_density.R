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
library(leaflet)
library(readxl) # Para leer archivo xlxs

# Variables globales
ANIO <- 2024
PATH_HABILITY <- "~/Descargas/Data_houses_and_habitability.xlsx"  #Año 2024
NUMBER_CLUSTER <- 1
NAME_MICRORED <- "MR_SAN_MARTIN_SOCABAYA"
PATH_DATA <- "~/Descargas/post_vancan_limpio.csv"
PATH_MZ <- "/home/pantro/Descargas/Mz_Cluster-selected"

PATH_MZ_CLUSTER <- "/home/pantro/Descargas/Mz_Cluster-selected/Cluster_01_Mz_16ene2026.csv"
PATH_HOUSE_CLUSTER = "~/Descargas/Points_cluster-selected/Point_Cluster_01_08mar2024.csv"

viviendas_cluster <- read.csv(PATH_HOUSE_CLUSTER, sep = ";")
# Total de viviendas en el cluster suponiendo que todas las viviendas participaron
#total_viviendas_cluster <- length(unique(viviendas_cluster$UNICODE))
# Total de viviendas en el cluster obtenidas por la habitabilidad de la localidad
houses_per_locality <- viviendas_cluster %>%
  mutate(
    name_locality = paste(P, D, L, sep = ".")
  ) %>%
  #distinct(name_locality, name_block) %>%  # evita duplicados
  count(name_locality, name = "n_houses") %>%
  select(name_locality, n_houses)
#---Conitnuo mas abajo por que necesito la habitabilidad

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

##---------------------------------------------------------- 
# HABITABILIDAD
locality_habitability <- read_excel(PATH_HABILITY,
                           sheet = NAME_MICRORED) %>%
  mutate(
    L = gsub("\\..*", "", L),
    Cluster = as.numeric(Cluster),
    name_locality = paste(P, D, L, sep = ".")
  ) %>%
  filter(Cluster == NUMBER_CLUSTER)%>%
  select(name_locality, Habitability)

# Rutas de viviendas, cuadras y localidades
PATH_POINTS <- "~/Descargas/Point_Cluster_01_10ene2024.kml" #"SPATIAL_DATA_AQP/Points_AQP/Points_cluster/Point_Cluster_01_10ene2024.kml"
PATH_BLOCKS <- "~/Descargas/Cluster_01_Mz_16ene2026.kml" #"SPATIAL_DATA_AQP/Blocks_AQP/Mz_Cluster/Cluster_01_Mz_16ene2026.kml"
# Leer
houses <- st_read(PATH_POINTS, quiet = TRUE) %>%
  select(Name, geometry) %>%
  rename(name_house = Name)
blocks <- st_read(PATH_BLOCKS, quiet = TRUE) %>%
  select(Name, geometry) %>%
  rename(name_block = Name)
# Validar geometrías solo utilizaremos para cuadras y viviendas
sf_use_s2(FALSE)
houses <- st_make_valid(houses)
blocks <- st_make_valid(blocks)
# 2. reproyectar a UTM 19S (Arequipa)
blocks_proj <- st_transform(blocks, 32719)
houses_proj <- st_transform(houses, 32719)
# limpiar geometrías problemáticas
blocks_proj <- st_buffer(blocks_proj, 0)
# Asignar viviendas a cuadras
houses_blocks <- st_join(
  houses_proj,
  blocks_proj,
  join = st_within,
  left = TRUE
)
# Viviendas sin cuadra
houses_outside <- houses_blocks %>%
  filter(is.na(name_block))  # elimine 3 viviendas
# Viviendas por cuadra
houses_per_block <- houses_blocks %>%
  st_drop_geometry() %>%
  count(name_block, name = "n_houses")
# Para encontrar Cuadras por localidad vamos a utilizar el nombre de las cuadras
blocks_per_locality <- blocks %>%
  st_drop_geometry() %>%
  mutate(
    name_locality = str_remove(name_block, "-.*$")
  ) %>%
  distinct(name_locality, name_block) %>%  # evita duplicados
  count(name_locality, name = "n_blocks") %>%
  arrange(desc(n_blocks))

##----------------------------------------------------------

# Leer los poligonos de las manzanas
#files <- list.files(
#  PATH_MZ,
#  pattern = "\\.csv$",
#  full.names = TRUE
#)
files <- PATH_MZ_CLUSTER

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

## Cantidad de viviendas en la muestra
# Viviendas en muestra
total_viviendas_muestra <- union_mzgps_numdogs %>%
  filter(!is.na(total_dogs)) %>%
  summarise(total = sum(viviendas, na.rm = TRUE)) %>%
  pull(total)

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

## Crear poligono total del cluster
# Corrige si hay poligonos que se chocan o si estan mal cerrados
manzanas_sf <- manzanas_sf %>%
  st_make_valid() %>%
  st_transform(32719)
# Polígono que cubre todo el cluster
cluster_poly <- st_combine(manzanas_sf) %>%
  st_union()
# Calcular área en km2 (UTM 32719 como ya usas)
area_cluster_km2 <- as.numeric(
  st_area(st_transform(cluster_poly, 32719))
) / 1e6

# Total de cuadras del cluster
#total_cuadras_cluster <- nrow(union_mzgps_numdogs) # Esto era pensando que el 100% de las cuadras de las localidades participaba
# Obteniendo la cantidad de cuadra por localidad con respecto a la habitabilidad
total_cuadras_cluster <- blocks_per_locality %>%
  left_join(locality_habitability, by = "name_locality") %>%
  mutate(
    habited_blocks = round(n_blocks * Habitability / 100)
  ) %>%
  summarise(total = sum(habited_blocks, na.rm = TRUE)) %>%
  pull(total)

# Total de viviendas en el cluster
total_viviendas_cluster <- houses_per_locality %>%
  left_join(locality_habitability, by = "name_locality") %>%
  mutate(
    habited_houses = round(n_houses * Habitability / 100)
  ) %>%
  summarise(total = sum(habited_houses, na.rm = TRUE)) %>%
  pull(total)

# Cuadras en la muestra
cuadras_muestra <- nrow(centroides)

# Total de perros en la muestra
total_perros_muestra <- sum(centroides$total_dogs, na.rm = TRUE)

## Estimacion por regla de 3 simple
# Estimación total perros en cluster (expansión por cuadras)
perros_estimados_cuadras <-
  (total_perros_muestra / cuadras_muestra) * total_cuadras_cluster
# Densidad km2 usando expansión por cuadras
densidad_km2_cuadras <-
  perros_estimados_cuadras / area_cluster_km2

## Regla de 3 para calcular la cantidad de perros por vivienda
# Estimación total perros usando viviendas
perros_estimados_viviendas <-
  (total_perros_muestra / total_viviendas_muestra) *
  total_viviendas_cluster
# Densidad km2 usando expansión por viviendas
densidad_km2_viviendas <-
  perros_estimados_viviendas / area_cluster_km2


# Densidad de perros por cuadra (usando muestra)
densidad_perros_cuadra <- total_perros_muestra / cuadras_muestra

# Crear el panel informativo
info_panel <- paste0(
  "<div style='background:white; padding:10px; border-radius:8px;
   box-shadow:0 0 10px rgba(0,0,0,0.2); font-size:14px;'>
   
   Área del cluster (km²): <b>", round(area_cluster_km2, 3), "</b><br>
   Total cuadras del cluster: <b>", total_cuadras_cluster, "</b><br>
   Cuadras muestra: <b>", cuadras_muestra, "</b><br>
   Total perros muestra: <b>", total_perros_muestra, "</b><br>
   Total viviendas muestra: <b>", total_viviendas_muestra, "</b><br>
   Total viviendas cluster: <b>", total_viviendas_cluster, "</b><br><br>
   
   <b>Estimación por cuadras</b><br>
   Perros estimados: <b> (", total_perros_muestra, " / ", cuadras_muestra, ") × ", total_cuadras_cluster,"=", round(perros_estimados_cuadras,0), "</b><br>
   Densidad km²: <b> (", round(perros_estimados_cuadras,0), " / ", round(area_cluster_km2,2),") = ", round(densidad_km2_cuadras,1), "</b><br><br>
   
   <b>Estimación por viviendas</b><br>
   Perros estimados: <b>(", total_perros_muestra, " / ", total_viviendas_muestra, ") × ", total_viviendas_cluster, "=", round(perros_estimados_viviendas,0), "</b><br>
   Densidad km²: <b> (", round(perros_estimados_viviendas,0), " / ", round(area_cluster_km2,2),") = ", round(densidad_km2_viviendas,1), "</b>
  
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



#==============================================================================
#==============================================================================
#           ----            PERROS DEAMBULANTES        ---------------
#=============================================================================
#==============================================================================
# Obligar a las columnas necesarias a ser numericas
survey <- survey %>%
  mutate(across(
    c(vive_calle, casa_calle, salen_solos_calle, can_escape, vive_casa, total_can),
    ~as.numeric(.)
  ))

# Crear variable de perros deambulantes
survey <- survey %>%
  mutate(
    perros_deambulantes =
      rowSums(across(
        c(vive_calle, casa_calle, salen_solos_calle, can_escape)
      ), na.rm = TRUE)
  )

# Verificar la consistencia de los datos de esas columnas
survey <- survey %>%
  mutate(
    check_total =
      perros_deambulantes + vive_casa
  )
survey %>%
  filter(!is.na(total_can) & check_total != total_can)

# Ahora agregamos el poligono de la cuadra
deamb_mz <- survey %>%
  filter(!is.na(unicode_mz)) %>%
  group_by(unicode_mz) %>%
  summarise(
    total_deamb = sum(perros_deambulantes, na.rm = TRUE),
    viviendas = n(),
    .groups = "drop"
  )

# Unir con tu la variable "mapa_final"
mapa_deamb <- mapa_final %>%
  select(ident, geometry) %>%   # solo lo necesario
  left_join(
    deamb_mz,
    by = c("ident" = "unicode_mz")
  )

## Variables para poner en el cuadro de texto en el mapa
cuadras_muestra_deamb <- sum(!is.na(mapa_deamb$total_deamb))
total_deamb_muestra <- sum(mapa_deamb$total_deamb, na.rm = TRUE)
# Expansión por cuadras
deamb_estimados_cuadras <-
  (total_deamb_muestra / cuadras_muestra_deamb) * total_cuadras_cluster
densidad_km2_deamb_cuadras <-
  deamb_estimados_cuadras / area_cluster_km2

# Expansión por viviendas
total_viviendas_muestra_deamb <- sum(mapa_deamb$viviendas, na.rm = TRUE)
deamb_estimados_viviendas <-
  (total_deamb_muestra / total_viviendas_muestra_deamb) *
  total_viviendas_cluster
densidad_km2_deamb_viviendas <-
  deamb_estimados_viviendas / area_cluster_km2

# Crear punto dentro de la cuadra y poner un punto proporcional a la cantidad de perros
centroides_deamb <- mapa_deamb %>%
  filter(!is.na(total_deamb) & total_deamb > 0) %>%
  st_point_on_surface() %>%
  mutate(
    radius = scales::rescale(total_deamb, to = c(4, 20)),
    label_html = paste0(
      "<div style='font-size:13px;'>",
      ident, "<br>",
      "<b>Stray Dogs:</b> ", total_deamb,
      "</div>"
    )
  )

# Panel informativo
info_panel_deamb <- paste0(
  "<div style='background:white; padding:10px; border-radius:8px;
   box-shadow:0 0 10px rgba(0,0,0,0.2); font-size:14px;'>",
  
  "Área del cluster (km²): <b>", round(area_cluster_km2, 3), "</b><br>",
  "Total cuadras del cluster: <b>", total_cuadras_cluster, "</b><br>",
  "Cuadras muestra: <b>", cuadras_muestra_deamb, "</b><br>",
  "Total perros deambulantes muestra: <b>", total_deamb_muestra, "</b><br>",
  "Total viviendas del cluster: <b>", total_viviendas_cluster, "</b><br><br>",
  
  "<b>Estimación por cuadras</b><br>",
  "Perros estimados: <b>",
  round(deamb_estimados_cuadras,0), "</b><br>",
  "Densidad km²: <b>",
  round(densidad_km2_deamb_cuadras,1), "</b><br><br>",
  
  "<b>Estimación por viviendas</b><br>",
  "Perros estimados: <b>",
  round(deamb_estimados_viviendas,0), "</b><br>",
  "Densidad km²: <b>",
  round(densidad_km2_deamb_viviendas,1), "</b>",
  
  "</div>"
)

# Mapa de perros deambulantes
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  addPolygons(
    data = mapa_final,
    fillColor = "gray85",
    fillOpacity = 0.5,
    color = "white",
    weight = 0.5
  ) %>%
  
  addCircleMarkers(
    data = centroides_deamb,
    lng = ~st_coordinates(geometry)[,1],
    lat = ~st_coordinates(geometry)[,2],
    radius = ~sqrt(total_deamb) * 2,
    fillColor = "purple",
    fillOpacity = 0.85,
    color = "white",
    weight = 1,
    label = ~lapply(label_html, htmltools::HTML),
    labelOptions = labelOptions(direction = "auto")
  ) %>%
  
  addControl(
    html = info_panel_deamb,
    position = "bottomright"
  )

