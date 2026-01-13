library(dplyr)
library(sf)
library(leaflet)
library(viridis)

#PATH_DATA <- "~/Documentos/GITHUB/RabiesLabPeru/vax_rabies/data/precision_vaccination/surveys_V1_30jun24.csv" # cluster3 y cluster 15
PATH_DATA <- "~/Descargas/surveys_2025-09-23.csv" # fase 2
#PATH_DATA <- "~/Descargas/surveys_fase3_vancan_2025.csv" # fase 3

CLUSTER <- "cluster30"
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
# =========================
# loop por usuario
# =========================
for (u in users) {
  
  df_user <- surveys_cluster_anio %>%
    filter(USER_APP == u) %>%
    arrange(DATE)
  
  # seguridad
  if (nrow(df_user) < 2) next
  
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
  
  # puntos (misma paleta que la ruta)
  m <- m %>%
    addCircleMarkers(
      lng = df_user$LONG,
      lat = df_user$LAT,
      radius = 5,
      stroke = FALSE,
      fillOpacity = 0.9,
      color = pal_global(df_user$t_norm),
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
NAME_RESULT <- paste0(PATH_RESULT, ANIO, "/ruta-flutter-", CLUSTER,".html")
htmlwidgets::saveWidget(
  m,
  NAME_RESULT,
  selfcontained = TRUE
)
