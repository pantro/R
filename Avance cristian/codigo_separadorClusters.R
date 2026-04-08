#######################################################---
# SCRIPT ETL: EXTRACCIÓN Y FILTRADO EXACTO (2025 - Cluster 3)
#######################################################---
rm(list = ls())
options(warn = -1)

# 1. LIBRERÍAS NECESARIAS
if (!require("dplyr")) install.packages("dplyr")
if (!require("readxl")) install.packages("readxl")

library(dplyr)
library(readxl)

# 2. DEFINICIÓN DE RUTAS
ruta_input <- "D:/github_UPCH/R/R/data_vacunacion/surveys_2025-10-07.xlsx"

# Actualizamos la ruta de salida para que coincida con la realidad de los datos
ruta_out_dir <- "D:/github_UPCH/R/R/data_vacunacion/2025/JLBYR_2025"
archivo_out <- file.path(ruta_out_dir, "Data_JLBYR_Cluster03_2025.csv")

cat(">>> 1. Validando directorio de destino...\n")
if (!dir.exists(ruta_out_dir)) {
  dir.create(ruta_out_dir, recursive = TRUE)
}

cat(">>> 2. Leyendo la base de datos maestra (Excel)...\n")
df_maestro <- read_excel(ruta_input)

# Estandarizamos los nombres de las columnas a minúsculas
colnames(df_maestro) <- tolower(colnames(df_maestro))

cat(">>> 3. Aplicando filtros (Año: 2025 | Cluster: 3)...\n")

# --- LÓGICA DE FILTRADO ---
df_filtrado <- df_maestro %>%
  # 1. Filtramos buscando '2025' en la columna 'date'
  filter(grepl("2025", as.character(date))) %>%
  
  # 2. Filtramos el Cluster siendo flexibles con la escritura (cluster3, cluster03, etc.)
  filter(tolower(cluster) %in% c("cluster3", "cluster03", "cluster 3", "cluster 03", "3", "03"))

cat("    [OK] Total de encuestas aisladas para el Cluster 3 (2025):", nrow(df_filtrado), "\n")

# 4. EXPORTACIÓN DE DATOS
if(nrow(df_filtrado) > 0) {
  cat(">>> 4. Exportando a formato CSV...\n")
  write.csv2(df_filtrado, file = archivo_out, row.names = FALSE, fileEncoding = "UTF-8")
  cat("    [ÉXITO TOTAL] Base de datos limpia guardada en:\n    ", archivo_out, "\n")
} else {
  cat("    [ALERTA] El filtro arrojó 0 resultados. Revisa los datos.\n")
}