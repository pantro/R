#######################################################---
# PASO 1: EXTRACCIÓN Y DESCARGA A CSV
# Extrae solo VP21, CLUSTER 15, 2025, P y DOG_ANOTHER_AREA
#######################################################---
rm(list = ls())
options(warn = -1)

library(readxl)
library(dplyr)

ruta_excel <- "D:/github_UPCH/R/R/data_vacunacion/2025/JLBYR_2025/surveys_2025-10-07.xlsx"
ruta_salida <- "D:/github_UPCH/R/R/data_vacunacion/2025/JLBYR_2025/Auditoria_VP21_C15.csv"

cat(">>> 1. Leyendo el archivo Excel pesado...\n")
df_raw <- read_excel(ruta_excel)
colnames(df_raw) <- tolower(colnames(df_raw))

cat(">>> 2. Aplicando filtros y calculando métricas...\n")
df_auditoria <- df_raw %>%
  mutate(
    cluster_std = toupper(gsub(" ", "", as.character(cluster))),
    user_std = toupper(trimws(as.character(user_app))),
    type_house_std = toupper(trimws(as.character(type_house))),
    
    number_dog_house = as.numeric(number_dog_house),
    number_dog_vaccinated_2024 = as.numeric(number_dog_vaccinated_2024),
    number_dog_vaccinated_sweep = as.numeric(number_dog_vaccinated_sweep),
    
    date_clean = as.POSIXct(as.character(date), format="%Y-%m-%dT%H:%M:%OS", tz="UTC"),
    anio_registro = format(date_clean, "%Y")
  ) %>%
  # 1er Filtro: Usuario, Cluster y Año
  filter(cluster_std == "CLUSTER15" & user_std == "VP21" & anio_registro == "2025") %>%
  
  # 2do Filtro Crítico: Solo viviendas "P" o "DOG_ANOTHER_AREA"
  filter(type_house_std %in% c("P", "DOG_ANOTHER_AREA")) %>%
  
  select(
    block_id, cluster_std, user_std, date, date_clean, lat, long, type_house_std,
    raise_dog_house, number_dog_house, number_dog_vaccinated_2024, number_dog_vaccinated_sweep
  ) %>%
  arrange(date_clean)

# Reemplazar NAs por ceros en las variables numéricas
df_auditoria <- df_auditoria %>%
  mutate(
    number_dog_house = ifelse(is.na(number_dog_house), 0, number_dog_house),
    number_dog_vaccinated_2024 = ifelse(is.na(number_dog_vaccinated_2024), 0, number_dog_vaccinated_2024),
    number_dog_vaccinated_sweep = ifelse(is.na(number_dog_vaccinated_sweep), 0, number_dog_vaccinated_sweep)
  )

# Guardar a CSV
write.csv(df_auditoria, file = ruta_salida, row.names = FALSE)

# Calcular métricas para imprimir
total_casas_p <- sum(df_auditoria$type_house_std == "P", na.rm = TRUE)
total_casas_otra <- sum(df_auditoria$type_house_std == "DOG_ANOTHER_AREA", na.rm = TRUE)
total_perros_casa <- sum(df_auditoria$number_dog_house, na.rm = TRUE)
total_vacunados_fijo <- sum(df_auditoria$number_dog_vaccinated_2024, na.rm = TRUE)
total_vacunados_barrido <- sum(df_auditoria$number_dog_vaccinated_sweep, na.rm = TRUE)
total_vacunados_general <- total_vacunados_fijo + total_vacunados_barrido

cat("\n==================================================\n")
cat("✅ EXTRACCIÓN EXITOSA Y GUARDADA EN CSV\n")
cat("==================================================\n")
cat("📊 Total de registros útiles:        ", nrow(df_auditoria), "\n")
cat("🏠 Registros tipo 'P':               ", total_casas_p, "\n")
cat("🏠 Registros 'DOG_ANOTHER_AREA':     ", total_casas_otra, "\n")
cat("🐕 Total perros habitando las casas: ", total_perros_casa, "\n")
cat("--------------------------------------------------\n")
cat("💉 Vacunados Punto Fijo (2024):      ", total_vacunados_fijo, "\n")
cat("💉 Vacunados Barrido (Sweep):        ", total_vacunados_barrido, "\n")
cat("📈 TOTAL VACUNADOS (Suma):           ", total_vacunados_general, "\n")
cat("==================================================\n")
cat("📁 Archivo guardado en:", ruta_salida, "\n")