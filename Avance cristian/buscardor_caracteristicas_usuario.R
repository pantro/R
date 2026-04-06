#######################################################---
# PASO 1: EXTRACCIÓN Y AUDITORÍA ESPECÍFICA (22 JUNIO)
# Filtra VP21, CLUSTER 15, Año 2025, Día 22
#######################################################---
rm(list = ls())
options(warn = -1)

library(readxl)
library(dplyr)

ruta_excel <- "D:/github_UPCH/R/R/data_vacunacion/2025/JLBYR_2025/surveys_2025-10-07.xlsx"
ruta_salida <- "D:/github_UPCH/R/R/data_vacunacion/2025/JLBYR_2025/Auditoria_VP21_C15_22Jun.csv"

cat(">>> 1. Leyendo el archivo Excel...\n")
df_raw <- read_excel(ruta_excel)
colnames(df_raw) <- tolower(colnames(df_raw))

cat(">>> 2. Aplicando filtros temporales y espaciales...\n")

df_auditoria <- df_raw %>%
  mutate(
    cluster_std = toupper(gsub(" ", "", as.character(cluster))),
    user_std = toupper(trimws(as.character(user_app))),
    type_house_std = toupper(trimws(as.character(type_house))),
    
    # Asegurar formato numérico
    number_dog_house = as.numeric(number_dog_house),
    number_dog_vaccinated_2024 = as.numeric(number_dog_vaccinated_2024),
    number_dog_vaccinated_sweep = as.numeric(number_dog_vaccinated_sweep),
    
    # Procesar fecha para Lima
    date_clean = as.POSIXct(as.character(date), format="%Y-%m-%dT%H:%M:%OS", tz="America/Lima"),
    fecha_solo = as.Date(date_clean)
  ) %>%
  # FILTRO MAESTRO: Usuario, Cluster y específicamente el día 22
  filter(cluster_std == "CLUSTER15" & user_std == "VP21") %>%
  filter(fecha_solo == as.Date("2025-06-22")) %>%
  
  # Limpiar NAs inmediatamente para el cálculo
  mutate(
    number_dog_house = ifelse(is.na(number_dog_house), 0, number_dog_house),
    number_dog_vaccinated_2024 = ifelse(is.na(number_dog_vaccinated_2024), 0, number_dog_vaccinated_2024),
    number_dog_vaccinated_sweep = ifelse(is.na(number_dog_vaccinated_sweep), 0, number_dog_vaccinated_sweep)
  ) %>%
  select(
    block_id, user_std, date_clean, type_house_std,
    raise_dog_house, number_dog_house, 
    number_dog_vaccinated_2024, number_dog_vaccinated_sweep
  ) %>%
  arrange(date_clean)

# Guardar a CSV el resultado del día 22
write.csv(df_auditoria, file = ruta_salida, row.names = FALSE)

# --- ANÁLISIS DE RESULTADOS ---

# 1. Conteo dinámico de cada tipo de casa
resumen_casas <- df_auditoria %>% 
  group_by(type_house_std) %>% 
  tally(name = "Cantidad")

# 2. Totales de perros
total_perros_casa <- sum(df_auditoria$number_dog_house, na.rm = TRUE)
total_vac_2024 <- sum(df_auditoria$number_dog_vaccinated_2024, na.rm = TRUE)
total_vac_sweep <- sum(df_auditoria$number_dog_vaccinated_sweep, na.rm = TRUE)

cat("\n==================================================\n")
cat("✅ REPORTE DE AUDITORÍA: DOMINGO 22 DE JUNIO\n")
cat("==================================================\n")
cat("📊 Total de registros encontrados:  ", nrow(df_auditoria), "\n")
cat("--------------------------------------------------\n")
cat("🏠 DESGLOSE POR TIPO DE VIVIENDA:\n")
print(as.data.frame(resumen_casas))
cat("--------------------------------------------------\n")
cat("🐕 ANÁLISIS DE CANES:\n")
cat("   Total perros registrados:      ", total_perros_casa, "\n")
cat("   Vacunados Punto Fijo (2024):   ", total_vac_2024, "\n")
cat("   Vacunados Barrido (Sweep):      ", total_vac_sweep, "\n")
cat("📈 TOTAL VACUNADOS EL DÍA 22:     ", total_vac_2024 + total_vac_sweep, "\n")
cat("==================================================\n")
cat("📁 Archivo filtrado guardado en:", ruta_salida, "\n")