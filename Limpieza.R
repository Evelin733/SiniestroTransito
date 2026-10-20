library(readxl)
library(dplyr)
library(tsibble)
library(lubridate)

# 1. Leer el archivo completo
df <- read_excel("Data/12. BDD_diciembre_2024_final_SM.xlsx", sheet = "BDD_2024")
#df <- read_excel(file.choose(), sheet = "BDD_2024")


# 2. Conversión de fecha (tanto texto como fechas numéricas de Excel)
if(is.numeric(df$FECHA)) {
  df$FECHA <- as.Date(df$FECHA, origin = "1899-12-30")
} else {
  # Si es texto, se convierte con parse_date_time de manera flexible
  df$FECHA <- parse_date_time(as.character(df$FECHA), orders = c("dmy", "ymd", "mdy", "db", "Ymd")) %>% as.Date()
}

# 4. Filtrar 
df_limpio <- df %>% 
  dplyr::filter(!is.na(FECHA), !is.na(PROVINCIA), PROVINCIA != "", PROVINCIA != "NA")

# 5. Ver cuántas filas quedaron tras la limpieza
print(paste("Filas después de limpiar:", nrow(df_limpio)))

# 6. Agrupar por fecha y provincia
df_agrupado <- df_limpio %>%
  group_by(FECHA, PROVINCIA) %>%
  summarise(total_siniestros = n(), .groups = "drop")

# 7. Crear el tsibble para TODAS las provincias
ts_datos <- df_agrupado %>%
  mutate(PROVINCIA = as.factor(PROVINCIA)) %>%
  as_tsibble(index = FECHA, key = PROVINCIA) %>%
  fill_gaps(total_siniestros = 0)

# 8. Comprobación final
#n_keys(ts_datos)
#range(ts_datos$FECHA)

write.csv(ts_datos, "Data/Data.csv", row.names = FALSE)