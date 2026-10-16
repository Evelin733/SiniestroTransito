library(readxl)
library(dplyr)
library(purrr)

#Leer la carpeta donde están las bases
ruta_carpeta <- "C:/Users/johan/OneDrive/Documents/SERIES TEMPORALES/DATOS_ST/Limpieza"

#Leer las hojas donde están los datos
archivos <- list.files(path = ruta_carpeta, pattern = "\\.xlsx$", full.names = TRUE)
print(archivos)

configuracion_hojas <- data.frame(
  archivo_path = archivos,
  nombre_hoja = case_when(
    grepl("2024", archivos) ~ "BDD_2024",  
    grepl("2025", archivos) ~ "BDD_2025",  
    grepl("2026", archivos) ~ "BDD_2026",  
    TRUE ~ "Hoja1"                         
  )
)

#
leer_con_nombre_hoja <- function(path, hoja) {
  message("Leyendo archivo: ", basename(path), " | Usando hoja: ", hoja)
  read_excel(path, sheet = hoja, col_types = "text")
}

#
df_total <- pmap_df(
  list(configuracion_hojas$archivo_path, configuracion_hojas$nombre_hoja), 
  ~ leer_con_nombre_hoja(..1, ..2)
)

#Ver encabezado
head(df_total)
colnames(df_total)

library(dplyr)
library(tsibble)
library(lubridate)

# Convertir la columna FECHA 
df_limpio <- df_total %>%
  mutate(
    FECHA = case_when(
      grepl("/", FECHA) ~ as.Date(FECHA, format = "%d/%m/%Y"),
      grepl("-", FECHA) ~ as.Date(FECHA, format = "%Y-%m-%d"),
      TRUE ~ as.Date(as.numeric(FECHA), origin = "1899-12-30")
    )
  ) %>%
  filter(!is.na(FECHA)) # Esto elimina de forma segura los registros sin fecha válida

df_diario_todas <- df_limpio %>%
  filter(!is.na(PROVINCIA)) %>% 
  group_by(FECHA, PROVINCIA) %>%
  summarise(total_siniestros = n(), .groups = "drop") %>%
  arrange(FECHA)

# Convertir a tsibble multivariante para todas las provincias
ts_todas_provincias <- df_diario_todas %>%
  as_tsibble(
    index = FECHA,
    key = PROVINCIA
  )

print(ts_todas_provincias)
