library(readxl)
library(dplyr)
library(tsibble)

# 1. Leer el archivo Excel usando ruta relativa (para que funcione con Git)
df <- read_excel("Data/12. BDD_diciembre_2024_final_SM.xlsx", sheet = 2, col_types = "text")

# 2. Convertir la columna FECHA con el formato correcto de barras (día/mes/año)
df$FECHA <- as.Date(df$FECHA, format = "%d/%m/%Y")

# 2.1. Limpiar los registros que hayan quedado como NA en la fecha o provincia
df <- df %>% filter(!is.na(FECHA) & !is.na(PROVINCIA))

# 2.2. Agrupar por fecha y provincia sumando el total de siniestros diarios por provincia
df_agrupado <- df %>%
  group_by(FECHA, PROVINCIA) %>%
  summarise(total_siniestros = n(), .groups = "drop")

# 3. Crear el tsibble indexado por FECHA y con la clave PROVINCIA
ts_datos <- df_agrupado %>%
  as_tsibble(index = FECHA, key = PROVINCIA)

# 4. Ver el resultado
head(ts_datos)
