library(readxl)
library(tidyverse)
library(lubridate)
library(tsibble)
library(slider)
library(feasts)
library(here)

# 1. Leer el archivo Excel original y limpiar
df <- read_excel("Data/12. BDD_diciembre_2024_final_SM.xlsx", sheet = "BDD_2024")

if(is.numeric(df$FECHA)) {
  df$FECHA <- as.Date(df$FECHA, origin = "1899-12-30")
} else {
  df$FECHA <- parse_date_time(as.character(df$FECHA), orders = c("dmy", "ymd", "mdy", "db", "Ymd")) %>% as.Date()
}

df_limpio <- df %>% 
  filter(!is.na(FECHA), !is.na(PROVINCIA), PROVINCIA != "", PROVINCIA != "NA")

# Agrupar por fecha y provincia
df_agrupado <- df_limpio %>%
  group_by(FECHA, PROVINCIA) %>%
  summarise(total_siniestros = n(), .groups = "drop")

# tsibble para todas las provincias
ts_datos <- df_agrupado %>%
  mutate(PROVINCIA = as.factor(PROVINCIA)) %>%
  as_tsibble(index = FECHA, key = PROVINCIA) %>%
  fill_gaps(total_siniestros = 0)

# Filtrar Pichincha
prov <- "PICHINCHA"
d <- ts_datos %>% filter(PROVINCIA == prov) %>% as_tibble()

# Cuando hay días con 0 siniestros, le sumamos un valor mínimo (0.001) 
# porque la división para 0 en el modelo multiplicativo da error
d <- d %>% mutate(y = if_else(total_siniestros == 0, 0.001, as.numeric(total_siniestros)))

# DESCOMPOSICIÓN MULTIPLICATIVA MANUAL (m = 7 para días)
# Modelo: yt = Tt * St * Rt

# Paso 1: Tendencia (Tt) con MA de orden 7 (3 antes y 3 después por ser m=7 impar)
d <- d %>%
  mutate(
    Tt      = slide_dbl(y, mean, .before = 3, .after = 3, .complete = TRUE),
    yt_Tt   = y / Tt,                             
    dia_sem = wday(FECHA, week_start = 1)
  )

# Paso 3: Índices estacionales por día de la semana
St_tab <- d %>%
  group_by(dia_sem) %>%
  summarise(S = mean(yt_Tt, na.rm = TRUE), .groups = "drop") %>%
  mutate(St = S * 7 / sum(S)) %>%                   
  select(dia_sem, St)

d <- d %>% left_join(St_tab, by = "dia_sem")

# Paso 4: Residuo (Rt) despejando por división
d <- d %>% mutate(Rt = y / (Tt * St))

# --- GRÁFICAS MANUALES ---

# Gráfica 1: Serie original y tendencia
ggplot(d, aes(FECHA)) +
  geom_line(aes(y = y), colour = "gray70") +
  geom_line(aes(y = Tt), colour = "#D55E00", linewidth = 1) +
  labs(x = NULL, y = "Siniestros por día",
       title = paste("Serie original y tendencia (MA 7) -", prov))

# Gráfica 2: Componente estacional (centrado alrededor de 1)
ggplot(d, aes(FECHA, St)) +
  geom_line(colour = "#0072B2") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(x = NULL, y = "St",
       title = paste("Componente estacional multiplicativo -", prov))

# Gráfica 3: Residuo (centrado alrededor de 1)
ggplot(d, aes(FECHA, Rt)) +
  geom_line(colour = "#009E73") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(x = NULL, y = "Rt",
       title = paste("Componente aleatorio multiplicativo -", prov))


# Directo
d_tsibble <- ts_datos %>% filter(PROVINCIA == prov)

modelo_comp <- d_tsibble %>%
  model(classical_decomposition(total_siniestros ~ season(7), type = "multiplicative")) %>%
  components()

# Gráfico del libro
modelo_comp %>%
  autoplot() +
  labs(title = paste("Descomposición clásica multiplicativa -", prov))