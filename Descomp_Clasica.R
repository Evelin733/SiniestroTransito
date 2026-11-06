library(tidyverse)
library(lubridate)
library(tsibble)
library(slider)
library(here)


archivo <- here("Data", "Data.csv")
sep <- if (grepl("\t", readLines(archivo, n = 1))) "\t" else ","

ts_datos <- read_delim(archivo, delim = sep, show_col_types = FALSE) |>
  mutate(FECHA = as.Date(FECHA), PROVINCIA = as.factor(PROVINCIA)) |>
  as_tsibble(index = FECHA, key = PROVINCIA) |>
  fill_gaps(total_siniestros = 0)

# Para Pichincha
prov <- "PICHINCHA"
d <- ts_datos |> filter(PROVINCIA == prov) |> as_tibble()

# Descomposición manual clasica aditiva periodo 7
d <- d |>
  mutate(
    Tt      = slide_dbl(total_siniestros, mean,
                        .before = 3, .after = 3, .complete = TRUE),
    yt_Tt   = total_siniestros - Tt,
    dia_sem = wday(FECHA, week_start = 1)
  )

St_tab <- d |>
  group_by(dia_sem) |>
  summarise(S = mean(yt_Tt, na.rm = TRUE), .groups = "drop") |>
  mutate(St = S - mean(S)) |>
  select(dia_sem, St)

d <- d |>
  left_join(St_tab, by = "dia_sem") |>
  mutate(Rt = yt_Tt - St)

#Grafica 1: serie original y tendencia
ggplot(d, aes(FECHA)) +
  geom_line(aes(y = total_siniestros), colour = "gray70") +
  geom_line(aes(y = Tt), colour = "#D55E00", linewidth = 1) +
  labs(x = NULL, y = "Siniestros por día",
       title = paste("Serie original y tendencia (MA 7) -", prov))

#Grafica 2: componente estacional
ggplot(d, aes(FECHA, St)) +
  geom_line(colour = "#0072B2") +
  labs(x = NULL, y = "St",
       title = paste("Componente estacional semanal -", prov))

#Grafica 3: residuo
ggplot(d, aes(FECHA, Rt)) +
  geom_line(colour = "#009E73") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = NULL, y = "Rt",
       title = paste("Componente aleatorio (residuo) -", prov))

# AUDITORÍA

# 1. Comprobar que la estacionalidad aditiva sume cero (o cercano a cero)
suma_estacional <- d |> filter(!is.na(St)) |> pull(St) |> sum()
print(paste("Suma del componente estacional (debe ser aprox 0):", round(suma_estacional, 10)))

# 2. Comprobar que el modelo reconstruya bien la serie original (Tt + St + Rt == total_siniestros)
d_audit <- d |> mutate(reconstruido = Tt + St + Rt)
max_error <- max(abs(d_audit$total_siniestros - d_audit$reconstruido), na.rm = TRUE)
print(paste("Máximo error de reconstrucción del modelo:", max_error))


#Interpretación gráficas
#Gráfica 1 (Serie original y tendencia - MA 7): La línea gris muestra los siniestros diarios en Pichincha 
#(con mucha variación) y la línea naranja gruesa muestra la tendencia suavizada mediante la media móvil 
#de orden 7. Se ve claramente cómo sube y baja a lo largo de los meses de 2024.

#Gráfica 2 (Componente estacional semanal): Muestra el patrón repetitivo de los días de la semana ($m = 7$). 
#Como es un modelo aditivo, los valles oscilan simétricamente alrededor del cero, reflejando qué días de la 
#semana hay más o menos siniestros de tránsito en Pichincha. 


#Gráfica 3 (Componente aleatorio o residuo): Muestra el ruido puro fluctuando aleatoriamente alrededor de la 
#línea punteada de cero.



