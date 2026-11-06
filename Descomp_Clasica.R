library(tidyverse)
library(lubridate)
library(tsibble)
library(slider)
library(here)


archivo <- here("Data.csv")
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


