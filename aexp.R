install.packages("ggtime")

library(ggtime)
library(ggplot2)
library(dplyr)
library(feasts)

#OBJETIVO:
#Realizar una análisis temporal de los siniestros de tránsito ocurridos en el Ecuador durante el año 2024.
#La finalidad es identificar patrones de comportamiento, estacionalidad, tendencias y diferencias 
#significativas en la incidencia de accidentes a lo largo del año tanto a escala nacional como por 
#provincia.

#GRAFICO I ***********************************************
#Evolución cronológica de la serie temporal diaria de siniestros de tránsito ( comparando provincias clave 
#como Pichincha, Guayas, Azuay y Chimborazo). Eje horizontal: Representa el tiempo transcurrido a lo largo 
#del año calendario 2024 (desde el 1 de enero hasta el 31 de diciembre).Eje vertical: Muestra el volumen o 
#recuento total de siniestros de tránsito registrados por día.

# Filtrar las provincias interesadas
provincias_principales <- ts_datos %>%
  filter(PROVINCIA %in% c("PICHINCHA", "GUAYAS", "AZUAY", "CHIMBORAZO"))

# Generar el gráfico comparativo
ggtime::autoplot(provincias_principales, total_siniestros) +
  labs(
    title = "Evolución diaria de siniestros de tránsito",
    subtitle = "Comparativa: Pichincha, Guayas, Azuay y Chimborazo (2024)",
    y = "Total de siniestros",
    x = "Fecha",
    color = "Provincia"
  ) +
  theme_minimal()


#GRAFICO II**********************************************
library(ggtime)
library(ggplot2)
library(dplyr)
library(feasts)
#Diagrama de subseries estacionales, diseñado para analizar el comportamiento del ciclo semanal en la 
#incidencia de siniestros de tránsito para la provincia de Pichincha durante el año 2024. Los datos de 
#todo el año se dividen y agrupan en siete minipaneles, correspondiendo cada uno a un día de la semana.
#Las líneas azules representan el promedio (media) de siniestros diarios

# 1. Filtrar una provincia específica y generar el gráfico de subseries
ts_datos %>%
  filter(PROVINCIA == "PICHINCHA") %>%
  gg_subseries(total_siniestros) +
  labs(
    title = "Gráfico de subseries estacionales de siniestros de tránsito",
    subtitle = "Provincia de Pichincha - Año 2024",
    y = "Total de siniestros diarios",
    x = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(), # Elimina el texto amontonado de abajo
    axis.ticks.x = element_blank() # Elimina las marcas del eje X
  )




# Revisor: Yajaira Toaquiza
# 1. ESTACIONARIEDAD
# No se observa evidencia de estacionariedad debil 
# especialmente en Pichincha.
# Los promedios cambian sistematicamente segun el dia
# de la semana, lo que sugiere que la media no es
# constante en el tiempo
#
# 2. TENDENCIA
# No se identifica una tendencia creciente o decreciente
# sostenida y claramente definida durante 2024.
# Las series presentan fluctuaciones frecuentes, sin
# una direccion general suficientemente evidente.

# 3. ESTACIONALIDAD
# Se identifica una estacionalidad semanal marcada.
# En Pichincha, los sabados presentan un promedio
# de siniestros superior al de los dias laborables.
# Este comportamiento evidencia un patron que se
# repite aproximadamente cada siete dias.
#
# 4. CICLOS
# No se identifican ciclos. Aunque la serie presenta 
# fluctuaciones, estas no evidencian movimientos de
# aumento y disminucion de duracion variable que
# permitan reconocer ciclos.
