# install.packages("tidyverse")

library(tidyverse)
library(lubridate)

# EJERCICIO: ¿Qué paquetes carga el tidyverse?


# PARTE 1 -----------------------------------------------------------------

# Importar datos con readr ------------------------------------------------

?read_csv # para ver la documentación

capitulos <- read_csv("capitulos_rladies.csv")
capitulos <- read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-26/capitulos_rladies.csv")

write_tsv(capitulos, "capitulos_rladies.tsv")
write_csv2(capitulos, file = "capitulos2.csv")

# ¿Cómo podemos tener una visión general de nuestro tablero?
capitulos
glimpse(capitulos)
View(capitulos)


# Manipular datos con dplyr -----------------------------------------------

# verbo(tablero, {argumentos})

# seleccionar columnas
select(capitulos, capitulo, ciudad, pais)
select(capitulos, -creacion)              # quiero ignorar la fecha
select(capitulos, capitulo, latitud:pais) # quiero ver solo variables geográficas
select(capitulos, starts_with("c"))       # columnas que empiezan por "c"

# EJERCICIO: ¿Cómo eliminar las columnas que terminan por "tud"?
select(capitulos, ends_with("tud"))
select(capitulos, -ends_with("tud"))
select(capitulos, -contains("tud"))

columnas_a_eliminar <- c("longitud", "latitud")
select(capitulos, -all_of(columnas_a_eliminar))


# filtrar filas según condiciones
filter(capitulos, pais == "ES")
filter(capitulos, pais %in% c("ES", "CO", "FR"))
filter(capitulos, miembros > 1000)
filter(capitulos, miembros > 1000 & pais == "ES")
filter(capitulos, miembros > 1000 | pais == "ES")
filter(capitulos, miembros > 1000, pais == "ES") # "," = "&"


# crear o modificar columnas
View(mutate(capitulos, miembros_por_cien = miembros * 100))
View(mutate(capitulos, miembros_total = sum(miembros)))
View(mutate(capitulos, miembros = miembros * 100 / sum(miembros)))


# resumir información de las columnas
summarise(
  capitulos,
  total_miembros = sum(miembros)
)
summarise(
  capitulos,
  rango_miembros = range(miembros)
)


# EJERCICIO: ¿Cuál fue el primer capítulo creado? ¿Cuál es el más reciente?
filter(capitulos, creacion == min(creacion))
filter(capitulos, creacion == max(creacion))
filter(capitulos, creacion %in% range(creacion))


# NUEVO CONCEPTO: El operador `<-`
# ¿Cómo podemos guardar en memoria uno de los tableros anteriores?


# aplicar operaciones por grupos
capitulos_agrupado <- group_by(capitulos, pais)

# ¿Qué pasa si cambiamos capitulos por capitulos_agrupado?
filter(capitulos, miembros == max(miembros))
filter(capitulos_agrupado, miembros == max(miembros))

# EJERCICIO: Antes de ejecutar el código, ¿qué diferencias esperamos en el
# resultado de mutate() con respecto a summarise()?
View(mutate(capitulos_agrupado, total_miembros = sum(miembros)))

# El mismo código de arriba, usando el pipe
capitulos_agrupado %>%
  mutate(total_miembros = sum(miembros)) %>%
  View()

summarise(capitulos, total_miembros = sum(miembros)) # retorna una fila
summarise(capitulos_agrupado, total_miembros = sum(miembros)) # una fila por grupo


# Encadenar operaciones con %>% -------------------------------------------

# ¿Cuántos miembros hay por país?
capitulos %>%
  group_by(pais) %>%
  summarise(miembros = sum(miembros))

# ¿Cuántos capítulos hay por país?
capitulos %>%
  group_by(pais) %>%
  summarise(
    miembros = sum(miembros),
    n = n()
    )

# BONUS: count()
count(capitulos, pais)

# PARTE 2 -----------------------------------------------------------------

# Cadenas de caracteres con stringr ---------------------------------------

eventos_titulos <-
  c(
    "¡Primer evento de R-Ladies Barcelona!",
    "Scalable Machine Learning in R and Python with H2O",
    "RMarkdown tutorial and bioinformatics talk",
    "Google Analytics in R!",
    "Data wrangling with dplyr and tidyr",
    "Introducció a Bokeh: Una llibreria de visualització diferent",
    "Descifrando datos ómicos en R",
    "WomenInTech Summer Picnic",
    "Plots with ggplot2 are better plots",
    "Take your R scripts to a new planet: Jupyter notebooks with R!",
    "Relaunch of R-Ladies Barcelona!",
    "Introduction to R programming + Version control with Git and RStudio",
    "Primer encuentro! Primeros pasos y conocernos",
    "Primer Meetup R-Ladies BRC!",
    "Taller R desde cero con tidyverse R-Ladies BRC!"
  )

str_view(eventos_titulos, "r")
str_view(eventos_titulos, "R")
str_detect(eventos_titulos, "R")
str_subset(eventos_titulos, "R")

str_view(eventos_titulos, "^D")
str_subset(eventos_titulos, "^D")
str_detect(eventos_titulos, "^D")
str_starts(eventos_titulos, "D") # muy útil

str_view(eventos_titulos, "^.{4} ")

str_view(eventos_titulos, "+")
str_view(eventos_titulos, "\\+")


# EJERCICIO: ¿Cuál es el único capítulo de las R-Ladies que no comienza por
# "R-Ladies"?
?str_starts

capitulos %>%
  filter(str_starts(capitulo, ..., ...)) %>%
  select(capitulo)

# Las diferentes opciones compartidas:
capitulos %>%
  filter(str_starts(capitulo, "R-Ladies", negate = TRUE)) %>%
  select(capitulo)

capitulos %>%
  filter(str_starts(capitulo, "[^R]")) %>%
  select(capitulo)

capitulos %>%
  filter(str_starts(capitulo, "R.Ladies", negate = TRUE)) %>%
  select(capitulo)

capitulos %>%
  filter(str_detect(capitulo, "^R-Ladies", negate = TRUE)) %>%
  select(capitulo)


# Fechas con lubridate ----------------------------------------------------

# ¿Cómo se imprimen las columnas de tipo date-time (fecha-hora)?

# Extraer elementos de una fecha-hora
capitulos %>%
  select(capitulo, creacion) %>%
  mutate(
    hora = hour(creacion),
    minuto = minute(creacion),
    segundo = second(creacion)
  )


# EJERCICIO: ¿En qué año se creó el mayor número de capítulos de R-Ladies?
# ¿Tenemos información para los 12 meses de cada año?
capitulos %>%
  mutate(
    anio = year(creacion),
    mes = month(creacion)
  ) %>%
  group_by(anio) %>%
  summarise(
    n_capitulos = n(),
    n_meses = n_distinct(mes)
  )

# ¿Cuál es el mes con más capítulos?
capitulos %>%
  mutate(
    anio = year(creacion),
    mes = month(creacion)
  ) %>%
  group_by(anio, mes) %>%
  summarise(n_capitulos = n()) %>%
  group_by(anio) %>% # este group_by() es opcional
  filter(n_capitulos == max(n_capitulos))


# PARTE 3 -----------------------------------------------------------------

# Datos relacionados con tidyr --------------------------------------------

# ¿Qué hace que "capitulos_coord_largo" no sea ordenado?
capitulos_coord_largo <- capitulos %>%
  pivot_longer(
    c(latitud, longitud),
    names_to = "nombre_coord",
    values_to = "valor_coord"
  )
capitulos_coord_largo

# Volvamos al estado original
capitulos_coord_largo %>%
  pivot_wider(
    names_from = nombre_coord,
    values_from = valor_coord
  )

# EJERCICIO: Importemos el archivo "eventos_mensuales_ancho.csv". ¿Cómo podemos
# volverlo ordenado?
eventos_mensuales_ancho <- read_csv("eventos_mensuales_ancho.csv")

eventos_mensuales_ancho %>%
  pivot_longer(
    -capitulo,
    names_to = "anio_mes",
    values_to = "eventos"
  )

eventos_mensuales_ancho %>%
  pivot_longer(
    -capitulo,
    names_to = c("anio", "mes"), # nombre de las dos columnas
    names_sep = "_",             # separador
    values_to = "eventos"
  )


# Datos relacionales ------------------------------------------------------

eventos <- read_csv("eventos_rladies.csv")

dim(eventos)
left_join(eventos, capitulos, by = "capitulo")


capitulos_mpc <- filter(capitulos, pais %in% c("ES", "FR", "CO"))
semi_join(eventos, capitulos_mpc, by = "capitulo")

# EJERCICIO: ¿Cuántos eventos hay por país?
eventos %>%
  left_join(capitulos, by = "capitulo") %>%
  count(pais)
