# Dr.(C) Antonio Villalpando Acuña
# Atlas de Riesgos para la Nutrición de la Niñez en México de Save the Children

library(readxl)
library(dplyr)
library(sf)
library(ggplot2)
library(ggrepel)
library(writexl)

archivo_datos <- "variables_dependientes.xlsx"
archivo_mapa <- "u_territorial_estados_mgn_inegi_2013.shp"
directorio_salida <- "salidas_prevalencias"

dir.create(directorio_salida, recursive = TRUE, showWarnings = FALSE)

catalogo_entidades <- data.frame(
  ent = as.integer(1:32),
  nombre_entidad = c(
    "Aguascalientes", "Baja California", "Baja California Sur", "Campeche",
    "Coahuila de Zaragoza", "Colima", "Chiapas", "Chihuahua", "Ciudad de México",
    "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "México",
    "Michoacán de Ocampo", "Morelos", "Nayarit", "Nuevo León", "Oaxaca",
    "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa",
    "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave",
    "Yucatán", "Zacatecas"
  )
)

variables_dependientes <- read_excel(archivo_datos) %>%
  mutate(ent = as.integer(ent))

if (!all(c("ent", "edad", "tallaedad", "pesoedad") %in% names(variables_dependientes))) {
  stop("variables_dependientes.xlsx debe contener las columnas ent, edad, tallaedad y pesoedad.")
}

mapa_estatal <- st_read(archivo_mapa, quiet = TRUE)

if (!"cvegeoedo" %in% names(mapa_estatal)) {
  stop("El mapa debe contener la columna cvegeoedo.")
}

mapa_estatal <- mapa_estatal %>%
  rename(ent = cvegeoedo) %>%
  mutate(ent = as.integer(ent))

estandarizar_por_edad <- function(datos, variable) {
  datos %>%
    group_by(edad) %>%
    mutate(
      mediana = median(.data[[variable]], na.rm = TRUE),
      distancia = .data[[variable]] - mediana,
      desviacion = sd(distancia, na.rm = TRUE),
      puntaje = ifelse(is.na(desviacion) | desviacion == 0, NA_real_, distancia / desviacion)
    ) %>%
    ungroup()
}

calcular_desnutricion <- function(datos, general = FALSE) {
  datos_estandarizados <- estandarizar_por_edad(datos, "tallaedad")

  if (general) {
    resultado <- datos_estandarizados %>%
      group_by(ent) %>%
      summarise(
        porcentaje_bajo_2sd = mean(puntaje < -2, na.rm = TRUE) * 100,
        porcentaje_entre_menos1_y_menos2sd = mean(puntaje >= -1.99 & puntaje < -1, na.rm = TRUE) * 100,
        .groups = "drop"
      ) %>%
      mutate(tmbyb = porcentaje_bajo_2sd + porcentaje_entre_menos1_y_menos2sd)
  } else {
    resultado <- datos_estandarizados %>%
      group_by(ent) %>%
      summarise(
        porcentaje_bajo_umbral = mean(puntaje < -2, na.rm = TRUE) * 100,
        .groups = "drop"
      )
  }

  resultado %>%
    left_join(catalogo_entidades, by = "ent") %>%
    select(nombre_entidad, everything())
}

calcular_obesidad <- function(datos, general = FALSE) {
  datos_estandarizados <- estandarizar_por_edad(datos, "pesoedad")

  if (general) {
    resultado <- datos_estandarizados %>%
      group_by(ent) %>%
      summarise(
        porcentaje_bajo_2sd2 = mean(puntaje > 1, na.rm = TRUE) * 100,
        .groups = "drop"
      )
  } else {
    resultado <- datos_estandarizados %>%
      group_by(ent) %>%
      summarise(
        porcentaje_sobre_umbral = mean(puntaje > 1, na.rm = TRUE) * 100,
        .groups = "drop"
      )
  }

  resultado %>%
    left_join(catalogo_entidades, by = "ent") %>%
    select(nombre_entidad, everything())
}

crear_mapa <- function(tabla, columna, titulo, subtitulo, fuente, archivo) {
  mapa_datos <- inner_join(mapa_estatal, tabla, by = "ent")

  grafica <- ggplot(mapa_datos) +
    geom_sf(aes(fill = .data[[columna]]), color = "white", linewidth = 0.2) +
    scale_fill_gradient(low = "#E7E7FF", high = "#0010C3", name = "Porcentaje") +
    geom_text_repel(
      aes(
        label = paste0(round(.data[[columna]], 1), "%"),
        geometry = geometry
      ),
      stat = "sf_coordinates",
      size = 4,
      fontface = "bold",
      nudge_x = 0.15,
      nudge_y = 0.15,
      max.overlaps = Inf,
      segment.color = "grey50",
      segment.size = 0.5
    ) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      fill = "%",
      caption = fuente
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 25),
      plot.subtitle = element_text(size = 20),
      plot.caption = element_text(size = 15, hjust = 0.5)
    )

  ggsave(
    filename = file.path(directorio_salida, archivo),
    plot = grafica,
    width = 27,
    height = 19
  )
}

grupos_edad <- list(
  "0_a_9" = variables_dependientes %>% filter(edad >= 0 & edad <= 9),
  "0_a_3" = variables_dependientes %>% filter(edad >= 0 & edad <= 3),
  "4_a_6" = variables_dependientes %>% filter(edad >= 4 & edad <= 6),
  "7_a_9" = variables_dependientes %>% filter(edad >= 7 & edad <= 9)
)

etiquetas_edad <- c(
  "0_a_9" = "0 a 9",
  "0_a_3" = "0 a 3",
  "4_a_6" = "4 a 6",
  "7_a_9" = "7 a 9"
)

for (grupo in names(grupos_edad)) {
  datos_grupo <- grupos_edad[[grupo]]
  general <- grupo == "0_a_9"
  rango <- etiquetas_edad[[grupo]]

  desnutricion <- calcular_desnutricion(datos_grupo, general)
  obesidad <- calcular_obesidad(datos_grupo, general)

  if (general) {
    archivo_desnutricion <- "desnutricion.xlsx"
    archivo_obesidad <- "obesidad.xlsx"
    mapa_desnutricion <- "CN_mapa9.svg"
    mapa_obesidad <- "CN_mapa13.svg"
    columna_desnutricion <- "porcentaje_bajo_2sd"
    columna_obesidad <- "porcentaje_bajo_2sd2"
    fuente_desnutricion <- "Fuente: ENSANUT Continua 2022"
    fuente_obesidad <- "Fuente: ENSANUT Continua 2022"
  } else {
    archivo_desnutricion <- paste0("desnutricion_", grupo, ".xlsx")
    archivo_obesidad <- paste0("obesidad_", grupo, ".xlsx")
    mapa_desnutricion <- paste0("desnutricion_", grupo, ".svg")
    mapa_obesidad <- paste0("obesidad_", grupo, ".svg")
    columna_desnutricion <- "porcentaje_bajo_umbral"
    columna_obesidad <- "porcentaje_sobre_umbral"
    fuente_desnutricion <- "Fuente: talla para la edad con datos de ENSANUT Continua 2022"
    fuente_obesidad <- "Fuente: peso para la edad con datos de ENSANUT Continua 2022"
  }

  write_xlsx(desnutricion, file.path(directorio_salida, archivo_desnutricion))
  write_xlsx(obesidad, file.path(directorio_salida, archivo_obesidad))

  crear_mapa(
    desnutricion,
    columna_desnutricion,
    paste0("Prevalencia de desnutrición crónica (baja talla) en niñas y niños de ", rango, " años"),
    "Porcentaje por debajo de -2 desviación estándar de la mediana ajustado por edad",
    fuente_desnutricion,
    mapa_desnutricion
  )

  crear_mapa(
    obesidad,
    columna_obesidad,
    paste0("Prevalencia de sobrepeso y obesidad en niñas y niños de ", rango, " años"),
    "Porcentaje por encima de +1 desviación estándar de la mediana ajustado por edad",
    fuente_obesidad,
    mapa_obesidad
  )
}
