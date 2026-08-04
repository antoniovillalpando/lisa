# Dr.(C) Antonio Villalpando Acuña
# Atlas de Riesgos para la Nutrición de la Niñez en México de Save the Children
# Script para mapas y data frames segmentados por grupos de edad

library(readxl)
library(dplyr)
library(sf)
library(ggplot2)
library(writexl)

# Cargar los datos
variables_dependientes <- read_excel("variables_dependientes.xlsx")

# Catálogo de entidades federativas (clave: nombre)
catalogo_entidades <- data.frame(
  ent = as.integer(1:32),
  nombre_entidad = c(
    "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Coahuila de Zaragoza",
    "Colima", "Chiapas", "Chihuahua", "Ciudad de México", "Durango", "Guanajuato", "Guerrero",
    "Hidalgo", "Jalisco", "México", "Michoacán de Ocampo", "Morelos", "Nayarit", "Nuevo León",
    "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora",
    "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave", "Yucatán", "Zacatecas"
  )
)

# Función para calcular porcentajes de desnutrición y obesidad
calcular_porcentajes <- function(datos, variable, umbral_inf = NULL, umbral_sup = NULL) {
  datos <- datos %>%
    group_by(edad) %>%
    mutate(
      mediana = median(!!sym(variable), na.rm = TRUE),
      distancia = !!sym(variable) - mediana,
      sd_distancia = sd(distancia, na.rm = TRUE),
      desviacion_estandar = distancia / sd_distancia
    ) %>%
    ungroup()
  
  if (!is.null(umbral_inf)) {
    porcentaje <- datos %>%
      group_by(ent) %>%
      summarise(
        porcentaje_bajo_umbral = mean(desviacion_estandar < umbral_inf, na.rm = TRUE) * 100
      )
  } else if (!is.null(umbral_sup)) {
    porcentaje <- datos %>%
      group_by(ent) %>%
      summarise(
        porcentaje_sobre_umbral = mean(desviacion_estandar > umbral_sup, na.rm = TRUE) * 100
      )
  }
  
  # Convertir `ent` a integer y unir con el catálogo para obtener los nombres de las entidades
  porcentaje <- porcentaje %>%
    mutate(ent = as.integer(ent)) %>%
    left_join(catalogo_entidades, by = "ent") %>%
    select(nombre_entidad, everything())
  
  return(porcentaje)
}

# Segmentar datos por grupos de edad
grupos_edad <- list(
  "0_a_3" = variables_dependientes %>% filter(edad >= 0 & edad <= 3),
  "4_a_6" = variables_dependientes %>% filter(edad >= 4 & edad <= 6),
  "7_a_9" = variables_dependientes %>% filter(edad >= 7 & edad <= 9)
)

resultados <- list()

for (grupo in names(grupos_edad)) {
  datos <- grupos_edad[[grupo]]
  
  # Calcular porcentajes para desnutrición y obesidad
  porcentaje_desnutricion <- calcular_porcentajes(datos, "tallaedad", umbral_inf = -2)
  porcentaje_obesidad <- calcular_porcentajes(datos, "pesoedad", umbral_sup = 1)
  
  resultados[[grupo]] <- list(
    desnutricion = porcentaje_desnutricion,
    obesidad = porcentaje_obesidad
  )
}

# Cargar mapa
mapaest <- st_read("u_territorial_estados_mgn_inegi_2013.shp")
names(mapaest)[names(mapaest) == "cvegeoedo"] <- "ent"
mapaest$ent <- as.integer(mapaest$ent)

# Generar mapas y guardar resultados
for (grupo in names(resultados)) {
  # Desnutrición
  df_desnutricion <- resultados[[grupo]]$desnutricion
  mapa_y_datos <- inner_join(mapaest, df_desnutricion, by = "ent")
  
  desnutricion_plot <- ggplot(mapa_y_datos) +
    geom_sf(aes(fill = porcentaje_bajo_umbral), color = "white", size = 0.2) +
    scale_fill_gradient(low = "#E7E7FF", high = "#0010C3", name = "Porcentaje") +
    labs(
      title = paste("Prevalencia de desnutrición crónica (baja talla) en niñas y niños de", gsub("([0-9]+)_a_([0-9]+)", "\\1 a \\2", grupo), "años"),
      subtitle = "Porcentaje por debajo de -2 desviación estándar de la mediana ajustado por edad",
      fill = "%",
      caption = "Fuente: talla para la edad con datos de ENSANUT Continua 2022"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 25),
      plot.subtitle = element_text(size = 20),
      plot.caption = element_text(size = 15, hjust = 0.5)
    )
  
  # Guardar gráfico y data frame
  ggsave(paste0("desnutricion_", grupo, ".svg"), plot = desnutricion_plot, width = 27, height = 19)
  write_xlsx(df_desnutricion, paste0("desnutricion_", grupo, ".xlsx"))
  
  # Obesidad
  df_obesidad <- resultados[[grupo]]$obesidad
  mapa_y_datos2 <- inner_join(mapaest, df_obesidad, by = "ent")
  
  obesidad_plot <- ggplot(mapa_y_datos2) +
    geom_sf(aes(fill = porcentaje_sobre_umbral), color = "white", size = 0.2) +
    scale_fill_gradient(low = "#E7E7FF", high = "#0010C3", name = "Porcentaje") +
    labs(
      title = paste("Prevalencia de sobrepeso y obesidad en niñas y niños de", gsub("([0-9]+)_a_([0-9]+)", "\\1 a \\2", grupo), "años"),
      subtitle = "Porcentaje por encima de +1 desviación estándar de la mediana ajustado por edad",
      fill = "%",
      caption = "Fuente: peso para la edad con datos de ENSANUT Continua 2022"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 25),
      plot.subtitle = element_text(size = 20),
      plot.caption = element_text(size = 15, hjust = 0.5)
    )
  
  # Guardar gráfico y data frame
  ggsave(paste0("obesidad_", grupo, ".svg"), plot = obesidad_plot, width = 27, height = 19)
  write_xlsx(df_obesidad, paste0("obesidad_", grupo, ".xlsx"))
}