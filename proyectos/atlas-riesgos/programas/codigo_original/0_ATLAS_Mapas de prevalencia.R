# Dr.(C) Antonio Villalpando Acuña
# Proyecto Atlas de riesgos de malnutrición infantil de Save the Children
# Script de generación de mapas de prevalencia

library(readxl)
library(writexl)
library(dplyr)
library(caret)
library(missForest)

variables_dependientes <- read_excel("variables_dependientes.xlsx")

variables_dependientes <- variables_dependientes %>%
  group_by(edad) %>%
  mutate(mediana_tallaedad = median(tallaedad, na.rm = TRUE))

variables_dependientes <- variables_dependientes %>%
  group_by(edad) %>%
  mutate(mediana_pesoedad = median(pesoedad, na.rm = TRUE))

## UMBRAL DE TALLA BAJA PARA LA EDAD

variables_dependientes <- variables_dependientes %>%
  mutate(distancia = tallaedad - mediana_tallaedad)
desviacion_edad <- variables_dependientes %>%
  group_by(edad) %>%
  summarise(sd_distancia = sd(distancia, na.rm = TRUE))
variables_dependientes <- variables_dependientes %>%
  left_join(desviacion_edad, by = "edad")
variables_dependientes <- variables_dependientes %>%
  mutate(desviaciones_estandar = distancia / sd_distancia)
porcentaje_bajo_2sd <- variables_dependientes %>%
  group_by(ent) %>%
  summarise(
    porcentaje_bajo_2sd = mean(desviaciones_estandar < -2, na.rm = TRUE) * 100,
    porcentaje_entre_menos1_y_menos2sd = mean(desviaciones_estandar >= -1.99 & desviaciones_estandar < -1, na.rm = TRUE) * 100
  )

## UMBRAL DE OBESIDAD

variables_dependientes <- variables_dependientes %>%
  mutate(distancia2 = pesoedad - mediana_pesoedad)
desviacion_edad2 <- variables_dependientes %>%
  group_by(edad) %>%
  summarise(sd_distancia2 = sd(distancia2, na.rm = TRUE))
variables_dependientes <- variables_dependientes %>%
  left_join(desviacion_edad2, by = "edad")
variables_dependientes <- variables_dependientes %>%
  mutate(desviaciones_estandar2 = distancia2 / sd_distancia2)
porcentaje_bajo_2sd2 <- variables_dependientes %>%
  group_by(ent) %>%
  summarise(
    porcentaje_bajo_2sd2 = mean(desviaciones_estandar2 > 1, na.rm = TRUE) * 100,
  )

library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)
library(maps)
library(ggrepel)

porcentaje_bajo_2sd <- porcentaje_bajo_2sd %>%
  mutate(tmbyb = porcentaje_bajo_2sd + porcentaje_entre_menos1_y_menos2sd)

mapaest <- sf::st_read("u_territorial_estados_mgn_inegi_2013.shp")
names(mapaest)[names(mapaest) == "cvegeoedo"] <- "ent"

mapaest$ent <- as.integer(mapaest$ent)
porcentaje_bajo_2sd$ent <- as.integer(porcentaje_bajo_2sd$ent)

mapa_y_datos <- dplyr::inner_join(mapaest, porcentaje_bajo_2sd, by = c("ent"))

plot9 <- ggplot(mapa_y_datos) +
  geom_sf(aes(fill = porcentaje_bajo_2sd), color = "white", size = 0.2) +
  scale_fill_gradient(low = "#E7E7FF", high = "#0010C3", name = "Porcentaje") +
  labs(
    title = "Prevalencia de desnutrición crónica (baja talla) en niñas y niños de 0 a 9 años",
    subtitle = "Porcentaje por debajo de -2 desviación estándar de la mediana ajustado por edad",
    fill = "%",
    caption = "Fuente: ENSANUT Continua 2022"
  ) +
  geom_text_repel(aes(label = paste0(round(porcentaje_bajo_2sd, 1), "%"), 
                      geometry = st_geometry(mapa_y_datos)),
                  stat = "sf_coordinates", size = 4, fontface = "bold", 
                  nudge_x = 0.15, nudge_y = 0.15, max.overlaps = Inf,
                  segment.color = "grey50", segment.size = 0.5) +  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 25),
    plot.subtitle = element_text(size = 20),
    plot.caption = element_text(size = 15, hjust = 0.5) 
  )

## OBESIDAD

porcentaje_bajo_2sd2$ent <- as.integer(porcentaje_bajo_2sd2$ent)
mapa_y_datos2 <- dplyr::inner_join(mapaest, porcentaje_bajo_2sd2, by = c("ent"))

plot13 <- ggplot(mapa_y_datos2) +
  geom_sf(aes(fill = porcentaje_bajo_2sd2), color = "white", size = 0.2) +
  scale_fill_gradient(low = "#E7E7FF", high = "#0010C3", name = "Porcentaje") +
  labs(
    title = "Prevalencia de sobrepeso y obesidad en niñas y niños de 0 a 9 años",
    subtitle = "Porcentaje por encima de +1 desviación estándar de la mediana ajustado por edad",
    fill = "%",
    caption = "Fuente: ENSANUT Continua 2022"
  ) +
geom_text_repel(aes(label = paste0(round(porcentaje_bajo_2sd2, 1), "%"), 
                  geometry = st_geometry(mapa_y_datos2)),
                  stat = "sf_coordinates", size = 4, fontface = "bold", 
                  nudge_x = 0.15, nudge_y = 0.15, max.overlaps = Inf,
                  segment.color = "grey50", segment.size = 0.5) +  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 25),
    plot.subtitle = element_text(size = 20),
    plot.caption = element_text(size = 15, hjust = 0.5) 
  )

library(writexl)

write_xlsx(porcentaje_bajo_2sd2, "obesidad.xlsx")
write_xlsx(porcentaje_bajo_2sd, "desnutricion.xlsx")

ggsave(file="CN_mapa9.svg", plot=plot9, width=27, height=19)
ggsave(file="CN_mapa13.svg", plot=plot13, width=27, height=19)
