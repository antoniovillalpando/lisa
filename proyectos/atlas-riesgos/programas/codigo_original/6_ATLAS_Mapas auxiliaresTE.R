# Dr.(C) Antonio Villalpando Acuña
# Proyecto Atlas de riesgos de malnutrición de Save the Children
# Script de mapas auxiliares

library(readxl)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df <- as.data.frame(read_xlsx("dc.xlsx"))

desnu <- dplyr::select(df, dg1014,
                       vim1,
                       des_grave,
                       dg14,
                       merc,
                       dgm1,
                       icv_hac,
                       tamhogesc,
                       lca,
                       dmm1,
                       mins04,
                       dch)

normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

desnu_n <- as.data.frame(lapply(desnu, normalize))
desnu_n$desnu_index <- rowMeans(desnu_n, na.rm = TRUE)
desnu_n$desnu_dicotomica <- as.factor(ifelse(desnu_n$desnu_index > 0.5, 1, 0))

rm(df, desnu)

# MAPAS

library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(plyr)
library(maps)

mapaest <- sf::st_read("u_territorial_estados_mgn_inegi_2013.shp")
names(mapaest)[names(mapaest) == "cvegeoedo"] <- "ent"
mapaest$ent <- as.integer(mapaest$ent)

desnu_n$ent <- mapaest$ent

mapa_des <- dplyr::inner_join(mapaest, desnu_n, by = c("ent"))
mapa_des$desnu_dicotomica <- factor(mapa_des$desnu_dicotomica, levels = c(0, 1), labels = c("Riesgo por debajo de la media nacional", "Riesgo por encima de la media nacional"))

colores <- c(
  "Riesgo por encima de la media nacional" = "#FF0000", 
  "Riesgo Por debajo de la media nacional" = "#000066"
)

## Mapa desnutrición grave
ggplot() +
  geom_sf(data = mapa_des, aes(fill = desnu_dicotomica), colour = "black") +
  scale_fill_manual(values = colores) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20, face = "bold"),      # Tamaño del título
        plot.subtitle = element_text(size = 16),                  # Tamaño del subtítulo
        plot.caption = element_text(size = 10, hjust = 0.5),      # Tamaño y centrado del pie de página
        legend.title = element_text(size = 12),                   # Tamaño del título de la leyenda
        legend.text = element_text(size = 10)) +                  # Tamaño del texto de la leyenda
  labs(title = "Focos rojos de desnutrición crónica",
       subtitle = "Valores de 2022 - Índice calculado linealmente",
       caption = "Fuente: elaboración propia con datos de ENSANUT 2022, DGIS-SS,\nINEGI-DENUE, CONEVAL-DataMun,\nCONEVAL-Pobreza 2022",
       fill = "Clasificación")

