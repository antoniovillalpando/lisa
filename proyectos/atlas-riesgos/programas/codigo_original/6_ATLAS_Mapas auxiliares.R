# Dr.(C) Antonio Villalpando Acuña
# Proyecto Atlas de riesgos de malnutrición de Save the Children
# Script de mapas auxiliares

library(readxl)
library(dplyr)

df <- as.data.frame(read_xlsx("dc.xlsx"))

desnu <- dplyr::select(df, plp, pobreza, pobreza_m, plp_e, ic_asalud,
           ic_segsoc, pobreza_e, ins_ali)

obe <- dplyr::select(df, esc_mat, vul_car, seg_aguapot, ifem, sane, ifp, inas_esc,
                     ifs, ifem)

normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

desnu_n <- as.data.frame(lapply(desnu, normalize))
obe_n <- as.data.frame(lapply(obe, normalize))

desnu_n$desnu_index <- rowMeans(desnu_n, na.rm = TRUE)
obe_n$obe_index <- rowMeans(obe_n, na.rm = TRUE)

desnu_n$desnu_dicotomica <- as.factor(ifelse(desnu_n$desnu_index > 0.5, 1, 0))
obe_n$obe_dicotomica <- as.factor(ifelse(obe_n$obe_index > 0.5, 1, 0))

rm(df, desnu, obe)

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
obe_n$ent <- mapaest$ent

mapa_des <- dplyr::inner_join(mapaest, desnu_n, by = c("ent"))
mapa_obe <- dplyr::inner_join(mapaest, obe_n, by = c("ent"))

mapa_des$desnu_dicotomica <- factor(mapa_des$desnu_dicotomica, levels = c(0, 1), labels = c("Por debajo de la media nacional", "Encima de la media nacional"))
mapa_obe$obe_dicotomica <- factor(mapa_obe$obe_dicotomica, levels = c(0, 1), labels = c("Por debajo de la media nacional", "Encima de la media nacional"))

colores <- c(
  "Encima de la media nacional" = "#990000", 
  "Por debajo de la media nacional" = "#00BB00"
)

## Mapa desnutrición severa
ggplot() +
  geom_sf(data = mapa_des, aes(fill = desnu_dicotomica), colour = "black") +
  scale_fill_manual(values = colores) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20, face = "bold"),     
        plot.subtitle = element_text(size = 16),                  
        plot.caption = element_text(size = 10, hjust = 0.5),     
        legend.title = element_text(size = 12),                   
        legend.text = element_text(size = 10)) +                  
  labs(title = "Índice de factores que precipitan la desnutrición",
       subtitle = "Valores de 2022 - Índice calculado linealmente",
       caption = "Fuente: elaboración propia con datos de ENSANUT 2022, DGIS-SS,\nINEGI-DENUE, CONEVAL-DataMun,\nCONEVAL-Pobreza 2022",
       fill = "Clasificación")

# Gráfico sobrepeso
ggplot() +
  geom_sf(data = mapa_obe, aes(fill = obe_dicotomica), colour = "black") +
  scale_fill_manual(values = colores) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20, face = "bold"),      
        plot.subtitle = element_text(size = 16),                  
        plot.caption = element_text(size = 10, hjust = 0.5),      
        legend.title = element_text(size = 12),                   
        legend.text = element_text(size = 10)) +                  
  labs(title = "Índice de factores que precipitan el sobrepeso y la obesidad",
       subtitle = "Valores de 2022 - Índice calculado linealmente",
       caption = "Fuente: elaboración propia con datos de ENSANUT 2022, DGIS-SS,\nINEGI-DENUE, CONEVAL-DataMun,\nCONEVAL-Pobreza 2022",
       fill = "Clasificación")