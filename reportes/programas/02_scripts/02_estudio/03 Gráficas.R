# ╔══════════════════════════════════════════════════════════════════════╗
# ║                                                                      ║
# ║            88            88    ad88888ba          db                 ║
# ║            88            88   d8"     "8b        d88b                ║
# ║            88            88   Y8,               d8'`8b               ║
# ║            88            88   `Y8aaaaa,        d8'  `8b              ║
# ║            88            88     `"""""8b,     d8YaaaaY8b             ║
# ║            88            88           `8b.   d8""""""""8b            ║
# ║            88            88   Y8a     a8P   d8'        `8b           ║
# ║            88888888888   88    "Y88888P"   d8'          `8b          ║
# ║                                                                      ║
# ║             LABORATORIO DE INVESTIGACIÓN SOCIAL AVANZADA             ║
# ║             "Las mejores causas merecen la mejor ciencia"            ║
# ║                                                                      ║
# ║======================================================================║
# ║              LISA Servicios Científicos, S.A.S. de C.V.              ║
# ╚══════════════════════════════════════════════════════════════════════╝

# ======================================================================
# PISA 2025 · DESEMPEÑO, TECNOLOGÍA Y ENTORNOS EDUCATIVOS
# GRÁFICAS DEL REPORTE
# ======================================================================

# Insumos
## data/processed/PISA2025_resultados.rds
## lisa_logo-full.png

# Salidas
## output/figures/PISA_Desempeno_importancia_factores.png
## output/figures/PISA_Desigualdad_ESCS_materias.png
## output/figures/PISA_Desigualdad_entre_escuelas.png
## output/figures/PISA_Capacidades_desempeno.png
## output/figures/PISA_Interacciones_ESCS_capacidades.png
## output/figures/PISA_Conversion_cognitiva_digital.png
## output/figures/PISA_Desempeno_digital_factores.png
## output/figures/PISA_Desempeno_digital_ESCS_genero.png

# ======================== INICIA PROCESAMIENTO =======================

# 1. CARGA DE PAQUETES

library(dplyr)
library(ggplot2)
library(cowplot)
library(png)
library(grid)

# 2. DEFINICIÓN DE RUTAS

archivo_resultados <- "data/processed/PISA2025_resultados.rds"
archivo_logo <- "lisa_logo-full.png"

archivo_salida_g01 <- "output/figures/PISA_Desempeno_importancia_factores.png"
archivo_salida_g02 <- "output/figures/PISA_Desigualdad_ESCS_materias.png"
archivo_salida_g03 <- "output/figures/PISA_Desigualdad_entre_escuelas.png"

dir.create(
  "output/figures",
  recursive = TRUE,
  showWarnings = FALSE
)

# 3. CARGA DE DATOS

resultados <- readRDS(
  archivo_resultados
)

importancia_desempeno <- resultados$desempeno$importancia_relativa
modelos_desempeno <- resultados$desempeno$modelos_escalonados
varianza_desempeno <- resultados$desempeno$descomposicion_varianza

# 4. IDENTIDAD GRÁFICA LISA

grafito <- "#263248"
gris_texto <- "#586273"
gris_linea <- "#DDE2EA"
positivo <- "#7869E8"
positivo_claro <- "#49AFE0"
negativo <- "#F58F75"

logo_lisa <- readPNG(
  archivo_logo
)

logo_grob <- rasterGrob(
  logo_lisa,
  interpolate = TRUE
)

# 5. GRÁFICA 01 · IMPORTANCIA RELATIVA

datos_g01 <- importancia_desempeno |>
  filter(
    term %in% c(
      "ESCS",
      "PERSEV",
      "CURIO",
      "SELFREG",
      "DISCLISCI",
      "SCH_EDUSHORT",
      "SCH_CREACTIV"
    )
  ) |>
  transmute(
    dominio = recode(
      dominio,
      matematicas = "Matemáticas",
      lectura = "Lectura",
      ciencias = "Ciencias"
    ),
    indicador = recode(
      term,
      ESCS = "Estatus socioeconómico",
      PERSEV = "Perseverancia",
      CURIO = "Curiosidad",
      SELFREG = "Autorregulación",
      DISCLISCI = "Clima disciplinario",
      SCH_EDUSHORT = "Escasez de recursos educativos",
      SCH_CREACTIV = "Actividades creativas escolares"
    ),
    efecto = estimate,
    conf.low = conf.low,
    conf.high = conf.high,
    direccion = if_else(
      estimate >= 0,
      "Positiva",
      "Negativa"
    )
  ) |>
  mutate(
    dominio = factor(
      dominio,
      levels = c(
        "Matemáticas",
        "Lectura",
        "Ciencias"
      )
    ),
    indicador = factor(
      indicador,
      levels = rev(
        c(
          "Estatus socioeconómico",
          "Perseverancia",
          "Curiosidad",
          "Autorregulación",
          "Clima disciplinario",
          "Escasez de recursos educativos",
          "Actividades creativas escolares"
        )
      )
    )
  )

grafica_g01_base <- ggplot(
  datos_g01,
  aes(
    x = efecto,
    y = indicador,
    color = direccion
  )
) +
  geom_vline(
    xintercept = 0,
    color = gris_texto,
    linewidth = 0.8
  ) +
  geom_segment(
    aes(
      x = conf.low,
      xend = conf.high,
      yend = indicador
    ),
    linewidth = 1
  ) +
  geom_point(
    size = 4.5
  ) +
  geom_text(
    aes(
      x = if_else(
        efecto >= 0,
        conf.high + 0.7,
        conf.low - 0.7
      ),
      label = sprintf(
        "%+.1f",
        efecto
      ),
      hjust = if_else(
        efecto >= 0,
        0,
        1
      )
    ),
    color = grafito,
    fontface = "bold",
    size = 3.8,
    show.legend = FALSE
  ) +
  facet_wrap(
    ~ dominio,
    ncol = 1
  ) +
  scale_color_manual(
    values = c(
      "Positiva" = positivo,
      "Negativa" = negativo
    )
  ) +
  scale_x_continuous(
    expand = expansion(
      mult = c(
        0.18,
        0.18
      )
    )
  ) +
  labs(
    title = paste0(
      "El origen socioeconómico sigue siendo uno de los factores\n",
      "más asociados con el desempeño"
    ),
    subtitle = paste0(
      "Cambio asociado en puntos PISA por una desviación estándar adicional en cada factor.\n",
      "Las líneas muestran intervalos de confianza de 95%."
    ),
    x = "Cambio asociado en el puntaje PISA",
    y = NULL
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  theme_minimal(
    base_size = 15
  ) +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      color = gris_linea,
      linewidth = 0.55
    ),
    axis.text.x = element_text(
      color = gris_texto,
      size = 11.5
    ),
    axis.text.y = element_text(
      color = grafito,
      size = 12.5,
      face = "bold"
    ),
    axis.title.x = element_text(
      color = gris_texto,
      size = 12.5,
      margin = margin(
        t = 16
      )
    ),
    strip.text = element_text(
      color = grafito,
      face = "bold",
      size = 14
    ),
    legend.position = "none",
    plot.title = element_text(
      color = grafito,
      face = "bold",
      size = 22.5,
      lineheight = 1.05
    ),
    plot.subtitle = element_text(
      color = gris_texto,
      size = 13,
      lineheight = 1.15,
      margin = margin(
        b = 22
      )
    ),
    plot.title.position = "plot",
    plot.subtitle.position = "plot",
    plot.margin = margin(
      t = 28,
      r = 60,
      b = 8,
      l = 28
    )
  )

# 5.1 PIE EDITORIAL

texto_pie_g01 <- paste0(
  "Fuente: elaboración propia con microdatos PISA 2025.\n",
  "Coeficientes semiestandarizados; modelos ajustados por características individuales,\n",
  "de aprendizaje y escolares. Asociaciones estadísticas; no representan efectos causales.\n",
  "Laboratorio de Investigación Social Avanzada · LISA"
)

pie_g01 <- ggdraw() +
  draw_grob(
    logo_grob,
    x = 0.00,
    y = 0.18,
    width = 0.21,
    height = 0.64
  ) +
  draw_line(
    x = c(
      0.225,
      0.225
    ),
    y = c(
      0.16,
      0.84
    ),
    color = "#9AA3B2",
    linewidth = 0.8
  ) +
  draw_label(
    texto_pie_g01,
    x = 0.245,
    y = 0.50,
    hjust = 0,
    vjust = 0.5,
    color = gris_texto,
    size = 8.7,
    lineheight = 1.25
  )

grafica_g01 <- plot_grid(
  grafica_g01_base,
  pie_g01,
  ncol = 1,
  rel_heights = c(
    0.88,
    0.12
  ),
  align = "v"
)

# 6. GRÁFICA 02 · ESTATUS SOCIOECONÓMICO

datos_g02 <- bind_rows(
  modelos_desempeno$matematicas$escuela |>
    filter(term == "ESCS") |>
    mutate(dominio = "Matemáticas"),
  modelos_desempeno$lectura$escuela |>
    filter(term == "ESCS") |>
    mutate(dominio = "Lectura"),
  modelos_desempeno$ciencias$escuela |>
    filter(term == "ESCS") |>
    mutate(dominio = "Ciencias")
) |>
  transmute(
    dominio,
    efecto = estimate,
    conf.low,
    conf.high
  ) |>
  mutate(
    dominio = factor(
      dominio,
      levels = c(
        "Matemáticas",
        "Lectura",
        "Ciencias"
      )
    )
  )

grafica_g02_base <- ggplot(
  datos_g02,
  aes(
    x = dominio,
    y = efecto
  )
) +
  geom_col(
    width = 0.58,
    fill = positivo
  ) +
  geom_errorbar(
    aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    width = 0.13,
    linewidth = 0.9,
    color = grafito
  ) +
  geom_text(
    aes(
      y = conf.high + 0.8,
      label = paste0(
        "+",
        sprintf(
          "%.1f",
          efecto
        ),
        " puntos"
      )
    ),
    color = grafito,
    fontface = "bold",
    size = 5.2
  ) +
  expand_limits(
    y = 0
  ) +
  scale_y_continuous(
    expand = expansion(
      mult = c(
        0,
        0.16
      )
    )
  ) +
  labs(
    title = paste0(
      "La desigualdad socioeconómica permanece\n",
      "asociada con el desempeño"
    ),
    subtitle = paste0(
      "Cambio asociado en el puntaje por una unidad adicional del índice ESCS,\n",
      "controlando características personales, capacidades y condiciones escolares."
    ),
    x = NULL,
    y = "Puntos PISA"
  ) +
  theme_minimal(
    base_size = 16
  ) +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = gris_linea,
      linewidth = 0.55
    ),
    axis.text.x = element_text(
      color = grafito,
      size = 14,
      face = "bold"
    ),
    axis.text.y = element_text(
      color = gris_texto,
      size = 12
    ),
    axis.title.y = element_text(
      color = gris_texto,
      size = 12.5
    ),
    plot.title = element_text(
      color = grafito,
      face = "bold",
      size = 23,
      lineheight = 1.05
    ),
    plot.subtitle = element_text(
      color = gris_texto,
      size = 13.5,
      lineheight = 1.15,
      margin = margin(
        b = 25
      )
    ),
    plot.title.position = "plot",
    plot.subtitle.position = "plot",
    plot.margin = margin(
      t = 28,
      r = 45,
      b = 8,
      l = 28
    )
  )

pie_g02 <- ggdraw() +
  draw_grob(
    logo_grob,
    x = 0,
    y = 0.16,
    width = 0.22,
    height = 0.68
  ) +
  draw_line(
    x = c(
      0.225,
      0.225
    ),
    y = c(
      0.20,
      0.80
    ),
    color = "#9AA3B2",
    linewidth = 0.8
  ) +
  draw_label(
    paste0(
      "Fuente: elaboración propia con microdatos PISA 2025.\n",
      "ESCS: índice de estatus económico, social y cultural. Intervalos de confianza de 95%.\n",
      "Asociaciones ajustadas; no representan efectos causales. · Laboratorio de Investigación Social Avanzada · LISA"
    ),
    x = 0.245,
    y = 0.50,
    hjust = 0,
    vjust = 0.5,
    color = gris_texto,
    size = 9.5,
    lineheight = 1.3
  )

grafica_g02 <- plot_grid(
  grafica_g02_base,
  pie_g02,
  ncol = 1,
  rel_heights = c(
    0.86,
    0.14
  ),
  align = "v"
)

# 7. GRÁFICA 03 · DIFERENCIAS ENTRE ESCUELAS

datos_icc <- varianza_desempeno |>
  filter(
    dominio %in% c(
      "matematicas",
      "lectura",
      "ciencias"
    )
  ) |>
  mutate(
    dominio = recode(
      dominio,
      matematicas = "Matemáticas",
      lectura = "Lectura",
      ciencias = "Ciencias"
    )
  )

datos_g03 <- bind_rows(
  datos_icc |>
    transmute(
      dominio,
      componente = "Entre escuelas",
      porcentaje = 100 * icc
    ),
  datos_icc |>
    transmute(
      dominio,
      componente = "Dentro de las escuelas",
      porcentaje = 100 * (1 - icc)
    )
) |>
  mutate(
    dominio = factor(
      dominio,
      levels = c(
        "Matemáticas",
        "Lectura",
        "Ciencias"
      )
    ),
    componente = factor(
      componente,
      levels = c(
        "Dentro de las escuelas",
        "Entre escuelas"
      )
    )
  )

grafica_g03_base <- ggplot(
  datos_g03,
  aes(
    x = dominio,
    y = porcentaje,
    fill = componente
  )
) +
  geom_col(
    width = 0.62
  ) +
  geom_text(
    aes(
      label = paste0(
        round(
          porcentaje,
          1
        ),
        "%"
      )
    ),
    position = position_stack(
      vjust = 0.5
    ),
    color = grafito,
    fontface = "bold",
    size = 5
  ) +
  scale_fill_manual(
    values = c(
      "Entre escuelas" = positivo,
      "Dentro de las escuelas" = gris_linea
    )
  ) +
  scale_y_continuous(
    breaks = c(
      0,
      25,
      50,
      75,
      100
    ),
    labels = function(x) {
      paste0(
        x,
        "%"
      )
    },
    limits = c(
      0,
      100
    ),
    expand = expansion(
      mult = c(
        0,
        0
      )
    )
  ) +
  labs(
    title = paste0(
      "Más de un tercio de las diferencias de desempeño\n",
      "ocurre entre escuelas"
    ),
    subtitle = paste0(
      "Descomposición de la varianza del desempeño de estudiantes mexicanos.\n",
      "El segmento morado corresponde a diferencias entre escuelas."
    ),
    x = NULL,
    y = "Porcentaje de la varianza",
    fill = NULL
  ) +
  theme_minimal(
    base_size = 16
  ) +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.grid = element_blank(),
    axis.text.x = element_text(
      color = grafito,
      size = 14,
      face = "bold"
    ),
    axis.text.y = element_text(
      color = gris_texto,
      size = 12
    ),
    axis.title.y = element_text(
      color = gris_texto,
      size = 12.5
    ),
    legend.position = "bottom",
    legend.text = element_text(
      color = gris_texto,
      size = 12
    ),
    plot.title = element_text(
      color = grafito,
      face = "bold",
      size = 23,
      lineheight = 1.05
    ),
    plot.subtitle = element_text(
      color = gris_texto,
      size = 13.5,
      lineheight = 1.15,
      margin = margin(
        b = 25
      )
    ),
    plot.title.position = "plot",
    plot.subtitle.position = "plot",
    plot.margin = margin(
      t = 28,
      r = 45,
      b = 8,
      l = 28
    )
  )

pie_g03 <- ggdraw() +
  draw_grob(
    logo_grob,
    x = 0,
    y = 0.16,
    width = 0.22,
    height = 0.68
  ) +
  draw_line(
    x = c(
      0.225,
      0.225
    ),
    y = c(
      0.20,
      0.80
    ),
    color = "#9AA3B2",
    linewidth = 0.8
  ) +
  draw_label(
    paste0(
      "Fuente: elaboración propia con microdatos PISA 2025.\n",
      "ICC calculado a partir de modelos multinivel para los 10 valores plausibles de cada dominio.\n",
      "Laboratorio de Investigación Social Avanzada · LISA"
    ),
    x = 0.245,
    y = 0.50,
    hjust = 0,
    vjust = 0.5,
    color = gris_texto,
    size = 9.5,
    lineheight = 1.3
  )

grafica_g03 <- plot_grid(
  grafica_g03_base,
  pie_g03,
  ncol = 1,
  rel_heights = c(
    0.86,
    0.14
  ),
  align = "v"
)

# 8. VISUALIZACIÓN

print(
  grafica_g01
)

print(
  grafica_g02
)

print(
  grafica_g03
)

# 9. EXPORTACIÓN

ggsave(
  filename = archivo_salida_g01,
  plot = grafica_g01,
  width = 10,
  height = 14,
  units = "in",
  dpi = 320,
  bg = "white"
)

ggsave(
  filename = archivo_salida_g02,
  plot = grafica_g02,
  width = 10,
  height = 11,
  units = "in",
  dpi = 320,
  bg = "white"
)

ggsave(
  filename = archivo_salida_g03,
  plot = grafica_g03,
  width = 10,
  height = 11,
  units = "in",
  dpi = 320,
  bg = "white"
)

# 10. GRÁFICA 04 · CAPACIDADES INDIVIDUALES

archivo_salida_g04 <- "output/figures/PISA_Capacidades_desempeno.png"

datos_g04 <- importancia_desempeno |>
  filter(
    term %in% c(
      "CURIO",
      "PERSEV",
      "SELFREG"
    )
  ) |>
  transmute(
    dominio = recode(
      dominio,
      matematicas = "Matemáticas",
      lectura = "Lectura",
      ciencias = "Ciencias"
    ),
    capacidad = recode(
      term,
      CURIO = "Curiosidad",
      PERSEV = "Perseverancia",
      SELFREG = "Autorregulación"
    ),
    efecto = estimate,
    conf.low,
    conf.high,
    direccion = if_else(
      estimate >= 0,
      "Positiva",
      "Negativa"
    )
  ) |>
  mutate(
    dominio = factor(
      dominio,
      levels = c(
        "Matemáticas",
        "Lectura",
        "Ciencias"
      )
    ),
    capacidad = factor(
      capacidad,
      levels = rev(
        c(
          "Curiosidad",
          "Perseverancia",
          "Autorregulación"
        )
      )
    )
  )

margen_g04 <- max(
  abs(
    c(
      datos_g04$conf.low,
      datos_g04$conf.high
    )
  ),
  na.rm = TRUE
) * 0.05

grafica_g04_base <- ggplot(
  datos_g04,
  aes(
    x = efecto,
    y = capacidad,
    color = direccion
  )
) +
  geom_vline(
    xintercept = 0,
    color = gris_texto,
    linewidth = 0.8
  ) +
  geom_segment(
    aes(
      x = conf.low,
      xend = conf.high,
      yend = capacidad
    ),
    linewidth = 1
  ) +
  geom_point(
    size = 4.5
  ) +
  geom_text(
    aes(
      x = if_else(
        efecto >= 0,
        conf.high + margen_g04,
        conf.low - margen_g04
      ),
      label = sprintf(
        "%+.1f",
        efecto
      ),
      hjust = if_else(
        efecto >= 0,
        0,
        1
      )
    ),
    color = grafito,
    fontface = "bold",
    size = 4,
    show.legend = FALSE
  ) +
  facet_wrap(
    ~ dominio,
    ncol = 1
  ) +
  scale_color_manual(
    values = c(
      "Positiva" = positivo,
      "Negativa" = negativo
    )
  ) +
  scale_x_continuous(
    expand = expansion(
      mult = c(
        0.20,
        0.20
      )
    )
  ) +
  labs(
    title = paste0(
      "Curiosidad, perseverancia y autorregulación\n",
      "se asocian con un mejor desempeño"
    ),
    subtitle = paste0(
      "Cambio asociado en puntos PISA por una desviación estándar adicional en cada capacidad.\n",
      "Las líneas muestran intervalos de confianza de 95%."
    ),
    x = "Cambio asociado en el puntaje PISA",
    y = NULL
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  theme_minimal(
    base_size = 15
  ) +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      color = gris_linea,
      linewidth = 0.55
    ),
    axis.text.x = element_text(
      color = gris_texto,
      size = 11.5
    ),
    axis.text.y = element_text(
      color = grafito,
      size = 13,
      face = "bold"
    ),
    axis.title.x = element_text(
      color = gris_texto,
      size = 12.5,
      margin = margin(
        t = 16
      )
    ),
    strip.text = element_text(
      color = grafito,
      face = "bold",
      size = 14
    ),
    legend.position = "none",
    plot.title = element_text(
      color = grafito,
      face = "bold",
      size = 22.5,
      lineheight = 1.05
    ),
    plot.subtitle = element_text(
      color = gris_texto,
      size = 13,
      lineheight = 1.15,
      margin = margin(
        b = 22
      )
    ),
    plot.title.position = "plot",
    plot.subtitle.position = "plot",
    plot.margin = margin(
      t = 28,
      r = 60,
      b = 8,
      l = 28
    )
  )

pie_g04 <- ggdraw() +
  draw_grob(
    logo_grob,
    x = 0,
    y = 0.18,
    width = 0.21,
    height = 0.64
  ) +
  draw_line(
    x = c(
      0.225,
      0.225
    ),
    y = c(
      0.16,
      0.84
    ),
    color = "#9AA3B2",
    linewidth = 0.8
  ) +
  draw_label(
    paste0(
      "Fuente: elaboración propia con microdatos PISA 2025.\n",
      "Coeficientes semiestandarizados; modelos ajustados por características individuales,\n",
      "de aprendizaje y escolares. Asociaciones estadísticas; no representan efectos causales.\n",
      "Laboratorio de Investigación Social Avanzada · LISA"
    ),
    x = 0.245,
    y = 0.50,
    hjust = 0,
    vjust = 0.5,
    color = gris_texto,
    size = 8.7,
    lineheight = 1.25
  )

grafica_g04 <- plot_grid(
  grafica_g04_base,
  pie_g04,
  ncol = 1,
  rel_heights = c(
    0.88,
    0.12
  ),
  align = "v"
)

# 11. GRÁFICA AUXILIAR · INTERACCIONES ESCS × CAPACIDADES

archivo_salida_g05 <- "output/figures/PISA_Interacciones_ESCS_capacidades.png"

interacciones_desempeno <- resultados$desempeno$interacciones_escs

datos_g05 <- interacciones_desempeno |>
  filter(
    term == paste0(
      "ESCS:",
      factor
    ) |
      term == paste0(
        factor,
        ":ESCS"
      )
  ) |>
  transmute(
    dominio = recode(
      dominio,
      matematicas = "Matemáticas",
      lectura = "Lectura",
      ciencias = "Ciencias"
    ),
    capacidad = recode(
      factor,
      CURIO = "Curiosidad",
      PERSEV = "Perseverancia",
      SELFREG = "Autorregulación",
      FAMSUP = "Apoyo familiar",
      BELONG = "Pertenencia",
      TEACHSUP = "Apoyo docente"
    ),
    efecto = estimate,
    conf.low,
    conf.high,
    direccion = if_else(
      estimate >= 0,
      "Amplía",
      "Reduce"
    )
  ) |>
  mutate(
    dominio = factor(
      dominio,
      levels = c(
        "Matemáticas",
        "Lectura",
        "Ciencias"
      )
    ),
    capacidad = factor(
      capacidad,
      levels = rev(
        c(
          "Curiosidad",
          "Perseverancia",
          "Autorregulación",
          "Apoyo familiar",
          "Pertenencia",
          "Apoyo docente"
        )
      )
    )
  )

margen_g05 <- max(
  abs(
    c(
      datos_g05$conf.low,
      datos_g05$conf.high
    )
  ),
  na.rm = TRUE
) * 0.05

grafica_g05_base <- ggplot(
  datos_g05,
  aes(
    x = efecto,
    y = capacidad,
    color = direccion
  )
) +
  geom_vline(
    xintercept = 0,
    color = gris_texto,
    linewidth = 0.9
  ) +
  geom_segment(
    aes(
      x = conf.low,
      xend = conf.high,
      yend = capacidad
    ),
    linewidth = 1
  ) +
  geom_point(
    size = 4.2
  ) +
  geom_text(
    aes(
      x = if_else(
        efecto >= 0,
        conf.high + margen_g05,
        conf.low - margen_g05
      ),
      label = sprintf(
        "%+.2f",
        efecto
      ),
      hjust = if_else(
        efecto >= 0,
        0,
        1
      )
    ),
    color = grafito,
    fontface = "bold",
    size = 3.6,
    show.legend = FALSE
  ) +
  facet_wrap(
    ~ dominio,
    ncol = 1
  ) +
  scale_color_manual(
    values = c(
      "Amplía" = negativo,
      "Reduce" = positivo
    )
  ) +
  scale_x_continuous(
    expand = expansion(
      mult = c(
        0.20,
        0.20
      )
    )
  ) +
  labs(
    title = paste0(
      "Las capacidades no reducen de manera sistemática\n",
      "la pendiente socioeconómica"
    ),
    subtitle = paste0(
      "Coeficientes de interacción entre ESCS y cada factor.\n",
      "Valores negativos reducen la pendiente socioeconómica; valores positivos la amplían."
    ),
    x = "Cambio en la pendiente asociada con ESCS",
    y = NULL
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  theme_minimal(
    base_size = 15
  ) +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      color = gris_linea,
      linewidth = 0.55
    ),
    axis.text.x = element_text(
      color = gris_texto,
      size = 11
    ),
    axis.text.y = element_text(
      color = grafito,
      size = 11.5,
      face = "bold"
    ),
    axis.title.x = element_text(
      color = gris_texto,
      size = 12.5,
      margin = margin(
        t = 16
      )
    ),
    strip.text = element_text(
      color = grafito,
      face = "bold",
      size = 14
    ),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(
      color = gris_texto,
      size = 11.5
    ),
    plot.title = element_text(
      color = grafito,
      face = "bold",
      size = 22.5,
      lineheight = 1.05
    ),
    plot.subtitle = element_text(
      color = gris_texto,
      size = 13,
      lineheight = 1.15,
      margin = margin(
        b = 22
      )
    ),
    plot.title.position = "plot",
    plot.subtitle.position = "plot",
    plot.margin = margin(
      t = 28,
      r = 65,
      b = 8,
      l = 28
    )
  )

pie_g05 <- ggdraw() +
  draw_grob(
    logo_grob,
    x = 0,
    y = 0.18,
    width = 0.21,
    height = 0.64
  ) +
  draw_line(
    x = c(
      0.225,
      0.225
    ),
    y = c(
      0.16,
      0.84
    ),
    color = "#9AA3B2",
    linewidth = 0.8
  ) +
  draw_label(
    paste0(
      "Fuente: elaboración propia con microdatos PISA 2025.\n",
      "Interacciones estimadas con los 10 valores plausibles y diseño de encuesta BRR-Fay.\n",
      "Los intervalos corresponden a 95%. Asociaciones estadísticas; no representan efectos causales.\n",
      "Laboratorio de Investigación Social Avanzada · LISA"
    ),
    x = 0.245,
    y = 0.50,
    hjust = 0,
    vjust = 0.5,
    color = gris_texto,
    size = 8.7,
    lineheight = 1.25
  )

grafica_g05 <- plot_grid(
  grafica_g05_base,
  pie_g05,
  ncol = 1,
  rel_heights = c(
    0.88,
    0.12
  ),
  align = "v"
)

# 12. VISUALIZACIÓN

print(
  grafica_g04
)

print(
  grafica_g05
)

# 13. EXPORTACIÓN

ggsave(
  filename = archivo_salida_g04,
  plot = grafica_g04,
  width = 10,
  height = 12,
  units = "in",
  dpi = 320,
  bg = "white"
)

ggsave(
  filename = archivo_salida_g05,
  plot = grafica_g05,
  width = 10,
  height = 15,
  units = "in",
  dpi = 320,
  bg = "white"
)

# 14. SECCIÓN II · TECNOLOGÍA, APRENDIZAJE DIGITAL E INTELIGENCIA ARTIFICIAL

modelo_digital <- resultados$digital_ia$modelo_ldw

termino_genero <- modelo_digital |>
  filter(
    grepl(
      "^sexo",
      term
    )
  ) |>
  slice(1) |>
  pull(term)

etiqueta_genero <- case_when(
  termino_genero == "sexoMujer" ~ "Mujer (ref. hombre)",
  termino_genero == "sexoHombre" ~ "Hombre (ref. mujer)",
  TRUE ~ "Género"
)

# 15. FIGURA 05 · CONVERSIÓN COGNITIVA DE LOS RECURSOS DIGITALES

archivo_salida_s2_g05 <- "output/figures/PISA_Conversion_cognitiva_digital.png"

datos_s2_g05 <- modelo_digital |>
  filter(
    term %in% c(
      "utilidad_cognitiva_digital",
      "alfabetizacion_critica_ia",
      "interes_digital"
    )
  ) |>
  transmute(
    indicador = recode(
      term,
      utilidad_cognitiva_digital = "Utilidad cognitiva",
      alfabetizacion_critica_ia = "Práctica crítica",
      interes_digital = "Interés digital"
    ),
    efecto = estimate,
    conf.low,
    conf.high,
    evidencia = case_when(
      conf.low > 0 | conf.high < 0 ~ "Asociación clara",
      TRUE ~ "Sin evidencia clara"
    )
  ) |>
  mutate(
    indicador = factor(
      indicador,
      levels = rev(
        c(
          "Utilidad cognitiva",
          "Práctica crítica",
          "Interés digital"
        )
      )
    )
  )

margen_s2_g05 <- max(
  abs(
    c(
      datos_s2_g05$conf.low,
      datos_s2_g05$conf.high
    )
  ),
  na.rm = TRUE
) * 0.05

grafica_s2_g05_base <- ggplot(
  datos_s2_g05,
  aes(
    x = efecto,
    y = indicador,
    color = evidencia
  )
) +
  geom_vline(
    xintercept = 0,
    color = gris_texto,
    linewidth = 0.8
  ) +
  geom_segment(
    aes(
      x = conf.low,
      xend = conf.high,
      yend = indicador
    ),
    linewidth = 1.2
  ) +
  geom_point(
    size = 5
  ) +
  geom_text(
    aes(
      x = if_else(
        efecto >= 0,
        conf.high + margen_s2_g05,
        conf.low - margen_s2_g05
      ),
      label = sprintf(
        "%+.1f",
        efecto
      ),
      hjust = if_else(
        efecto >= 0,
        0,
        1
      )
    ),
    color = grafito,
    fontface = "bold",
    size = 4.5,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Asociación clara" = positivo,
      "Sin evidencia clara" = gris_texto
    )
  ) +
  scale_x_continuous(
    expand = expansion(
      mult = c(
        0.16,
        0.20
      )
    )
  ) +
  labs(
    title = paste0(
      "Interesarse por la tecnología no es lo mismo\n",
      "que saber utilizarla para aprender"
    ),
    subtitle = paste0(
      "Asociaciones ajustadas con el desempeño digital de PISA 2025.\n",
      "Las líneas muestran intervalos de confianza de 95%."
    ),
    x = "Cambio asociado en el puntaje digital",
    y = NULL
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  theme_minimal(
    base_size = 16
  ) +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      color = gris_linea,
      linewidth = 0.55
    ),
    axis.text.x = element_text(
      color = gris_texto,
      size = 12
    ),
    axis.text.y = element_text(
      color = grafito,
      size = 14,
      face = "bold"
    ),
    axis.title.x = element_text(
      color = gris_texto,
      size = 12.5,
      margin = margin(
        t = 16
      )
    ),
    legend.position = "none",
    plot.title = element_text(
      color = grafito,
      face = "bold",
      size = 23,
      lineheight = 1.05
    ),
    plot.subtitle = element_text(
      color = gris_texto,
      size = 13.5,
      lineheight = 1.15,
      margin = margin(
        b = 25
      )
    ),
    plot.title.position = "plot",
    plot.subtitle.position = "plot",
    plot.margin = margin(
      t = 28,
      r = 55,
      b = 8,
      l = 28
    )
  )

pie_s2_g05 <- ggdraw() +
  draw_grob(
    logo_grob,
    x = 0,
    y = 0.18,
    width = 0.21,
    height = 0.64
  ) +
  draw_line(
    x = c(
      0.225,
      0.225
    ),
    y = c(
      0.16,
      0.84
    ),
    color = "#9AA3B2",
    linewidth = 0.8
  ) +
  draw_label(
    paste0(
      "Fuente: elaboración propia con microdatos PISA 2025.\n",
      "Coeficientes del modelo multivariado de desempeño digital. Intervalos de confianza de 95%.\n",
      "Asociaciones estadísticas; no representan efectos causales.\n",
      "Laboratorio de Investigación Social Avanzada · LISA"
    ),
    x = 0.245,
    y = 0.50,
    hjust = 0,
    vjust = 0.5,
    color = gris_texto,
    size = 8.9,
    lineheight = 1.25
  )

grafica_s2_g05 <- plot_grid(
  grafica_s2_g05_base,
  pie_s2_g05,
  ncol = 1,
  rel_heights = c(
    0.86,
    0.14
  ),
  align = "v"
)

# 16. FIGURA 06 · FACTORES ASOCIADOS CON EL DESEMPEÑO DIGITAL

archivo_salida_s2_g06 <- "output/figures/PISA_Desempeno_digital_factores.png"

datos_s2_g06 <- modelo_digital |>
  filter(
    term %in% c(
      termino_genero,
      "ESCS",
      "CURIO",
      "PERSEV",
      "SELFREG",
      "alfabetizacion_critica_ia",
      "utilidad_cognitiva_digital",
      "interes_digital"
    )
  ) |>
  transmute(
    indicador = case_when(
      term == termino_genero ~ etiqueta_genero,
      term == "ESCS" ~ "Estatus socioeconómico",
      term == "CURIO" ~ "Curiosidad",
      term == "PERSEV" ~ "Perseverancia",
      term == "SELFREG" ~ "Autorregulación",
      term == "alfabetizacion_critica_ia" ~ "Práctica crítica con chatbots",
      term == "utilidad_cognitiva_digital" ~ "Utilidad cognitiva de los recursos digitales",
      term == "interes_digital" ~ "Interés por los recursos digitales"
    ),
    efecto = estimate,
    conf.low,
    conf.high,
    direccion = case_when(
      conf.low > 0 ~ "Positiva",
      conf.high < 0 ~ "Negativa",
      TRUE ~ "Sin evidencia clara"
    )
  )

niveles_s2_g06 <- c(
  etiqueta_genero,
  "Estatus socioeconómico",
  "Utilidad cognitiva de los recursos digitales",
  "Curiosidad",
  "Práctica crítica con chatbots",
  "Perseverancia",
  "Autorregulación",
  "Interés por los recursos digitales"
)

datos_s2_g06 <- datos_s2_g06 |>
  mutate(
    indicador = factor(
      indicador,
      levels = rev(niveles_s2_g06)
    )
  )

margen_s2_g06 <- max(
  abs(
    c(
      datos_s2_g06$conf.low,
      datos_s2_g06$conf.high
    )
  ),
  na.rm = TRUE
) * 0.05

grafica_s2_g06_base <- ggplot(
  datos_s2_g06,
  aes(
    x = efecto,
    y = indicador,
    color = direccion
  )
) +
  geom_vline(
    xintercept = 0,
    color = gris_texto,
    linewidth = 0.8
  ) +
  geom_segment(
    aes(
      x = conf.low,
      xend = conf.high,
      yend = indicador
    ),
    linewidth = 1
  ) +
  geom_point(
    size = 4.5
  ) +
  geom_text(
    aes(
      x = if_else(
        efecto >= 0,
        conf.high + margen_s2_g06,
        conf.low - margen_s2_g06
      ),
      label = sprintf(
        "%+.1f",
        efecto
      ),
      hjust = if_else(
        efecto >= 0,
        0,
        1
      )
    ),
    color = grafito,
    fontface = "bold",
    size = 4,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Positiva" = positivo,
      "Negativa" = negativo,
      "Sin evidencia clara" = gris_texto
    )
  ) +
  scale_x_continuous(
    expand = expansion(
      mult = c(
        0.18,
        0.20
      )
    )
  ) +
  labs(
    title = paste0(
      "El desempeño digital se asocia con factores sociales,\n",
      "cognitivos y de uso de la tecnología"
    ),
    subtitle = paste0(
      "Coeficientes del modelo multivariado de desempeño digital de PISA 2025.\n",
      "Las variables conservan sus escalas originales; las magnitudes no son directamente comparables."
    ),
    x = "Cambio asociado en el puntaje digital",
    y = NULL
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  theme_minimal(
    base_size = 15
  ) +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      color = gris_linea,
      linewidth = 0.55
    ),
    axis.text.x = element_text(
      color = gris_texto,
      size = 11.5
    ),
    axis.text.y = element_text(
      color = grafito,
      size = 12.5,
      face = "bold"
    ),
    axis.title.x = element_text(
      color = gris_texto,
      size = 12.5,
      margin = margin(
        t = 16
      )
    ),
    legend.position = "none",
    plot.title = element_text(
      color = grafito,
      face = "bold",
      size = 22.5,
      lineheight = 1.05
    ),
    plot.subtitle = element_text(
      color = gris_texto,
      size = 13,
      lineheight = 1.15,
      margin = margin(
        b = 22
      )
    ),
    plot.title.position = "plot",
    plot.subtitle.position = "plot",
    plot.margin = margin(
      t = 28,
      r = 70,
      b = 8,
      l = 28
    )
  )

pie_s2_g06 <- ggdraw() +
  draw_grob(
    logo_grob,
    x = 0.04,
    y = 0.18,
    width = 0.12,
    height = 0.64
  ) +
  draw_line(
    x = c(
      0.18,
      0.18
    ),
    y = c(
      0.15,
      0.85
    ),
    color = "#9AA3B2",
    linewidth = 0.8
  ) +
  draw_label(
    paste0(
      "Fuente: elaboración propia con microdatos PISA 2025.\n",
      "Modelo multivariado ajustado por características individuales,\n",
      "capacidades y disposiciones digitales. Intervalos de confianza de 95%.\n",
      "Asociaciones estadísticas; no representan efectos causales.\n",
      "Laboratorio de Investigación Social Avanzada · LISA"
    ),
    x = 0.20,
    y = 0.50,
    hjust = 0,
    vjust = 0.5,
    color = gris_texto,
    size = 8.0,
    lineheight = 1.18
  )

grafica_s2_g06 <- plot_grid(
  grafica_s2_g06_base,
  pie_s2_g06,
  ncol = 1,
  rel_heights = c(
    0.85,
    0.15
  ),
  align = "v"
)

# 17. FIGURA 07 · ESCS Y GÉNERO EN EL DESEMPEÑO DIGITAL

archivo_salida_s2_g07 <- "output/figures/PISA_Desempeno_digital_ESCS_genero.png"

beta_escs_s2_g07 <- modelo_digital |>
  filter(
    term == "ESCS"
  ) |>
  pull(estimate)

beta_mujer_s2_g07 <- if ("sexoMujer" %in% modelo_digital$term) {
  modelo_digital |>
    filter(
      term == "sexoMujer"
    ) |>
    pull(estimate)
} else if ("sexoHombre" %in% modelo_digital$term) {
  -1 * (
    modelo_digital |>
      filter(
        term == "sexoHombre"
      ) |>
      pull(estimate)
  )
} else {
  stop(
    "No se encontró el coeficiente de género en modelo_digital."
  )
}

escs_s2_g07 <- seq(
  -2.5,
  1,
  length.out = 100
)

datos_s2_g07 <- bind_rows(
  data.frame(
    ESCS = escs_s2_g07,
    genero = "Hombre",
    diferencia = beta_escs_s2_g07 * escs_s2_g07
  ),
  data.frame(
    ESCS = escs_s2_g07,
    genero = "Mujer",
    diferencia = beta_escs_s2_g07 * escs_s2_g07 + beta_mujer_s2_g07
  )
) |>
  mutate(
    genero = factor(
      genero,
      levels = c(
        "Hombre",
        "Mujer"
      )
    )
  )

grafica_s2_g07_base <- ggplot(
  datos_s2_g07,
  aes(
    x = ESCS,
    y = diferencia,
    color = genero
  )
) +
  geom_hline(
    yintercept = 0,
    color = gris_linea,
    linewidth = 0.8
  ) +
  geom_vline(
    xintercept = 0,
    color = gris_linea,
    linewidth = 0.8
  ) +
  geom_line(
    linewidth = 1.7
  ) +
  scale_color_manual(
    values = c(
      "Hombre" = positivo,
      "Mujer" = negativo
    )
  ) +
  scale_x_continuous(
    breaks = c(
      -2.5,
      -2,
      -1,
      0,
      1
    )
  ) +
  labs(
    title = paste0(
      "El origen socioeconómico y el género se acumulan\n",
      "en las diferencias de desempeño digital"
    ),
    subtitle = paste0(
      "Diferencia ajustada atribuible a ESCS y género respecto de un hombre con ESCS = 0.\n",
      "Las líneas representan la estructura aditiva estimada por el modelo."
    ),
    x = "Índice ESCS",
    y = "Diferencia ajustada en puntos",
    color = NULL
  ) +
  theme_minimal(
    base_size = 16
  ) +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      color = gris_linea,
      linewidth = 0.55
    ),
    axis.text.x = element_text(
      color = gris_texto,
      size = 12
    ),
    axis.text.y = element_text(
      color = gris_texto,
      size = 12
    ),
    axis.title.x = element_text(
      color = gris_texto,
      size = 12.5
    ),
    axis.title.y = element_text(
      color = gris_texto,
      size = 12.5
    ),
    legend.position = "bottom",
    legend.text = element_text(
      color = grafito,
      size = 12.5,
      face = "bold"
    ),
    plot.title = element_text(
      color = grafito,
      face = "bold",
      size = 23,
      lineheight = 1.05
    ),
    plot.subtitle = element_text(
      color = gris_texto,
      size = 13.5,
      lineheight = 1.15,
      margin = margin(
        b = 25
      )
    ),
    plot.title.position = "plot",
    plot.subtitle.position = "plot",
    plot.margin = margin(
      t = 28,
      r = 45,
      b = 8,
      l = 28
    )
  )

pie_s2_g07 <- ggdraw() +
  draw_grob(
    logo_grob,
    x = 0,
    y = 0.18,
    width = 0.21,
    height = 0.64
  ) +
  draw_line(
    x = c(
      0.225,
      0.225
    ),
    y = c(
      0.16,
      0.84
    ),
    color = "#9AA3B2",
    linewidth = 0.8
  ) +
  draw_label(
    paste0(
      "Fuente: elaboración propia con microdatos PISA 2025.\n",
      "La escala muestra diferencias condicionales respecto de un hombre con ESCS = 0;\n",
      "no corresponde a puntajes absolutos predichos. Asociaciones estadísticas; no representan efectos causales.\n",
      "Laboratorio de Investigación Social Avanzada · LISA"
    ),
    x = 0.245,
    y = 0.50,
    hjust = 0,
    vjust = 0.5,
    color = gris_texto,
    size = 8.7,
    lineheight = 1.25
  )

grafica_s2_g07 <- plot_grid(
  grafica_s2_g07_base,
  pie_s2_g07,
  ncol = 1,
  rel_heights = c(
    0.87,
    0.13
  ),
  align = "v"
)

# 18. VISUALIZACIÓN

print(
  grafica_s2_g05
)

print(
  grafica_s2_g06
)

print(
  grafica_s2_g07
)

# 19. EXPORTACIÓN

ggsave(
  filename = archivo_salida_s2_g05,
  plot = grafica_s2_g05,
  width = 10,
  height = 9,
  units = "in",
  dpi = 320,
  bg = "white"
)

ggsave(
  filename = archivo_salida_s2_g06,
  plot = grafica_s2_g06,
  width = 10,
  height = 12,
  units = "in",
  dpi = 320,
  bg = "white"
)

ggsave(
  filename = archivo_salida_s2_g07,
  plot = grafica_s2_g07,
  width = 10,
  height = 10,
  units = "in",
  dpi = 320,
  bg = "white"
)

# 17. SECCIÓN III · AMBIENTE, ACCIÓN Y ESCUELA

modelo_capacidad <- resultados$ambiente_escuela$modelo_agencia
modelo_participacion <- resultados$ambiente_escuela$modelo_accion
modelos_escolares_ambiente <- resultados$ambiente_escuela$modelos_escolares
centros_ambientales <- resultados$ambiente_escuela$centros_ambientales
distribucion_perfiles_ambientales <- resultados$ambiente_escuela$distribucion_perfiles_ambientales

archivo_salida_g08 <- "output/figures/PISA_Ambiente_capacidad_participacion.png"
archivo_salida_g09 <- "output/figures/PISA_Ambiente_ESCS_puntajes_participacion.png"
archivo_salida_g10 <- "output/figures/PISA_Ambiente_perfiles_kmeans.png"

# 18. FIGURA 08 · DE LA CAPACIDAD PERCIBIDA A LA PARTICIPACIÓN

datos_g08 <- bind_rows(
  modelo_capacidad |>
    mutate(
      resultado = "Capacidad percibida"
    ),
  modelo_participacion |>
    mutate(
      resultado = "Participación ambiental"
    )
) |>
  filter(
    term != "(Intercept)",
    term %in% c(
      "ESCS",
      "ENVAWARE",
      "ENVCAPCH",
      "COLLENEFF",
      "OPENVLRN",
      "CURIO",
      "PERSEV",
      "BELONG",
      "TEACHSUP"
    ) |
      grepl(
        "^sexo",
        term
      )
  ) |>
  transmute(
    resultado,
    indicador = case_when(
      grepl("^sexoMujer", term) ~ "Mujer (ref. hombre)",
      grepl("^sexoHombre", term) ~ "Hombre (ref. mujer)",
      term == "ESCS" ~ "Estatus socioeconómico",
      term == "ENVAWARE" ~ "Conocimiento ambiental",
      term == "ENVCAPCH" ~ "Capacidad percibida",
      term == "COLLENEFF" ~ "Eficacia de la acción colectiva",
      term == "OPENVLRN" ~ "Apertura al aprendizaje",
      term == "CURIO" ~ "Curiosidad",
      term == "PERSEV" ~ "Perseverancia",
      term == "BELONG" ~ "Pertenencia a la escuela",
      term == "TEACHSUP" ~ "Apoyo docente",
      TRUE ~ term
    ),
    efecto = estimate,
    conf.low,
    conf.high,
    evidencia = case_when(
      conf.low > 0 ~ "Positiva",
      conf.high < 0 ~ "Negativa",
      TRUE ~ "Sin evidencia clara"
    )
  ) |>
  mutate(
    resultado = factor(
      resultado,
      levels = c(
        "Capacidad percibida",
        "Participación ambiental"
      )
    ),
    indicador = factor(
      indicador,
      levels = rev(
        c(
          "Mujer (ref. hombre)",
          "Estatus socioeconómico",
          "Conocimiento ambiental",
          "Capacidad percibida",
          "Eficacia de la acción colectiva",
          "Apertura al aprendizaje",
          "Curiosidad",
          "Perseverancia",
          "Pertenencia a la escuela",
          "Apoyo docente"
        )
      )
    )
  )

margen_g08 <- max(
  abs(
    c(
      datos_g08$conf.low,
      datos_g08$conf.high
    )
  ),
  na.rm = TRUE
) * 0.05

grafica_g08_base <- ggplot(
  datos_g08,
  aes(
    x = efecto,
    y = indicador,
    color = evidencia
  )
) +
  geom_vline(
    xintercept = 0,
    color = gris_texto,
    linewidth = 0.8
  ) +
  geom_segment(
    aes(
      x = conf.low,
      xend = conf.high,
      yend = indicador
    ),
    linewidth = 1.15
  ) +
  geom_point(
    size = 4.5
  ) +
  geom_text(
    aes(
      x = if_else(
        efecto >= 0,
        conf.high + margen_g08,
        conf.low - margen_g08
      ),
      label = sprintf(
        "%+.2f",
        efecto
      ),
      hjust = if_else(
        efecto >= 0,
        0,
        1
      )
    ),
    color = grafito,
    fontface = "bold",
    size = 3.8,
    show.legend = FALSE
  ) +
  facet_wrap(
    ~resultado,
    ncol = 2,
    scales = "free_y"
  ) +
  scale_color_manual(
    values = c(
      "Positiva" = positivo,
      "Negativa" = negativo,
      "Sin evidencia clara" = gris_texto
    )
  ) +
  scale_x_continuous(
    expand = expansion(
      mult = c(
        0.18,
        0.22
      )
    )
  ) +
  labs(
    title = "¿Qué está asociado con sentirse capaz y con actuar?",
    subtitle = paste0(
      "Factores asociados con la capacidad percibida para intervenir y con la participación ambiental.\n",
      "Las líneas muestran intervalos de confianza de 95%."
    ),
    x = "Coeficiente",
    y = NULL,
    color = NULL
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  theme_minimal(
    base_size = 16
  ) +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      color = gris_linea,
      linewidth = 0.55
    ),
    axis.text.x = element_text(
      color = gris_texto,
      size = 11
    ),
    axis.text.y = element_text(
      color = grafito,
      size = 11.5
    ),
    axis.title.x = element_text(
      color = gris_texto,
      size = 12,
      margin = margin(
        t = 14
      )
    ),
    strip.text = element_text(
      color = grafito,
      face = "bold",
      size = 14
    ),
    strip.background = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(
      color = gris_texto,
      size = 10.5
    ),
    plot.title = element_text(
      color = grafito,
      face = "bold",
      size = 23,
      lineheight = 1.05
    ),
    plot.subtitle = element_text(
      color = gris_texto,
      size = 13,
      lineheight = 1.15,
      margin = margin(
        b = 20
      )
    ),
    plot.title.position = "plot",
    plot.subtitle.position = "plot",
    plot.margin = margin(
      t = 28,
      r = 45,
      b = 5,
      l = 28
    )
  )

pie_g08 <- ggdraw() +
  draw_grob(
    logo_grob,
    x = 0,
    y = 0.18,
    width = 0.21,
    height = 0.64
  ) +
  draw_line(
    x = c(
      0.225,
      0.225
    ),
    y = c(
      0.16,
      0.84
    ),
    color = "#9AA3B2",
    linewidth = 0.8
  ) +
  draw_label(
    paste0(
      "Fuente: elaboración propia con microdatos PISA 2025.\n",
      "Modelos multivariables con diseño muestral complejo BRR-Fay. Intervalos de confianza de 95%.\n",
      "Los resultados expresan asociaciones estadísticas y no representan efectos causales.\n",
      "Laboratorio de Investigación Social Avanzada · LISA"
    ),
    x = 0.245,
    y = 0.50,
    hjust = 0,
    vjust = 0.5,
    color = gris_texto,
    size = 8.7,
    lineheight = 1.25
  )

grafica_g08 <- plot_grid(
  grafica_g08_base,
  pie_g08,
  ncol = 1,
  rel_heights = c(
    0.86,
    0.14
  ),
  align = "v"
)

print(
  grafica_g08
)

ggsave(
  filename = archivo_salida_g08,
  plot = grafica_g08,
  width = 13,
  height = 9,
  units = "in",
  dpi = 320,
  bg = "white"
)

# 19. FIGURA 09 · ESCS EN PUNTAJES Y PARTICIPACIÓN

datos_g09_puntajes <- bind_rows(
  lapply(
    names(modelos_escolares_ambiente),
    function(dominio) {
      modelos_escolares_ambiente[[dominio]] |>
        filter(
          term == "ESCS"
        ) |>
        mutate(
          dominio = dominio
        )
    }
  )
) |>
  transmute(
    dominio = recode(
      dominio,
      matematicas = "Matemáticas",
      lectura = "Lectura",
      ciencias = "Ciencias",
      digital = "Desempeño digital",
      ambiental = "Ciencias ambientales"
    ),
    efecto = estimate,
    conf.low,
    conf.high
  ) |>
  mutate(
    dominio = factor(
      dominio,
      levels = rev(
        c(
          "Matemáticas",
          "Lectura",
          "Ciencias",
          "Desempeño digital",
          "Ciencias ambientales"
        )
      )
    )
  )

datos_g09_participacion <- modelo_participacion |>
  filter(
    term == "ESCS"
  ) |>
  transmute(
    indicador = "Participación ambiental",
    efecto = estimate,
    conf.low,
    conf.high,
    evidencia = case_when(
      conf.low > 0 | conf.high < 0 ~ "Asociación clara",
      TRUE ~ "Sin evidencia clara"
    )
  )

margen_g09a <- max(
  datos_g09_puntajes$conf.high,
  na.rm = TRUE
) * 0.05

rango_g09b <- max(
  abs(
    c(
      datos_g09_participacion$conf.low,
      datos_g09_participacion$conf.high
    )
  ),
  na.rm = TRUE
)

grafica_g09a <- ggplot(
  datos_g09_puntajes,
  aes(
    x = efecto,
    y = dominio
  )
) +
  geom_vline(
    xintercept = 0,
    color = gris_texto,
    linewidth = 0.8
  ) +
  geom_segment(
    aes(
      x = conf.low,
      xend = conf.high,
      yend = dominio
    ),
    color = positivo,
    linewidth = 1.3
  ) +
  geom_point(
    color = positivo,
    size = 5
  ) +
  geom_text(
    aes(
      x = conf.high + margen_g09a,
      label = sprintf(
        "%+.1f",
        efecto
      )
    ),
    hjust = 0,
    color = grafito,
    fontface = "bold",
    size = 4.1
  ) +
  scale_x_continuous(
    expand = expansion(
      mult = c(
        0.05,
        0.22
      )
    )
  ) +
  labs(
    title = "Puntajes",
    subtitle = "Cambio asociado con una unidad adicional de ESCS",
    x = "Puntos PISA",
    y = NULL
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  theme_minimal(
    base_size = 15
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      color = gris_linea,
      linewidth = 0.55
    ),
    axis.text = element_text(
      color = gris_texto
    ),
    axis.text.y = element_text(
      color = grafito,
      size = 12
    ),
    axis.title.x = element_text(
      color = gris_texto,
      size = 11.5
    ),
    plot.title = element_text(
      color = grafito,
      face = "bold",
      size = 18
    ),
    plot.subtitle = element_text(
      color = gris_texto,
      size = 11.5,
      lineheight = 1.1
    ),
    plot.margin = margin(
      t = 10,
      r = 35,
      b = 10,
      l = 10
    )
  )

grafica_g09b <- ggplot(
  datos_g09_participacion,
  aes(
    x = efecto,
    y = indicador,
    color = evidencia
  )
) +
  geom_vline(
    xintercept = 0,
    color = gris_texto,
    linewidth = 0.8
  ) +
  geom_segment(
    aes(
      x = conf.low,
      xend = conf.high,
      yend = indicador
    ),
    linewidth = 1.3
  ) +
  geom_point(
    size = 5
  ) +
  geom_text(
    aes(
      x = conf.high + rango_g09b * 0.15,
      label = sprintf(
        "%+.3f",
        efecto
      )
    ),
    hjust = 0,
    color = grafito,
    fontface = "bold",
    size = 4.1,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Asociación clara" = positivo,
      "Sin evidencia clara" = gris_texto
    )
  ) +
  scale_x_continuous(
    expand = expansion(
      mult = c(
        0.25,
        0.35
      )
    )
  ) +
  labs(
    title = "Participación",
    subtitle = "Asociación de ESCS con participación ambiental",
    x = "Desviaciones estándar",
    y = NULL
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  theme_minimal(
    base_size = 15
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      color = gris_linea,
      linewidth = 0.55
    ),
    axis.text = element_text(
      color = gris_texto
    ),
    axis.text.y = element_text(
      color = grafito,
      size = 12
    ),
    axis.title.x = element_text(
      color = gris_texto,
      size = 11.5
    ),
    plot.title = element_text(
      color = grafito,
      face = "bold",
      size = 18
    ),
    plot.subtitle = element_text(
      color = gris_texto,
      size = 11.5,
      lineheight = 1.1
    ),
    legend.position = "none",
    plot.margin = margin(
      t = 10,
      r = 45,
      b = 10,
      l = 15
    )
  )

titulo_g09 <- ggdraw() +
  draw_label(
    "El origen social pierde peso cuando pasamos del desempeño a la participación",
    x = 0.03,
    y = 0.70,
    hjust = 0,
    vjust = 0.5,
    color = grafito,
    fontface = "bold",
    size = 22
  ) +
  draw_label(
    "Asociación del índice socioeconómico ESCS con distintos resultados · México · PISA 2025",
    x = 0.03,
    y = 0.22,
    hjust = 0,
    vjust = 0.5,
    color = gris_texto,
    size = 12.8
  )

paneles_g09 <- plot_grid(
  grafica_g09a,
  grafica_g09b,
  nrow = 1,
  rel_widths = c(
    0.66,
    0.34
  ),
  align = "h",
  axis = "tb"
)

pie_g09 <- ggdraw() +
  draw_grob(
    logo_grob,
    x = 0,
    y = 0.18,
    width = 0.21,
    height = 0.64
  ) +
  draw_line(
    x = c(
      0.225,
      0.225
    ),
    y = c(
      0.16,
      0.84
    ),
    color = "#9AA3B2",
    linewidth = 0.8
  ) +
  draw_label(
    paste0(
      "Fuente: elaboración propia con microdatos PISA 2025.\n",
      "El panel izquierdo expresa efectos en puntos PISA; el derecho, en desviaciones estándar de participación.\n",
      "Las magnitudes de ambos paneles no son directamente comparables. Las líneas muestran intervalos de confianza de 95%.\n",
      "Laboratorio de Investigación Social Avanzada · LISA"
    ),
    x = 0.245,
    y = 0.50,
    hjust = 0,
    vjust = 0.5,
    color = gris_texto,
    size = 8.6,
    lineheight = 1.25
  )

grafica_g09 <- plot_grid(
  titulo_g09,
  paneles_g09,
  pie_g09,
  ncol = 1,
  rel_heights = c(
    0.13,
    0.73,
    0.14
  ),
  align = "v"
)

print(
  grafica_g09
)

ggsave(
  filename = archivo_salida_g09,
  plot = grafica_g09,
  width = 13,
  height = 8,
  units = "in",
  dpi = 320,
  bg = "white"
)

# 20. FIGURA 10 · PERFILES AMBIENTALES

centros_g10 <- centros_ambientales

centros_g10$promedio <- rowMeans(
  centros_g10[
    c(
      "ENVAWARE",
      "ENVCAPCH",
      "COLLENEFF",
      "ENVAPART"
    )
  ],
  na.rm = TRUE
)

idx_informados <- which.max(
  centros_g10$promedio
)

idx_desvinculados <- which.min(
  centros_g10$promedio
)

idx_restantes <- setdiff(
  seq_len(
    nrow(
      centros_g10
    )
  ),
  c(
    idx_informados,
    idx_desvinculados
  )
)

idx_participativos <- idx_restantes[
  which.max(
    centros_g10$ENVAPART[
      idx_restantes
    ]
  )
]

idx_baja_participacion <- setdiff(
  idx_restantes,
  idx_participativos
)

centros_g10$perfil <- NA_character_
centros_g10$perfil[idx_informados] <- "Informados y activos"
centros_g10$perfil[idx_desvinculados] <- "Desvinculados"
centros_g10$perfil[idx_participativos] <- "Participativos medios"
centros_g10$perfil[idx_baja_participacion] <- "De baja participación"

lookup_perfiles_g10 <- centros_g10 |>
  transmute(
    cluster = as.character(
      perfil_ambiental
    ),
    perfil
  )

distribucion_g10 <- distribucion_perfiles_ambientales |>
  transmute(
    cluster = sub(
      ".*?([0-9]+)$",
      "\\1",
      medida
    ),
    porcentaje = estimate * 100
  ) |>
  left_join(
    lookup_perfiles_g10,
    by = "cluster"
  )

datos_g10 <- centros_g10 |>
  mutate(
    cluster = as.character(
      perfil_ambiental
    )
  ) |>
  dplyr::select(
    cluster,
    perfil,
    ENVAWARE,
    ENVCAPCH,
    COLLENEFF,
    ENVAPART
  ) |>
  tidyr::pivot_longer(
    cols = c(
      ENVAWARE,
      ENVCAPCH,
      COLLENEFF,
      ENVAPART
    ),
    names_to = "variable",
    values_to = "valor"
  ) |>
  left_join(
    distribucion_g10 |>
      dplyr::select(
        cluster,
        porcentaje
      ),
    by = "cluster"
  ) |>
  mutate(
    dimension = recode(
      variable,
      ENVAWARE = "Conocimiento",
      ENVCAPCH = "Capacidad percibida",
      COLLENEFF = "Eficacia colectiva",
      ENVAPART = "Participación"
    ),
    dimension = factor(
      dimension,
      levels = rev(
        c(
          "Conocimiento",
          "Capacidad percibida",
          "Eficacia colectiva",
          "Participación"
        )
      )
    ),
    perfil = factor(
      perfil,
      levels = c(
        "Informados y activos",
        "Desvinculados",
        "Participativos medios",
        "De baja participación"
      )
    ),
    perfil_etiqueta = paste0(
      perfil,
      "\n",
      round(
        porcentaje
      ),
      "% de estudiantes"
    ),
    perfil_etiqueta = factor(
      perfil_etiqueta,
      levels = unique(
        perfil_etiqueta[
          order(
            perfil
          )
        ]
      )
    ),
    direccion = if_else(
      valor >= 0,
      "Por encima del promedio",
      "Por debajo del promedio"
    )
  )

margen_g10 <- max(
  abs(
    datos_g10$valor
  ),
  na.rm = TRUE
) * 0.07

grafica_g10_base <- ggplot(
  datos_g10,
  aes(
    x = valor,
    y = dimension,
    color = direccion
  )
) +
  geom_vline(
    xintercept = 0,
    color = gris_texto,
    linewidth = 0.8
  ) +
  geom_segment(
    aes(
      x = 0,
      xend = valor,
      yend = dimension
    ),
    linewidth = 2
  ) +
  geom_point(
    size = 5
  ) +
  geom_text(
    aes(
      x = if_else(
        valor >= 0,
        valor + margen_g10,
        valor - margen_g10
      ),
      label = sprintf(
        "%+.2f",
        valor
      ),
      hjust = if_else(
        valor >= 0,
        0,
        1
      )
    ),
    color = grafito,
    fontface = "bold",
    size = 3.7,
    show.legend = FALSE
  ) +
  facet_wrap(
    ~perfil_etiqueta,
    ncol = 2
  ) +
  scale_color_manual(
    values = c(
      "Por encima del promedio" = positivo,
      "Por debajo del promedio" = negativo
    )
  ) +
  scale_x_continuous(
    expand = expansion(
      mult = c(
        0.18,
        0.18
      )
    )
  ) +
  labs(
    title = "Cuatro formas de relacionarse con los problemas ambientales",
    subtitle = paste0(
      "Perfiles construidos a partir de conocimiento, capacidad percibida,\n",
      "eficacia de la acción colectiva y participación ambiental."
    ),
    x = "Desviaciones estándar respecto del promedio",
    y = NULL,
    color = NULL
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  theme_minimal(
    base_size = 16
  ) +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      color = gris_linea,
      linewidth = 0.55
    ),
    axis.text.x = element_text(
      color = gris_texto,
      size = 10.5
    ),
    axis.text.y = element_text(
      color = grafito,
      size = 11.5,
      face = "bold"
    ),
    axis.title.x = element_text(
      color = gris_texto,
      size = 12,
      margin = margin(
        t = 14
      )
    ),
    strip.text = element_text(
      color = grafito,
      face = "bold",
      size = 13.5,
      lineheight = 1.05
    ),
    strip.background = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(
      color = gris_texto,
      size = 10.5
    ),
    plot.title = element_text(
      color = grafito,
      face = "bold",
      size = 23,
      lineheight = 1.05
    ),
    plot.subtitle = element_text(
      color = gris_texto,
      size = 13,
      lineheight = 1.15,
      margin = margin(
        b = 20
      )
    ),
    plot.title.position = "plot",
    plot.subtitle.position = "plot",
    plot.margin = margin(
      t = 28,
      r = 45,
      b = 5,
      l = 28
    )
  )

pie_g10 <- ggdraw() +
  draw_grob(
    logo_grob,
    x = 0,
    y = 0.18,
    width = 0.21,
    height = 0.64
  ) +
  draw_line(
    x = c(
      0.225,
      0.225
    ),
    y = c(
      0.16,
      0.84
    ),
    color = "#9AA3B2",
    linewidth = 0.8
  ) +
  draw_label(
    paste0(
      "Fuente: elaboración propia con microdatos PISA 2025.\n",
      "Perfiles obtenidos mediante k-means sobre cuatro escalas estandarizadas; k = 4 y 100 inicializaciones.\n",
      "Los porcentajes corresponden a la distribución ponderada de estudiantes entre los perfiles.\n",
      "Laboratorio de Investigación Social Avanzada · LISA"
    ),
    x = 0.245,
    y = 0.50,
    hjust = 0,
    vjust = 0.5,
    color = gris_texto,
    size = 8.7,
    lineheight = 1.25
  )

grafica_g10 <- plot_grid(
  grafica_g10_base,
  pie_g10,
  ncol = 1,
  rel_heights = c(
    0.86,
    0.14
  ),
  align = "v"
)

print(
  grafica_g10
)

ggsave(
  filename = archivo_salida_g10,
  plot = grafica_g10,
  width = 13,
  height = 9,
  units = "in",
  dpi = 320,
  bg = "white"
)
