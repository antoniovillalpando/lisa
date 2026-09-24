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
# ║              https://antoniovillalpando.github.io/lisa/.             ║
# ║               Contacto: profesorvillalpando@gmail.com                ║
# ╚══════════════════════════════════════════════════════════════════════╝

# ======================================================================
# Learning Poverty in Mexico
# 01 · EVOLUCIÓN HISTÓRICA DE LOS PUNTAJES PISA
# Fecha de consignación: 9 de septiembre de 2026
# Identificador: PISA-2025-G01
# ======================================================================

# Insumos
## data/processed/PISA_historical_scores_2000_2022.rds
## data/processed/PISA2025_world_scores.rds
## lisa_logo-full.png

# Salidas
## output/figures/PISA_Mexico_historico_2000_2025.png

# ======================== INICIA PROCESAMIENTO =======================


# 1. CARGA DE PAQUETES

library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(png)
library(grid)


# 2. ESTRUCTURA DE DIRECTORIOS

dir.create(
  "output/figures",
  recursive = TRUE,
  showWarnings = FALSE
)


# 3. DEFINICIÓN DE RUTAS

archivo_historico <- "data/processed/PISA_historical_scores_2000_2022.rds"

archivo_2025 <- "data/processed/PISA2025_world_scores.rds"

archivo_logo <- "lisa_logo-full.png"

archivo_salida <- "output/figures/PISA_Mexico_historico_2000_2025.png"


# 4. VERIFICACIÓN DE INSUMOS

archivos_requeridos <- c(
  archivo_historico,
  archivo_2025,
  archivo_logo
)

faltantes <- archivos_requeridos[
  !file.exists(
    archivos_requeridos
  )
]

if (length(faltantes) > 0) {
  
  stop(
    paste0(
      "No se encontraron los siguientes archivos:\n",
      paste(
        faltantes,
        collapse = "\n"
      )
    )
  )
}


# 5. CARGA DE DATOS

pisa_historical <- readRDS(
  archivo_historico
)

pisa_2025 <- readRDS(
  archivo_2025
)


# 6. INSPECCIÓN Y NORMALIZACIÓN DEL RDS 2025

names(pisa_2025) <- tolower(
  names(
    pisa_2025
  )
)

# Detectar columna de país

variable_pais <- intersect(
  c(
    "country",
    "cnt",
    "country_code",
    "code"
  ),
  names(
    pisa_2025
  )
)[1]

if (is.na(variable_pais)) {
  
  stop(
    paste0(
      "No pude identificar la variable de país en PISA2025_world_scores.rds.\n",
      "Variables disponibles:\n",
      paste(
        names(pisa_2025),
        collapse = ", "
      )
    )
  )
}


# Detectar columnas de puntaje

variable_math <- intersect(
  c(
    "math",
    "mathematics",
    "math_score",
    "mean_math"
  ),
  names(
    pisa_2025
  )
)[1]

variable_reading <- intersect(
  c(
    "reading",
    "read",
    "reading_score",
    "mean_reading"
  ),
  names(
    pisa_2025
  )
)[1]

variable_science <- intersect(
  c(
    "science",
    "scie",
    "science_score",
    "mean_science"
  ),
  names(
    pisa_2025
  )
)[1]


if (
  any(
    is.na(
      c(
        variable_math,
        variable_reading,
        variable_science
      )
    )
  )
) {
  
  stop(
    paste0(
      "No pude identificar las tres variables de puntaje en ",
      "PISA2025_world_scores.rds.\n",
      "Variables disponibles:\n",
      paste(
        names(pisa_2025),
        collapse = ", "
      )
    )
  )
}


# 7. EXTRAER MÉXICO 2025

mexico_2025 <- pisa_2025 |>
  
  filter(
    .data[[variable_pais]] == "MEX"
  ) |>
  
  transmute(
    year = 2025L,
    country = "MEX",
    math = as.numeric(
      .data[[variable_math]]
    ),
    reading = as.numeric(
      .data[[variable_reading]]
    ),
    science = as.numeric(
      .data[[variable_science]]
    )
  )


if (nrow(mexico_2025) != 1) {
  
  stop(
    paste0(
      "Se esperaba una sola fila para México en 2025, pero se encontraron ",
      nrow(mexico_2025),
      "."
    )
  )
}


# 8. INTEGRACIÓN DE LA SERIE 2000–2025

pisa_mexico <- pisa_historical |>
  
  filter(
    country == "MEX"
  ) |>
  
  select(
    year,
    country,
    math,
    reading,
    science
  ) |>
  
  bind_rows(
    mexico_2025
  ) |>
  
  arrange(
    year
  )


# 9. PASO A FORMATO LARGO

pisa_mexico_long <- pisa_mexico |>
  
  select(
    year,
    math,
    reading,
    science
  ) |>
  
  pivot_longer(
    cols = c(
      math,
      reading,
      science
    ),
    names_to = "materia",
    values_to = "puntaje"
  ) |>
  
  mutate(
    materia = recode(
      materia,
      math = "Matemáticas",
      reading = "Lectura",
      science = "Ciencias"
    ),
    
    materia = factor(
      materia,
      levels = c(
        "Matemáticas",
        "Lectura",
        "Ciencias"
      )
    )
  )


# 10. ÚLTIMO DATO DISPONIBLE POR MATERIA

ultimos <- pisa_mexico_long |>
  
  filter(
    !is.na(
      puntaje
    )
  ) |>
  
  group_by(
    materia
  ) |>
  
  slice_max(
    year,
    n = 1,
    with_ties = FALSE
  ) |>
  
  ungroup()


# 11. IDENTIDAD GRÁFICA LISA

grafito <- "#263248"

gris_texto <- "#586273"

gris_linea <- "#DDE2EA"

colores_lisa <- c(
  "Matemáticas" = "#F58F75",
  "Lectura"     = "#7869E8",
  "Ciencias"    = "#49AFE0"
)

formas_lisa <- c(
  "Matemáticas" = 16,
  "Lectura"     = 17,
  "Ciencias"    = 15
)


# 12. CONSTRUCCIÓN DE LA GRÁFICA BASE

grafica_base <- ggplot(
  pisa_mexico_long,
  aes(
    x = year,
    y = puntaje,
    color = materia,
    shape = materia,
    group = materia
  )
) +
  
  geom_line(
    linewidth = 1.6,
    alpha = 0.95,
    na.rm = TRUE
  ) +
  
  geom_point(
    size = 5.8,
    color = "white",
    na.rm = TRUE
  ) +
  
  geom_point(
    size = 4.2,
    stroke = 0.6,
    na.rm = TRUE
  ) +
  
  # Resaltar la ronda 2025
  
  geom_point(
    data = pisa_mexico_long |>
      filter(
        year == 2025
      ),
    size = 7.0,
    color = "white",
    show.legend = FALSE,
    na.rm = TRUE
  ) +
  
  geom_point(
    data = pisa_mexico_long |>
      filter(
        year == 2025
      ),
    size = 5.2,
    stroke = 0.7,
    show.legend = FALSE,
    na.rm = TRUE
  ) +
  
  # Etiquetas directas
  
  geom_text(
    data = ultimos,
    aes(
      x = year + 0.55,
      label = paste0(
        materia,
        "  ",
        round(
          puntaje
        )
      )
    ),
    hjust = 0,
    fontface = "bold",
    size = 5.2,
    show.legend = FALSE
  ) +
  
  scale_color_manual(
    values = colores_lisa
  ) +
  
  scale_shape_manual(
    values = formas_lisa
  ) +
  
  scale_x_continuous(
    breaks = sort(
      unique(
        pisa_mexico_long$year
      )
    ),
    limits = c(
      2000,
      2029
    ),
    expand = expansion(
      mult = c(
        0.01,
        0
      )
    )
  ) +
  
  scale_y_continuous(
    breaks = seq(
      380,
      430,
      10
    ),
    limits = c(
      380,
      430
    ),
    expand = expansion(
      mult = c(
        0,
        0.02
      )
    )
  ) +
  
  labs(
    title = "México en PISA",
    subtitle = "Evolución del puntaje promedio en matemáticas, lectura y ciencias · 2000–2025",
    x = NULL,
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
    
    panel.grid.major.x = element_blank(),
    
    panel.grid.major.y = element_line(
      color = gris_linea,
      linewidth = 0.55
    ),
    
    axis.text.x = element_text(
      color = gris_texto,
      size = 13,
      margin = margin(
        t = 8
      )
    ),
    
    axis.text.y = element_text(
      color = gris_texto,
      size = 13
    ),
    
    legend.position = "none",
    
    plot.title = element_text(
      color = grafito,
      face = "bold",
      size = 30,
      margin = margin(
        b = 6
      )
    ),
    
    plot.subtitle = element_text(
      color = gris_texto,
      size = 16,
      margin = margin(
        b = 26
      )
    ),
    
    plot.margin = margin(
      t = 28,
      r = 175,
      b = 10,
      l = 24
    )
  )


# 13. CARGA DEL LOGO LISA

logo_lisa <- readPNG(
  archivo_logo
)

logo_grob <- rasterGrob(
  logo_lisa,
  interpolate = TRUE
)


# 14. CONSTRUCCIÓN DEL PIE EDITORIAL

pie_texto <- ggdraw() +
  
  draw_grob(
    logo_grob,
    x = 0.00,
    y = 0.10,
    width = 0.19,
    height = 0.80
  ) +
  
  draw_line(
    x = c(
      0.205,
      0.205
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
      "Fuente: elaboración propia con datos armonizados de PISA vía learningtower ",
      "y microdatos PISA 2025.\n",
      "Laboratorio de Investigación Social Avanzada · LISA"
    ),
    x = 0.225,
    y = 0.50,
    hjust = 0,
    vjust = 0.5,
    color = gris_texto,
    size = 12.5,
    lineheight = 1.35
  )


# 15. INTEGRACIÓN FINAL

grafica_final <- plot_grid(
  grafica_base,
  pie_texto,
  ncol = 1,
  rel_heights = c(
    0.86,
    0.14
  ),
  align = "v"
)


# 16. VISUALIZACIÓN

print(
  grafica_final
)


# 17. EXPORTACIÓN

ggsave(
  filename = archivo_salida,
  plot = grafica_final,
  width = 12,
  height = 7,
  units = "in",
  dpi = 320,
  bg = "white"
)

# ======================================================================
# GRÁFICA 02 · NIVELES DE COMPETENCIA PISA 2025
# ======================================================================


# 18. DEFINICIÓN DE RUTAS

archivo_mexico_2025 <- "data/processed/PISA2025_MEX_student.rds"

archivo_salida_g02 <- "output/figures/PISA_Mexico_niveles_competencia_2025.png"


# 19. VERIFICACIÓN DEL INSUMO

if (!file.exists(archivo_mexico_2025)) {
  
  stop(
    paste(
      "No se encontró el archivo:",
      archivo_mexico_2025
    )
  )
}


# 20. CARGA DE MICRODATOS DE MÉXICO

pisa_mexico_2025 <- readRDS(
  archivo_mexico_2025
)


# 21. VERIFICACIÓN DE VARIABLES

variables_requeridas_g02 <- c(
  "W_FSTUWT",
  paste0("PV", 1:10, "MATH"),
  paste0("PV", 1:10, "READ"),
  paste0("PV", 1:10, "SCIE")
)

variables_faltantes_g02 <- setdiff(
  variables_requeridas_g02,
  names(pisa_mexico_2025)
)

if (length(variables_faltantes_g02) > 0) {
  
  stop(
    paste0(
      "Faltan las siguientes variables en PISA2025_MEX_student.rds:\n",
      paste(
        variables_faltantes_g02,
        collapse = ", "
      )
    )
  )
}


# 22. PREPARACIÓN DE VALORES PLAUSIBLES

pisa_pv_long <- pisa_mexico_2025 |>
  
  select(
    W_FSTUWT,
    all_of(
      paste0("PV", 1:10, "MATH")
    ),
    all_of(
      paste0("PV", 1:10, "READ")
    ),
    all_of(
      paste0("PV", 1:10, "SCIE")
    )
  ) |>
  
  pivot_longer(
    cols = starts_with("PV"),
    names_to = "variable",
    values_to = "puntaje"
  ) |>
  
  extract(
    variable,
    into = c(
      "pv",
      "dominio"
    ),
    regex = "^PV([0-9]+)(MATH|READ|SCIE)$"
  ) |>
  
  mutate(
    pv = as.integer(
      pv
    ),
    
    materia = recode(
      dominio,
      MATH = "Matemáticas",
      READ = "Lectura",
      SCIE = "Ciencias"
    )
  )


# 23. CLASIFICACIÓN EN NIVELES DE COMPETENCIA

pisa_pv_long <- pisa_pv_long |>
  
  mutate(
    nivel = case_when(
      
      dominio == "MATH" &
        puntaje < 420.07 ~
        "Debajo de Nivel 2",
      
      dominio == "MATH" &
        puntaje < 606.99 ~
        "Nivel 2–4",
      
      dominio == "MATH" &
        puntaje >= 606.99 ~
        "Nivel 5–6",
      
      
      dominio == "READ" &
        puntaje < 407.47 ~
        "Debajo de Nivel 2",
      
      dominio == "READ" &
        puntaje < 625.61 ~
        "Nivel 2–4",
      
      dominio == "READ" &
        puntaje >= 625.61 ~
        "Nivel 5–6",
      
      
      dominio == "SCIE" &
        puntaje < 409.54 ~
        "Debajo de Nivel 2",
      
      dominio == "SCIE" &
        puntaje < 633.33 ~
        "Nivel 2–4",
      
      dominio == "SCIE" &
        puntaje >= 633.33 ~
        "Nivel 5–6",
      
      TRUE ~ NA_character_
    )
  )


# 24. PROPORCIONES PONDERADAS PARA CADA VALOR PLAUSIBLE

niveles_por_pv <- pisa_pv_long |>
  
  filter(
    !is.na(puntaje),
    !is.na(W_FSTUWT),
    W_FSTUWT > 0,
    !is.na(nivel)
  ) |>
  
  group_by(
    materia,
    pv,
    nivel
  ) |>
  
  summarise(
    peso = sum(
      W_FSTUWT,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  
  complete(
    materia,
    pv,
    nivel = c(
      "Debajo de Nivel 2",
      "Nivel 2–4",
      "Nivel 5–6"
    ),
    fill = list(
      peso = 0
    )
  ) |>
  
  group_by(
    materia,
    pv
  ) |>
  
  mutate(
    proporcion = peso /
      sum(
        peso
      )
  ) |>
  
  ungroup()


# 25. COMBINACIÓN DE LOS 10 VALORES PLAUSIBLES

niveles_mexico_2025 <- niveles_por_pv |>
  
  group_by(
    materia,
    nivel
  ) |>
  
  summarise(
    proporcion = mean(
      proporcion,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  
  mutate(
    porcentaje = 100 *
      proporcion,
    
    materia = factor(
      materia,
      levels = c(
        "Matemáticas",
        "Lectura",
        "Ciencias"
      )
    ),
    
    nivel = factor(
      nivel,
      levels = c(
        "Debajo de Nivel 2",
        "Nivel 2–4",
        "Nivel 5–6"
      )
    )
  )


# 26. COMPROBACIÓN DE RESULTADOS

control_g02 <- niveles_mexico_2025 |>
  
  group_by(
    materia
  ) |>
  
  summarise(
    total = sum(
      porcentaje
    ),
    .groups = "drop"
  )

print(
  niveles_mexico_2025
)

print(
  control_g02
)

# 27. POSICIÓN DE ETIQUETAS

niveles_mexico_2025 <- niveles_mexico_2025 |>
  
  mutate(
    nivel = factor(
      nivel,
      levels = c(
        "Debajo de Nivel 2",
        "Nivel 2–4",
        "Nivel 5–6"
      )
    )
  ) |>
  
  group_by(
    materia
  ) |>
  
  arrange(
    nivel,
    .by_group = TRUE
  ) |>
  
  mutate(
    posicion = cumsum(
      porcentaje
    ) -
      porcentaje / 2,
    
    etiqueta = if_else(
      porcentaje >= 2,
      paste0(
        round(
          porcentaje
        ),
        "%"
      ),
      ""
    )
  ) |>
  
  ungroup()


# 28. IDENTIDAD GRÁFICA DE LOS NIVELES

colores_niveles <- c(
  "Debajo de Nivel 2" = "#F58F75",
  "Nivel 2–4"         = "#49AFE0",
  "Nivel 5–6"         = "#7869E8"
)
# 29. CONSTRUCCIÓN DE LA GRÁFICA 02

grafica_02_base <- ggplot(
  niveles_mexico_2025,
  aes(
    x = porcentaje,
    y = materia,
    fill = nivel
  )
) +
  
  geom_col(
    width = 0.58,
    color = "white",
    linewidth = 1.2,
    position = position_stack(
      reverse = TRUE
    )
  ) +
  
  geom_text(
    aes(
      x = posicion,
      label = etiqueta
    ),
    color = "white",
    fontface = "bold",
    size = 5.4
  ) +
  
  scale_fill_manual(
    values = colores_niveles,
    breaks = c(
      "Debajo de Nivel 2",
      "Nivel 2–4",
      "Nivel 5–6"
    )
  ) +
  
  scale_x_continuous(
    breaks = seq(
      0,
      100,
      20
    ),
    labels = function(x) {
      paste0(
        x,
        "%"
      )
    },
    expand = expansion(
      mult = c(
        0,
        0
      )
    )
  ) +
  
  coord_cartesian(
    xlim = c(
      0,
      100
    ),
    clip = "off"
  ) +
  
  labs(
    title = "¿Cuántos estudiantes alcanzan \n las competencias básicas?",
    subtitle = "Distribución de estudiantes mexicanos por nivel de competencia · PISA 2025",
    x = NULL,
    y = NULL,
    fill = NULL
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
      size = 13,
      margin = margin(
        t = 8
      )
    ),
    
    axis.text.y = element_text(
      color = grafito,
      size = 15,
      face = "bold",
      margin = margin(
        r = 12
      )
    ),
    
    legend.position = "bottom",
    
    legend.justification = "left",
    
    legend.text = element_text(
      color = gris_texto,
      size = 13
    ),
    
    legend.key.width = unit(
      1.4,
      "cm"
    ),
    
    legend.spacing.x = unit(
      0.25,
      "cm"
    ),
    
    plot.title = element_text(
      color = grafito,
      face = "bold",
      size = 30,
      margin = margin(
        b = 6
      )
    ),
    
    plot.subtitle = element_text(
      color = gris_texto,
      size = 16,
      margin = margin(
        b = 28
      )
    ),
    
    plot.margin = margin(
      t = 28,
      r = 45,
      b = 5,
      l = 24
    )
  )


# 30. NOTA INTERPRETATIVA

nota_g02 <- ggdraw() +
  
  draw_label(
    paste0(
      "Nota: menos de 3 de cada 1,000 estudiantes alcanzan los niveles 5–6. \n ",
      "La proporción es de 0.13% en matemáticas, 0.29% en lectura y 0.22% en ciencias."
    ),
    x = 0.00,
    y = 0.50,
    hjust = 0,
    vjust = 0.5,
    color = grafito,
    size = 12.5,
    fontface = "bold",
    lineheight = 1.25
  )


# 31. PIE EDITORIAL DE LA GRÁFICA 02

pie_g02 <- ggdraw() +
  
  draw_grob(
    logo_grob,
    x = 0.00,
    y = 0.10,
    width = 0.19,
    height = 0.80
  ) +
  
  draw_line(
    x = c(
      0.205,
      0.205
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
      "Fuente: elaboración propia con microdatos de PISA 2025.\n",
      "Los porcentajes combinan los 10 valores plausibles utilizando el peso final del estudiante.\n",
      "Laboratorio de Investigación Social Avanzada · LISA"
    ),
    x = 0.225,
    y = 0.50,
    hjust = 0,
    vjust = 0.5,
    color = gris_texto,
    size = 11.5,
    lineheight = 1.30
  )


# 32. INTEGRACIÓN FINAL DE LA GRÁFICA 02

grafica_02 <- plot_grid(
  grafica_02_base,
  nota_g02,
  pie_g02,
  ncol = 1,
  rel_heights = c(
    0.78,
    0.07,
    0.15
  ),
  align = "v"
)


# 33. VISUALIZACIÓN

print(
  grafica_02
)


# 34. EXPORTACIÓN

ggsave(
  filename = archivo_salida_g02,
  plot = grafica_02,
  width = 12,
  height = 7,
  units = "in",
  dpi = 320,
  bg = "white"
)

# ======================================================================
# GRÁFICA 03 · POSICIÓN INTERNACIONAL DE MÉXICO
# ======================================================================


# 35. CARGA DE PAQUETES ADICIONALES

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


# 36. DEFINICIÓN DE RUTAS

archivo_salida_g03 <- "output/figures/PISA_Mundo_matematicas_2025.png"


# 37. PREPARACIÓN DE PUNTAJES INTERNACIONALES

pisa_world_2025 <- pisa_2025 |>
  
  transmute(
    country = .data[[variable_pais]],
    math = as.numeric(
      .data[[variable_math]]
    )
  ) |>
  
  filter(
    !is.na(country),
    !is.na(math)
  )


# 38. CARGA DE CARTOGRAFÍA MUNDIAL

mapa_mundo <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)


# 39. HOMOLOGACIÓN DE CÓDIGOS TERRITORIALES

mapa_mundo <- mapa_mundo |>
  
  mutate(
    country = iso_a3,
    
    country = case_when(
      adm0_a3 == "FRA" ~ "FRA",
      adm0_a3 == "NOR" ~ "NOR",
      adm0_a3 == "KOS" ~ "XKX",
      TRUE ~ country
    )
  )


# 40. INTEGRACIÓN DE MAPA Y RESULTADOS PISA

mapa_pisa_2025 <- mapa_mundo |>
  
  left_join(
    pisa_world_2025,
    by = "country"
  )


# 41. IDENTIFICACIÓN DE MÉXICO

mapa_mexico <- mapa_pisa_2025 |>
  
  filter(
    country == "MEX"
  )


# 42. SELECCIÓN DE PAÍSES DE REFERENCIA

comparadores_g03 <- c(
  "MEX",
  "USA",
  "CAN",
  "CHL",
  "BRA",
  "COL",
  "ARG",
  "ESP",
  "JPN",
  "KOR"
)


# 43. CONSTRUCCIÓN DE ETIQUETAS

etiquetas_g03 <- mapa_pisa_2025 |>
  
  filter(
    country %in% comparadores_g03,
    !is.na(math)
  )

puntos_g03 <- st_point_on_surface(
  etiquetas_g03$geometry
)

coordenadas_g03 <- st_coordinates(
  puntos_g03
)

etiquetas_g03 <- etiquetas_g03 |>
  
  st_drop_geometry() |>
  
  mutate(
    lon = coordenadas_g03[, 1],
    lat = coordenadas_g03[, 2],
    
    etiqueta = paste0(
      country,
      " · ",
      round(math)
    )
  )


# 44. POSICIONES MANUALES DE LAS ETIQUETAS

etiquetas_g03 <- etiquetas_g03 |>
  
  mutate(
    
    lon_etiqueta = case_when(
      country == "MEX" ~ lon - 10,
      country == "USA" ~ lon - 8,
      country == "CAN" ~ lon - 5,
      country == "CHL" ~ lon - 10,
      country == "BRA" ~ lon + 12,
      country == "COL" ~ lon - 12,
      country == "ARG" ~ lon + 10,
      country == "ESP" ~ lon - 10,
      country == "JPN" ~ lon + 10,
      country == "KOR" ~ lon - 12,
      TRUE ~ lon
    ),
    
    lat_etiqueta = case_when(
      country == "MEX" ~ lat - 8,
      country == "USA" ~ lat + 8,
      country == "CAN" ~ lat + 8,
      country == "CHL" ~ lat,
      country == "BRA" ~ lat,
      country == "COL" ~ lat + 5,
      country == "ARG" ~ lat - 2,
      country == "ESP" ~ lat + 7,
      country == "JPN" ~ lat + 5,
      country == "KOR" ~ lat - 5,
      TRUE ~ lat
    )
  )


# 45. IDENTIDAD GRÁFICA DEL MAPA

color_sin_datos <- "#EEF1F5"

color_frontera <- "white"

color_mexico <- "#F58F75"


# 46. CONSTRUCCIÓN DE LA GRÁFICA 03

grafica_03_base <- ggplot() +
  
  geom_sf(
    data = mapa_pisa_2025,
    aes(
      fill = math
    ),
    color = color_frontera,
    linewidth = 0.18
  ) +
  
  geom_sf(
    data = mapa_mexico,
    fill = color_mexico,
    color = grafito,
    linewidth = 1.15
  ) +
  
  geom_segment(
    data = etiquetas_g03,
    aes(
      x = lon,
      y = lat,
      xend = lon_etiqueta,
      yend = lat_etiqueta
    ),
    inherit.aes = FALSE,
    color = "#9AA3B2",
    linewidth = 0.35
  ) +
  
  geom_label(
    data = etiquetas_g03,
    aes(
      x = lon_etiqueta,
      y = lat_etiqueta,
      label = etiqueta
    ),
    inherit.aes = FALSE,
    size = 3.8,
    fontface = "bold",
    color = grafito,
    fill = "white",
    label.size = 0.18,
    label.padding = unit(
      0.18,
      "lines"
    )
  ) +
  
  scale_fill_gradientn(
    colours = c(
      "#F7D1C7",
      "#F3A28C",
      "#AFA7EE",
      "#7869E8",
      "#49AFE0"
    ),
    na.value = color_sin_datos,
    name = "Puntaje"
  ) +
  
  coord_sf(
    xlim = c(
      -170,
      180
    ),
    ylim = c(
      -58,
      85
    ),
    expand = FALSE
  ) +
  
  labs(
    title = "México frente al mundo",
    subtitle = "Puntaje promedio en matemáticas · PISA 2025",
    x = NULL,
    y = NULL
  ) +
  
  theme_void(
    base_size = 15
  ) +
  
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    legend.position = "bottom",
    
    legend.direction = "horizontal",
    
    legend.title = element_text(
      color = grafito,
      face = "bold",
      size = 12
    ),
    
    legend.text = element_text(
      color = gris_texto,
      size = 11
    ),
    
    legend.key.width = unit(
      2.5,
      "cm"
    ),
    
    plot.title = element_text(
      color = grafito,
      face = "bold",
      size = 30,
      margin = margin(
        b = 6
      )
    ),
    
    plot.subtitle = element_text(
      color = gris_texto,
      size = 16,
      margin = margin(
        b = 18
      )
    ),
    
    plot.margin = margin(
      t = 28,
      r = 24,
      b = 4,
      l = 24
    )
  )


# 47. PIE EDITORIAL DE LA GRÁFICA 03

pie_g03 <- ggdraw() +
  
  draw_grob(
    logo_grob,
    x = 0.00,
    y = 0.10,
    width = 0.19,
    height = 0.80
  ) +
  
  draw_line(
    x = c(
      0.205,
      0.205
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
      "Fuente: elaboración propia con microdatos de PISA 2025.\n",
      "El color representa el puntaje promedio nacional en matemáticas; México se destaca en salmón.\n",
      "Laboratorio de Investigación Social Avanzada · LISA"
    ),
    x = 0.225,
    y = 0.50,
    hjust = 0,
    vjust = 0.5,
    color = gris_texto,
    size = 11.5,
    lineheight = 1.30
  )


# 48. INTEGRACIÓN FINAL DE LA GRÁFICA 03

grafica_03 <- plot_grid(
  grafica_03_base,
  pie_g03,
  ncol = 1,
  rel_heights = c(
    0.85,
    0.15
  ),
  align = "v"
)


# 49. VISUALIZACIÓN

print(
  grafica_03
)


# 50. EXPORTACIÓN

ggsave(
  filename = archivo_salida_g03,
  plot = grafica_03,
  width = 12,
  height = 7,
  units = "in",
  dpi = 320,
  bg = "white"
)

# ======================================================================
# GRÁFICA 03B · POSICIÓN INTERNACIONAL DE MÉXICO EN LECTURA
# ======================================================================


# 51. PREPARACIÓN DE PUNTAJES INTERNACIONALES EN LECTURA

pisa_world_2025_read <- pisa_2025 |>
  
  transmute(
    country = .data[[variable_pais]],
    reading = as.numeric(
      .data[[variable_reading]]
    )
  ) |>
  
  filter(
    !is.na(country),
    !is.na(reading)
  )


# 52. INTEGRACIÓN DE MAPA Y RESULTADOS PISA EN LECTURA

mapa_pisa_2025_read <- mapa_mundo |>
  
  left_join(
    pisa_world_2025_read,
    by = "country"
  )


# 53. IDENTIFICACIÓN DE MÉXICO EN LECTURA

mapa_mexico_read <- mapa_pisa_2025_read |>
  
  filter(
    country == "MEX"
  )


# 54. CONSTRUCCIÓN DE ETIQUETAS EN LECTURA

etiquetas_g03_read <- mapa_pisa_2025_read |>
  
  filter(
    country %in% comparadores_g03,
    !is.na(reading)
  )

puntos_g03_read <- st_point_on_surface(
  etiquetas_g03_read$geometry
)

coordenadas_g03_read <- st_coordinates(
  puntos_g03_read
)

etiquetas_g03_read <- etiquetas_g03_read |>
  
  st_drop_geometry() |>
  
  mutate(
    lon = coordenadas_g03_read[, 1],
    lat = coordenadas_g03_read[, 2],
    
    etiqueta = paste0(
      country,
      " · ",
      round(reading)
    ),
    
    lon_etiqueta = case_when(
      country == "MEX" ~ lon - 10,
      country == "USA" ~ lon - 8,
      country == "CAN" ~ lon - 5,
      country == "CHL" ~ lon - 10,
      country == "BRA" ~ lon + 12,
      country == "COL" ~ lon - 12,
      country == "ARG" ~ lon + 10,
      country == "ESP" ~ lon - 10,
      country == "JPN" ~ lon + 10,
      country == "KOR" ~ lon - 12,
      TRUE ~ lon
    ),
    
    lat_etiqueta = case_when(
      country == "MEX" ~ lat - 8,
      country == "USA" ~ lat + 8,
      country == "CAN" ~ lat + 8,
      country == "CHL" ~ lat,
      country == "BRA" ~ lat,
      country == "COL" ~ lat + 5,
      country == "ARG" ~ lat - 2,
      country == "ESP" ~ lat + 7,
      country == "JPN" ~ lat + 5,
      country == "KOR" ~ lat - 5,
      TRUE ~ lat
    )
  )


# 55. CONSTRUCCIÓN DE LA GRÁFICA DE LECTURA

grafica_03_read_base <- ggplot() +
  
  geom_sf(
    data = mapa_pisa_2025_read,
    aes(
      fill = reading
    ),
    color = color_frontera,
    linewidth = 0.18
  ) +
  
  geom_sf(
    data = mapa_mexico_read,
    fill = color_mexico,
    color = grafito,
    linewidth = 1.15
  ) +
  
  geom_segment(
    data = etiquetas_g03_read,
    aes(
      x = lon,
      y = lat,
      xend = lon_etiqueta,
      yend = lat_etiqueta
    ),
    inherit.aes = FALSE,
    color = "#9AA3B2",
    linewidth = 0.35
  ) +
  
  geom_label(
    data = etiquetas_g03_read,
    aes(
      x = lon_etiqueta,
      y = lat_etiqueta,
      label = etiqueta
    ),
    inherit.aes = FALSE,
    size = 3.8,
    fontface = "bold",
    color = grafito,
    fill = "white",
    label.size = 0.18,
    label.padding = unit(
      0.18,
      "lines"
    )
  ) +
  
  scale_fill_gradientn(
    colours = c(
      "#F7D1C7",
      "#F3A28C",
      "#AFA7EE",
      "#7869E8",
      "#49AFE0"
    ),
    na.value = color_sin_datos,
    name = "Puntaje"
  ) +
  
  coord_sf(
    xlim = c(
      -170,
      180
    ),
    ylim = c(
      -58,
      85
    ),
    expand = FALSE
  ) +
  
  labs(
    title = "México frente al mundo",
    subtitle = "Puntaje promedio en lectura · PISA 2025",
    x = NULL,
    y = NULL
  ) +
  
  theme_void(
    base_size = 15
  ) +
  
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    legend.position = "bottom",
    
    legend.direction = "horizontal",
    
    legend.title = element_text(
      color = grafito,
      face = "bold",
      size = 12
    ),
    
    legend.text = element_text(
      color = gris_texto,
      size = 11
    ),
    
    legend.key.width = unit(
      2.5,
      "cm"
    ),
    
    plot.title = element_text(
      color = grafito,
      face = "bold",
      size = 30,
      margin = margin(
        b = 6
      )
    ),
    
    plot.subtitle = element_text(
      color = gris_texto,
      size = 16,
      margin = margin(
        b = 18
      )
    ),
    
    plot.margin = margin(
      t = 28,
      r = 24,
      b = 4,
      l = 24
    )
  )


# 56. PIE EDITORIAL DE LA GRÁFICA DE LECTURA

pie_g03_read <- ggdraw() +
  
  draw_grob(
    logo_grob,
    x = 0.00,
    y = 0.10,
    width = 0.19,
    height = 0.80
  ) +
  
  draw_line(
    x = c(
      0.205,
      0.205
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
      "Fuente: elaboración propia con microdatos de PISA 2025.\n",
      "El color representa el puntaje promedio nacional en lectura; México se destaca en salmón.\n",
      "Laboratorio de Investigación Social Avanzada · LISA"
    ),
    x = 0.225,
    y = 0.50,
    hjust = 0,
    vjust = 0.5,
    color = gris_texto,
    size = 11.5,
    lineheight = 1.30
  )


# 57. INTEGRACIÓN FINAL DE LA GRÁFICA DE LECTURA

grafica_03_read <- plot_grid(
  grafica_03_read_base,
  pie_g03_read,
  ncol = 1,
  rel_heights = c(
    0.85,
    0.15
  ),
  align = "v"
)


# 58. EXPORTACIÓN DE LA GRÁFICA DE LECTURA

ggsave(
  filename = "output/figures/PISA_Mundo_lectura_2025.png",
  plot = grafica_03_read,
  width = 12,
  height = 7,
  units = "in",
  dpi = 320,
  bg = "white"
)

# ======================================================================
# GRÁFICA 03C · POSICIÓN INTERNACIONAL DE MÉXICO EN CIENCIAS
# ======================================================================


# 59. PREPARACIÓN DE PUNTAJES INTERNACIONALES EN CIENCIAS

pisa_world_2025_science <- pisa_2025 |>
  
  transmute(
    country = .data[[variable_pais]],
    science = as.numeric(
      .data[[variable_science]]
    )
  ) |>
  
  filter(
    !is.na(country),
    !is.na(science)
  )


# 60. INTEGRACIÓN DE MAPA Y RESULTADOS PISA EN CIENCIAS

mapa_pisa_2025_science <- mapa_mundo |>
  
  left_join(
    pisa_world_2025_science,
    by = "country"
  )


# 61. IDENTIFICACIÓN DE MÉXICO EN CIENCIAS

mapa_mexico_science <- mapa_pisa_2025_science |>
  
  filter(
    country == "MEX"
  )


# 62. CONSTRUCCIÓN DE ETIQUETAS EN CIENCIAS

etiquetas_g03_science <- mapa_pisa_2025_science |>
  
  filter(
    country %in% comparadores_g03,
    !is.na(science)
  )

puntos_g03_science <- st_point_on_surface(
  etiquetas_g03_science$geometry
)

coordenadas_g03_science <- st_coordinates(
  puntos_g03_science
)

etiquetas_g03_science <- etiquetas_g03_science |>
  
  st_drop_geometry() |>
  
  mutate(
    lon = coordenadas_g03_science[, 1],
    lat = coordenadas_g03_science[, 2],
    
    etiqueta = paste0(
      country,
      " · ",
      round(science)
    ),
    
    lon_etiqueta = case_when(
      country == "MEX" ~ lon - 10,
      country == "USA" ~ lon - 8,
      country == "CAN" ~ lon - 5,
      country == "CHL" ~ lon - 10,
      country == "BRA" ~ lon + 12,
      country == "COL" ~ lon - 12,
      country == "ARG" ~ lon + 10,
      country == "ESP" ~ lon - 10,
      country == "JPN" ~ lon + 10,
      country == "KOR" ~ lon - 12,
      TRUE ~ lon
    ),
    
    lat_etiqueta = case_when(
      country == "MEX" ~ lat - 8,
      country == "USA" ~ lat + 8,
      country == "CAN" ~ lat + 8,
      country == "CHL" ~ lat,
      country == "BRA" ~ lat,
      country == "COL" ~ lat + 5,
      country == "ARG" ~ lat - 2,
      country == "ESP" ~ lat + 7,
      country == "JPN" ~ lat + 5,
      country == "KOR" ~ lat - 5,
      TRUE ~ lat
    )
  )


# 63. CONSTRUCCIÓN DE LA GRÁFICA DE CIENCIAS

grafica_03_science_base <- ggplot() +
  
  geom_sf(
    data = mapa_pisa_2025_science,
    aes(
      fill = science
    ),
    color = color_frontera,
    linewidth = 0.18
  ) +
  
  geom_sf(
    data = mapa_mexico_science,
    fill = color_mexico,
    color = grafito,
    linewidth = 1.15
  ) +
  
  geom_segment(
    data = etiquetas_g03_science,
    aes(
      x = lon,
      y = lat,
      xend = lon_etiqueta,
      yend = lat_etiqueta
    ),
    inherit.aes = FALSE,
    color = "#9AA3B2",
    linewidth = 0.35
  ) +
  
  geom_label(
    data = etiquetas_g03_science,
    aes(
      x = lon_etiqueta,
      y = lat_etiqueta,
      label = etiqueta
    ),
    inherit.aes = FALSE,
    size = 3.8,
    fontface = "bold",
    color = grafito,
    fill = "white",
    label.size = 0.18,
    label.padding = unit(
      0.18,
      "lines"
    )
  ) +
  
  scale_fill_gradientn(
    colours = c(
      "#F7D1C7",
      "#F3A28C",
      "#AFA7EE",
      "#7869E8",
      "#49AFE0"
    ),
    na.value = color_sin_datos,
    name = "Puntaje"
  ) +
  
  coord_sf(
    xlim = c(
      -170,
      180
    ),
    ylim = c(
      -58,
      85
    ),
    expand = FALSE
  ) +
  
  labs(
    title = "México frente al mundo",
    subtitle = "Puntaje promedio en ciencias · PISA 2025",
    x = NULL,
    y = NULL
  ) +
  
  theme_void(
    base_size = 15
  ) +
  
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    legend.position = "bottom",
    
    legend.direction = "horizontal",
    
    legend.title = element_text(
      color = grafito,
      face = "bold",
      size = 12
    ),
    
    legend.text = element_text(
      color = gris_texto,
      size = 11
    ),
    
    legend.key.width = unit(
      2.5,
      "cm"
    ),
    
    plot.title = element_text(
      color = grafito,
      face = "bold",
      size = 30,
      margin = margin(
        b = 6
      )
    ),
    
    plot.subtitle = element_text(
      color = gris_texto,
      size = 16,
      margin = margin(
        b = 18
      )
    ),
    
    plot.margin = margin(
      t = 28,
      r = 24,
      b = 4,
      l = 24
    )
  )


# 64. PIE EDITORIAL DE LA GRÁFICA DE CIENCIAS

pie_g03_science <- ggdraw() +
  
  draw_grob(
    logo_grob,
    x = 0.00,
    y = 0.10,
    width = 0.19,
    height = 0.80
  ) +
  
  draw_line(
    x = c(
      0.205,
      0.205
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
      "Fuente: elaboración propia con microdatos de PISA 2025.\n",
      "El color representa el puntaje promedio nacional en ciencias; México se destaca en salmón.\n",
      "Laboratorio de Investigación Social Avanzada · LISA"
    ),
    x = 0.225,
    y = 0.50,
    hjust = 0,
    vjust = 0.5,
    color = gris_texto,
    size = 11.5,
    lineheight = 1.30
  )


# 65. INTEGRACIÓN FINAL DE LA GRÁFICA DE CIENCIAS

grafica_03_science <- plot_grid(
  grafica_03_science_base,
  pie_g03_science,
  ncol = 1,
  rel_heights = c(
    0.85,
    0.15
  ),
  align = "v"
)


# 66. EXPORTACIÓN DE LA GRÁFICA DE CIENCIAS

ggsave(
  filename = "output/figures/PISA_Mundo_ciencias_2025.png",
  plot = grafica_03_science,
  width = 12,
  height = 7,
  units = "in",
  dpi = 320,
  bg = "white"
)

# ======================================================================
# GRÁFICA 04 · DESIGUALDAD SOCIOECONÓMICA
# ======================================================================


# 67. DEFINICIÓN DE RUTA DE SALIDA

archivo_salida_g04 <- "output/figures/PISA_Mexico_ESCS_2025.png"


# 68. CONSTRUCCIÓN DE CUARTILES PONDERADOS DE ESCS

pisa_escs_base <- pisa_mexico_2025 |>
  
  filter(
    !is.na(ESCS),
    !is.na(W_FSTUWT),
    W_FSTUWT > 0
  ) |>
  
  arrange(
    ESCS
  ) |>
  
  mutate(
    peso_acumulado = cumsum(
      W_FSTUWT
    ),
    
    proporcion_acumulada = peso_acumulado /
      sum(
        W_FSTUWT
      ),
    
    cuartil_escs = case_when(
      proporcion_acumulada <= 0.25 ~ "Q1",
      proporcion_acumulada <= 0.50 ~ "Q2",
      proporcion_acumulada <= 0.75 ~ "Q3",
      TRUE ~ "Q4"
    ),
    
    cuartil_escs = factor(
      cuartil_escs,
      levels = c(
        "Q1",
        "Q2",
        "Q3",
        "Q4"
      )
    )
  )


# 69. CONTROL DE DISTRIBUCIÓN PONDERADA POR CUARTIL

control_cuartiles_escs <- pisa_escs_base |>
  
  group_by(
    cuartil_escs
  ) |>
  
  summarise(
    peso = sum(
      W_FSTUWT
    ),
    .groups = "drop"
  ) |>
  
  mutate(
    porcentaje = 100 * peso /
      sum(
        peso
      )
  )

print(
  control_cuartiles_escs
)


# 70. CONVERSIÓN DE VALORES PLAUSIBLES A FORMATO LARGO

pisa_escs_long <- pisa_escs_base |>
  
  select(
    cuartil_escs,
    W_FSTUWT,
    all_of(
      paste0(
        "PV",
        1:10,
        "MATH"
      )
    ),
    all_of(
      paste0(
        "PV",
        1:10,
        "READ"
      )
    ),
    all_of(
      paste0(
        "PV",
        1:10,
        "SCIE"
      )
    )
  ) |>
  
  pivot_longer(
    cols = starts_with(
      "PV"
    ),
    names_to = "variable",
    values_to = "puntaje"
  ) |>
  
  extract(
    variable,
    into = c(
      "pv",
      "dominio"
    ),
    regex = "^PV([0-9]+)(MATH|READ|SCIE)$"
  ) |>
  
  mutate(
    pv = as.integer(
      pv
    ),
    
    materia = recode(
      dominio,
      MATH = "Matemáticas",
      READ = "Lectura",
      SCIE = "Ciencias"
    )
  )


# 71. ESTIMACIÓN DEL PUNTAJE POR CUARTIL Y VALOR PLAUSIBLE

puntajes_escs_por_pv <- pisa_escs_long |>
  
  filter(
    !is.na(puntaje),
    !is.na(W_FSTUWT),
    W_FSTUWT > 0
  ) |>
  
  group_by(
    materia,
    cuartil_escs,
    pv
  ) |>
  
  summarise(
    puntaje = weighted.mean(
      puntaje,
      W_FSTUWT,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# 72. COMBINACIÓN DE LOS 10 VALORES PLAUSIBLES

puntajes_escs_2025 <- puntajes_escs_por_pv |>
  
  group_by(
    materia,
    cuartil_escs
  ) |>
  
  summarise(
    puntaje = mean(
      puntaje,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  
  mutate(
    materia = factor(
      materia,
      levels = c(
        "Matemáticas",
        "Lectura",
        "Ciencias"
      )
    ),
    
    cuartil_escs = factor(
      cuartil_escs,
      levels = c(
        "Q1",
        "Q2",
        "Q3",
        "Q4"
      )
    )
  )


# 73. CONSTRUCCIÓN DE TRAMOS Q1–Q4 Y CÁLCULO DE BRECHAS

tramos_escs_2025 <- puntajes_escs_2025 |>
  
  select(
    materia,
    cuartil_escs,
    puntaje
  ) |>
  
  pivot_wider(
    names_from = cuartil_escs,
    values_from = puntaje
  ) |>
  
  mutate(
    brecha = Q4 - Q1,
    
    etiqueta_brecha = paste0(
      "+",
      round(
        brecha
      ),
      " puntos"
    ),
    
    y = case_when(
      materia == "Matemáticas" ~ 3,
      materia == "Lectura" ~ 2,
      materia == "Ciencias" ~ 1
    )
  )

print(
  tramos_escs_2025
)


# 74. CONSTRUCCIÓN DE MARCAS Q1–Q4

marcas_escs_2025 <- puntajes_escs_2025 |>
  
  mutate(
    y = case_when(
      materia == "Matemáticas" ~ 3,
      materia == "Lectura" ~ 2,
      materia == "Ciencias" ~ 1
    ),
    
    etiqueta = paste0(
      cuartil_escs,
      "\n",
      round(
        puntaje
      )
    )
  )


# 75. IDENTIDAD GRÁFICA

colores_g04 <- c(
  "Matemáticas" = "#F58F75",
  "Lectura" = "#7869E8",
  "Ciencias" = "#49AFE0"
)


# 76. CONSTRUCCIÓN DE LA GRÁFICA 04

grafica_04_base <- ggplot() +
  
  geom_segment(
    data = tramos_escs_2025,
    aes(
      x = Q1,
      xend = Q4,
      y = y,
      yend = y,
      color = materia
    ),
    linewidth = 9,
    lineend = "round"
  ) +
  
  geom_segment(
    data = marcas_escs_2025,
    aes(
      x = puntaje,
      xend = puntaje,
      y = y - 0.13,
      yend = y + 0.13
    ),
    color = "white",
    linewidth = 1.8,
    lineend = "butt"
  ) +
  
  geom_text(
    data = marcas_escs_2025,
    aes(
      x = puntaje,
      y = y + 0.30,
      label = etiqueta
    ),
    color = grafito,
    size = 4.0,
    fontface = "bold",
    lineheight = 0.95,
    vjust = 0
  ) +
  
  geom_text(
    data = tramos_escs_2025,
    aes(
      x = Q4,
      y = y,
      label = etiqueta_brecha,
      color = materia
    ),
    hjust = -0.40,
    vjust = 0.05,
    size = 5.0,
    fontface = "bold"
  ) +
  
  scale_color_manual(
    values = colores_g04,
    guide = "none"
  ) +
  
  scale_x_continuous(
    breaks = seq(
      300,
      600,
      50
    ),
    expand = expansion(
      mult = c(
        0.03,
        0.18
      )
    )
  ) +
  
  scale_y_continuous(
    breaks = c(
      3,
      2,
      1
    ),
    labels = c(
      "Matemáticas",
      "Lectura",
      "Ciencias"
    )
  ) +
  
  coord_cartesian(
    ylim = c(
      0.55,
      3.55
    ),
    clip = "off"
  ) +
  
  labs(
    title = "El origen social deja una distancia medible",
    subtitle = "Puntaje promedio según cuartil socioeconómico · México · PISA 2025",
    x = "Puntaje promedio",
    y = NULL
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
      size = 12.5,
      margin = margin(
        t = 8
      )
    ),
    
    axis.text.y = element_text(
      color = grafito,
      size = 15,
      face = "bold",
      margin = margin(
        r = 14
      )
    ),
    
    axis.title.x = element_text(
      color = gris_texto,
      size = 12,
      margin = margin(
        t = 10
      )
    ),
    
    plot.title = element_text(
      color = grafito,
      face = "bold",
      size = 30,
      margin = margin(
        b = 6
      )
    ),
    
    plot.subtitle = element_text(
      color = gris_texto,
      size = 16,
      margin = margin(
        b = 24
      )
    ),
    
    plot.margin = margin(
      t = 28,
      r = 130,
      b = 5,
      l = 24
    )
  )


# 77. NOTA INTERPRETATIVA

nota_g04 <- ggdraw() +
  
  draw_label(
    paste0(
      "Cada barra muestra la distancia entre el cuartil socioeconómico inferior (Q1) ",
      "y el superior (Q4). Las marcas intermedias corresponden a Q2 y Q3."
    ),
    x = 0.00,
    y = 0.50,
    hjust = 0,
    vjust = 0.5,
    color = grafito,
    size = 12.5,
    fontface = "bold",
    lineheight = 1.25
  )


# 78. PIE EDITORIAL DE LA GRÁFICA 04

pie_g04 <- ggdraw() +
  
  draw_grob(
    logo_grob,
    x = 0.00,
    y = 0.10,
    width = 0.19,
    height = 0.80
  ) +
  
  draw_line(
    x = c(
      0.205,
      0.205
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
      "Fuente: elaboración propia con microdatos de PISA 2025.\n",
      "Los cuartiles de ESCS se construyen utilizando el peso final del estudiante; ",
      "los puntajes combinan los 10 valores plausibles.\n",
      "Laboratorio de Investigación Social Avanzada · LISA"
    ),
    x = 0.225,
    y = 0.50,
    hjust = 0,
    vjust = 0.5,
    color = gris_texto,
    size = 11.5,
    lineheight = 1.30
  )


# 79. INTEGRACIÓN FINAL DE LA GRÁFICA 04

grafica_04 <- plot_grid(
  grafica_04_base,
  nota_g04,
  pie_g04,
  ncol = 1,
  rel_heights = c(
    0.78,
    0.07,
    0.15
  ),
  align = "v"
)


# 80. VISUALIZACIÓN

print(
  grafica_04
)


# 81. EXPORTACIÓN

ggsave(
  filename = archivo_salida_g04,
  plot = grafica_04,
  width = 12,
  height = 7,
  units = "in",
  dpi = 320,
  bg = "white"
)

# ======================================================================
# GRÁFICA 05 · CUARTILES SOCIOECONÓMICOS Y PAÍSES EQUIVALENTES
# ======================================================================


# 82. DEFINICIÓN DE RUTA DE SALIDA

archivo_salida_g05 <- "output/figures/PISA_Mexico_Math_cuartiles_paises_2025.png"


# 83. CARGA DE PUNTAJES INTERNACIONALES

pisa_world_scores <- readRDS(
  "data/processed/PISA2025_world_scores.rds"
)


# 84. IDENTIFICACIÓN DE VARIABLES DEL ARCHIVO INTERNACIONAL

col_codigo_g05 <- intersect(
  c(
    "CNT",
    "ISO3",
    "iso3",
    "iso3c",
    "country_code"
  ),
  names(
    pisa_world_scores
  )
)[1]


col_math_g05 <- intersect(
  c(
    "math",
    "math_score",
    "score_math",
    "mean_math",
    "MATH"
  ),
  names(
    pisa_world_scores
  )
)[1]


if (
  is.na(
    col_codigo_g05
  )
) {
  
  stop(
    "No se encontró una variable de código de país en PISA2025_world_scores.rds"
  )
}


if (
  is.na(
    col_math_g05
  )
) {
  
  stop(
    "No se encontró una variable de puntaje de matemáticas en PISA2025_world_scores.rds"
  )
}


# 85. PUNTAJES DE MATEMÁTICAS POR CUARTIL SOCIOECONÓMICO DE MÉXICO

mexico_math_cuartiles_g05 <- puntajes_escs_2025 |>
  
  filter(
    materia == "Matemáticas"
  ) |>
  
  transmute(
    cuartil_escs = as.character(
      cuartil_escs
    ),
    
    score_mex = puntaje
  )

print(
  mexico_math_cuartiles_g05
)


# 86. DEFINICIÓN DE PAÍSES COMPARADORES

comparadores_definidos_g05 <- tibble(
  
  cuartil_escs = c(
    "Q1",
    "Q2",
    "Q3",
    "Q4"
  ),
  
  codigo = c(
    "KGZ",
    "PER",
    "SAU",
    "ROU"
  ),
  
  pais = c(
    "Kirguistán",
    "Perú",
    "Arabia Saudita",
    "Rumania"
  )
)


# 87. BASE INTERNACIONAL DE MATEMÁTICAS

world_math_g05 <- pisa_world_scores |>
  
  transmute(
    codigo = as.character(
      .data[[col_codigo_g05]]
    ),
    
    score_pais = as.numeric(
      .data[[col_math_g05]]
    )
  ) |>
  
  filter(
    !is.na(
      codigo
    ),
    !is.na(
      score_pais
    )
  )

print(
  world_math_g05
)


# 88. INTEGRACIÓN DE COMPARADORES Y PUNTAJES

comparadores_g05 <- comparadores_definidos_g05 |>
  
  left_join(
    world_math_g05,
    by = "codigo"
  ) |>
  
  left_join(
    mexico_math_cuartiles_g05,
    by = "cuartil_escs"
  )

print(
  comparadores_g05
)


# 89. CARGA DE CONTORNOS DE LOS PAÍSES

mundo_contornos_g05 <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) |>
  
  select(
    codigo = iso_a3,
    geometry
  ) |>
  
  filter(
    codigo %in% comparadores_g05$codigo
  )


# 90. IDENTIDAD GRÁFICA

color_mexico_g05 <- "#F58F75"

relleno_pais_g05 <- "#EEF1F5"


# 91. FUNCIÓN PARA CONSTRUIR CADA CUADRANTE

crear_cuadrante_g05 <- function(
    cuartil_objetivo
) {
  
  
  ficha_panel <- comparadores_g05 |>
    
    filter(
      cuartil_escs == cuartil_objetivo
    )
  
  
  contorno_panel <- mundo_contornos_g05 |>
    
    filter(
      codigo == ficha_panel$codigo
    )
  
  
  if (
    nrow(
      ficha_panel
    ) != 1
  ) {
    
    stop(
      paste0(
        "No se encontró información única para ",
        cuartil_objetivo
      )
    )
  }
  
  
  if (
    nrow(
      contorno_panel
    ) == 0
  ) {
    
    stop(
      paste0(
        "No se encontró el contorno geográfico de ",
        ficha_panel$pais
      )
    )
  }
  
  
  # 91.1 SILUETA DEL PAÍS
  
  silueta_panel <- ggplot() +
    
    geom_sf(
      data = contorno_panel,
      fill = relleno_pais_g05,
      color = grafito,
      linewidth = 0.75
    ) +
    
    coord_sf(
      datum = NA,
      expand = FALSE
    ) +
    
    theme_void() +
    
    theme(
      plot.background = element_rect(
        fill = "white",
        color = NA
      ),
      
      plot.margin = margin(
        t = 0,
        r = 18,
        b = -16,
        l = 18
      )
    )
  
  
  # 91.2 NOMBRE Y PUNTAJES
  
  texto_panel <- ggdraw() +
    
    draw_label(
      ficha_panel$pais,
      x = 0.50,
      y = 0.84,
      hjust = 0.5,
      vjust = 0.5,
      color = grafito,
      size = 18,
      fontface = "bold"
    ) +
    
    draw_label(
      paste0(
        round(
          ficha_panel$score_pais
        ),
        " puntos"
      ),
      x = 0.50,
      y = 0.66,
      hjust = 0.5,
      vjust = 0.5,
      color = grafito,
      size = 16
    ) +
    
    draw_label(
      paste0(
        ficha_panel$cuartil_escs,
        " · México"
      ),
      x = 0.50,
      y = 0.35,
      hjust = 0.5,
      vjust = 0.5,
      color = color_mexico_g05,
      size = 16,
      fontface = "bold"
    ) +
    
    draw_label(
      paste0(
        round(
          ficha_panel$score_mex
        ),
        " puntos"
      ),
      x = 0.50,
      y = 0.17,
      hjust = 0.5,
      vjust = 0.5,
      color = color_mexico_g05,
      size = 16,
      fontface = "bold"
    )
  
  
  # 91.3 INTEGRACIÓN DEL CUADRANTE
  
  cuadrante_panel <- plot_grid(
    silueta_panel,
    texto_panel,
    ncol = 1,
    rel_heights = c(
      0.47,
      0.53
    )
  )
  
  
  return(
    cuadrante_panel
  )
}


# 92. CONSTRUCCIÓN DE LOS CUATRO CUADRANTES

panel_q1_g05 <- crear_cuadrante_g05(
  "Q1"
)

panel_q2_g05 <- crear_cuadrante_g05(
  "Q2"
)

panel_q3_g05 <- crear_cuadrante_g05(
  "Q3"
)

panel_q4_g05 <- crear_cuadrante_g05(
  "Q4"
)


# 93. DISTRIBUCIÓN 2 × 2

fila_superior_g05 <- plot_grid(
  panel_q1_g05,
  panel_q2_g05,
  ncol = 2,
  rel_widths = c(
    1,
    1
  )
)

fila_inferior_g05 <- plot_grid(
  panel_q3_g05,
  panel_q4_g05,
  ncol = 2,
  rel_widths = c(
    1,
    1
  )
)

cuerpo_g05 <- plot_grid(
  fila_superior_g05,
  fila_inferior_g05,
  ncol = 1,
  rel_heights = c(
    1,
    1
  )
)


# 94. ENCABEZADO DE LA GRÁFICA 05

encabezado_g05 <- ggdraw() +
  
  draw_label(
    "Un país distinto para cada nivel socioeconómico",
    x = 0.02,
    y = 0.72,
    hjust = 0,
    vjust = 0.5,
    color = grafito,
    size = 21,
    fontface = "bold"
  ) +
  
  draw_label(
    paste0(
      "Puntaje promedio en matemáticas por cuartil socioeconómico ",
      "y país con desempeño más cercano · México · PISA 2025"
    ),
    x = 0.02,
    y = 0.20,
    hjust = 0,
    vjust = 0.5,
    color = gris_texto,
    size = 12.8
  )


# 95. NOTA INTERPRETATIVA

nota_g05 <- ggdraw() +
  
  draw_label(
    paste0(
      "Cada cuadrante compara el puntaje promedio en matemáticas de un cuartil ",
      "socioeconómico de México con el promedio nacional de un país de desempeño similar."
    ),
    x = 0.02,
    y = 0.50,
    hjust = 0,
    vjust = 0.5,
    color = grafito,
    size = 12.3,
    fontface = "bold",
    lineheight = 1.25
  )


# 96. PIE EDITORIAL DE LA GRÁFICA 05

pie_g05 <- ggdraw() +
  
  draw_grob(
    logo_grob,
    x = 0.02,
    y = 0.10,
    width = 0.17,
    height = 0.80
  ) +
  
  draw_line(
    x = c(
      0.205,
      0.205
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
      "Fuente: elaboración propia con microdatos de PISA 2025.\n",
      "Los cuartiles de ESCS utilizan el peso final del estudiante; ",
      "los puntajes combinan los 10 valores plausibles.\n",
      "Laboratorio de Investigación Social Avanzada · LISA"
    ),
    x = 0.225,
    y = 0.50,
    hjust = 0,
    vjust = 0.5,
    color = gris_texto,
    size = 11.3,
    lineheight = 1.30
  )


# 97. INTEGRACIÓN FINAL DE LA GRÁFICA 05

grafica_05 <- plot_grid(
  encabezado_g05,
  cuerpo_g05,
  nota_g05,
  pie_g05,
  ncol = 1,
  rel_heights = c(
    0.09,
    0.71,
    0.06,
    0.14
  )
)


# 98. VISUALIZACIÓN Y EXPORTACIÓN

print(
  grafica_05
)

ggsave(
  filename = archivo_salida_g05,
  plot = grafica_05,
  width = 12,
  height = 10,
  units = "in",
  dpi = 320,
  bg = "white"
)

# ======================================================================
# GRÁFICA 06 · CUARTILES SOCIOECONÓMICOS Y PAÍSES EQUIVALENTES · CIENCIAS
# ======================================================================


# 99. DEFINICIÓN DE RUTA DE SALIDA

archivo_salida_g06 <- "output/figures/PISA_Mexico_Science_cuartiles_paises_2025.png"


# 100. CARGA DE PUNTAJES INTERNACIONALES

pisa_world_scores <- readRDS(
  "data/processed/PISA2025_world_scores.rds"
)


# 101. IDENTIFICACIÓN DE VARIABLES DEL ARCHIVO INTERNACIONAL

col_codigo_g06 <- intersect(
  c(
    "CNT",
    "ISO3",
    "iso3",
    "iso3c",
    "country_code"
  ),
  names(
    pisa_world_scores
  )
)[1]


col_science_g06 <- intersect(
  c(
    "science",
    "science_score",
    "score_science",
    "mean_science",
    "SCIE"
  ),
  names(
    pisa_world_scores
  )
)[1]


if (
  is.na(
    col_codigo_g06
  )
) {
  
  stop(
    "No se encontró una variable de código de país en PISA2025_world_scores.rds"
  )
}


if (
  is.na(
    col_science_g06
  )
) {
  
  stop(
    "No se encontró una variable de puntaje de ciencias en PISA2025_world_scores.rds"
  )
}


# 102. PUNTAJES DE CIENCIAS POR CUARTIL SOCIOECONÓMICO DE MÉXICO

mexico_science_cuartiles_g06 <- puntajes_escs_2025 |>
  
  filter(
    materia == "Ciencias"
  ) |>
  
  transmute(
    cuartil_escs = as.character(
      cuartil_escs
    ),
    
    score_mex = puntaje
  )

print(
  mexico_science_cuartiles_g06
)


# 103. BASE INTERNACIONAL DE CIENCIAS

world_science_g06 <- pisa_world_scores |>
  
  transmute(
    codigo = as.character(
      .data[[col_codigo_g06]]
    ),
    
    score_pais = as.numeric(
      .data[[col_science_g06]]
    )
  ) |>
  
  filter(
    !is.na(
      codigo
    ),
    !is.na(
      score_pais
    ),
    codigo != "MEX"
  )

print(
  world_science_g06
)


# 104. CARGA DE CONTORNOS Y NOMBRES DE PAÍSES

mundo_contornos_raw_g06 <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)


if (
  "name_es" %in% names(
    mundo_contornos_raw_g06
  )
) {
  
  mundo_contornos_g06 <- mundo_contornos_raw_g06 |>
    
    transmute(
      codigo = iso_a3,
      pais = name_es,
      geometry
    )
  
} else {
  
  mundo_contornos_g06 <- mundo_contornos_raw_g06 |>
    
    transmute(
      codigo = iso_a3,
      pais = name_long,
      geometry
    )
}


mundo_contornos_g06 <- mundo_contornos_g06 |>
  
  filter(
    !is.na(
      codigo
    ),
    codigo != "-99"
  )


# 105. RESTRICCIÓN A PAÍSES CON CONTORNO DISPONIBLE

world_science_contornos_g06 <- world_science_g06 |>
  
  inner_join(
    mundo_contornos_g06 |>
      st_drop_geometry() |>
      select(
        codigo,
        pais
      ),
    by = "codigo"
  ) |>
  
  distinct(
    codigo,
    .keep_all = TRUE
  )

print(
  world_science_contornos_g06
)


# 106. IDENTIFICACIÓN DEL PAÍS MÁS PRÓXIMO A CADA CUARTIL

comparadores_g06 <- tidyr::crossing(
  mexico_science_cuartiles_g06,
  world_science_contornos_g06
) |>
  
  mutate(
    diferencia = score_pais - score_mex,
    
    distancia_abs = abs(
      diferencia
    )
  ) |>
  
  group_by(
    cuartil_escs
  ) |>
  
  slice_min(
    order_by = distancia_abs,
    n = 1,
    with_ties = FALSE
  ) |>
  
  ungroup()


print(
  comparadores_g06
)


# 107. IDENTIDAD GRÁFICA

color_mexico_g06 <- "#49AFE0"

relleno_pais_g06 <- "#EEF1F5"


# 108. FUNCIÓN PARA CONSTRUIR CADA CUADRANTE

crear_cuadrante_g06 <- function(
    cuartil_objetivo
) {
  
  
  ficha_panel <- comparadores_g06 |>
    
    filter(
      cuartil_escs == cuartil_objetivo
    )
  
  
  contorno_panel <- mundo_contornos_g06 |>
    
    filter(
      codigo == ficha_panel$codigo
    )
  
  
  if (
    nrow(
      ficha_panel
    ) != 1
  ) {
    
    stop(
      paste0(
        "No se encontró información única para ",
        cuartil_objetivo
      )
    )
  }
  
  
  if (
    nrow(
      contorno_panel
    ) == 0
  ) {
    
    stop(
      paste0(
        "No se encontró el contorno geográfico de ",
        ficha_panel$pais
      )
    )
  }
  
  
  # 108.1 SILUETA DEL PAÍS
  
  silueta_panel <- ggplot() +
    
    geom_sf(
      data = contorno_panel,
      fill = relleno_pais_g06,
      color = grafito,
      linewidth = 0.75
    ) +
    
    coord_sf(
      datum = NA,
      expand = FALSE
    ) +
    
    theme_void() +
    
    theme(
      plot.background = element_rect(
        fill = "white",
        color = NA
      ),
      
      plot.margin = margin(
        t = 0,
        r = 18,
        b = -16,
        l = 18
      )
    )
  
  
  # 108.2 NOMBRE Y PUNTAJES
  
  texto_panel <- ggdraw() +
    
    draw_label(
      ficha_panel$pais,
      x = 0.50,
      y = 0.84,
      hjust = 0.5,
      vjust = 0.5,
      color = grafito,
      size = 18,
      fontface = "bold"
    ) +
    
    draw_label(
      paste0(
        round(
          ficha_panel$score_pais
        ),
        " puntos"
      ),
      x = 0.50,
      y = 0.66,
      hjust = 0.5,
      vjust = 0.5,
      color = grafito,
      size = 16
    ) +
    
    draw_label(
      paste0(
        ficha_panel$cuartil_escs,
        " · México"
      ),
      x = 0.50,
      y = 0.35,
      hjust = 0.5,
      vjust = 0.5,
      color = color_mexico_g06,
      size = 16,
      fontface = "bold"
    ) +
    
    draw_label(
      paste0(
        round(
          ficha_panel$score_mex
        ),
        " puntos"
      ),
      x = 0.50,
      y = 0.17,
      hjust = 0.5,
      vjust = 0.5,
      color = color_mexico_g06,
      size = 16,
      fontface = "bold"
    )
  
  
  # 108.3 INTEGRACIÓN DEL CUADRANTE
  
  cuadrante_panel <- plot_grid(
    silueta_panel,
    texto_panel,
    ncol = 1,
    rel_heights = c(
      0.47,
      0.53
    )
  )
  
  
  return(
    cuadrante_panel
  )
}


# 109. CONSTRUCCIÓN DE LOS CUATRO CUADRANTES

panel_q1_g06 <- crear_cuadrante_g06(
  "Q1"
)

panel_q2_g06 <- crear_cuadrante_g06(
  "Q2"
)

panel_q3_g06 <- crear_cuadrante_g06(
  "Q3"
)

panel_q4_g06 <- crear_cuadrante_g06(
  "Q4"
)


# 110. DISTRIBUCIÓN 2 × 2

fila_superior_g06 <- plot_grid(
  panel_q1_g06,
  panel_q2_g06,
  ncol = 2,
  rel_widths = c(
    1,
    1
  )
)

fila_inferior_g06 <- plot_grid(
  panel_q3_g06,
  panel_q4_g06,
  ncol = 2,
  rel_widths = c(
    1,
    1
  )
)

cuerpo_g06 <- plot_grid(
  fila_superior_g06,
  fila_inferior_g06,
  ncol = 1,
  rel_heights = c(
    1,
    1
  )
)


# 111. ENCABEZADO DE LA GRÁFICA 06

encabezado_g06 <- ggdraw() +
  
  draw_label(
    "Un país distinto para cada nivel socioeconómico",
    x = 0.02,
    y = 0.72,
    hjust = 0,
    vjust = 0.5,
    color = grafito,
    size = 21,
    fontface = "bold"
  ) +
  
  draw_label(
    paste0(
      "Puntaje promedio en ciencias por cuartil socioeconómico ",
      "y país con desempeño más cercano · México · PISA 2025"
    ),
    x = 0.02,
    y = 0.20,
    hjust = 0,
    vjust = 0.5,
    color = gris_texto,
    size = 12.8
  )


# 112. NOTA INTERPRETATIVA

nota_g06 <- ggdraw() +
  
  draw_label(
    paste0(
      "Cada cuadrante compara el puntaje promedio en ciencias de un cuartil ",
      "socioeconómico de México con el promedio nacional de un país de desempeño similar."
    ),
    x = 0.02,
    y = 0.50,
    hjust = 0,
    vjust = 0.5,
    color = grafito,
    size = 12.3,
    fontface = "bold",
    lineheight = 1.25
  )


# 113. PIE EDITORIAL DE LA GRÁFICA 06

pie_g06 <- ggdraw() +
  
  draw_grob(
    logo_grob,
    x = 0.02,
    y = 0.10,
    width = 0.17,
    height = 0.80
  ) +
  
  draw_line(
    x = c(
      0.205,
      0.205
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
      "Fuente: elaboración propia con microdatos de PISA 2025.\n",
      "Los cuartiles de ESCS utilizan el peso final del estudiante; ",
      "los puntajes combinan los 10 valores plausibles.\n",
      "Laboratorio de Investigación Social Avanzada · LISA"
    ),
    x = 0.225,
    y = 0.50,
    hjust = 0,
    vjust = 0.5,
    color = gris_texto,
    size = 11.3,
    lineheight = 1.30
  )


# 114. INTEGRACIÓN FINAL DE LA GRÁFICA 06

grafica_06 <- plot_grid(
  encabezado_g06,
  cuerpo_g06,
  nota_g06,
  pie_g06,
  ncol = 1,
  rel_heights = c(
    0.09,
    0.71,
    0.06,
    0.14
  )
)


# 115. VISUALIZACIÓN Y EXPORTACIÓN

print(
  grafica_06
)

ggsave(
  filename = archivo_salida_g06,
  plot = grafica_06,
  width = 12,
  height = 10,
  units = "in",
  dpi = 320,
  bg = "white"
)

# ======================================================================
# GRÁFICA 07 · RESULTADOS POR GÉNERO
# ======================================================================


# 116. DEFINICIÓN DE RUTA DE SALIDA

archivo_salida_g07 <- "output/figures/PISA_Mexico_genero_2025.png"


# 117. VERIFICACIÓN DE VARIABLE DE GÉNERO

if (
  !"ST004D01T" %in% names(
    pisa_mexico_2025
  )
) {
  
  stop(
    paste0(
      "La variable ST004D01T no está incluida en PISA2025_MEX_student.rds. ",
      "Agrégala al script 01_data.R, regenera el RDS y vuelve a ejecutar esta sección."
    )
  )
}


# 118. CONSTRUCCIÓN DE VARIABLE DE GÉNERO

pisa_genero_base <- pisa_mexico_2025 |>
  
  mutate(
    genero_original = haven::as_factor(
      ST004D01T
    ),
    
    genero = case_when(
      
      stringr::str_detect(
        stringr::str_to_lower(
          as.character(
            genero_original
          )
        ),
        "female|mujer|girl"
      ) ~ "Mujeres",
      
      stringr::str_detect(
        stringr::str_to_lower(
          as.character(
            genero_original
          )
        ),
        "male|hombre|boy"
      ) ~ "Hombres",
      
      as.numeric(
        ST004D01T
      ) == 1 ~ "Mujeres",
      
      as.numeric(
        ST004D01T
      ) == 2 ~ "Hombres",
      
      TRUE ~ NA_character_
    ),
    
    genero = factor(
      genero,
      levels = c(
        "Mujeres",
        "Hombres"
      )
    )
  ) |>
  
  filter(
    !is.na(
      genero
    ),
    !is.na(
      W_FSTUWT
    ),
    W_FSTUWT > 0
  )


print(
  table(
    pisa_genero_base$genero,
    useNA = "ifany"
  )
)


# 119. CONVERSIÓN DE VALORES PLAUSIBLES A FORMATO LARGO

pisa_genero_long <- pisa_genero_base |>
  
  select(
    genero,
    W_FSTUWT,
    all_of(
      paste0(
        "PV",
        1:10,
        "MATH"
      )
    ),
    all_of(
      paste0(
        "PV",
        1:10,
        "READ"
      )
    ),
    all_of(
      paste0(
        "PV",
        1:10,
        "SCIE"
      )
    )
  ) |>
  
  pivot_longer(
    cols = starts_with(
      "PV"
    ),
    names_to = "variable",
    values_to = "puntaje"
  ) |>
  
  extract(
    variable,
    into = c(
      "pv",
      "dominio"
    ),
    regex = "^PV([0-9]+)(MATH|READ|SCIE)$"
  ) |>
  
  mutate(
    pv = as.integer(
      pv
    ),
    
    materia = recode(
      dominio,
      MATH = "Matemáticas",
      READ = "Lectura",
      SCIE = "Ciencias"
    )
  )


# 120. ESTIMACIÓN DEL PUNTAJE POR GÉNERO Y VALOR PLAUSIBLE

puntajes_genero_por_pv <- pisa_genero_long |>
  
  filter(
    !is.na(
      puntaje
    )
  ) |>
  
  group_by(
    materia,
    genero,
    pv
  ) |>
  
  summarise(
    puntaje = weighted.mean(
      puntaje,
      W_FSTUWT,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# 121. COMBINACIÓN DE LOS 10 VALORES PLAUSIBLES

puntajes_genero_2025 <- puntajes_genero_por_pv |>
  
  group_by(
    materia,
    genero
  ) |>
  
  summarise(
    puntaje = mean(
      puntaje,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  
  mutate(
    materia = factor(
      materia,
      levels = c(
        "Matemáticas",
        "Lectura",
        "Ciencias"
      )
    ),
    
    genero = factor(
      genero,
      levels = c(
        "Mujeres",
        "Hombres"
      )
    )
  )


print(
  puntajes_genero_2025
)


# 122. CÁLCULO DE BRECHAS DE GÉNERO

brechas_genero_2025 <- puntajes_genero_2025 |>
  
  select(
    materia,
    genero,
    puntaje
  ) |>
  
  pivot_wider(
    names_from = genero,
    values_from = puntaje
  ) |>
  
  mutate(
    brecha = Mujeres - Hombres,
    
    minimo = pmin(
      Mujeres,
      Hombres
    ),
    
    maximo = pmax(
      Mujeres,
      Hombres
    ),
    
    etiqueta_brecha = case_when(
      
      brecha > 0 ~ paste0(
        "Mujeres +",
        round(
          abs(
            brecha
          )
        ),
        " puntos"
      ),
      
      brecha < 0 ~ paste0(
        "Hombres +",
        round(
          abs(
            brecha
          )
        ),
        " puntos"
      ),
      
      TRUE ~ "Sin diferencia"
    )
  )


print(
  brechas_genero_2025
)


# 123. CONSTRUCCIÓN DE BASE PARA ETIQUETAS

etiquetas_genero_g07 <- puntajes_genero_2025 |>
  
  mutate(
    etiqueta = paste0(
      genero,
      " · ",
      round(
        puntaje
      )
    ),
    
    posicion_vertical = case_when(
      genero == "Mujeres" ~ 0.24,
      genero == "Hombres" ~ -0.24
    )
  )


# 124. IDENTIDAD GRÁFICA

colores_g07 <- c(
  "Matemáticas" = "#F58F75",
  "Lectura" = "#7869E8",
  "Ciencias" = "#49AFE0"
)


# 125. LÍMITES DE LA ESCALA

limite_min_g07 <- floor(
  min(
    puntajes_genero_2025$puntaje
  ) / 10
) * 10 - 10


limite_max_g07 <- ceiling(
  max(
    puntajes_genero_2025$puntaje
  ) / 10
) * 10 + 30


# 126. CONSTRUCCIÓN DE LA GRÁFICA 07

grafica_07_base <- ggplot() +
  
  geom_segment(
    data = brechas_genero_2025,
    aes(
      x = minimo,
      xend = maximo,
      y = materia,
      yend = materia,
      color = materia
    ),
    linewidth = 7,
    lineend = "round",
    alpha = 0.22
  ) +
  
  geom_point(
    data = puntajes_genero_2025 |>
      filter(
        genero == "Mujeres"
      ),
    aes(
      x = puntaje,
      y = materia,
      fill = materia
    ),
    shape = 21,
    size = 7.2,
    color = "white",
    stroke = 1.15
  ) +
  
  geom_point(
    data = puntajes_genero_2025 |>
      filter(
        genero == "Hombres"
      ),
    aes(
      x = puntaje,
      y = materia,
      color = materia
    ),
    shape = 21,
    size = 7.2,
    fill = "white",
    stroke = 1.7
  ) +
  
  geom_text(
    data = etiquetas_genero_g07,
    aes(
      x = puntaje,
      y = materia,
      label = etiqueta,
      color = materia
    ),
    nudge_y = etiquetas_genero_g07$posicion_vertical,
    size = 4.3,
    fontface = "bold",
    show.legend = FALSE
  ) +
  
  geom_text(
    data = brechas_genero_2025,
    aes(
      x = maximo + 6,
      y = materia,
      label = etiqueta_brecha
    ),
    inherit.aes = FALSE,
    color = grafito,
    size = 4.5,
    fontface = "bold",
    hjust = 0
  ) +
  
  scale_color_manual(
    values = colores_g07,
    guide = "none"
  ) +
  
  scale_fill_manual(
    values = colores_g07,
    guide = "none"
  ) +
  
  scale_x_continuous(
    limits = c(
      limite_min_g07,
      limite_max_g07
    ),
    breaks = seq(
      limite_min_g07,
      limite_max_g07,
      10
    ),
    expand = expansion(
      mult = c(
        0,
        0
      )
    )
  ) +
  
  coord_cartesian(
    clip = "off"
  ) +
  
  labs(
    title = "La brecha de género cambia según la materia",
    subtitle = "Puntaje promedio de mujeres y hombres · México · PISA 2025",
    x = "Puntaje promedio",
    y = NULL
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
      linewidth = 0.50
    ),
    
    axis.text.x = element_text(
      color = gris_texto,
      size = 11.5,
      margin = margin(
        t = 8
      )
    ),
    
    axis.text.y = element_text(
      color = grafito,
      size = 15,
      face = "bold",
      margin = margin(
        r = 14
      )
    ),
    
    axis.title.x = element_text(
      color = gris_texto,
      size = 12,
      margin = margin(
        t = 10
      )
    ),
    
    plot.title = element_text(
      color = grafito,
      face = "bold",
      size = 30,
      margin = margin(
        b = 6
      )
    ),
    
    plot.subtitle = element_text(
      color = gris_texto,
      size = 16,
      margin = margin(
        b = 24
      )
    ),
    
    plot.margin = margin(
      t = 28,
      r = 150,
      b = 5,
      l = 24
    )
  )


# 127. NOTA INTERPRETATIVA

nota_g07 <- ggdraw() +
  
  draw_label(
    paste0(
      "El punto sólido corresponde a mujeres y el punto blanco a hombres. ",
      "La cifra al extremo derecho indica qué grupo obtiene el mayor puntaje y la magnitud de la diferencia."
    ),
    x = 0.02,
    y = 0.50,
    hjust = 0,
    vjust = 0.5,
    color = grafito,
    size = 12.3,
    fontface = "bold",
    lineheight = 1.25
  )


# 128. PIE EDITORIAL DE LA GRÁFICA 07

pie_g07 <- ggdraw() +
  
  draw_grob(
    logo_grob,
    x = 0.02,
    y = 0.10,
    width = 0.17,
    height = 0.80
  ) +
  
  draw_line(
    x = c(
      0.205,
      0.205
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
      "Fuente: elaboración propia con microdatos de PISA 2025.\n",
      "Los puntajes utilizan el peso final del estudiante y combinan los 10 valores plausibles de cada dominio.\n",
      "Laboratorio de Investigación Social Avanzada · LISA"
    ),
    x = 0.225,
    y = 0.50,
    hjust = 0,
    vjust = 0.5,
    color = gris_texto,
    size = 11.3,
    lineheight = 1.30
  )


# 129. INTEGRACIÓN FINAL DE LA GRÁFICA 07

grafica_07 <- plot_grid(
  grafica_07_base,
  nota_g07,
  pie_g07,
  ncol = 1,
  rel_heights = c(
    0.78,
    0.07,
    0.15
  ),
  align = "v"
)


# 130. VISUALIZACIÓN Y EXPORTACIÓN

print(
  grafica_07
)

ggsave(
  filename = archivo_salida_g07,
  plot = grafica_07,
  width = 12,
  height = 7,
  units = "in",
  dpi = 320,
  bg = "white"
)