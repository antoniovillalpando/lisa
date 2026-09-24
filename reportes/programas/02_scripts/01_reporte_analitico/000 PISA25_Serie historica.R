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
# 00 · SERIE HISTÓRICA PISA 2000–2022
# Fecha de consignación: 9 de septiembre de 2026
# Identificador: PISA-2025-000
# ======================================================================

# Insumos
## Paquete learningtower
## Microdatos armonizados PISA 2000–2022
## https://github.com/kevinwang09/learningtower

# Salidas
## data/processed/PISA_historical_scores_2000_2022.rds

# ======================== INICIA PROCESAMIENTO =======================


# 1. CARGA DE PAQUETES

library(learningtower)
library(dplyr)


# 2. ESTRUCTURA DE DIRECTORIOS

dir.create(
  "data/raw",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "output/figures",
  recursive = TRUE,
  showWarnings = FALSE
)


# 3. DEFINICIÓN DE RUTAS

salida_historica <- "data/processed/PISA_historical_scores_2000_2022.rds"


# 4. CARGA DE MICRODATOS HISTÓRICOS

pisa_historical_raw <- load_student("all")


# 5. VERIFICACIÓN DE ESTRUCTURA

variables_requeridas <- c(
  "year",
  "country",
  "math",
  "read",
  "science",
  "stu_wgt"
)

variables_faltantes <- setdiff(
  variables_requeridas,
  names(pisa_historical_raw)
)

if (length(variables_faltantes) > 0) {
  
  stop(
    paste(
      "Faltan las siguientes variables:",
      paste(
        variables_faltantes,
        collapse = ", "
      )
    )
  )
}


# 6. FUNCIÓN DE MEDIA PONDERADA

media_ponderada <- function(x, w) {
  
  validos <- !is.na(x) &
    !is.na(w) &
    is.finite(x) &
    is.finite(w) &
    w > 0
  
  if (!any(validos)) {
    return(NA_real_)
  }
  
  weighted.mean(
    x[validos],
    w[validos]
  )
}


# 7. ESTIMACIÓN DE PUNTAJES POR PAÍS Y AÑO

pisa_historical_scores <- pisa_historical_raw |>
  group_by(
    year,
    country
  ) |>
  summarise(
    math = media_ponderada(
      math,
      stu_wgt
    ),
    reading = media_ponderada(
      read,
      stu_wgt
    ),
    science = media_ponderada(
      science,
      stu_wgt
    ),
    n_students = n(),
    .groups = "drop"
  ) |>
  mutate(
    country = as.character(country),
    across(
      c(
        math,
        reading,
        science
      ),
      ~ round(.x, 1)
    )
  ) |>
  arrange(
    country,
    year
  )


# 8. GUARDADO DE SERIE HISTÓRICA

saveRDS(
  pisa_historical_scores,
  salida_historica,
  compress = "xz"
)


# 9. VERIFICACIÓN DE MÉXICO

mexico_historical <- pisa_historical_scores |>
  filter(
    country == "MEX"
  )

print(
  mexico_historical
)


# 10. VERIFICACIÓN GENERAL

cat(
  "\nAños disponibles:",
  paste(
    sort(
      unique(
        pisa_historical_scores$year
      )
    ),
    collapse = ", "
  ),
  "\n"
)

cat(
  "Países/economías:",
  n_distinct(
    pisa_historical_scores$country
  ),
  "\n"
)

cat(
  "Registros país-año:",
  nrow(
    pisa_historical_scores
  ),
  "\n"
)

cat(
  "\nArchivo generado:\n",
  salida_historica,
  "\n"
)


# ======================== FINALIZA PROCESAMIENTO ======================