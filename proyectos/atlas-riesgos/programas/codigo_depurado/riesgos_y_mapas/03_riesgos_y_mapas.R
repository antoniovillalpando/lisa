# Dr.(C) Antonio Villalpando Acuña
# Atlas de Riesgos para la Nutrición de la Niñez en México de Save the Children

library(readxl)
library(writexl)
library(dplyr)
library(caret)
library(xgboost)
library(sf)
library(ggplot2)
library(ggrepel)

archivo_datos <- "dc.xlsx"
archivo_mapa <- "u_territorial_estados_mgn_inegi_2013.shp"
directorio_salida <- "salidas_riesgos_y_mapas"
columnas_predictoras <- 9:143
semilla <- 123
rondas <- 100

dir.create(directorio_salida, recursive = TRUE, showWarnings = FALSE)

normalizar <- function(x) {
  rango <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)

  if (!is.finite(rango) || rango == 0) {
    return(rep(0, length(x)))
  }

  (x - min(x, na.rm = TRUE)) / rango
}

preparar_predictores <- function(datos, columnas) {
  predictores <- names(datos)[columnas]
  predictores <- predictores[predictores %in% names(datos)]

  datos[predictores] <- lapply(
    datos[predictores],
    function(x) {
      if (is.numeric(x)) {
        x
      } else {
        as.numeric(as.character(x))
      }
    }
  )

  list(
    datos = datos,
    predictores = predictores
  )
}

ajustar_xgboost <- function(
  datos,
  variable_dependiente,
  predictores,
  semilla,
  rondas
) {
  columnas_modelo <- c(variable_dependiente, predictores)
  casos_completos <- complete.cases(datos[columnas_modelo])
  datos_completos <- datos[casos_completos, , drop = FALSE]

  x <- as.matrix(datos_completos[predictores])
  y <- datos_completos[[variable_dependiente]]

  set.seed(semilla)

  indice_entrenamiento <- createDataPartition(
    y,
    p = 0.8,
    list = FALSE,
    times = 1
  )

  x_entrenamiento <- x[indice_entrenamiento, , drop = FALSE]
  y_entrenamiento <- y[indice_entrenamiento]

  dtrain <- xgb.DMatrix(
    data = x_entrenamiento,
    label = y_entrenamiento
  )

  modelo <- xgboost(
    data = dtrain,
    params = list(
      objective = "reg:squarederror",
      eval_metric = "rmse"
    ),
    nrounds = rondas,
    verbose = 0
  )

  predicciones <- predict(
    modelo,
    newdata = xgb.DMatrix(x)
  )

  importancia <- xgb.importance(
    feature_names = predictores,
    model = modelo
  )

  list(
    datos = datos_completos,
    predicciones = predicciones,
    modelo = modelo,
    importancia = head(importancia, 10)
  )
}

calcular_riesgo <- function(
  ajuste,
  nombre_riesgo,
  direccion,
  desviaciones
) {
  mediana <- median(ajuste$predicciones, na.rm = TRUE)
  desviacion <- sd(ajuste$predicciones, na.rm = TRUE)

  if (direccion == "inferior") {
    umbral <- mediana - desviaciones * desviacion
    probabilidad <- pnorm(
      umbral,
      mean = ajuste$predicciones,
      sd = desviacion
    )
  } else {
    umbral <- mediana + desviaciones * desviacion
    probabilidad <- 1 - pnorm(
      umbral,
      mean = ajuste$predicciones,
      sd = desviacion
    )
  }

  data.frame(
    ent = ajuste$datos$ent,
    valor = probabilidad
  ) %>%
    group_by(ent) %>%
    summarise(
      valor = mean(valor, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(!!nombre_riesgo := valor)
}

crear_mapa_continuo <- function(
  mapa,
  datos,
  variable,
  titulo,
  subtitulo,
  pie,
  archivo
) {
  mapa_datos <- inner_join(
    mapa,
    datos,
    by = "ent"
  )

  grafica <- ggplot(mapa_datos) +
    geom_sf(
      aes(fill = .data[[variable]]),
      color = "white",
      linewidth = 0.2
    ) +
    scale_fill_gradient(
      low = "#FFE7E7",
      high = "#C30010",
      name = "Probabilidad"
    ) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      caption = pie
    ) +
    geom_text_repel(
      aes(
        label = paste0(
          round(.data[[variable]] * 100, 1),
          "%"
        ),
        geometry = st_geometry(mapa_datos)
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
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle = element_text(size = 18),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      plot.caption = element_text(size = 14)
    )

  ggsave(
    filename = file.path(directorio_salida, archivo),
    plot = grafica,
    width = 27,
    height = 19
  )

  grafica
}

calcular_indice <- function(
  datos,
  variables,
  nombre_indice,
  nombre_clasificacion,
  etiquetas
) {
  variables <- unique(variables)
  matriz <- datos %>%
    select(all_of(variables)) %>%
    mutate(across(everything(), normalizar))

  indice <- rowMeans(matriz, na.rm = TRUE)

  data.frame(
    ent = datos$ent,
    indice = indice,
    clasificacion = ifelse(indice > 0.5, 1, 0)
  ) %>%
    group_by(ent) %>%
    summarise(
      indice = mean(indice, na.rm = TRUE),
      clasificacion = as.integer(mean(clasificacion, na.rm = TRUE) > 0.5),
      .groups = "drop"
    ) %>%
    rename(
      !!nombre_indice := indice,
      !!nombre_clasificacion := clasificacion
    ) %>%
    mutate(
      !!nombre_clasificacion := factor(
        .data[[nombre_clasificacion]],
        levels = c(0, 1),
        labels = etiquetas
      )
    )
}

crear_mapa_categorico <- function(
  mapa,
  datos,
  variable,
  colores,
  titulo,
  subtitulo,
  pie,
  archivo
) {
  mapa_datos <- inner_join(
    mapa,
    datos,
    by = "ent"
  )

  grafica <- ggplot(mapa_datos) +
    geom_sf(
      aes(fill = .data[[variable]]),
      colour = "black"
    ) +
    scale_fill_manual(values = colores) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle = element_text(size = 16),
      plot.caption = element_text(size = 10, hjust = 0.5),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    ) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      caption = pie,
      fill = "Clasificación"
    )

  ggsave(
    filename = file.path(directorio_salida, archivo),
    plot = grafica,
    width = 27,
    height = 19
  )

  grafica
}

datos <- as.data.frame(read_xlsx(archivo_datos))

preparacion <- preparar_predictores(
  datos,
  columnas_predictoras
)

datos <- preparacion$datos
predictores <- preparacion$predictores

ajuste_pesoedad <- ajustar_xgboost(
  datos,
  "pesoedad",
  predictores,
  semilla,
  rondas
)

ajuste_pesotalla <- ajustar_xgboost(
  datos,
  "pesotalla",
  predictores,
  semilla,
  rondas
)

ajuste_tallaedad <- ajustar_xgboost(
  datos,
  "tallaedad",
  predictores,
  semilla,
  rondas
)

riesgo_obesidad_pesoedad <- calcular_riesgo(
  ajuste_pesoedad,
  "prob_riesgo_obesidad_pesoedad",
  "superior",
  1
)

riesgo_desnutricion_pesotalla <- calcular_riesgo(
  ajuste_pesotalla,
  "prob_riesgo_desnutricion_pesotalla",
  "inferior",
  2
)

riesgo_obesidad_pesotalla <- calcular_riesgo(
  ajuste_pesotalla,
  "prob_riesgo_obesidad_pesotalla",
  "superior",
  2
)

riesgo_baja_talla <- calcular_riesgo(
  ajuste_tallaedad,
  "prob_riesgo_baja_talla",
  "inferior",
  2
)

riesgos_estatales <- list(
  obesidad_pesoedad = riesgo_obesidad_pesoedad,
  desnutricion_pesotalla = riesgo_desnutricion_pesotalla,
  obesidad_pesotalla = riesgo_obesidad_pesotalla,
  baja_talla = riesgo_baja_talla
)

factores_importantes <- list(
  pesoedad = as.data.frame(ajuste_pesoedad$importancia),
  pesotalla = as.data.frame(ajuste_pesotalla$importancia),
  tallaedad = as.data.frame(ajuste_tallaedad$importancia)
)

write_xlsx(
  riesgos_estatales,
  file.path(
    directorio_salida,
    "riesgos_estatales.xlsx"
  )
)

write_xlsx(
  factores_importantes,
  file.path(
    directorio_salida,
    "factores_importantes.xlsx"
  )
)

saveRDS(
  ajuste_pesoedad$modelo,
  file.path(
    directorio_salida,
    "modelo_riesgo_pesoedad.rds"
  )
)

saveRDS(
  ajuste_pesotalla$modelo,
  file.path(
    directorio_salida,
    "modelo_riesgo_pesotalla.rds"
  )
)

saveRDS(
  ajuste_tallaedad$modelo,
  file.path(
    directorio_salida,
    "modelo_riesgo_tallaedad.rds"
  )
)

mapa <- st_read(
  archivo_mapa,
  quiet = TRUE
)

names(mapa)[names(mapa) == "cvegeoedo"] <- "ent"
mapa$ent <- as.integer(mapa$ent)

mapa_obesidad_pesoedad <- crear_mapa_continuo(
  mapa,
  riesgo_obesidad_pesoedad,
  "prob_riesgo_obesidad_pesoedad",
  "Riesgo de sobrepeso y obesidad en niñas y niños de 0 a 9 años",
  "Probabilidad de tener un peso para la edad de +1 desviación estándar por encima de la mediana",
  "Resultado del modelo XGBoost para el peso para la edad",
  "mapa_riesgo_obesidad_pesoedad.svg"
)

mapa_desnutricion_pesotalla <- crear_mapa_continuo(
  mapa,
  riesgo_desnutricion_pesotalla,
  "prob_riesgo_desnutricion_pesotalla",
  "Riesgo de desnutrición grave en niñas y niños de 0 a 9 años",
  "Probabilidad de tener un peso para la talla por debajo de -2 desviaciones estándar de la mediana",
  "Resultado del modelo XGBoost para el peso para la talla",
  "mapa_riesgo_desnutricion_pesotalla.svg"
)

mapa_obesidad_pesotalla <- crear_mapa_continuo(
  mapa,
  riesgo_obesidad_pesotalla,
  "prob_riesgo_obesidad_pesotalla",
  "Riesgo de obesidad en niñas y niños de 0 a 9 años",
  "Probabilidad de tener un peso para la talla por encima de +2 desviaciones estándar de la mediana",
  "Resultado del modelo XGBoost para el peso para la talla",
  "mapa_riesgo_obesidad_pesotalla.svg"
)

mapa_baja_talla <- crear_mapa_continuo(
  mapa,
  riesgo_baja_talla,
  "prob_riesgo_baja_talla",
  "Riesgo de desnutrición crónica en niñas y niños de 0 a 9 años",
  "Probabilidad de tener una talla para la edad de -2 desviaciones estándar por debajo de la mediana",
  "Resultado del modelo XGBoost para la talla para la edad",
  "mapa_riesgo_baja_talla.svg"
)

variables_indice_desnutricion <- c(
  "plp",
  "pobreza",
  "pobreza_m",
  "plp_e",
  "ic_asalud",
  "ic_segsoc",
  "pobreza_e",
  "ins_ali"
)

variables_indice_obesidad <- c(
  "esc_mat",
  "vul_car",
  "seg_aguapot",
  "ifem",
  "sane",
  "ifp",
  "inas_esc",
  "ifs"
)

variables_focos_baja_talla <- c(
  "dg1014",
  "vim1",
  "des_grave",
  "dg14",
  "merc",
  "dgm1",
  "icv_hac",
  "tamhogesc",
  "lca",
  "dmm1",
  "mins04",
  "dch"
)

indice_desnutricion <- calcular_indice(
  datos,
  variables_indice_desnutricion,
  "indice_factores_desnutricion",
  "clasificacion_factores_desnutricion",
  c(
    "Por debajo de la media nacional",
    "Encima de la media nacional"
  )
)

indice_obesidad <- calcular_indice(
  datos,
  variables_indice_obesidad,
  "indice_factores_obesidad",
  "clasificacion_factores_obesidad",
  c(
    "Por debajo de la media nacional",
    "Encima de la media nacional"
  )
)

indice_baja_talla <- calcular_indice(
  datos,
  variables_focos_baja_talla,
  "indice_focos_baja_talla",
  "clasificacion_focos_baja_talla",
  c(
    "Riesgo por debajo de la media nacional",
    "Riesgo por encima de la media nacional"
  )
)

write_xlsx(
  list(
    factores_desnutricion = indice_desnutricion,
    factores_obesidad = indice_obesidad,
    focos_baja_talla = indice_baja_talla
  ),
  file.path(
    directorio_salida,
    "indices_auxiliares.xlsx"
  )
)

mapa_indice_desnutricion <- crear_mapa_categorico(
  mapa,
  indice_desnutricion,
  "clasificacion_factores_desnutricion",
  c(
    "Encima de la media nacional" = "#990000",
    "Por debajo de la media nacional" = "#00BB00"
  ),
  "Índice de factores que precipitan la desnutrición",
  "Valores de 2022 - Índice calculado linealmente",
  "Fuente: elaboración propia con datos de ENSANUT 2022, DGIS-SS, INEGI-DENUE, CONEVAL-DataMun y CONEVAL-Pobreza 2022",
  "mapa_indice_factores_desnutricion.svg"
)

mapa_indice_obesidad <- crear_mapa_categorico(
  mapa,
  indice_obesidad,
  "clasificacion_factores_obesidad",
  c(
    "Encima de la media nacional" = "#990000",
    "Por debajo de la media nacional" = "#00BB00"
  ),
  "Índice de factores que precipitan el sobrepeso y la obesidad",
  "Valores de 2022 - Índice calculado linealmente",
  "Fuente: elaboración propia con datos de ENSANUT 2022, DGIS-SS, INEGI-DENUE, CONEVAL-DataMun y CONEVAL-Pobreza 2022",
  "mapa_indice_factores_obesidad.svg"
)

mapa_focos_baja_talla <- crear_mapa_categorico(
  mapa,
  indice_baja_talla,
  "clasificacion_focos_baja_talla",
  c(
    "Riesgo por encima de la media nacional" = "#FF0000",
    "Riesgo por debajo de la media nacional" = "#000066"
  ),
  "Focos rojos de desnutrición crónica",
  "Valores de 2022 - Índice calculado linealmente",
  "Fuente: elaboración propia con datos de ENSANUT 2022, DGIS-SS, INEGI-DENUE, CONEVAL-DataMun y CONEVAL-Pobreza 2022",
  "mapa_focos_baja_talla.svg"
)
