# Dr.(C) Antonio Villalpando Acuña
# Atlas de Riesgos para la Nutrición de la Niñez en México de Save the Children

library(readxl)
library(writexl)
library(lme4)
library(quantreg)
library(nlme)
library(glmmTMB)
library(dplyr)
library(caret)
library(missForest)
library(xgboost)

archivo_dependientes <- "variables_dependientes.xlsx"
archivo_independientes <- "variables_independientes.xlsx"
directorio_salida <- "salidas_modelos"
tamano_bloque <- 10
numero_folds <- 10
corte_correlacion <- 0.9
semilla <- 123

dir.create(directorio_salida, recursive = TRUE, showWarnings = FALSE)
set.seed(semilla)

limpiar_nombres <- function(nombres) {
  nombres <- gsub("-", "_", nombres)
  nombres <- gsub(" ", "_", nombres)
  nombres
}

extraer_coeficientes <- function(modelo) {
  if (inherits(modelo, "lme")) {
    tabla <- as.data.frame(summary(modelo)$tTable)
    resultado <- data.frame(
      Variable = rownames(tabla),
      Estimate = tabla[, 1],
      Std_Error = tabla[, 2],
      Statistic = tabla[, 3],
      P_Value = tabla[, 5],
      row.names = NULL
    )
  } else if (inherits(modelo, "glmmTMB")) {
    tabla <- as.data.frame(summary(modelo)$coefficients$cond)
    resultado <- data.frame(
      Variable = rownames(tabla),
      Estimate = tabla[, 1],
      Std_Error = tabla[, 2],
      Statistic = tabla[, 3],
      P_Value = tabla[, 4],
      row.names = NULL
    )
  } else {
    tabla <- as.data.frame(summary(modelo)$coefficients)
    resultado <- data.frame(
      Variable = rownames(tabla),
      Estimate = tabla[, 1],
      Std_Error = if (ncol(tabla) >= 2) tabla[, 2] else NA_real_,
      Statistic = if (ncol(tabla) >= 3) tabla[, 3] else NA_real_,
      P_Value = if (ncol(tabla) >= 4) tabla[, 4] else NA_real_,
      row.names = NULL
    )
  }

  resultado
}

ajustar_modelos_aic <- function(datos, variable_dependiente, variables_independientes) {
  grupos <- split(
    variables_independientes,
    ceiling(seq_along(variables_independientes) / tamano_bloque)
  )

  resultados <- list()
  modelo_cuantil <- NULL

  for (grupo in grupos) {
    formula_modelo <- as.formula(
      paste(variable_dependiente, "~", paste(grupo, collapse = " + "))
    )

    modelos <- list(
      Multinivel = tryCatch(
        lmer(
          update(formula_modelo, . ~ . + (1 | ent)),
          data = datos,
          control = lmerControl(check.conv.grad = "ignore")
        ),
        error = function(e) NULL
      ),
      Efectos_mixtos = tryCatch(
        lme(
          formula_modelo,
          random = ~ 1 | ent,
          data = datos
        ),
        error = function(e) NULL
      ),
      Cuantil = tryCatch(
        rq(formula_modelo, data = datos, tau = 0.5),
        error = function(e) NULL
      ),
      Agrupado = tryCatch(
        lm(formula_modelo, data = datos),
        error = function(e) NULL
      ),
      Logit_multinivel = tryCatch(
        glmmTMB(
          update(formula_modelo, . ~ . + (1 | ent)),
          data = datos,
          family = binomial
        ),
        error = function(e) NULL
      )
    )

    valores_aic <- sapply(
      modelos,
      function(modelo) {
        if (is.null(modelo)) {
          NA_real_
        } else {
          tryCatch(AIC(modelo), error = function(e) NA_real_)
        }
      }
    )

    valores_validos <- valores_aic[!is.na(valores_aic)]

    if (length(valores_validos) == 0) {
      next
    }

    nombre_mejor <- names(valores_validos)[which.min(valores_validos)]
    mejor_modelo <- modelos[[nombre_mejor]]

    tabla <- tryCatch(
      extraer_coeficientes(mejor_modelo),
      error = function(e) NULL
    )

    if (!is.null(tabla)) {
      tabla <- tabla |>
        mutate(
          Dependent_Var = variable_dependiente,
          Best_Model = nombre_mejor,
          AIC = valores_aic[[nombre_mejor]]
        )

      resultados[[length(resultados) + 1]] <- tabla
    }

    if (nombre_mejor == "Cuantil") {
      modelo_cuantil <- mejor_modelo
    }
  }

  list(
    coeficientes = bind_rows(resultados),
    modelo_cuantil = modelo_cuantil
  )
}

ajustar_modelos_cv <- function(datos, variable_dependiente, variables_independientes) {
  grupos <- split(
    variables_independientes,
    ceiling(seq_along(variables_independientes) / tamano_bloque)
  )

  rmse_cuantil <- numeric()
  coeficientes_cuantil <- list()
  rmse_xgboost <- numeric()
  importancias_xgboost <- list()
  modelos_xgboost <- list()
  variables_xgboost <- list()

  for (grupo in grupos) {
    formula_modelo <- as.formula(
      paste(variable_dependiente, "~", paste(grupo, collapse = " + "))
    )

    folds <- createFolds(
      datos[[variable_dependiente]],
      k = numero_folds,
      list = TRUE,
      returnTrain = TRUE
    )

    errores_cuantil <- numeric()
    modelo_cuantil <- NULL

    for (indices_entrenamiento in folds) {
      indices_prueba <- setdiff(seq_len(nrow(datos)), indices_entrenamiento)
      datos_entrenamiento <- datos[indices_entrenamiento, , drop = FALSE]
      datos_prueba <- datos[indices_prueba, , drop = FALSE]

      modelo_cuantil <- rq(
        formula_modelo,
        data = datos_entrenamiento,
        tau = 0.5
      )

      predicciones <- predict(
        modelo_cuantil,
        newdata = datos_prueba
      )

      errores_cuantil <- c(
        errores_cuantil,
        sqrt(mean(
          (datos_prueba[[variable_dependiente]] - predicciones)^2
        ))
      )
    }

    rmse_cuantil <- c(rmse_cuantil, mean(errores_cuantil))
    coeficientes_cuantil[[length(coeficientes_cuantil) + 1]] <- coef(modelo_cuantil)

    control <- trainControl(
      method = "cv",
      number = numero_folds
    )

    modelo_xgboost <- train(
      x = datos[grupo],
      y = datos[[variable_dependiente]],
      method = "xgbTree",
      trControl = control,
      tuneGrid = expand.grid(
        nrounds = 100,
        max_depth = 6,
        eta = 0.1,
        gamma = 0,
        colsample_bytree = 1,
        min_child_weight = 1,
        subsample = 1
      )
    )

    rmse_xgboost <- c(
      rmse_xgboost,
      min(modelo_xgboost$results$RMSE)
    )

    importancias_xgboost[[length(importancias_xgboost) + 1]] <- xgb.importance(
      feature_names = grupo,
      model = modelo_xgboost$finalModel
    )

    modelos_xgboost[[length(modelos_xgboost) + 1]] <- modelo_xgboost$finalModel
    variables_xgboost[[length(variables_xgboost) + 1]] <- grupo
  }

  indice_cuantil <- which.min(rmse_cuantil)
  indice_xgboost <- which.min(rmse_xgboost)
  mejor_rmse_cuantil <- rmse_cuantil[indice_cuantil]
  mejor_rmse_xgboost <- rmse_xgboost[indice_xgboost]

  if (mejor_rmse_cuantil < mejor_rmse_xgboost) {
    mejor_modelo <- "Quantile"
    mejor_rmse <- mejor_rmse_cuantil
    mejores_coeficientes <- coeficientes_cuantil[[indice_cuantil]]

    tabla_importancia <- data.frame(
      Variable = names(mejores_coeficientes),
      Coefficient = as.numeric(mejores_coeficientes)
    )
  } else {
    mejor_modelo <- "XGBoost"
    mejor_rmse <- mejor_rmse_xgboost
    mejor_importancia <- importancias_xgboost[[indice_xgboost]]

    tabla_importancia <- data.frame(
      Variable = mejor_importancia$Feature,
      Coefficient = mejor_importancia$Gain
    )
  }

  tabla_importancia <- tabla_importancia |>
    arrange(desc(Coefficient)) |>
    head(10)

  list(
    best_model = mejor_modelo,
    best_rmse = mejor_rmse,
    quantile_rmse = mejor_rmse_cuantil,
    xgboost_rmse = mejor_rmse_xgboost,
    importance = tabla_importancia,
    xgboost_model = modelos_xgboost[[indice_xgboost]],
    xgboost_variables = variables_xgboost[[indice_xgboost]]
  )
}

calcular_niveles_riesgo <- function(datos, modelo) {
  predicciones <- predict(modelo, newdata = datos)
  umbral_bajo <- quantile(predicciones, probs = 0.3333)
  umbral_alto <- quantile(predicciones, probs = 0.6667)
  amplitud <- umbral_alto - umbral_bajo

  data.frame(
    ent = datos$ent,
    Obesidad = ifelse(predicciones > umbral_alto, "Alto", "Bajo"),
    Sobrepeso = ifelse(
      predicciones <= umbral_alto & predicciones > umbral_bajo,
      "Medio",
      "Bajo"
    ),
    Desnutricion_leve = ifelse(
      predicciones <= umbral_bajo &
        predicciones > umbral_bajo - 0.3333 * amplitud,
      "Medio",
      "Bajo"
    ),
    Desnutricion_moderada = ifelse(
      predicciones <= umbral_bajo - 0.3333 * amplitud &
        predicciones > umbral_bajo - 0.6667 * amplitud,
      "Medio",
      "Bajo"
    ),
    Desnutricion_grave = ifelse(
      predicciones <= umbral_bajo - 0.6667 * amplitud,
      "Alto",
      "Bajo"
    )
  )
}

calcular_factores_categoria <- function(
  datos,
  variables_independientes,
  modelo,
  limite_inferior,
  limite_superior,
  categoria
) {
  predicciones <- predict(modelo, newdata = datos)
  seleccion <- predicciones >= limite_inferior &
    predicciones < limite_superior

  medias <- colMeans(
    datos[seleccion, variables_independientes, drop = FALSE],
    na.rm = TRUE
  )

  data.frame(
    Variable = names(medias),
    Mean_Value = as.numeric(medias),
    Category = categoria
  ) |>
    arrange(desc(Mean_Value)) |>
    head(10)
}

agregar_por_entidad <- function(datos) {
  datos |>
    group_by(ent) |>
    summarise(
      across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )
}

predecir_xgboost <- function(datos, modelo, variables) {
  matriz <- datos |>
    select(all_of(variables)) |>
    as.matrix()

  predict(
    modelo,
    newdata = xgb.DMatrix(matriz)
  )
}

variables_dependientes <- read_excel(archivo_dependientes)
variables_independientes <- read_excel(archivo_independientes)

colnames(variables_dependientes) <- limpiar_nombres(
  colnames(variables_dependientes)
)

colnames(variables_independientes) <- limpiar_nombres(
  colnames(variables_independientes)
)

datos <- merge(
  variables_dependientes,
  variables_independientes,
  by = "ent"
)

variables_modelo_aic <- colnames(variables_dependientes)[-c(1:3)]
variables_modelo_cv <- colnames(variables_dependientes)[4:8]
variables_independientes_modelo <- colnames(variables_independientes)[-1]
variables_independientes_modelo <- variables_independientes_modelo[
  variables_independientes_modelo %in% colnames(datos)
]

for (variable in variables_independientes_modelo) {
  if (!is.numeric(datos[[variable]])) {
    datos[[variable]] <- as.numeric(as.character(datos[[variable]]))
  }
}

if (any(!vapply(
  datos[variables_independientes_modelo],
  is.numeric,
  logical(1)
))) {
  stop("Algunas variables independientes no son numéricas.")
}

datos[variables_independientes_modelo] <- missForest(
  datos[variables_independientes_modelo]
)$ximp

preprocesamiento <- preProcess(
  datos[variables_independientes_modelo],
  method = c("center", "scale")
)

datos[variables_independientes_modelo] <- predict(
  preprocesamiento,
  datos[variables_independientes_modelo]
)

matriz_correlaciones <- cor(
  datos[variables_independientes_modelo],
  use = "pairwise.complete.obs"
)

indices_correlacionados <- findCorrelation(
  matriz_correlaciones,
  cutoff = corte_correlacion
)

if (length(indices_correlacionados) > 0) {
  variables_independientes_modelo <- variables_independientes_modelo[
    -indices_correlacionados
  ]
}

resultados_aic <- list()
modelos_cuantiles <- list()

for (variable_dependiente in variables_modelo_aic) {
  resultado <- ajustar_modelos_aic(
    datos,
    variable_dependiente,
    variables_independientes_modelo
  )

  resultados_aic[[variable_dependiente]] <- resultado$coeficientes
  modelos_cuantiles[[variable_dependiente]] <- resultado$modelo_cuantil
}

tabla_aic <- bind_rows(resultados_aic)

write_xlsx(
  tabla_aic,
  file.path(directorio_salida, "modelos_resultados.xlsx")
)

modelo_cuantil_pesotalla <- modelos_cuantiles[["pesotalla"]]

if (!is.null(modelo_cuantil_pesotalla)) {
  niveles_riesgo <- calcular_niveles_riesgo(
    datos,
    modelo_cuantil_pesotalla
  )

  tabla_riesgo <- niveles_riesgo |>
    group_by(ent) |>
    summarise(
      across(everything(), first),
      .groups = "drop"
    ) |>
    arrange(ent)

  write_xlsx(
    tabla_riesgo,
    file.path(
      directorio_salida,
      "niveles_de_riesgo_por_entidad.xlsx"
    )
  )

  predicciones_cuantiles <- predict(
    modelo_cuantil_pesotalla,
    newdata = datos
  )

  umbral_bajo <- quantile(
    predicciones_cuantiles,
    probs = 0.3333
  )

  umbral_alto <- quantile(
    predicciones_cuantiles,
    probs = 0.6667
  )

  amplitud <- umbral_alto - umbral_bajo

  factores <- bind_rows(
    calcular_factores_categoria(
      datos,
      variables_independientes_modelo,
      modelo_cuantil_pesotalla,
      umbral_alto,
      Inf,
      "Obesidad"
    ),
    calcular_factores_categoria(
      datos,
      variables_independientes_modelo,
      modelo_cuantil_pesotalla,
      umbral_bajo,
      umbral_alto,
      "Sobrepeso"
    ),
    calcular_factores_categoria(
      datos,
      variables_independientes_modelo,
      modelo_cuantil_pesotalla,
      umbral_bajo - 0.3333 * amplitud,
      umbral_bajo,
      "Desnutrición leve"
    ),
    calcular_factores_categoria(
      datos,
      variables_independientes_modelo,
      modelo_cuantil_pesotalla,
      umbral_bajo - 0.6667 * amplitud,
      umbral_bajo - 0.3333 * amplitud,
      "Desnutrición moderada"
    ),
    calcular_factores_categoria(
      datos,
      variables_independientes_modelo,
      modelo_cuantil_pesotalla,
      -Inf,
      umbral_bajo - 0.6667 * amplitud,
      "Desnutrición grave"
    )
  ) |>
    arrange(Category, desc(Mean_Value))

  write_xlsx(
    factores,
    file.path(
      directorio_salida,
      "factores_contribuyentes_por_categoria.xlsx"
    )
  )
}

resultados_cv <- list()

for (variable_dependiente in variables_modelo_cv) {
  resultado <- ajustar_modelos_cv(
    datos,
    variable_dependiente,
    variables_independientes_modelo
  )

  resultado$importance <- resultado$importance |>
    mutate(
      Dependent_Var = variable_dependiente,
      Best_Model = resultado$best_model
    )

  resultados_cv[[variable_dependiente]] <- resultado
}

tabla_rmse <- data.frame(
  Dependent_Var = variables_modelo_cv,
  Quantile_RMSE = vapply(
    resultados_cv,
    function(resultado) resultado$quantile_rmse,
    numeric(1)
  ),
  XGBoost_RMSE = vapply(
    resultados_cv,
    function(resultado) resultado$xgboost_rmse,
    numeric(1)
  ),
  Best_Model = vapply(
    resultados_cv,
    function(resultado) resultado$best_model,
    character(1)
  ),
  Best_RMSE = vapply(
    resultados_cv,
    function(resultado) resultado$best_rmse,
    numeric(1)
  )
)

tabla_importancias <- bind_rows(
  lapply(
    resultados_cv,
    function(resultado) resultado$importance
  )
)

write_xlsx(
  tabla_rmse,
  file.path(directorio_salida, "modelos_resultados_rmse.xlsx")
)

write_xlsx(
  tabla_importancias,
  file.path(
    directorio_salida,
    "modelos_resultados_coeficientes_rmse.xlsx"
  )
)

modelo_iemc <- resultados_cv[["iemc"]]$xgboost_model
variables_iemc <- resultados_cv[["iemc"]]$xgboost_variables
modelo_pesotalla <- resultados_cv[["pesotalla"]]$xgboost_model
variables_pesotalla <- resultados_cv[["pesotalla"]]$xgboost_variables

datos_estatales <- agregar_por_entidad(datos)
datos_0_3 <- agregar_por_entidad(
  filter(datos, edad >= 0 & edad <= 3)
)
datos_4_6 <- agregar_por_entidad(
  filter(datos, edad >= 4 & edad <= 6)
)
datos_7_9 <- agregar_por_entidad(
  filter(datos, edad >= 7 & edad <= 9)
)

datos_estatales$iemc_pred <- predecir_xgboost(
  datos_estatales,
  modelo_iemc,
  variables_iemc
)

datos_estatales$pesotalla_pred <- predecir_xgboost(
  datos_estatales,
  modelo_pesotalla,
  variables_pesotalla
)

write_xlsx(
  datos_estatales,
  file.path(directorio_salida, "dc.xlsx")
)

write_xlsx(
  datos_0_3,
  file.path(directorio_salida, "dc03.xlsx")
)

write_xlsx(
  datos_4_6,
  file.path(directorio_salida, "dc46.xlsx")
)

write_xlsx(
  datos_7_9,
  file.path(directorio_salida, "dc79.xlsx")
)

importancia_iemc <- xgb.importance(
  feature_names = variables_iemc,
  model = modelo_iemc
)

importancia_pesotalla <- xgb.importance(
  feature_names = variables_pesotalla,
  model = modelo_pesotalla
)

write_xlsx(
  as.data.frame(importancia_iemc),
  file.path(directorio_salida, "iemc_importance.xlsx")
)

write_xlsx(
  as.data.frame(importancia_pesotalla),
  file.path(directorio_salida, "pesotalla_importance.xlsx")
)

saveRDS(
  list(
    model = modelo_iemc,
    variables = variables_iemc
  ),
  file.path(directorio_salida, "modelo_iemc.rds")
)

saveRDS(
  list(
    model = modelo_pesotalla,
    variables = variables_pesotalla
  ),
  file.path(directorio_salida, "modelo_pesotalla.rds")
)
