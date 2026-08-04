# Dr.(C) Antonio Villalpando Acuña
# Proyecto Atlas de riesgos de malnutrición de Save the Children
# Script de validación cruzada de la capacidad predictiva de los modelos

library(readxl)
library(writexl)
library(quantreg)
library(xgboost)
library(dplyr)
library(missForest)
library(caret)
library(rstudioapi)

## Función para limpiar nombres de variables
clean_names <- function(names) {
  names <- gsub("-", "_", names)  
  names <- gsub(" ", "_", names)  
  return(names)
}

variables_dependientes <- read_excel("variables_dependientes.xlsx")
variables_independientes <- read_excel("variables_independientes.xlsx")

colnames(variables_dependientes) <- clean_names(colnames(variables_dependientes))
colnames(variables_independientes) <- clean_names(colnames(variables_independientes))

data <- merge(variables_dependientes, variables_independientes, by = "ent")

dependent_vars <- colnames(variables_dependientes)[4:8]
independent_vars <- colnames(variables_independientes)[-1]
independent_vars <- independent_vars[independent_vars %in% colnames(data)]

## Función para convertir las variables independientes a numéricas si no lo son
for (var in independent_vars) {
  if (!is.numeric(data[[var]])) {
    data[[var]] <- as.numeric(as.character(data[[var]]))
  }
}

if (any(sapply(data[independent_vars], class) != "numeric")) {
  stop("Algunas variables independientes no son numéricas")
}

## Imputación de valores faltantes usando Random Forest
imputed_data <- missForest(data[independent_vars])
data[independent_vars] <- imputed_data$ximp

preProc <- preProcess(data[independent_vars], method = c("center", "scale"))
data[independent_vars] <- predict(preProc, data[independent_vars])

## Eliminación de predictores altamente correlacionados
cor_matrix <- cor(data[independent_vars], use = "pairwise.complete.obs")
highly_correlated <- findCorrelation(cor_matrix, cutoff = 0.9)
independent_vars <- independent_vars[-highly_correlated]
independent_vars <- independent_vars[independent_vars %in% colnames(data)]

## Función para segmentar el data frame en 8 partes de 10
fit_models_part <- function(data, dep_var, ind_vars) {
  ind_var_groups <- split(ind_vars, ceiling(seq_along(ind_vars) / 10))
  quantile_rmse_list <- c()
  quantile_coefs_list <- list()
  xgb_rmse_list <- c()
  xgb_importance_list <- list()
  best_xgb_model <- NULL
  best_xgb_rmse <- Inf  
  
  for (group in ind_var_groups) {
    formula_str <- paste(dep_var, "~", paste(group, collapse = " + "))
    formula <- as.formula(formula_str)
    
    k <- 10
    folds <- createFolds(data[[dep_var]], k = k, list = TRUE, returnTrain = TRUE)
    quantile_rmses <- c()
    
    for (i in 1:k) {
      train_indices <- folds[[i]]
      test_indices <- setdiff(1:nrow(data), train_indices)
      
      train_data <- data[train_indices, ]
      test_data <- data[test_indices, ]
      
      quantile_model_temp <- rq(formula, data = train_data, tau = 0.5)
      predictions <- predict(quantile_model_temp, newdata = test_data)
      rmse <- sqrt(mean((test_data[[dep_var]] - predictions)^2))
      quantile_rmses <- c(quantile_rmses, rmse)
    }
    
    quantile_rmse <- mean(quantile_rmses)
    quantile_coefs <- coef(quantile_model_temp)
    quantile_rmse_list <- c(quantile_rmse_list, quantile_rmse)
    quantile_coefs_list <- c(quantile_coefs_list, list(quantile_coefs))
    
    train_control <- trainControl(method = "cv", number = 10)
    xgb_model_cv <- train(
      x = data[group], y = data[[dep_var]], 
      method = "xgbTree", 
      trControl = train_control, 
      tuneGrid = expand.grid(
        nrounds = 100, max_depth = 6, eta = 0.1, gamma = 0, 
        colsample_bytree = 1, min_child_weight = 1, subsample = 1
      )
    )
    xgb_rmse <- min(xgb_model_cv$results$RMSE)
    xgb_importance <- xgb.importance(model = xgb_model_cv$finalModel)
    xgb_rmse_list <- c(xgb_rmse_list, xgb_rmse)
    xgb_importance_list <- c(xgb_importance_list, list(xgb_importance))
    
    if (xgb_rmse < best_xgb_rmse) {
      best_xgb_rmse <- xgb_rmse
      best_xgb_model <- xgb_model_cv$finalModel
    }
  }
  
  best_quantile_rmse <- min(quantile_rmse_list)
  best_xgb_rmse <- min(xgb_rmse_list)
  
  if (best_quantile_rmse < best_xgb_rmse) {
    best_model <- "Quantile"
    best_rmse <- best_quantile_rmse
    best_coefs <- quantile_coefs_list[[which.min(quantile_rmse_list)]]
  } else {
    best_model <- "XGBoost"
    best_rmse <- best_xgb_rmse
    best_coefs <- xgb_importance_list[[which.min(xgb_rmse_list)]]
  }
  
  if (best_model == "Quantile") {
    coefs_df <- data.frame(Variable = names(best_coefs), Coefficient = as.numeric(best_coefs))
  } else {
    coefs_df <- data.frame(Variable = best_coefs$Feature, Coefficient = best_coefs$Gain)
  }
  
  coefs_df <- coefs_df %>% arrange(desc(Coefficient)) %>% head(10)
  
  return(list(best_model = best_model, best_rmse = best_rmse, quantile_rmse = best_quantile_rmse, xgb_rmse = best_xgb_rmse, coefs_df = coefs_df, xgb_model = best_xgb_model))
}

results <- list()

## Función para elegir modelos

for (dep_var in dependent_vars) {
  res <- fit_models_part(data, dep_var, independent_vars)
  res$coefs_df <- res$coefs_df %>% mutate(Dependent_Var = dep_var, Best_Model = res$best_model)
  results[[dep_var]] <- res
}

rmse_results <- data.frame(
  Dependent_Var = dependent_vars,
  Quantile_RMSE = sapply(results, function(x) x$quantile_rmse),
  XGBoost_RMSE = sapply(results, function(x) x$xgb_rmse),
  Best_Model = sapply(results, function(x) x$best_model),
  Best_RMSE = sapply(results, function(x) x$best_rmse)
)

write_xlsx(rmse_results, "modelos_resultados_rmse.xlsx")

## Unión de coeficientes más importantes en un solo data frame
coefs_results <- bind_rows(lapply(results, function(x) {
  x$coefs_df %>% filter(Best_Model == x$best_model)
}))

write_xlsx(coefs_results, "modelos_resultados_coeficientes_rmse.xlsx")

## Guardar los modelos XGBoost de iemc y pesotalla como objetos en el entorno
xgb_model_iemc <- results[["iemc"]][["xgb_model"]]
xgb_model_pesotalla <- results[["pesotalla"]][["xgb_model"]]

xgb_model_iemc_b <- xgb_model_iemc
xgb_model_pesotalla_b <- xgb_model_pesotalla

results <- list()
xgb_models <- list()

## Función para ajustar mejores modelos 
for (dep_var in dependent_vars) {
  res <- fit_models_part(data, dep_var, independent_vars)
  res$coefs_df <- res$coefs_df %>% mutate(Dependent_Var = dep_var, Best_Model = res$best_model)
  results[[dep_var]] <- res
  if (res$best_model == "XGBoost") {
    xgb_models[[dep_var]] <- res$xgb_model
  }
}

rmse_results <- data.frame(
  Dependent_Var = dependent_vars,
  Quantile_RMSE = sapply(results, function(x) x$quantile_rmse),
  XGBoost_RMSE = sapply(results, function(x) x$xgb_rmse),
  Best_Model = sapply(results, function(x) x$best_model),
  Best_RMSE = sapply(results, function(x) x$best_rmse)
)

write_xlsx(rmse_results, "modelos_resultados_rmse.xlsx")

coefs_results <- bind_rows(lapply(results, function(x) {
  x$coefs_df %>% filter(Best_Model == x$best_model)
}))

write_xlsx(coefs_results, "modelos_resultados_coeficientes_rmse.xlsx")

## Guardar los modelos XGBoost de iemc y pesotalla como objetos en el entorno
xgb_model_iemc <- results[["iemc"]]$xgb_model_cv$finalModel
xgb_model_pesotalla <- results[["pesotalla"]]$xgb_model_cv$finalModel

# Cálculo de segmentos de riesgo con la NOM-008-SSA2

## Función para calcular los valores de corte y asignar categorías
calculate_categories <- function(data, variable) {
  median_value <- median(data[[variable]], na.rm = TRUE)
  sd_value <- sd(data[[variable]], na.rm = TRUE)
  
  intervals <- c(-Inf, median_value - 3 * sd_value, median_value - 2 * sd_value, median_value - 1 * sd_value, median_value + 1 * sd_value, median_value + 2 * sd_value, Inf)
  categories <- c("Desnutrición grave", "Desnutrición moderada", "Desnutrición leve", "Peso normal", "Sobrepeso", "Obesidad")
  
  data[[paste0(variable, "_category")]] <- cut(data[[variable]], breaks = intervals, labels = categories, include.lowest = TRUE)
  
  return(list(data = data, intervals = intervals))
}

result_pesotalla <- calculate_categories(data, "pesotalla")
data <- result_pesotalla$data

pesotalla_intervals <- result_pesotalla$intervals
pesotalla_categories <- c("Desnutrición grave", "Desnutrición moderada", "Desnutrición leve", "Peso normal", "Sobrepeso", "Obesidad")
pesotalla_cutpoints <- data.frame(Interval = head(pesotalla_intervals, -1), Interval_Upper = tail(pesotalla_intervals, -1), Category = pesotalla_categories)
print(pesotalla_cutpoints)

result_iemc <- calculate_categories(data, "iemc")
data <- result_iemc$data

iemc_intervals <- result_iemc$intervals
iemc_categories <- c("Desnutrición grave", "Desnutrición moderada", "Desnutrición leve", "Peso normal", "Sobrepeso", "Obesidad")
iemc_cutpoints <- data.frame(Interval = head(iemc_intervals, -1), Interval_Upper = tail(iemc_intervals, -1), Category = iemc_categories)
print(iemc_cutpoints)

# Presentación de valores predichos

data_compact <- data %>%
  group_by(ent) %>%
  summarise_all(mean, na.rm = TRUE)

# Filtrar y agrupar por rangos de edad 0-3 años
data_compact_0_3 <- data %>%
  filter(edad >= 0 & edad <= 3) %>%
  group_by(ent) %>%
  summarise_all(mean, na.rm = TRUE)

# Filtrar y agrupar por rangos de edad 4-6 años
data_compact_4_6 <- data %>%
  filter(edad >= 4 & edad <= 6) %>%
  group_by(ent) %>%
  summarise_all(mean, na.rm = TRUE)

# Filtrar y agrupar por rangos de edad 7-9 años
data_compact_7_9 <- data %>%
  filter(edad >= 7 & edad <= 9) %>%
  group_by(ent) %>%
  summarise_all(mean, na.rm = TRUE)

model_features_iemc <- xgb_model_iemc_b$feature_names
model_features_pesotalla <- xgb_model_pesotalla_b$feature_names

if (!all(model_features_iemc %in% colnames(data_compact)) || !all(model_features_pesotalla %in% colnames(data_compact))) {
  stop("Las características de los datos de predicción no coinciden con las características del modelo.")
}

data_matrix_iemc <- data_compact %>%
  select(all_of(model_features_iemc)) %>%
  as.matrix()

data_matrix_pesotalla <- data_compact %>%
  select(all_of(model_features_pesotalla)) %>%
  as.matrix()

data_compact$iemc_pred <- predict(xgb_model_iemc_b, newdata = xgb.DMatrix(data_matrix_iemc))
data_compact$pesotalla_pred <- predict(xgb_model_pesotalla_b, newdata = xgb.DMatrix(data_matrix_pesotalla))

# write_xlsx(data_compact, "dc.xlsx")
write_xlsx(data_compact_0_3, "dc03.xlsx")
write_xlsx(data_compact_4_6, "dc46.xlsx")
write_xlsx(data_compact_7_9, "dc79.xlsx")


iemc_importance <- xgb.importance(feature_names = model_features_iemc, model = xgb_model_iemc_b)
iemc_importance_df <- as.data.frame(iemc_importance)

pesotalla_importance <- xgb.importance(feature_names = model_features_pesotalla, model = xgb_model_pesotalla_b)
pesotalla_importance_df <- as.data.frame(pesotalla_importance)

## Guardar los data frames en archivos Excel

write_xlsx(iemc_importance_df, "iemc_importance.xlsx")
write_xlsx(pesotalla_importance_df, "pesotalla_importance.xlsx")