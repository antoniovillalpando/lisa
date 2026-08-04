# Dr.(C) Antonio Villalpando Acuña
# Proyecto Atlas de riesgos de malnutrición infantil de Save the Children
# Script de especificación de modelos

library(readxl)
library(writexl)
library(lme4)
library(quantreg)
library(nlme)
library(glmmTMB)
library(dplyr)
library(caret)
library(missForest)

variables_dependientes <- read_excel("variables_dependientes.xlsx")
variables_independientes <- read_excel("variables_independientes.xlsx")
data <- merge(variables_dependientes, variables_independientes, by = "ent")

## Función para limpiar nombres de variables
clean_names <- function(names) {
  names <- gsub("-", "_", names)
  names <- gsub(" ", "_", names)
  return(names)
}

colnames(data) <- clean_names(colnames(data))
colnames(variables_dependientes) <- clean_names(colnames(variables_dependientes))
colnames(variables_independientes) <- clean_names(colnames(variables_independientes))

dependent_vars <- colnames(variables_dependientes)[-c(1:3)]
dependent_vars <- clean_names(dependent_vars)

ind_vars <- colnames(variables_independientes)[-1]
ind_vars <- clean_names(ind_vars)

data$tinfor <- as.numeric(data$tinfor)

## Imputación de valores faltantes con Random Forest
imputed_data <- missForest(data[ind_vars])
data[ind_vars] <- imputed_data$ximp

## Escalar las variables independientes
preProc <- preProcess(data[ind_vars], method = c("center", "scale"))
data[ind_vars] <- predict(preProc, data[ind_vars])

## Eliminar predictores altamente correlacionados
cor_matrix <- cor(data[ind_vars], use = "pairwise.complete.obs")
highly_correlated <- findCorrelation(cor_matrix, cutoff = 0.9)
ind_vars <- ind_vars[-highly_correlated]
ind_vars <- ind_vars[ind_vars %in% colnames(data)]
cat("Variables independientes después de eliminar correlación alta:", ind_vars, "\n")

## Función para ajustar y evaluar modelos en partes
fit_models_part <- function(data, dep_var, ind_vars) {
  ind_var_groups <- split(ind_vars, ceiling(seq_along(ind_vars) / 10))
  all_coefs <- list()
  quantile_model <- NULL
  
  for (group in ind_var_groups) {
    formula_str <- paste(dep_var, "~", paste(group, collapse = " + "))
    formula <- as.formula(formula_str)
    
    models <- list()
    models[["Multilevel"]] <- tryCatch(lmer(update(formula, . ~ . + (1 | ent)), data = data, control = lmerControl(check.conv.grad = "ignore")), error = function(e) NULL)
    models[["Mixed Effects"]] <- tryCatch(lme(update(formula, . ~ .), random = ~ 1 | ent, data = data), error = function(e) NULL)
    models[["Quantile"]] <- tryCatch(rq(formula, data = data, tau = 0.5), error = function(e) NULL)
    models[["Pooled"]] <- tryCatch(lm(formula, data = data), error = function(e) NULL)
    models[["Multilevel Logit"]] <- tryCatch(glmmTMB(update(formula, . ~ . + (1 | ent)), data = data, family = binomial), error = function(e) NULL)
    
    aic_values <- sapply(models, function(model) if (!is.null(model)) AIC(model) else NA)
    valid_aic_values <- aic_values[!is.na(aic_values)]
    if (length(valid_aic_values) == 0) next
    best_model <- names(aic_values)[which.min(valid_aic_values)]
    
    if (!is.null(models[[best_model]])) {
      if (inherits(models[[best_model]], "lm") | inherits(models[[best_model]], "rq")) {
        coef_df <- as.data.frame(summary(models[[best_model]])$coefficients)
        colnames(coef_df) <- c("Estimate", "Std.Error", "t value", "Pr(>|t|)")
      } else {
        coef_df <- as.data.frame(summary(models[[best_model]])$coefficients[, 1:4])
        colnames(coef_df) <- c("Estimate", "Std.Error", "z value", "Pr(>|z|)")
      }
      coef_df$Variable <- rownames(coef_df)
      coef_df <- coef_df %>% mutate(Dependent_Var = dep_var, Best_Model = best_model, AIC = aic_values[best_model])
      all_coefs <- append(all_coefs, list(coef_df))
      
      if (best_model == "Quantile") {
        quantile_model <- models[["Quantile"]]
      }
    }
  }
  
  combined_coefs <- bind_rows(all_coefs)
  return(list(combined_coefs = combined_coefs, quantile_model = quantile_model))
}

results <- list()
quantile_models <- list()

## Función para ajustar modelos

for (dep_var in dependent_vars) {
  cat("Ajustando modelos para:", dep_var, "\n")
  fit_result <- fit_models_part(data, dep_var, ind_vars)
  results[[dep_var]] <- fit_result$combined_coefs
  if (dep_var == "pesotalla") {
    quantile_models[["pesotalla"]] <- fit_result$quantile_model
  }
}

final_results <- bind_rows(results)

write_xlsx(final_results, "modelos_resultados.xlsx")

## Función para calcular los niveles de riesgo
calculate_risk_levels <- function(data, dep_var, ind_vars, model) {
  predictions <- predict(model, newdata = data[ind_vars])
  
  low_threshold <- quantile(predictions, probs = 0.3333)
  high_threshold <- quantile(predictions, probs = 0.6667)
  
  risk_levels <- data.frame(
    ent = data$ent,
    Obesidad = ifelse(predictions > high_threshold, "Alto", "Bajo"),
    Sobrepeso = ifelse(predictions <= high_threshold & predictions > low_threshold, "Medio", "Bajo"),
    Desnutrición_leve = ifelse(predictions <= low_threshold & predictions > low_threshold - 0.3333 * (high_threshold - low_threshold), "Medio", "Bajo"),
    Desnutrición_moderada = ifelse(predictions <= low_threshold - 0.3333 * (high_threshold - low_threshold) & predictions > low_threshold - 0.6667 * (high_threshold - low_threshold), "Medio", "Bajo"),
    Desnutrición_grave = ifelse(predictions <= low_threshold - 0.6667 * (high_threshold - low_threshold), "Alto", "Bajo")
  )
  
  return(risk_levels)
}

## Calcular niveles de riesgo para pesotalla usando RCA
quantile_model_pesotalla <- quantile_models[["pesotalla"]]
risk_levels_pesotalla <- calculate_risk_levels(data, "pesotalla", ind_vars, quantile_model_pesotalla)

risk_table <- risk_levels_pesotalla %>%
  group_by(ent) %>%
  summarize(
    Obesidad = first(Obesidad),
    Sobrepeso = first(Sobrepeso),
    Desnutrición_leve = first(Desnutrición_leve),
    Desnutrición_moderada = first(Desnutrición_moderada),
    Desnutrición_grave = first(Desnutrición_grave)
  ) %>%
  arrange(ent)

write_xlsx(risk_table, "niveles_de_riesgo_por_entidad.xlsx")

## Función para calcular los factores que más contribuyen a cada categoría
calculate_contributing_factors <- function(data, dep_var, ind_vars, model, threshold_low, threshold_high) {
  predictions <- predict(model, newdata = data[ind_vars])
  risk_data <- data[predictions >= threshold_low & predictions < threshold_high, ]
  
  contributing_factors <- colMeans(risk_data[ind_vars], na.rm = TRUE)
  contributing_factors_df <- data.frame(Variable = names(contributing_factors), Mean_Value = contributing_factors)
  contributing_factors_df <- contributing_factors_df %>% arrange(desc(Mean_Value)) %>% head(10)
  
  return(contributing_factors_df)
}

low_threshold <- quantile(predict(quantile_model_pesotalla, newdata = data[ind_vars]), probs = 0.3333)
high_threshold <- quantile(predict(quantile_model_pesotalla, newdata = data[ind_vars]), probs = 0.6667)

## Calcular factores contribuyentes para cada categoría de riesgo
factors_obesidad <- calculate_contributing_factors(data, "pesotalla", ind_vars, quantile_model_pesotalla, high_threshold, Inf) %>%
  mutate(Category = "Obesidad")
factors_sobrepeso <- calculate_contributing_factors(data, "pesotalla", ind_vars, quantile_model_pesotalla, low_threshold, high_threshold) %>%
  mutate(Category = "Sobrepeso")
factors_desnutricion_leve <- calculate_contributing_factors(data, "pesotalla", ind_vars, quantile_model_pesotalla, low_threshold - 0.3333 * (high_threshold - low_threshold), low_threshold) %>%
  mutate(Category = "Desnutrición leve")
factors_desnutricion_moderada <- calculate_contributing_factors(data, "pesotalla", ind_vars, quantile_model_pesotalla, low_threshold - 0.6667 * (high_threshold - low_threshold), low_threshold - 0.3333 * (high_threshold - low_threshold)) %>%
  mutate(Category = "Desnutrición moderada")
factors_desnutricion_grave <- calculate_contributing_factors(data, "pesotalla", ind_vars, quantile_model_pesotalla, -Inf, low_threshold - 0.6667 * (high_threshold - low_threshold)) %>%
  mutate(Category = "Desnutrición grave")

contributing_factors_table <- bind_rows(
  factors_obesidad,
  factors_sobrepeso,
  factors_desnutricion_leve,
  factors_desnutricion_moderada,
  factors_desnutricion_grave
) %>%
  arrange(Category, desc(Mean_Value))

write_xlsx(contributing_factors_table, "factores_contribuyentes_por_categoria.xlsx")