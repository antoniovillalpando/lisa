# Dr.(C) Antonio Villalpando Acuña
# Script de mapeo de riesgos

library(readxl)
library(writexl)
library(caret)
library(xgboost)
library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(plyr)
library(maps)

# Cargar los datos
datos <- as.data.frame(read_xlsx("dc.xlsx"))

# Selección de variables independientes y dependientes
x <- datos[, 9:146]
y <- datos$pesotalla

# Eliminar filas con valores NA en la variable dependiente o en las predictoras
data_complete <- datos[complete.cases(x, y), ]
x_complete <- data_complete[, 9:143]
y_complete <- data_complete$pesotalla

# Datos de entrenamiento y prueba
set.seed(123)  # Para reproducibilidad
trainIndex <- createDataPartition(y_complete, p = .8, list = FALSE, times = 1)
x_train <- x_complete[trainIndex, ]
x_test <- x_complete[-trainIndex, ]
y_train <- y_complete[trainIndex]
y_test <- y_complete[-trainIndex]

# Convertir los datos a la matriz DMatrix de XGBoost
dtrain <- xgb.DMatrix(data = as.matrix(x_train), label = y_train)
dtest <- xgb.DMatrix(data = as.matrix(x_test), label = y_test)

# Entrenamiento del modelo XGBoost
params <- list(objective = "reg:squarederror", eval_metric = "rmse")
xgb_model <- xgboost(data = dtrain, params = params, nrounds = 100, verbose = 0)

# Predicción con el modelo XGBoost
xgb_predictions <- predict(xgb_model, newdata = dtest)

# Agregar predicciones al dataframe original
datos$pesotalla_pred <- NA
datos$pesotalla_pred[complete.cases(x, y)] <- predict(xgb_model, newdata = xgb.DMatrix(data = as.matrix(x_complete)))

# Verificación de la columna de predicciones
cat("Valores únicos en pesotalla_pred:", unique(datos$pesotalla_pred), "\n")

# Calcular mediana y desviación estándar de las predicciones
median_pred <- median(datos$pesotalla_pred, na.rm = TRUE)
sd_pred <- sd(datos$pesotalla_pred, na.rm = TRUE)

cat("Mediana de las predicciones:", median_pred, "\n")
cat("Desviación estándar de las predicciones:", sd_pred, "\n")

# Calcular riesgo de desnutrición crónica (pesotalla < -2 desviaciones estándar)
threshold_desnutricion <- median_pred - 2 * sd_pred
datos$prob_riesgo_desnutricion <- pnorm(threshold_desnutricion, mean = datos$pesotalla_pred, sd = sd_pred)

# Calcular riesgo de obesidad (pesotalla > +2 desviaciones estándar)
threshold_obesidad <- median_pred + 2 * sd_pred
datos$prob_riesgo_obesidad <- 1 - pnorm(threshold_obesidad, mean = datos$pesotalla_pred, sd = sd_pred)

# Crear subsets del data frame con las variables de riesgo
r_datos_desnutricion <- datos[, c("ent", "prob_riesgo_desnutricion")]
r_datos_obesidad <- datos[, c("ent", "prob_riesgo_obesidad")]

# Guardar los dataframes con las predicciones y los riesgos
write_xlsx(r_datos_desnutricion, "datos_de_riesgos_desnutricionPT.xlsx")
write_xlsx(r_datos_obesidad, "datos_de_riesgos_obesidadPT.xlsx")

## Factores que más contribuyen

# Identificar los factores más importantes
importance_matrix <- xgb.importance(feature_names = colnames(x_train), model = xgb_model)
top_10_factors <- importance_matrix[1:10, ]

# Guardar los factores importantes en un archivo Excel
write_xlsx(top_10_factors, "factores_importantes_pesotalla.xlsx")

## MAPAS

mapaest <- sf::st_read("u_territorial_estados_mgn_inegi_2013.shp")
risk_desnutricion <- read_xlsx("datos_de_riesgos_desnutricionPT.xlsx")
risk_obesidad <- read_xlsx("datos_de_riesgos_obesidadPT.xlsx")

names(mapaest)[names(mapaest) == "cvegeoedo"] <- "ent"

mapaest$ent <- as.integer(mapaest$ent)
risk_desnutricion$ent <- as.integer(risk_desnutricion$ent)
risk_obesidad$ent <- as.integer(risk_obesidad$ent)

# Asegurar que las columnas de unión sean del mismo tipo
mapaest$ent <- as.integer(mapaest$ent)
risk_desnutricion$ent <- as.integer(risk_desnutricion$ent)
risk_obesidad$ent <- as.integer(risk_obesidad$ent)

# Unir los datos
mapa_y_datos_desnutricion <- dplyr::inner_join(mapaest, risk_desnutricion, by = "ent")
mapa_y_datos_obesidad <- dplyr::inner_join(mapaest, risk_obesidad, by = "ent")

# Crear la gráfica de desnutrición
ggplot(mapa_y_datos_desnutricion) +
  geom_sf(aes(fill = prob_riesgo_desnutricion), color = "white", size = 0.2) +
  scale_fill_gradient(low = "#FFE7E7", high = "#C30010", name = "Probabilidad") +
  labs(title = "Riesgo de desnutrición grave en niños y niñas de 0 a 9 años",
       subtitle = "Probabilidad de tener un peso para la talla por debajo de -2 desviaciones estándar de la mediana",
       caption = "Resultado del modelo XGBoost para el peso para la talla") +
  geom_text_repel(aes(label = paste0(round(prob_riesgo_desnutricion * 100, 1), "%"), 
                      geometry = st_geometry(mapa_y_datos_desnutricion)),
                  stat = "sf_coordinates", size = 4, fontface = "bold", 
                  nudge_x = 0.15, nudge_y = 0.15, max.overlaps = Inf,
                  segment.color = "grey50", segment.size = 0.5) +  
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

# Crear la gráfica de obesidad
ggplot(mapa_y_datos_obesidad) +
  geom_sf(aes(fill = prob_riesgo_obesidad), color = "white", size = 0.2) +
  scale_fill_gradient(low = "#FFE7E7", high = "#C30010", name = "Probabilidad") +
  labs(title = "Riesgo de obesidad en niños de 0 a 9 años",
       subtitle = "Probabilidad de tener un peso para la talla por encima de +2 desviaciones estándar de la mediana",
       caption = "Resultado del modelo XGBoost para el peso para la talla") +
  geom_text_repel(aes(label = paste0(round(prob_riesgo_obesidad * 100, 1), "%"), 
                      geometry = st_geometry(mapa_y_datos_obesidad)),
                  stat = "sf_coordinates", size = 4, fontface = "bold", 
                  nudge_x = 0.15, nudge_y = 0.15, max.overlaps = Inf,
                  segment.color = "grey50", segment.size = 0.5) + 
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