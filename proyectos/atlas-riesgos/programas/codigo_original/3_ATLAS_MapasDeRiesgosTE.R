# Dr.(C) Antonio Villalpando Acuña
# Proyecto Atlas de riesgos de malnutrición de Save the Children
# Script de mapeo de riesgos

library(readxl)
library(writexl)
library(caret)
library(xgboost)

# Cargar los datos
datos <- as.data.frame(read_xlsx("dc.xlsx"))

# Verificar valores faltantes en las columnas de interés
cat("Valores faltantes en tallaedad:", sum(is.na(datos$tallaedad)), "\n")
cat("Valores faltantes en las variables predictoras:", sum(is.na(datos[, 9:143])), "\n")

# Selección de variables independientes y dependientes
x <- datos[, 9:143]
y <- datos$tallaedad

# Eliminar filas con valores NA en la variable dependiente o en las predictoras
data_complete <- datos[complete.cases(x, y), ]
x_complete <- data_complete[, 9:143]
y_complete <- data_complete$tallaedad

# Datos de entrenamiento y prueba
set.seed(123)  
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
datos$tallaedad_pred <- NA
datos$tallaedad_pred[complete.cases(x, y)] <- predict(xgb_model, newdata = xgb.DMatrix(data = as.matrix(x_complete)))

# Verificación de la columna de predicciones
cat("Valores únicos en tallaedad_pred:", unique(datos$tallaedad_pred), "\n")

# Calcular media y desviación estándar de las predicciones
median_pred <- median(datos$tallaedad_pred, na.rm = TRUE)
sd_pred <- sd(datos$tallaedad_pred, na.rm = TRUE)

cat("Mediana de las predicciones:", median_pred, "\n")
cat("Desviación estándar de las predicciones:", sd_pred, "\n")

# Calcular riesgo de que tallaedad caiga por debajo de -2 desviación estándar
threshold <- median_pred - 2 * sd_pred
datos$prob_riesgo_baja_talla <- pnorm(threshold, mean = datos$tallaedad_pred, sd = sd_pred)

# Crear un subset del data frame con la variable "ent" y la variable de riesgo
r_datos <- datos[, c("ent", "prob_riesgo_baja_talla")]

# Guardar el dataframe con las predicciones y los riesgos
write_xlsx(r_datos, "datos_de_riesgos_baja_talla.xlsx")

## Factores que más contribuyen

# Identificar los factores más importantes
importance_matrix <- xgb.importance(feature_names = colnames(x_train), model = xgb_model)
top_10_factors <- importance_matrix[1:10, ]

# Guardar los factores importantes en un archivo Excel
write_xlsx(top_10_factors, "factores_importantes_tallaedad.xlsx")

## MAPAS

library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(plyr)
library(maps)
library(ggrepel)

mapaest <- sf::st_read("u_territorial_estados_mgn_inegi_2013.shp")
risk <- read_xlsx("datos_de_riesgos_baja_talla.xlsx")

names(mapaest)[names(mapaest) == "cvegeoedo"] <- "ent"

mapaest$ent <- as.integer(mapaest$ent)
risk$ent <- as.integer(risk$ent)

# Asegurar que las columnas de unión sean del mismo tipo
mapaest$ent <- as.integer(mapaest$ent)
risk$ent <- as.integer(risk$ent)

# Unir los datos
mapa_y_datos <- dplyr::inner_join(mapaest, risk, by = "ent")

# Crear la gráfica
plot <- ggplot(mapa_y_datos) +
  geom_sf(aes(fill = prob_riesgo_baja_talla), color = "white", size = 0.2) +
  scale_fill_gradient(low = "#FFE7E7", high = "#C30010", name = "Probabilidad") +
  labs(title = "Riesgo de desnutrición crónica (baja talla) en niños y niñas de 0 a 9 años",
       subtitle = "Probabilidad de tener una Talla para la edad de –2 desviaciones estándar por debajo de la mediana",
       caption = "Resultado del modelo XGBoost para la talla para la edad") +
  geom_text_repel(aes(label = paste0(round(prob_riesgo_baja_talla * 100, 1), "%"), 
                      geometry = st_geometry(mapa_y_datos)),
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

ggsave(file="CN_mapa1.svg", plot=plot, width=27, height=19)
