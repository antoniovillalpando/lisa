# Dr.(C) Antonio Villalpando Acuña
# Proyecto Atlas de riesgos de malnutrición de Save the Children
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
library(ggrepel)

datos <- as.data.frame(read_xlsx("dc.xlsx"))

cat("Valores faltantes en pesoedad:", sum(is.na(datos$pesoedad)), "\n")
cat("Valores faltantes en las variables predictoras:", sum(is.na(datos[, 9:143])), "\n")

x <- datos[, 9:143]
y <- datos$pesoedad

data_complete <- datos[complete.cases(x, y), ]
x_complete <- data_complete[, 9:143]
y_complete <- data_complete$pesoedad

## Datos de entrenamiento y prueba
set.seed(123)
trainIndex <- createDataPartition(y_complete, p = .8, list = FALSE, times = 1)
x_train <- x_complete[trainIndex, ]
x_test <- x_complete[-trainIndex, ]
y_train <- y_complete[trainIndex]
y_test <- y_complete[-trainIndex]

## Convertir los datos a la matriz DMatrix de XGBoost
dtrain <- xgb.DMatrix(data = as.matrix(x_train), label = y_train)
dtest <- xgb.DMatrix(data = as.matrix(x_test), label = y_test)

## Entrenamiento del modelo XGBoost
params <- list(objective = "reg:squarederror", eval_metric = "rmse")
xgb_model <- xgboost(data = dtrain, params = params, nrounds = 100, verbose = 0)

## Predicción con el modelo XGBoost
xgb_predictions <- predict(xgb_model, newdata = dtest)

## Agregar predicciones al dataframe original
datos$pesoedad_pred <- NA
datos$pesoedad_pred[complete.cases(x, y)] <- predict(xgb_model, newdata = xgb.DMatrix(data = as.matrix(x_complete)))

cat("Valores únicos en pesoedad_pred:", unique(datos$pesoedad_pred), "\n")

median_pred <- median(datos$pesoedad_pred, na.rm = TRUE)
sd_pred <- sd(datos$pesoedad_pred, na.rm = TRUE)

cat("Mediana de las predicciones:", median_pred, "\n")
cat("Desviación estándar de las predicciones:", sd_pred, "\n")

threshold <- median_pred + 1 * sd_pred
datos$prob_riesgo_obesidad <- 1 - pnorm(threshold, mean = datos$pesoedad_pred, sd = sd_pred)

r_datos <- datos[, c("ent", "prob_riesgo_obesidad")]
write_xlsx(r_datos, "datos_de_riesgos_obesidad.xlsx")

# Factores que más contribuyen

importance_matrix <- xgb.importance(feature_names = colnames(x_train), model = xgb_model)
top_10_factors <- importance_matrix[1:10, ]

## Guardar los factores importantes en un archivo Excel
# write_xlsx(top_10_factors, "factores_importantes_pesoedad.xlsx")

# MAPAS

mapaest <- sf::st_read("u_territorial_estados_mgn_inegi_2013.shp")
risk <- read_xlsx("datos_de_riesgos_obesidad.xlsx")

names(mapaest)[names(mapaest) == "cvegeoedo"] <- "ent"

mapaest$ent <- as.integer(mapaest$ent)
risk$ent <- as.integer(risk$ent)

mapaest$ent <- as.integer(mapaest$ent)
risk$ent <- as.integer(risk$ent)

mapa_y_datos <- dplyr::inner_join(mapaest, risk, by = "ent")

plot5 <- ggplot(mapa_y_datos) +
  geom_sf(aes(fill = prob_riesgo_obesidad), color = "white", size = 0.2) +
  scale_fill_gradient(low = "#FFE7E7", high = "#C30010", name = "Probabilidad") +
  labs(title = "Riesgo de sobrepeso y obesidad en niñas y niños de 0 a 9 años",
       subtitle = "Probabilidad de tener un Peso para la edad de +1 desviación estándar por encima de la mediana",
       caption = "Resultado del modelo XGBoost para el peso para la edad") +
  geom_text_repel(aes(label = paste0(round(prob_riesgo_obesidad * 100, 1), "%"), 
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

ggsave(file="CN_mapa5.svg", plot=plot5, width=27, height=19)