# Dr.(C) Antonio Villalpando Acuña
# Proyecto Atlas de riesgos de malnutrición de Save the Children
# Script de análisis de factores

library(readxl)
library(olsrr)
library(dplyr)
library(ggplot2)
library(factoextra)
library(FactoMineR)
library(writexl)

dc_pt <- read_excel("dc_pt.xlsx")
dc_pe <- read_excel("dc_pe.xlsx")
dc_iemc <- read_excel("dc_iemc.xlsx")
dc_te <- read_excel("dc_te.xlsx")

dc_pt <- dplyr::select(dc_pt, pesotalla, hli, discap, mcv_ips, mcv_ips1, mcv_ips2,
                       mcv_ips3, tinfor, merc, sum, cab, ic_rezedu, inas_esc, ic_asalud,
                       ic_segsoc, ic_cv, ic_sbv, tot_iamen, ins_ali)

dc_pe <- dplyr::select(dc_pe, pesoedad, hli, discap, mcv_ips, mcv_ips1, mcv_ips2,
                       mcv_ips3, tinfor, merc, sum, cab, ic_rezedu, inas_esc, ic_asalud,
                       ic_segsoc, ic_cv, ic_sbv, tot_iamen, ins_ali)


dc_iemc <- dplyr::select(dc_iemc, iemc, hli, discap, mcv_ips, mcv_ips1, mcv_ips2,
                       mcv_ips3, tinfor, merc, sum, cab, ic_rezedu, inas_esc, ic_asalud,
                       ic_segsoc, ic_cv, ic_sbv, tot_iamen, ins_ali)


dc_te <- dplyr::select(dc_te, tallaedad, hli, discap, mcv_ips, mcv_ips1, mcv_ips2,
                       mcv_ips3, tinfor, merc, sum, cab, ic_rezedu, inas_esc, ic_asalud,
                       ic_segsoc, ic_cv, ic_sbv, tot_iamen, ins_ali)

# Modelo completo inicial
pt_model <- lm(pesotalla ~ ., data = dc_pt)
pe_model <- lm(pesoedad ~ ., data = dc_pe)
iemc_model <- lm(iemc ~ ., data = dc_iemc)
te_model <- lm(tallaedad ~ ., data = dc_te)

## FORWARD

stepwise_forward_pt <- ols_step_forward_p(pt_model, details = TRUE)
print(stepwise_forward_pt$model)

stepwise_forward_pe <- ols_step_forward_p(pe_model, details = TRUE)
print(stepwise_forward_pe$model)

stepwise_forward_iemc <- ols_step_forward_p(iemc_model, details = TRUE)
print(stepwise_forward_iemc$model)

stepwise_forward_te <- ols_step_forward_p(te_model, details = TRUE)
print(stepwise_forward_te$model)

## ANALISIS DE COMPONENTES PRINCIPALES

dc_pt <- read_excel("dc_pt.xlsx")
dc_pe <- read_excel("dc_pe.xlsx")
dc_iemc <- read_excel("dc_iemc.xlsx")
dc_te <- read_excel("dc_te.xlsx")

dc_pt <- dc_pt[, -1]
dc_pe <- dc_pe[, -1]
dc_iemc <- dc_iemc[, -1]
dc_te <- dc_te[, -1]

pca_pt <- prcomp(dc_pt, scale. = TRUE)
pca_pe <- prcomp(dc_pe, scale. = TRUE)
pca_iemc <- prcomp(dc_iemc, scale. = TRUE)
pca_te <- prcomp(dc_te, scale. = TRUE)

summary(pca_pt)
fviz_eig(pca_pt)

summary(pca_pe)
fviz_eig(pca_pe)

summary(pca_iemc)
fviz_eig(pca_iemc)

summary(pca_te)
fviz_eig(pca_te)

## Obtener los pesos de cada variable para los primeros diez componentes principales
weights_pt <- as.data.frame(pca_pt$rotation[, 1:10])
weights_pe <- as.data.frame(pca_pe$rotation[, 1:10])
weights_iemc <- as.data.frame(pca_iemc$rotation[, 1:10])
weights_te <- as.data.frame(pca_te$rotation[, 1:10])

weights_pt$Variable <- rownames(weights_pt)
weights_pe$Variable <- rownames(weights_pe)
weights_iemc$Variable <- rownames(weights_iemc)
weights_te$Variable <- rownames(weights_te)

weights_pt <- weights_pt[, c("Variable", setdiff(names(weights_pt), "Variable"))]
weights_pe <- weights_pe[, c("Variable", setdiff(names(weights_pe), "Variable"))]
weights_iemc <- weights_iemc[, c("Variable", setdiff(names(weights_iemc), "Variable"))]
weights_te <- weights_te[, c("Variable", setdiff(names(weights_te), "Variable"))]

write_xlsx(list(
  "Weights_PT" = weights_pt,
  "Weights_PE" = weights_pe,
  "Weights_IEMC" = weights_iemc,
  "Weights_TE" = weights_te
), "pca_pesos.xlsx")

## POR COMPONENTES

## Obtener las cargas de cada variable para los componentes principales
loadings_pt <- as.data.frame(pca_pt$rotation)
loadings_pe <- as.data.frame(pca_pe$rotation)
loadings_iemc <- as.data.frame(pca_iemc$rotation)
loadings_te <- as.data.frame(pca_te$rotation)

## Función para identificar las variables más importantes para cada componente
identify_top_variables <- function(loadings, n = 10) {
  top_variables <- list()
  for (i in 1:ncol(loadings)) {
    # Ordenar las variables por la magnitud de las cargas (valores absolutos)
    sorted_variables <- loadings[order(abs(loadings[, i]), decreasing = TRUE), ]
    top_variables[[paste("PC", i, sep = "")]] <- rownames(sorted_variables)[1:n]
  }
  return(top_variables)
}

## Identificar las variables más importantes para los primeros 10 componentes principales
top_variables_pt <- identify_top_variables(loadings_pt, n = 10)
top_variables_pe <- identify_top_variables(loadings_pe, n = 10)
top_variables_iemc <- identify_top_variables(loadings_iemc, n = 10)
top_variables_te <- identify_top_variables(loadings_te, n = 10)

## Escalar componentes
pca_pt <- prcomp(dc_pt, scale. = TRUE)
pca_pe <- prcomp(dc_pe, scale. = TRUE)
pca_iemc <- prcomp(dc_iemc, scale. = TRUE)
pca_te <- prcomp(dc_te, scale. = TRUE)

## Calcular la varianza explicada y la varianza explicada acumulada
explained_variance_pt <- pca_pt$sdev^2 / sum(pca_pt$sdev^2)
explained_variance_pe <- pca_pe$sdev^2 / sum(pca_pe$sdev^2)
explained_variance_iemc <- pca_iemc$sdev^2 / sum(pca_iemc$sdev^2)
explained_variance_te <- pca_te$sdev^2 / sum(pca_te$sdev^2)

cumulative_variance_pt <- cumsum(explained_variance_pt)[1:10]
cumulative_variance_pe <- cumsum(explained_variance_pe)[1:10]
cumulative_variance_iemc <- cumsum(explained_variance_iemc)[1:10]
cumulative_variance_te <- cumsum(explained_variance_te)[1:10]

## Función para resumir nombres de columnas y añadir la fila de varianza explicada acumulada
create_summary_table <- function(top_variables, component_names, cumulative_variance) {
  summary_table <- data.frame(matrix(ncol = length(component_names), nrow = max(sapply(top_variables, length)) + 1))
  colnames(summary_table) <- component_names
  for (i in seq_along(top_variables)) {
    summary_table[1:length(top_variables[[i]]), i] <- top_variables[[i]]
  }
  summary_table[nrow(summary_table), ] <- c(cumulative_variance, rep(NA, ncol(summary_table) - length(cumulative_variance)))
  rownames(summary_table) <- c(1:(nrow(summary_table) - 1), "Proporción acumulada de la varianza")
  return(summary_table)
}

component_names_pt <- paste0("PC", 1:10)
component_names_pe <- paste0("PC", 1:10)
component_names_iemc <- paste0("PC", 1:10)
component_names_te <- paste0("PC", 1:10)

## Identificar las variables más importantes de los 10 componentes principales
top_variables_pt <- identify_top_variables(pca_pt$rotation, n = 10)
top_variables_pe <- identify_top_variables(pca_pe$rotation, n = 10)
top_variables_iemc <- identify_top_variables(pca_iemc$rotation, n = 10)
top_variables_te <- identify_top_variables(pca_te$rotation, n = 10)

summary_table_pt <- create_summary_table(top_variables_pt, component_names_pt, cumulative_variance_pt)
summary_table_pe <- create_summary_table(top_variables_pe, component_names_pe, cumulative_variance_pe)
summary_table_iemc <- create_summary_table(top_variables_iemc, component_names_iemc, cumulative_variance_iemc)
summary_table_te <- create_summary_table(top_variables_te, component_names_te, cumulative_variance_te)

summary_tables <- list(
  "Summary_PT" = summary_table_pt,
  "Summary_PE" = summary_table_pe,
  "Summary_IEMC" = summary_table_iemc,
  "Summary_TE" = summary_table_te
)

write_xlsx(summary_tables, "resumenes_pca.xlsx")