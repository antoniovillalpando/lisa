# Dr.(C) Antonio Villalpando Acuña
# Proyecto Atlas de riesgos de malnutrición de Save the Children
# Script de regresiones y análisis univariado

library(tidyverse)
library(broom)
library(openxlsx)
library(readxl)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

datos <- as.data.frame(read_xlsx("dc.xlsx"))

dependent_vars <- c("tallaedad")
independent_vars <- names(datos)[10:146]


results_list <- list()

## Función para hacer regresiones
for (dep_var in dependent_vars) {
  for (indep_var in independent_vars) {
    model <- lm(as.formula(paste(dep_var, "~", indep_var)), data = datos)
    model_summary <- tidy(model)
    
    model_summary <- model_summary %>%
      filter(term == indep_var) %>%
      select(term, estimate, std.error, p.value)
    
    model_summary <- model_summary %>%
      mutate(dependent_var = dep_var, independent_var = indep_var)
    
    results_list[[paste(dep_var, indep_var, sep = "_")]] <- model_summary
  }
}

results_df <- bind_rows(results_list)

## Reordenar las columnas
results_df <- results_df %>%
  select(dependent_var, independent_var, everything())

write.xlsx(results_df, file = "regresion_resultadosTE.xlsx")


# residuos

independent_vars <- c("edad", "sexo", "inc_vi", "merc", "cab", "sum",
"rururb", "inas_esc", "agent_san", "ic_asalud", "ic_segsoc",
"ic_cv", "ic_sbv", "isb_agua", "isb_dren", "isb_luz",
"isb_combus", "ins_ali", "ic_ali", "lca", "dch", "plp_e",
"plp", "pobreza", "pobreza_e", "pobreza_m", "vul_car", "vul_ing",
"hli", "discap", "mcv_ips", "mcv_ips1", "mcv_ips2", "mcv_ips3",
"mins_tot", "mins04", "ifp", "ifs", "ifem", "seg_aguapot",
"sane", "gini", "esc_mat")


summary(model)
