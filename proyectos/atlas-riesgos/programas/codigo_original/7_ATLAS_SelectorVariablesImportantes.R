# Dr.(C) Antonio Villalpando Acuña
# Proyecto Atlas de riesgos de malnutrición de Save the Children
# Script de selección de variables

library(readxl)
library(dplyr)
library(writexl)

vars <- as.data.frame(read_xlsx("variables_independientes.xlsx"))

des <- dplyr::select(vars, ent, dsm1, ds59, ic_rezedu, i_privacion, ic_asalud,
                     ic_segsoc)

write_xlsx(des, "des.xlsx")

obs <- dplyr::select(vars, ent, pobreza_e, reg_esp, i_privacion, tamhogesc, obm1, ob14, esc_mat)

write_xlsx(obs, "obs.xlsx")


