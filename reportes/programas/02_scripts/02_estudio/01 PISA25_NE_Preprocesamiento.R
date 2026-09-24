# ╔══════════════════════════════════════════════════════════════════════╗
# ║                                                                      ║
# ║            88            88    ad88888ba          db                 ║
# ║            88            88   d8"     "8b        d88b                ║
# ║            88            88   Y8,               d8'`8b               ║
# ║            88            88   `Y8aaaaa,        d8'  `8b              ║
# ║            88            88     `"""""8b,     d8YaaaaY8b             ║
# ║            88            88           `8b.   d8""""""""8b            ║
# ║            88            88   Y8a     a8P   d8'        `8b           ║
# ║            88888888888   88    "Y88888P"   d8'          `8b          ║
# ║                                                                      ║
# ║             LABORATORIO DE INVESTIGACIÓN SOCIAL AVANZADA             ║
# ║             "Las mejores causas merecen la mejor ciencia"            ║
# ║                                                                      ║
# ║======================================================================║
# ║              LISA Servicios Científicos, S.A.S. de C.V.              ║
# ║              https://antoniovillalpando.github.io/lisa/.             ║
# ║               Contacto: profesorvillalpando@gmail.com                ║
# ╚══════════════════════════════════════════════════════════════════════╝

# ======================================================================
# PISA 2025 · DESEMPEÑO, TECNOLOGÍA Y ENTORNOS EDUCATIVOS
# 01 · PREPARACIÓN DE DATOS PISA 2025
# Fecha de consignación: 10 de septiembre de 2026
# Identificador: PISA-2025-001
# ======================================================================

# Insumos
## CY09_MS_STU_PUF.sav
## CY09_MS_LDW_20260806.sav
## CY09_MS_SCH_PUF.sav
## CY09_MS_STU_TT_PUF.sav
## PISA2025_Codebook.xlsx
##
## Disponibles, pero NO requeridos para el reporte actual:
## CY09_MS_COG_20260806.sav
## CY09_MS_COG_PROCESS_20260806.sav

# Salida
## PISA2025_analisis.rds

# ======================== INICIA PROCESAMIENTO =======================


# 1. CARGA DE PAQUETES

library(haven)
library(dplyr)
library(purrr)
library(readxl)
library(stringr)


# 2. DEFINICIÓN DE RUTAS

archivo_stu <- "CY09_MS_STU_PUF.sav"
archivo_ldw <- "CY09_MS_LDW_20260806.sav"
archivo_sch <- "CY09_MS_SCH_PUF.sav"
archivo_stu_tt <- "CY09_MS_STU_TT_PUF.sav"
archivo_codebook <- "PISA2025_Codebook.xlsx"

# Estos dos archivos se dejan registrados, pero no se leen en esta versión.
# Véase la sección 11.
archivo_cog <- "CY09_MS_COG_20260806.sav"
archivo_cog_process <- "CY09_MS_COG_PROCESS_20260806.sav"

salida_analisis <- "data/processed/PISA2025_analisis.rds"

dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)


# 3. LECTURA DEL CODEBOOK Y DEFINICIÓN DE VARIABLES

leer_codebook <- function(sheet) {
  read_excel(
    archivo_codebook,
    sheet = sheet
  ) |>
    filter(!is.na(NAME)) |>
    dplyr::select(
      NAME,
      VARLABEL,
      TYPE,
      FORMAT,
      VARNUM
    )
}

dic_stu <- leer_codebook("STU")
dic_ldw <- leer_codebook("LDW")

# ----------------------------------------------------------------------
# 3.1. Estudiantes
# ----------------------------------------------------------------------
# El RDS de estudiantes deja de ser un extracto mínimo para las gráficas.
# Se convierte en el microdato analítico canónico del reporte.
#
# Conserva:
#   a) identificadores y opciones de cuestionario;
#   b) género y grado internacional;
#   c) bloques crudos necesarios para las agendas sustantivas del reporte;
#   d) todos los índices derivados relevantes (VARNUM 604–690);
#   e) peso final, diseño BRR-Fay, senate weight y todos los plausible values.

vars_identificacion <- dic_stu |>
  filter(VARNUM <= 18) |>
  pull(NAME)

vars_demografia_basica <- c(
  "ST001D01T",
  "ST001D01T_4",
  "ST001D01T_3",
  "ST004D01T",
  "MALE"
)

# Bloques crudos que sostienen las preguntas científicas del reporte.
# Se mantienen completos para no tener que regresar al PUF en cada análisis.
prefijos_stu_reporte <- c(
  # Clima, identidad y pedagogía de ciencias
  "ST094",
  "ST097",
  "ST129",
  "ST131",
  "ST468",
  "ST469",
  "ST515",
  "ST531",
  
  # Autorregulación, curiosidad, perseverancia y resolución de problemas
  "ST216",
  "ST301",
  "ST307",
  "ST432",
  "ST433",
  "ST434",
  "ST435",
  
  # Ecosistema digital, acceso, tiempo de uso e IA
  "ST250",
  "ST253",
  "ST254",
  "ST322",
  "ST326",
  "ST436",
  "ST437",
  "ST438",
  
  # Agencia y aprendizaje ambiental
  "ST092",
  "ST495",
  "ST498",
  "ST499",
  "ST500",
  "ST501",
  "ST533",
  
  # Esfuerzo en la evaluación
  "ST331"
)

patron_stu_reporte <- paste0(
  "^(?:",
  paste(prefijos_stu_reporte, collapse = "|"),
  ")"
)

vars_bloques_reporte <- dic_stu |>
  filter(str_detect(NAME, patron_stu_reporte)) |>
  pull(NAME)

# Índices WLE y variables derivadas: pertenencia, clima, apoyo docente,
# activación cognitiva, autoeficacia, agencia ambiental, IA, autorregulación,
# ICT, ESCS, ocupaciones, expectativas educativas, inmigración, edad, etc.
vars_indices_derivados <- dic_stu |>
  filter(between(VARNUM, 604, 690)) |>
  pull(NAME)

# Peso final, 80 pesos replicados BRR-Fay, UNIT/WVARSTRR,
# todos los plausible values —incluidos CMPS, CPPK, SEPS, SEDE, SEID,
# SENV, CMOD y CPRO—, SENWT y fecha de versión.
vars_diseno_y_pv <- dic_stu |>
  filter(between(VARNUM, 691, 885)) |>
  pull(NAME)

vars_keep_student_mex <- unique(
  c(
    vars_identificacion,
    vars_demografia_basica,
    vars_bloques_reporte,
    vars_indices_derivados,
    vars_diseno_y_pv
  )
)

# Conjuntos de PV utilizados por las salidas descriptivas ya construidas.
pv_math <- paste0("PV", 1:10, "MATH")
pv_read <- paste0("PV", 1:10, "READ")
pv_scie <- paste0("PV", 1:10, "SCIE")
pv_ldw <- paste0("PV", 1:10, "CMPS")
pv_env <- paste0("PV", 1:10, "SENV")

vars_factores_internacionales <- c(
  "ESCS",
  "IMMIG",
  "LANGN",
  "REPEAT",
  "SKIPPING",
  "TARDYSD",
  "WORKPAY",
  "AGE",
  "GRADE",
  "BELONG",
  "DISCLISCI",
  "TEACHSUP",
  "COGACSC",
  "SCIECOLLR",
  "JOYSCIE",
  "EFFSCIE",
  "STBELSCIE",
  "OPENVLRN",
  "ENVAPART",
  "COLLENEFF",
  "ENVCAPCH",
  "ENVAWARE",
  "AIUSESCH",
  "FAMSUP",
  "ENGSCIPR",
  "PERSEV",
  "CURIO",
  "ENPROBS",
  "GOALSET",
  "SELFREG",
  "ICTRES",
  "ICTSCH",
  "ICTHOME",
  "ICTQUAL",
  "ICTFEED",
  "ICTOUT",
  "ICTWKDY",
  "ICTWKEND",
  "ICTREG",
  "ICTINFO",
  "DIGINTLRN",
  "INTICT",
  "SOIAICT",
  "ICTAVHOME",
  "ICTAVSCH",
  "ICTDISTR"
)

vars_items_internacionales <- c(
  "ST097Q06DA",
  paste0("ST326Q0", 1:6, "JA"),
  "ST436Q16DA",
  paste0("ST437Q0", 1:2, "DA"),
  paste0("ST438Q0", 1:4, "DA")
)

vars_pesos <- c(
  "W_FSTUWT",
  paste0("W_FSTURWT", 1:80)
)

vars_keep_student_world <- unique(
  c(
    vars_identificacion,
    vars_demografia_basica,
    vars_factores_internacionales,
    vars_items_internacionales,
    vars_pesos,
    pv_math,
    pv_read,
    pv_scie,
    pv_ldw,
    pv_env
  )
)

# ----------------------------------------------------------------------
# 3.2. Learning in the Digital World
# ----------------------------------------------------------------------
# Para estudiar "learnability" conservamos:
#   - identificadores y metadatos del módulo;
#   - respuesta puntuada (S);
#   - tiempo total (TT);
#   - número de acciones (A);
#   - tiempo hasta la primera acción (F).
#
# Las respuestas crudas (R) se excluyen porque no son necesarias para las
# métricas previstas y son la parte menos eficiente del archivo.

vars_keep_ldw <- dic_ldw |>
  filter(
    VARNUM <= 13 |
      str_detect(NAME, "(?:S|TT|A|F)$") |
      NAME == "VER_DAT"
  ) |>
  pull(NAME) |>
  unique()


# 4. VALIDACIÓN DE ARCHIVOS Y VARIABLES

archivos_requeridos <- c(
  archivo_stu,
  archivo_ldw,
  archivo_sch,
  archivo_stu_tt,
  archivo_codebook
)

faltan_archivos <- archivos_requeridos[!file.exists(archivos_requeridos)]

if (length(faltan_archivos) > 0) {
  stop(
    paste0(
      "No se encontraron los siguientes insumos:\n",
      paste(faltan_archivos, collapse = "\n")
    )
  )
}

validar_variables_sav <- function(archivo, variables) {
  nombres_sav <- names(
    read_sav(
      archivo,
      n_max = 0
    )
  )
  
  faltantes <- setdiff(
    variables,
    nombres_sav
  )
  
  if (length(faltantes) > 0) {
    stop(
      paste0(
        "Variables no encontradas en ",
        archivo,
        ":\n",
        paste(faltantes, collapse = "\n")
      )
    )
  }
  
  invisible(TRUE)
}

validar_variables_sav(
  archivo_stu,
  unique(
    c(
      vars_keep_student_mex,
      vars_keep_student_world
    )
  )
)

validar_variables_sav(
  archivo_ldw,
  vars_keep_ldw
)


# 5. MICRODATO ANALÍTICO INTERNACIONAL DE ESTUDIANTES

pisa_student_world_full <- read_sav(
  archivo_stu,
  col_select = all_of(vars_keep_student_mex)
)

pisa_student_mex <- pisa_student_world_full |>
  filter(CNT == "MEX")

pisa_student_world <- pisa_student_world_full |>
  dplyr::select(
    all_of(vars_keep_student_world)
  )

rm(pisa_student_world_full)
gc()

# 6. PUNTAJES NACIONALES PARA COMPARACIÓN INTERNACIONAL

media_pv <- function(data, pv_vars, weight = "W_FSTUWT") {
  
  medias <- map_dbl(
    pv_vars,
    \(pv) weighted.mean(
      data[[pv]],
      w = data[[weight]],
      na.rm = TRUE
    )
  )
  
  mean(
    medias,
    na.rm = TRUE
  )
}

pisa_world_scores <- pisa_student_world |>
  dplyr::select(
    CNT,
    W_FSTUWT,
    all_of(pv_math),
    all_of(pv_read),
    all_of(pv_scie),
    all_of(pv_ldw),
    all_of(pv_env)
  ) |>
  group_by(CNT) |>
  group_modify(
    ~ tibble(
      math = media_pv(
        .x,
        pv_math
      ),
      reading = media_pv(
        .x,
        pv_read
      ),
      science = media_pv(
        .x,
        pv_scie
      ),
      ldw = media_pv(
        .x,
        pv_ldw
      ),
      environmental_science = media_pv(
        .x,
        pv_env
      ),
      n_students = nrow(.x)
    )
  ) |>
  ungroup() |>
  mutate(
    across(
      c(
        math,
        reading,
        science,
        ldw,
        environmental_science
      ),
      ~ round(.x, 1)
    )
  )

# 7. MICRODATO DE ESCUELAS · MÉXICO

# El archivo escolar es pequeño; se conserva completo para México.
# Unión posterior con estudiantes: CNTSCHID.

pisa_school_mex <- read_sav(
  archivo_sch
) |>
  filter(CNT == "MEX")


# 8. LEARNING IN THE DIGITAL WORLD Y TIEMPOS DE CUESTIONARIO · MÉXICO

# ----------------------------------------------------------------------
# 8.1. LDW
# ----------------------------------------------------------------------
# Unión posterior con estudiantes: CNTSTUID + CNTSCHID.

pisa_ldw_world <- read_sav(
  archivo_ldw,
  col_select = all_of(vars_keep_ldw)
)

pisa_ldw_mex <- pisa_ldw_world |>
  filter(CNT == "MEX")

# ----------------------------------------------------------------------
# 8.2. Tiempos de respuesta del cuestionario de estudiantes
# ----------------------------------------------------------------------
# Archivo pequeño y de sólo 108 variables: se conserva completo.
# Será útil para controles de calidad, esfuerzo y sensibilidad.

pisa_stu_tt_mex <- read_sav(
  archivo_stu_tt,
  encoding = "latin1"
) |>
  filter(CNT == "MEX")

dim(pisa_stu_tt_mex)
table(pisa_stu_tt_mex$CNT, useNA = "ifany")


# 9. OBJETO CANÓNICO DE ANÁLISIS

pisa2025_analisis <- list(
  student_world = pisa_student_world,
  student_mex = pisa_student_mex,
  ldw_world = pisa_ldw_world,
  ldw_mex = pisa_ldw_mex,
  school_mex = pisa_school_mex,
  student_timing_mex = pisa_stu_tt_mex,
  world_scores = pisa_world_scores
)

# 10. ARCHIVOS COG Y COG_PROCESS · RESERVA METODOLÓGICA

# NO se procesan en el pipeline principal del reporte.
#
# Razón:
#   1) CY09_MS_COG_20260806.sav (~13.9 GB) contiene respuestas cognitivas
#      crudas. Para las preguntas actuales usamos los plausible values y
#      subescalas ya contenidos en STU.
#
#   2) CY09_MS_COG_PROCESS_20260806.sav (~12.3 GB) contiene tiempos y
#      acciones de los ítems cognitivos generales. Para la agenda de
#      "learnability" usamos CY09_MS_LDW_20260806.sav, que ya contiene Show/Learn/Apply con
#      score, tiempo, acciones y latencia a primera acción.
#
# Leer ambos archivos completos encarecería mucho el preprocesamiento sin
# añadir información necesaria para el reporte actual.
#
# Si después se abre una pregunta que requiera respuestas crudas o process
# data FUERA de LDW, conviene crear un extractor específico de las variables
# necesarias, no un RDS gigante por defecto.


# 11. VERIFICACIÓN DE RESULTADOS Y COBERTURA DE UNIONES

print(
  pisa_world_scores |>
    filter(CNT == "MEX")
)

cat(
  "\nESTUDIANTES\n",
  "Observaciones México: ", nrow(pisa_student_mex), "\n",
  "Variables conservadas: ", ncol(pisa_student_mex), "\n",
  "\nESCUELAS\n",
  "Observaciones México: ", nrow(pisa_school_mex), "\n",
  "Variables conservadas: ", ncol(pisa_school_mex), "\n",
  "\nLDW\n",
  "Observaciones México: ", nrow(pisa_ldw_mex), "\n",
  "Variables conservadas: ", ncol(pisa_ldw_mex), "\n",
  "\nSTUDENT TIMING\n",
  "Observaciones México: ", nrow(pisa_stu_tt_mex), "\n",
  "Variables conservadas: ", ncol(pisa_stu_tt_mex), "\n",
  sep = ""
)

# Cobertura de llaves para las uniones posteriores.
cobertura_school <- mean(
  pisa_student_mex$CNTSCHID %in% pisa_school_mex$CNTSCHID
)

cobertura_ldw <- mean(
  pisa_student_mex$CNTSTUID %in% pisa_ldw_mex$CNTSTUID
)

cobertura_stu_tt <- mean(
  pisa_student_mex$CNTSTUID %in% pisa_stu_tt_mex$CNTSTUID
)

cat(
  "\nCOBERTURA DE UNIONES\n",
  "Estudiantes con escuela en SCH: ",
  round(100 * cobertura_school, 2), "%\n",
  "Estudiantes presentes en LDW: ",
  round(100 * cobertura_ldw, 2), "%\n",
  "Estudiantes presentes en STU_TT: ",
  round(100 * cobertura_stu_tt, 2), "%\n",
  sep = ""
)

cat(
  "\nPaíses/economías en world_scores: ",
  nrow(pisa_world_scores),
  "\n",
  sep = ""
)

saveRDS(
  pisa2025_analisis,
  salida_analisis,
  compress = "xz"
)

cat(
  "\nArchivo generado:\n",
  salida_analisis, "\n",
  sep = ""
)

# ======================== FINALIZA PROCESAMIENTO ======================