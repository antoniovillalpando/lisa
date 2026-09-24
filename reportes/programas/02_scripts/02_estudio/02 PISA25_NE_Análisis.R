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
# 02 · CÁLCULOS CENTRALES
# Fecha de consignación: 15 de septiembre de 2026
# Identificador: PISA-2025-002
# ======================================================================

# Insumo
## data/processed/PISA2025_analisis.rds

# Salida
## data/processed/PISA2025_resultados.rds

# ======================== INICIA PROCESAMIENTO =======================


# 1. CARGA DE PAQUETES

library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(survey)
library(lme4)
library(broom)


# 2. CARGA DE DATOS

archivo_analisis <- "data/processed/PISA2025_analisis.rds"
salida_resultados <- "data/processed/PISA2025_resultados.rds"

pisa2025_analisis <- readRDS(archivo_analisis)

student_mex <- pisa2025_analisis$student_mex
ldw_mex <- pisa2025_analisis$ldw_mex
school_mex <- pisa2025_analisis$school_mex

dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)


# 3. DISEÑO PISA Y FUNCIONES DE ESTIMACIÓN

pesos_replicados <- paste0("W_FSTURWT", 1:80)

pv_dominios <- list(
  matematicas = paste0("PV", 1:10, "MATH"),
  lectura = paste0("PV", 1:10, "READ"),
  ciencias = paste0("PV", 1:10, "SCIE"),
  digital = paste0("PV", 1:10, "CMPS"),
  ambiental = paste0("PV", 1:10, "SENV")
)

crear_diseno <- function(datos) {
  svrepdesign(
    weights = ~W_FSTUWT,
    repweights = datos[, pesos_replicados],
    data = datos,
    type = "Fay",
    rho = 0.5,
    combined.weights = TRUE
  )
}

ajustar_modelos_pv <- function(datos, valores_plausibles, predictores) {
  variables_predictoras <- unique(
    unlist(
      str_split(
        predictores,
        ":"
      )
    )
  )
  
  modelos <- map(
    valores_plausibles,
    function(pv) {
      variables <- unique(
        c(
          pv,
          variables_predictoras,
          "W_FSTUWT",
          pesos_replicados
        )
      )
      
      datos_modelo <- datos |>
        dplyr::select(all_of(variables)) |>
        filter(
          if_all(
            all_of(c(pv, variables_predictoras)),
            ~ !is.na(.x)
          )
        )
      
      diseno <- crear_diseno(datos_modelo)
      
      svyglm(
        reformulate(
          predictores,
          response = pv
        ),
        design = diseno
      )
    }
  )
  
  estimaciones <- map2_dfr(
    modelos,
    seq_along(modelos),
    ~ tidy(.x) |>
      mutate(pv = .y)
  )
  
  estimaciones |>
    group_by(term) |>
    summarise(
      varianza_muestral = mean(std.error^2),
      varianza_pv = var(estimate),
      coeficiente = mean(estimate),
      std.error = sqrt(
        varianza_muestral +
          (1 + 1 / length(valores_plausibles)) * varianza_pv
      ),
      statistic = coeficiente / std.error,
      p.value = 2 * pnorm(abs(statistic), lower.tail = FALSE),
      conf.low = coeficiente - 1.96 * std.error,
      conf.high = coeficiente + 1.96 * std.error,
      .groups = "drop"
    ) |>
    rename(
      estimate = coeficiente
    )
}

ajustar_modelo_rep <- function(datos, respuesta, predictores) {
  variables_predictoras <- unique(
    unlist(
      str_split(
        predictores,
        ":"
      )
    )
  )
  
  variables <- unique(
    c(
      respuesta,
      variables_predictoras,
      "W_FSTUWT",
      pesos_replicados
    )
  )
  
  datos_modelo <- datos |>
    dplyr::select(all_of(variables)) |>
    filter(
      if_all(
        all_of(c(respuesta, variables_predictoras)),
        ~ !is.na(.x)
      )
    )
  
  diseno <- crear_diseno(datos_modelo)
  
  svyglm(
    reformulate(
      predictores,
      response = respuesta
    ),
    design = diseno
  ) |>
    tidy(
      conf.int = TRUE,
      conf.level = 0.95
    )
}

resumir_medidas_rep <- function(datos, variables) {
  diseno <- crear_diseno(datos)
  
  estimacion <- svymean(
    reformulate(variables),
    design = diseno,
    na.rm = TRUE
  )
  
  intervalos <- confint(estimacion)
  
  tibble(
    medida = names(coef(estimacion)),
    estimate = as.numeric(coef(estimacion)),
    std.error = as.numeric(SE(estimacion)),
    conf.low = intervalos[, 1],
    conf.high = intervalos[, 2]
  )
}

calcular_icc <- function(datos, valores_plausibles, predictores = NULL) {
  resultados <- map_dfr(
    valores_plausibles,
    function(pv) {
      formula_modelo <- if (is.null(predictores)) {
        as.formula(paste0(pv, " ~ 1 + (1 | CNTSCHID)"))
      } else {
        as.formula(
          paste0(
            pv,
            " ~ ",
            paste(predictores, collapse = " + "),
            " + (1 | CNTSCHID)"
          )
        )
      }
      
      variables <- unique(
        c(
          pv,
          predictores,
          "CNTSCHID",
          "W_FSTUWT"
        )
      )
      
      datos_modelo <- datos |>
        dplyr::select(all_of(variables)) |>
        filter(if_all(everything(), ~ !is.na(.x))) |>
        mutate(
          peso_modelo = W_FSTUWT / mean(W_FSTUWT)
        )
      
      modelo <- lmer(
        formula_modelo,
        data = datos_modelo,
        weights = peso_modelo,
        REML = TRUE,
        control = lmerControl(
          check.conv.singular = "ignore"
        )
      )
      
      varianzas <- as.data.frame(VarCorr(modelo))
      var_escuela <- varianzas$vcov[varianzas$grp == "CNTSCHID"]
      var_estudiante <- varianzas$vcov[varianzas$grp == "Residual"]
      
      tibble(
        var_escuela = var_escuela,
        var_estudiante = var_estudiante,
        icc = var_escuela / (var_escuela + var_estudiante)
      )
    }
  )
  
  resultados |>
    summarise(
      var_escuela = mean(var_escuela),
      var_estudiante = mean(var_estudiante),
      icc = mean(icc)
    )
}

estandarizar <- function(x) {
  as.numeric(scale(x))
}


# 4. CONSTRUCCIÓN DEL MICRODATO ANALÍTICO DE MÉXICO

factores_individuales <- c(
  "ESCS",
  "AGE",
  "GRADE",
  "REPEAT",
  "IMMIG",
  "SKIPPING",
  "TARDYSD",
  "WORKPAY"
)

factores_aprendizaje <- c(
  "BELONG",
  "DISCLISCI",
  "TEACHSUP",
  "COGACSC",
  "FAMSUP",
  "ENGSCIPR",
  "PERSEV",
  "CURIO",
  "GOALSET",
  "SELFREG"
)

factores_digitales <- c(
  "AIUSESCH",
  "ICTRES",
  "ICTSCH",
  "ICTHOME",
  "ICTQUAL",
  "ICTFEED",
  "ICTOUT",
  "ICTREG",
  "ICTINFO",
  "DIGINTLRN",
  "INTICT",
  "SOIAICT",
  "ICTAVHOME",
  "ICTAVSCH",
  "ICTDISTR"
)

factores_ambientales <- c(
  "OPENVLRN",
  "ENVAPART",
  "COLLENEFF",
  "ENVCAPCH",
  "ENVAWARE"
)

factores_escolares_candidatos <- c(
  "SCHLTYPE",
  "SCHSIZE",
  "STRATIO",
  "STAFFSHORT",
  "EDUSHORT",
  "CREACTIV",
  "LEAD",
  "SCHAUT",
  "RATCMP1",
  "RATCMP2"
)

factores_individuales <- intersect(
  factores_individuales,
  names(student_mex)
)

factores_aprendizaje <- intersect(
  factores_aprendizaje,
  names(student_mex)
)

factores_digitales <- intersect(
  factores_digitales,
  names(student_mex)
)

factores_ambientales <- intersect(
  factores_ambientales,
  names(student_mex)
)

factores_escolares <- intersect(
  factores_escolares_candidatos,
  names(school_mex)
)

school_analitico <- school_mex |>
  dplyr::select(
    CNTSCHID,
    all_of(factores_escolares)
  ) |>
  rename_with(
    ~ paste0("SCH_", .x),
    -CNTSCHID
  )

factores_escolares <- paste0(
  "SCH_",
  factores_escolares
)

student_analitico <- student_mex |>
  left_join(
    school_analitico,
    by = "CNTSCHID"
  ) |>
  mutate(
    sexo = factor(
      if_else(as.numeric(MALE) == 1, "Hombre", "Mujer")
    )
  )

variables_continuas <- intersect(
  c(
    factores_individuales,
    factores_aprendizaje,
    factores_digitales,
    factores_ambientales,
    factores_escolares
  ),
  names(student_analitico)
)

student_analitico <- student_analitico |>
  mutate(
    across(
      all_of(variables_continuas),
      ~ if (is.numeric(.x)) {
        as.numeric(zap_labels(.x))
      } else {
        as.factor(.x)
      }
    )
  )


# 5. MÓDULO I · FACTORES DEL DESEMPEÑO

predictores_m1 <- c(
  "sexo",
  intersect(
    c("ESCS", "AGE", "GRADE", "REPEAT", "IMMIG"),
    factores_individuales
  )
)

predictores_m2 <- unique(
  c(
    predictores_m1,
    factores_aprendizaje,
    intersect(
      c("SKIPPING", "TARDYSD", "WORKPAY"),
      factores_individuales
    )
  )
)

predictores_m3 <- unique(
  c(
    predictores_m2,
    factores_escolares
  )
)

modelos_desempeno <- imap(
  pv_dominios[c("matematicas", "lectura", "ciencias")],
  function(pv, dominio) {
    list(
      individual = ajustar_modelos_pv(
        student_analitico,
        pv,
        predictores_m1
      ),
      aprendizaje = ajustar_modelos_pv(
        student_analitico,
        pv,
        predictores_m2
      ),
      escuela = ajustar_modelos_pv(
        student_analitico,
        pv,
        predictores_m3
      )
    )
  }
)

descomposicion_varianza <- imap_dfr(
  pv_dominios,
  function(pv, dominio) {
    calcular_icc(
      student_analitico,
      pv
    ) |>
      mutate(dominio = dominio)
  }
)

variables_importancia <- unique(
  c(
    "ESCS",
    factores_aprendizaje,
    factores_escolares
  )
)

variables_importancia <- variables_importancia[
  map_lgl(
    student_analitico[variables_importancia],
    is.numeric
  )
]

student_estandarizado <- student_analitico |>
  mutate(
    across(
      all_of(variables_importancia),
      estandarizar
    )
  )

importancia_relativa <- imap_dfr(
  pv_dominios[c("matematicas", "lectura", "ciencias")],
  function(pv, dominio) {
    ajustar_modelos_pv(
      student_estandarizado,
      pv,
      c("sexo", variables_importancia)
    ) |>
      filter(term != "(Intercept)") |>
      mutate(
        dominio = dominio,
        importancia = abs(estimate)
      ) |>
      group_by(dominio) |>
      mutate(
        orden = rank(-importancia, ties.method = "min")
      ) |>
      ungroup()
  }
)

factores_protectores <- intersect(
  c(
    "CURIO",
    "PERSEV",
    "SELFREG",
    "FAMSUP",
    "BELONG",
    "TEACHSUP"
  ),
  names(student_analitico)
)

interacciones_escs <- crossing(
  dominio = names(
    pv_dominios[c("matematicas", "lectura", "ciencias")]
  ),
  factor = factores_protectores
) |>
  mutate(
    resultado = map2(
      dominio,
      factor,
      function(dominio, factor) {
        ajustar_modelos_pv(
          student_analitico,
          pv_dominios[[dominio]],
          c(
            "sexo",
            "ESCS",
            factor,
            paste0("ESCS:", factor)
          )
        )
      }
    )
  ) |>
  unnest(resultado)


# 6. MÓDULO II · APRENDIZAJE DIGITAL E INTELIGENCIA ARTIFICIAL

catalogo_ldw <- tibble(
  variable = names(ldw_mex),
  etiqueta = map_chr(
    ldw_mex,
    ~ if (is.null(attr(.x, "label"))) "" else attr(.x, "label")
  )
) |>
  mutate(
    metrica = case_when(
      str_detect(variable, "TT$") ~ "tiempo",
      str_detect(variable, "A$") ~ "acciones",
      str_detect(variable, "F$") ~ "latencia",
      str_detect(variable, "S$") ~ "puntaje",
      TRUE ~ NA_character_
    ),
    tarea = str_remove(variable, "(?:TT|A|F|S)$"),
    fase = case_when(
      str_detect(etiqueta, regex("show", ignore_case = TRUE)) ~ "show",
      str_detect(etiqueta, regex("learn", ignore_case = TRUE)) ~ "learn",
      str_detect(etiqueta, regex("apply", ignore_case = TRUE)) ~ "apply",
      TRUE ~ NA_character_
    )
  ) |>
  filter(
    !is.na(metrica),
    !is.na(fase)
  ) |>
  mutate(
    maximo_credito = map_dbl(
      variable,
      function(variable) {
        if (!str_detect(variable, "S$")) {
          return(NA_real_)
        }
        
        etiquetas_valor <- attr(
          ldw_mex[[variable]],
          "labels"
        )
        
        if (is.null(etiquetas_valor)) {
          return(NA_real_)
        }
        
        valores_credito <- suppressWarnings(
          as.numeric(
            unname(
              etiquetas_valor[
                str_detect(
                  names(etiquetas_valor),
                  regex("credit", ignore_case = TRUE)
                )
              ]
            )
          )
        )
        
        valores_credito <- valores_credito[
          !is.na(valores_credito)
        ]
        
        if (length(valores_credito) == 0) {
          return(NA_real_)
        }
        
        max(valores_credito)
      }
    )
  )

if (nrow(catalogo_ldw) == 0) {
  stop(
    "No fue posible identificar las fases Show, Learn y Apply en las etiquetas de LDW."
  )
}

ldw_largo <- map_dfr(
  seq_len(nrow(catalogo_ldw)),
  function(i) {
    tibble(
      CNTSTUID = ldw_mex$CNTSTUID,
      tarea = catalogo_ldw$tarea[i],
      fase = catalogo_ldw$fase[i],
      metrica = catalogo_ldw$metrica[i],
      maximo_credito = catalogo_ldw$maximo_credito[i],
      valor = suppressWarnings(
        as.numeric(
          zap_labels(
            ldw_mex[[catalogo_ldw$variable[i]]]
          )
        )
      )
    )
  }
) |>
  mutate(
    valor = case_when(
      metrica == "puntaje" &
        !is.na(maximo_credito) &
        valor >= 0 &
        valor <= maximo_credito ~ valor / maximo_credito,
      metrica == "puntaje" ~ NA_real_,
      TRUE ~ valor
    )
  ) |>
  dplyr::select(
    -maximo_credito
  ) |>
  group_by(
    CNTSTUID,
    tarea,
    fase,
    metrica
  ) |>
  summarise(
    valor = mean(valor, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    valor = if_else(is.nan(valor), NA_real_, valor)
  ) |>
  pivot_wider(
    names_from = metrica,
    values_from = valor
  )

ldw_estudiante_fase <- ldw_largo |>
  group_by(
    CNTSTUID,
    fase
  ) |>
  summarise(
    puntaje = mean(puntaje, na.rm = TRUE),
    tiempo = mean(tiempo, na.rm = TRUE),
    acciones = mean(acciones, na.rm = TRUE),
    latencia = mean(latencia, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    across(
      c(puntaje, tiempo, acciones, latencia),
      ~ if_else(is.nan(.x), NA_real_, .x)
    )
  ) |>
  pivot_wider(
    names_from = fase,
    values_from = c(
      puntaje,
      tiempo,
      acciones,
      latencia
    )
  ) |>
  mutate(
    ganancia_show_apply = puntaje_apply - puntaje_show,
    ganancia_show_learn = puntaje_learn - puntaje_show,
    eficiencia_apply = estandarizar(puntaje_apply) -
      estandarizar(log1p(tiempo_apply)) -
      0.25 * estandarizar(log1p(acciones_apply))
  )

items_ia <- intersect(
  paste0("ST438Q0", 1:4, "DA"),
  names(student_analitico)
)

variables_digital_ia <- intersect(
  c(
    "ST436Q16DA",
    "ST437Q01DA",
    "ST437Q02DA",
    items_ia
  ),
  names(student_analitico)
)

catalogo_ia <- tibble(
  variable = variables_digital_ia,
  etiqueta = map_chr(
    student_analitico[variables_digital_ia],
    ~ if (is.null(attr(.x, "label"))) "" else attr(.x, "label")
  ),
  dimension = case_match(
    variable,
    "ST438Q01DA" ~ "sustitucion",
    "ST438Q02DA" ~ "ampliacion",
    "ST438Q03DA" ~ "sustitucion",
    "ST438Q04DA" ~ "ampliacion",
    "ST436Q16DA" ~ "alfabetizacion_critica",
    "ST437Q01DA" ~ "utilidad_cognitiva",
    "ST437Q02DA" ~ "interes_digital"
  )
)

student_digital <- student_analitico |>
  left_join(
    ldw_estudiante_fase,
    by = "CNTSTUID"
  ) |>
  mutate(
    across(
      all_of(variables_digital_ia),
      ~ as.numeric(zap_labels(.x))
    ),
    alfabetizacion_critica_ia = ST436Q16DA,
    utilidad_cognitiva_digital = ST437Q01DA,
    interes_digital = ST437Q02DA
  )

items_ampliacion <- catalogo_ia |>
  filter(dimension == "ampliacion") |>
  pull(variable)

items_sustitucion <- catalogo_ia |>
  filter(dimension == "sustitucion") |>
  pull(variable)

student_digital$ia_ampliacion <- if (length(items_ampliacion) > 0) {
  rowMeans(
    student_digital[items_ampliacion],
    na.rm = TRUE
  )
} else {
  NA_real_
}

student_digital$ia_sustitucion <- if (length(items_sustitucion) > 0) {
  rowMeans(
    student_digital[items_sustitucion],
    na.rm = TRUE
  )
} else {
  NA_real_
}

student_digital <- student_digital |>
  mutate(
    ia_ampliacion = if_else(
      is.nan(ia_ampliacion),
      NA_real_,
      ia_ampliacion
    ),
    ia_sustitucion = if_else(
      is.nan(ia_sustitucion),
      NA_real_,
      ia_sustitucion
    ),
    balance_ia = ia_ampliacion - ia_sustitucion
  )

resumen_aprendizaje_digital <- resumir_medidas_rep(
  student_digital,
  intersect(
    c(
      "puntaje_show",
      "puntaje_learn",
      "puntaje_apply",
      "ganancia_show_apply",
      "ganancia_show_learn",
      "eficiencia_apply",
      "ia_ampliacion",
      "ia_sustitucion",
      "balance_ia",
      "alfabetizacion_critica_ia",
      "utilidad_cognitiva_digital",
      "interes_digital"
    ),
    names(student_digital)
  )
)

datos_perfiles_ia <- student_digital |>
  dplyr::select(
    CNTSTUID,
    all_of(items_ia)
  ) |>
  drop_na()

set.seed(2509)

kmeans_ia <- kmeans(
  scale(datos_perfiles_ia[items_ia]),
  centers = 4,
  nstart = 100
)

perfiles_ia <- datos_perfiles_ia |>
  dplyr::select(CNTSTUID) |>
  mutate(
    perfil_ia = factor(kmeans_ia$cluster)
  )

centros_ia <- as_tibble(
  kmeans_ia$centers,
  rownames = "perfil_ia"
)

student_digital <- student_digital |>
  left_join(
    perfiles_ia,
    by = "CNTSTUID"
  )

distribucion_perfiles_ia <- resumir_medidas_rep(
  student_digital,
  "perfil_ia"
)

predictores_ldw <- intersect(
  c(
    "sexo",
    "ESCS",
    "SELFREG",
    "GOALSET",
    "CURIO",
    "PERSEV",
    "ia_ampliacion",
    "ia_sustitucion",
    "alfabetizacion_critica_ia",
    "utilidad_cognitiva_digital",
    "interes_digital"
  ),
  names(student_digital)
)

predictores_ldw <- predictores_ldw[
  map_lgl(
    student_digital[predictores_ldw],
    ~ any(!is.na(.x))
  )
]

modelo_ldw <- ajustar_modelos_pv(
  student_digital,
  pv_dominios$digital,
  predictores_ldw
)

modelo_ganancia_ldw <- ajustar_modelo_rep(
  student_digital,
  "ganancia_show_apply",
  predictores_ldw
)

modelo_eficiencia_ldw <- ajustar_modelo_rep(
  student_digital,
  "eficiencia_apply",
  predictores_ldw
)

interacciones_digitales <- map_dfr(
  intersect(
    c(
      "ia_ampliacion",
      "ia_sustitucion",
      "alfabetizacion_critica_ia",
      "SELFREG"
    ),
    names(student_digital)
  ),
  function(factor) {
    ajustar_modelos_pv(
      student_digital,
      pv_dominios$digital,
      c(
        "sexo",
        "ESCS",
        factor,
        paste0("ESCS:", factor)
      )
    ) |>
      mutate(factor = factor)
  }
)


# 7. MÓDULO III · AMBIENTE Y ESCUELA

variables_perfil_ambiental <- intersect(
  c(
    "ENVAWARE",
    "ENVCAPCH",
    "COLLENEFF",
    "ENVAPART"
  ),
  names(student_analitico)
)

datos_perfiles_ambientales <- student_analitico |>
  dplyr::select(
    CNTSTUID,
    all_of(variables_perfil_ambiental)
  ) |>
  drop_na()

set.seed(2510)

kmeans_ambiental <- kmeans(
  scale(datos_perfiles_ambientales[variables_perfil_ambiental]),
  centers = 4,
  nstart = 100
)

perfiles_ambientales <- datos_perfiles_ambientales |>
  dplyr::select(CNTSTUID) |>
  mutate(
    perfil_ambiental = factor(kmeans_ambiental$cluster)
  )

centros_ambientales <- as_tibble(
  kmeans_ambiental$centers,
  rownames = "perfil_ambiental"
)

student_ambiental <- student_analitico |>
  left_join(
    perfiles_ambientales,
    by = "CNTSTUID"
  ) |>
  mutate(
    conocimiento_ambiental = estandarizar(ENVAWARE),
    agencia_ambiental = rowMeans(
      cbind(
        estandarizar(ENVCAPCH),
        estandarizar(COLLENEFF)
      ),
      na.rm = TRUE
    ),
    accion_ambiental = estandarizar(ENVAPART),
    brecha_conocimiento_accion = conocimiento_ambiental - accion_ambiental,
    brecha_agencia_accion = agencia_ambiental - accion_ambiental
  )

resumen_brechas_ambientales <- resumir_medidas_rep(
  student_ambiental,
  c(
    "brecha_conocimiento_accion",
    "brecha_agencia_accion"
  )
)

distribucion_perfiles_ambientales <- resumir_medidas_rep(
  student_ambiental |>
    mutate(
      across(
        perfil_ambiental,
        ~ factor(.x)
      )
    ),
  "perfil_ambiental"
)

predictores_agencia <- intersect(
  c(
    "sexo",
    "ESCS",
    "ENVAWARE",
    "OPENVLRN",
    "CURIO",
    "PERSEV",
    "BELONG",
    "TEACHSUP",
    factores_escolares
  ),
  names(student_ambiental)
)

modelo_agencia_ambiental <- ajustar_modelo_rep(
  student_ambiental,
  "agencia_ambiental",
  predictores_agencia
)

predictores_accion <- intersect(
  c(
    "sexo",
    "ESCS",
    "ENVAWARE",
    "ENVCAPCH",
    "COLLENEFF",
    "OPENVLRN",
    "CURIO",
    "PERSEV",
    "BELONG",
    "TEACHSUP",
    factores_escolares
  ),
  names(student_ambiental)
)

modelo_accion_ambiental <- ajustar_modelo_rep(
  student_ambiental,
  "accion_ambiental",
  predictores_accion
)

modelos_escolares <- imap(
  pv_dominios,
  function(pv, dominio) {
    ajustar_modelos_pv(
      student_analitico,
      pv,
      unique(
        c(
          "sexo",
          "ESCS",
          factores_escolares
        )
      )
    )
  }
)

interacciones_escuela_escs <- map_dfr(
  factores_escolares,
  function(factor) {
    ajustar_modelos_pv(
      student_analitico,
      pv_dominios$ciencias,
      c(
        "sexo",
        "ESCS",
        factor,
        paste0("ESCS:", factor)
      )
    ) |>
      mutate(factor = factor)
  }
)


# 8. DIAGNÓSTICOS Y OBJETO DE RESULTADOS

diagnosticos <- list(
  factores_individuales = factores_individuales,
  factores_aprendizaje = factores_aprendizaje,
  factores_digitales = factores_digitales,
  factores_ambientales = factores_ambientales,
  factores_escolares = factores_escolares,
  catalogo_ldw = catalogo_ldw,
  catalogo_ia = catalogo_ia,
  cobertura_ldw = mean(!is.na(student_digital$ganancia_show_apply)),
  cobertura_perfiles_ia = nrow(perfiles_ia) / nrow(student_digital),
  cobertura_perfiles_ambientales = nrow(perfiles_ambientales) /
    nrow(student_ambiental)
)

pisa2025_resultados <- list(
  desempeno = list(
    descomposicion_varianza = descomposicion_varianza,
    modelos_escalonados = modelos_desempeno,
    importancia_relativa = importancia_relativa,
    interacciones_escs = interacciones_escs
  ),
  digital_ia = list(
    metricas_ldw = ldw_estudiante_fase,
    resumen_aprendizaje = resumen_aprendizaje_digital,
    catalogo_ia = catalogo_ia,
    indicadores_ia = student_digital |>
      dplyr::select(
        CNTSTUID,
        ia_ampliacion,
        ia_sustitucion,
        balance_ia,
        alfabetizacion_critica_ia,
        utilidad_cognitiva_digital,
        interes_digital,
        perfil_ia
      ),
    perfiles_ia = perfiles_ia,
    centros_ia = centros_ia,
    distribucion_perfiles_ia = distribucion_perfiles_ia,
    modelo_ldw = modelo_ldw,
    modelo_ganancia_ldw = modelo_ganancia_ldw,
    modelo_eficiencia_ldw = modelo_eficiencia_ldw,
    interacciones = interacciones_digitales
  ),
  ambiente_escuela = list(
    perfiles_ambientales = perfiles_ambientales,
    centros_ambientales = centros_ambientales,
    distribucion_perfiles_ambientales = distribucion_perfiles_ambientales,
    resumen_brechas_ambientales = resumen_brechas_ambientales,
    brechas_ambientales = student_ambiental |>
      dplyr::select(
        CNTSTUID,
        conocimiento_ambiental,
        agencia_ambiental,
        accion_ambiental,
        brecha_conocimiento_accion,
        brecha_agencia_accion
      ),
    modelo_agencia = modelo_agencia_ambiental,
    modelo_accion = modelo_accion_ambiental,
    modelos_escolares = modelos_escolares,
    interacciones_escuela_escs = interacciones_escuela_escs
  ),
  diagnosticos = diagnosticos
)

saveRDS(
  pisa2025_resultados,
  salida_resultados,
  compress = "xz"
)

cat(
  "\nArchivo generado:\n",
  salida_resultados,
  "\n",
  sep = ""
)


# ======================== FINALIZA PROCESAMIENTO ======================
