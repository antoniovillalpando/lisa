# Dr.(C) Antonio Villalpando Acuña
# Atlas de Riesgos para la Nutrición de la Niñez en México de Save the Children

library(readxl)
library(writexl)
library(dplyr)
library(broom)
library(olsrr)
library(ggplot2)
library(factoextra)

archivo_datos <- "dc.xlsx"
archivo_variables <- "variables_independientes.xlsx"
archivo_codigos <- "Variables_independientes_códigos.xlsx"
archivo_factores <- "factores_importantes.xlsx"
directorio_salida <- "salidas_analisis_complementario"
numero_componentes <- 10
numero_variables_componente <- 10

dir.create(directorio_salida, recursive = TRUE, showWarnings = FALSE)

ajustar_regresiones_univariadas <- function(
  datos,
  variables_dependientes,
  variables_independientes
) {
  resultados <- list()

  for (variable_dependiente in variables_dependientes) {
    for (variable_independiente in variables_independientes) {
      formula_modelo <- reformulate(
        variable_independiente,
        response = variable_dependiente
      )

      modelo <- lm(
        formula_modelo,
        data = datos
      )

      tabla <- tidy(modelo) %>%
        filter(term == variable_independiente) %>%
        transmute(
          dependent_var = variable_dependiente,
          independent_var = variable_independiente,
          term,
          estimate,
          std.error,
          statistic,
          p.value,
          n = nobs(modelo),
          r_squared = glance(modelo)$r.squared
        )

      resultados[[paste(
        variable_dependiente,
        variable_independiente,
        sep = "_"
      )]] <- tabla
    }
  }

  bind_rows(resultados)
}

ajustar_forward <- function(
  datos,
  variable_dependiente,
  variables_independientes
) {
  formula_modelo <- reformulate(
    variables_independientes,
    response = variable_dependiente
  )

  modelo_completo <- lm(
    formula_modelo,
    data = datos
  )

  seleccion <- ols_step_forward_p(
    modelo_completo,
    details = TRUE
  )

  modelo_seleccionado <- seleccion$model

  list(
    modelo = modelo_seleccionado,
    coeficientes = tidy(modelo_seleccionado) %>%
      mutate(
        dependent_var = variable_dependiente,
        formula = paste(
          deparse(formula(modelo_seleccionado)),
          collapse = ""
        ),
        r_squared = glance(modelo_seleccionado)$r.squared,
        adjusted_r_squared = glance(modelo_seleccionado)$adj.r.squared,
        aic = AIC(modelo_seleccionado),
        bic = BIC(modelo_seleccionado)
      )
  )
}

preparar_pca <- function(
  datos,
  variable_dependiente,
  variables_independientes
) {
  variables <- c(
    variable_dependiente,
    variables_independientes
  )

  base <- datos %>%
    select(all_of(variables)) %>%
    mutate(
      across(
        everything(),
        ~ as.numeric(as.character(.x))
      )
    ) %>%
    filter(if_all(everything(), is.finite))

  prcomp(
    base,
    center = TRUE,
    scale. = TRUE
  )
}

extraer_pesos_pca <- function(
  pca,
  numero_componentes
) {
  numero <- min(
    numero_componentes,
    ncol(pca$rotation)
  )

  pesos <- as.data.frame(
    pca$rotation[, seq_len(numero), drop = FALSE]
  )

  pesos$Variable <- rownames(pesos)

  pesos %>%
    select(
      Variable,
      everything()
    )
}

identificar_variables_componentes <- function(
  pca,
  numero_componentes,
  numero_variables
) {
  numero <- min(
    numero_componentes,
    ncol(pca$rotation)
  )

  resultados <- vector(
    "list",
    numero
  )

  for (i in seq_len(numero)) {
    cargas <- pca$rotation[, i]

    variables <- names(
      sort(
        abs(cargas),
        decreasing = TRUE
      )
    )

    resultados[[i]] <- head(
      variables,
      numero_variables
    )
  }

  names(resultados) <- paste0(
    "PC",
    seq_len(numero)
  )

  resultados
}

crear_resumen_pca <- function(
  pca,
  numero_componentes,
  numero_variables
) {
  variables <- identificar_variables_componentes(
    pca,
    numero_componentes,
    numero_variables
  )

  numero <- length(variables)
  filas <- max(
    lengths(variables)
  ) + 1

  tabla <- as.data.frame(
    matrix(
      NA_character_,
      nrow = filas,
      ncol = numero
    ),
    stringsAsFactors = FALSE
  )

  names(tabla) <- names(variables)

  for (i in seq_along(variables)) {
    tabla[
      seq_along(variables[[i]]),
      i
    ] <- variables[[i]]
  }

  varianza <- pca$sdev^2 / sum(pca$sdev^2)
  acumulada <- cumsum(varianza)[seq_len(numero)]

  tabla[filas, ] <- format(
    acumulada,
    digits = 6,
    scientific = FALSE
  )

  tabla <- cbind(
    Fila = c(
      paste0(
        "Variable_",
        seq_len(filas - 1)
      ),
      "Proporción acumulada de la varianza"
    ),
    tabla
  )

  tabla
}

guardar_grafica_varianza <- function(
  pca,
  nombre
) {
  grafica <- fviz_eig(
    pca,
    addlabels = TRUE
  )

  ggsave(
    filename = file.path(
      directorio_salida,
      paste0(
        "varianza_",
        nombre,
        ".svg"
      )
    ),
    plot = grafica,
    width = 12,
    height = 8
  )
}

datos <- as.data.frame(
  read_xlsx(archivo_datos)
)

variables_independientes_base <- read_xlsx(
  archivo_variables
)

variables_independientes_completas <- setdiff(
  names(variables_independientes_base),
  "ent"
)

variables_independientes_completas <- intersect(
  variables_independientes_completas,
  names(datos)
)

variables_dependientes <- c(
  "tallaedad",
  "pesotalla",
  "iemc"
)

variables_univariadas <- c(
  "edad",
  "sexo",
  "inc_vi",
  "merc",
  "cab",
  "sum",
  "rururb",
  "inas_esc",
  "agent_san",
  "ic_asalud",
  "ic_segsoc",
  "ic_cv",
  "ic_sbv",
  "isb_agua",
  "isb_dren",
  "isb_luz",
  "isb_combus",
  "ins_ali",
  "ic_ali",
  "lca",
  "dch",
  "plp_e",
  "plp",
  "pobreza",
  "pobreza_e",
  "pobreza_m",
  "vul_car",
  "vul_ing",
  "hli",
  "discap",
  "mcv_ips",
  "mcv_ips1",
  "mcv_ips2",
  "mcv_ips3",
  "mins_tot",
  "mins04",
  "ifp",
  "ifs",
  "ifem",
  "seg_aguapot",
  "sane",
  "gini",
  "esc_mat"
)

variables_univariadas <- intersect(
  variables_univariadas,
  names(datos)
)

regresiones_univariadas <- ajustar_regresiones_univariadas(
  datos,
  variables_dependientes,
  variables_univariadas
)

limite_superior <- min(
  146,
  ncol(datos)
)

variables_tallaedad <- names(datos)[
  seq.int(
    10,
    limite_superior
  )
]

variables_tallaedad <- setdiff(
  variables_tallaedad,
  "tallaedad"
)

regresiones_tallaedad <- ajustar_regresiones_univariadas(
  datos,
  "tallaedad",
  variables_tallaedad
)

write_xlsx(
  list(
    regresiones_generales = regresiones_univariadas,
    regresiones_tallaedad = regresiones_tallaedad
  ),
  file.path(
    directorio_salida,
    "regresiones_univariadas.xlsx"
  )
)

variables_forward <- c(
  "hli",
  "discap",
  "mcv_ips",
  "mcv_ips1",
  "mcv_ips2",
  "mcv_ips3",
  "tinfor",
  "merc",
  "sum",
  "cab",
  "ic_rezedu",
  "inas_esc",
  "ic_asalud",
  "ic_segsoc",
  "ic_cv",
  "ic_sbv",
  "tot_iamen",
  "ins_ali"
)

variables_forward <- intersect(
  variables_forward,
  names(datos)
)

resultados_forward <- lapply(
  c(
    pesotalla = "pesotalla",
    pesoedad = "pesoedad",
    iemc = "iemc",
    tallaedad = "tallaedad"
  ),
  function(variable_dependiente) {
    ajustar_forward(
      datos,
      variable_dependiente,
      variables_forward
    )
  }
)

coeficientes_forward <- bind_rows(
  lapply(
    resultados_forward,
    function(resultado) resultado$coeficientes
  )
)

write_xlsx(
  coeficientes_forward,
  file.path(
    directorio_salida,
    "modelos_forward.xlsx"
  )
)

modelos_pca <- lapply(
  c(
    PT = "pesotalla",
    PE = "pesoedad",
    IEMC = "iemc",
    TE = "tallaedad"
  ),
  function(variable_dependiente) {
    preparar_pca(
      datos,
      variable_dependiente,
      variables_independientes_completas
    )
  }
)

pesos_pca <- lapply(
  modelos_pca,
  extraer_pesos_pca,
  numero_componentes = numero_componentes
)

names(pesos_pca) <- paste0(
  "Weights_",
  names(pesos_pca)
)

write_xlsx(
  pesos_pca,
  file.path(
    directorio_salida,
    "pca_pesos.xlsx"
  )
)

resumenes_pca <- lapply(
  modelos_pca,
  crear_resumen_pca,
  numero_componentes = numero_componentes,
  numero_variables = numero_variables_componente
)

names(resumenes_pca) <- paste0(
  "Summary_",
  names(resumenes_pca)
)

write_xlsx(
  resumenes_pca,
  file.path(
    directorio_salida,
    "resumenes_pca.xlsx"
  )
)

invisible(
  Map(
    guardar_grafica_varianza,
    modelos_pca,
    names(modelos_pca)
  )
)

variables_seleccionadas <- list(
  desnutricion = variables_independientes_base %>%
    select(
      ent,
      dsm1,
      ds59,
      ic_rezedu,
      i_privacion,
      ic_asalud,
      ic_segsoc
    ),
  obesidad = variables_independientes_base %>%
    select(
      ent,
      pobreza_e,
      reg_esp,
      i_privacion,
      tamhogesc,
      obm1,
      ob14,
      esc_mat
    )
)

write_xlsx(
  variables_seleccionadas,
  file.path(
    directorio_salida,
    "variables_seleccionadas.xlsx"
  )
)

codigos <- read_excel(
  archivo_codigos
)

factores_tallaedad <- read_excel(
  archivo_factores,
  sheet = "tallaedad"
)

factores_descritos <- factores_tallaedad %>%
  left_join(
    codigos,
    by = c(
      "Feature" = "Código"
    )
  ) %>%
  select(
    Feature,
    Gain,
    Descripción
  ) %>%
  filter(
    !is.na(Descripción)
  ) %>%
  arrange(
    desc(Gain)
  ) %>%
  head(10)

write_xlsx(
  factores_descritos,
  file.path(
    directorio_salida,
    "factores_tallaedad_con_descripciones.xlsx"
  )
)

grafica_factores <- ggplot(
  factores_descritos,
  aes(
    x = reorder(
      Descripción,
      Gain
    ),
    y = Gain
  )
) +
  geom_col(
    fill = "skyblue"
  ) +
  geom_text(
    aes(
      label = Descripción
    ),
    color = "black",
    size = 5,
    hjust = -0.1,
    vjust = 0.5
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(
      mult = c(
        0.1,
        0.1
      )
    )
  ) +
  labs(
    title = "Factores importantes para tallaedad",
    x = "Descripción del factor",
    y = "Ganancia de importancia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 24,
      face = "bold"
    ),
    axis.title = element_text(
      size = 18
    ),
    axis.text.x = element_text(
      size = 14
    ),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(
      1,
      1,
      1,
      1,
      unit = "cm"
    )
  )

ggsave(
  filename = file.path(
    directorio_salida,
    "factores_importantes_tallaedad.svg"
  ),
  plot = grafica_factores,
  width = 14,
  height = 10
)
