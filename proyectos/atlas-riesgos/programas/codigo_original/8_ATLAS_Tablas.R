# Dr.(C) Antonio Villalpando Acuña
# Proyecto Atlas de riesgos de malnutrición de Save the Children
# Script para graficar factores importantes con descripciones

library(readxl)
library(dplyr)
library(ggplot2)

# Cargar los datos
codigos <- read_excel("Variables_independientes_códigos.xlsx")
factores <- read_excel("factores_importantes_tallaedad.xlsx")

# Realizar el merge entre los factores importantes y el libro de códigos
df_merged <- factores %>%
  left_join(codigos, by = c("Feature" = "Código"))

# Filtrar las columnas necesarias y eliminar filas con NA en Descripción
df_merged <- df_merged %>%
  select(Feature, Gain, Descripción) %>%
  filter(!is.na(Descripción))

# Ordenar los factores por importancia y seleccionar los 10 más importantes
df_merged <- df_merged %>%
  arrange(desc(Gain)) %>%
  head(10)

# Crear la gráfica
ggplot(df_merged, aes(x = reorder(Descripción, Gain), y = Gain)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = Descripción), color = "black", size = 5, hjust = -0.1, vjust = 0.5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  labs(title = "Factores importantes para tallaedad",
       x = "Descripción del factor",
       y = "Ganancia de importancia") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title = element_text(size = 18),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_blank(),  
    panel.grid.major.y = element_blank(),  
    plot.margin = unit(c(1, 1, 1, 1), "cm")  
  )
