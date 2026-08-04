# Prevalencias observadas

Este directorio contiene el código utilizado para calcular y representar las prevalencias estatales de baja talla y sobrepeso y obesidad para niñas y niños de 0 a 9 años.

El script integra los cálculos generales y los resultados segmentados en los siguientes grupos de edad:

- 0 a 3 años;
- 4 a 6 años;
- 7 a 9 años.

Los resultados se obtienen a partir de los indicadores antropométricos contenidos en `variables_dependientes.xlsx` y se representan territorialmente mediante el Marco Geoestadístico Estatal.

## Ejecución

Ejecutar:

```r
source("01_prevalencias.R")
