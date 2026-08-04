# Riesgos y mapas

Este directorio contiene la versión depurada de los procedimientos utilizados para estimar los indicadores de riesgo y producir su representación cartográfica en el **Atlas de Riesgos para la Nutrición de la Niñez en México**.

El código integra en un solo flujo las operaciones que originalmente se encontraban distribuidas entre distintos scripts para peso para la edad, talla para la edad y peso para la talla.

## Script principal

`03_riesgos_y_mapas.R`

## Contenido

El script comprende:

- carga de las bases estatales generadas por los modelos predictivos;
- estimación de los indicadores de riesgo;
- identificación de los factores con mayor importancia predictiva;
- generación de archivos estatales de resultados;
- integración de los resultados con el Marco Geoestadístico Estatal;
- elaboración de mapas de riesgo para los distintos indicadores antropométricos;
- generación de mapas auxiliares de factores asociados.
