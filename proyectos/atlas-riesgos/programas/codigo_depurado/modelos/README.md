# Modelos predictivos

Este directorio contiene la versión depurada del proceso de comparación, validación y selección de modelos predictivos utilizado en el **Atlas de Riesgos para la Nutrición de la Niñez en México**.

El código integra en un solo flujo las operaciones que originalmente se encontraban distribuidas entre varios scripts, eliminando repeticiones y bloques residuales, pero conservando la lógica metodológica del estudio publicado.

## Script principal

`02_modelos.R`

## Contenido

El script comprende:

- integración de las variables dependientes e independientes;
- preparación e imputación de los predictores;
- eliminación de variables altamente correlacionadas;
- comparación entre regresión cuantílica y XGBoost;
- validación cruzada mediante RMSE;
- selección de los modelos con mejor desempeño;
- extracción de los factores con mayor importancia predictiva;
- generación de bases estatales generales y segmentadas por grupo de edad.

El código se publica con fines de transparencia, trazabilidad y auditoría metodológica.
