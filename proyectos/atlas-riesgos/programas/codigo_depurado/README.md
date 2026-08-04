# Versión depurada y reproducible

Este directorio contiene una versión reorganizada del flujo de trabajo utilizado para elaborar el **Atlas de Riesgos para la Nutrición de la Niñez en México**.

La depuración busca reducir repeticiones, ordenar la secuencia de ejecución y facilitar la lectura y auditoría del código, sin modificar deliberadamente las decisiones metodológicas ni los resultados del producto publicado.

## Componentes

### Prevalencias observadas

Cálculo y representación territorial de las prevalencias estatales de baja talla y sobrepeso y obesidad, tanto para el conjunto de niñas y niños de 0 a 9 años como para grupos de edad específicos.

[**Consulta el código de prevalencias →**](prevalencias/)

### Modelos predictivos

Comparación, validación y selección de los modelos utilizados para identificar patrones relacionados con los indicadores de malnutrición infantil.

[**Consulta el código de modelización →**](modelos/)

### Riesgos y mapas

Estimación de indicadores de riesgo y generación de mapas estatales para los distintos resultados antropométricos considerados en el Atlas.

[**Consulta el código de riesgos y mapas →**](riesgos_y_mapas/)

### Análisis complementario

Regresiones exploratorias, análisis de componentes principales, selección de variables y elaboración de tablas y gráficas auxiliares.

[**Consulta los análisis complementarios →**](analisis_complementario/)

## Criterios de depuración

La reorganización del código contempla:

- integración de scripts que realizan operaciones equivalentes;
- eliminación de repeticiones y bloques residuales;
- sustitución de selecciones de columnas por posición cuando sea posible;
- concentración de parámetros y archivos de entrada;
- separación clara entre insumos, procesamiento y productos;
- conservación de la lógica sustantiva utilizada en el estudio publicado.
