# PISA 2025 · Programas de cálculo

Este directorio contiene los programas de cálculo utilizados por el Laboratorio de Investigación Social Avanzada (LISA) para el análisis de los resultados de PISA 2025 en México.

## Estructura

### Reporte analítico
`02_scripts/01_reporte_analitico/`

- `000 PISA25_Serie historica.R`: serie histórica de resultados.
- `01 PISA25_Preprocesamiento.R`: construcción de los objetos analíticos.
- `02 PISA25_Gráficas.R`: generación de las visualizaciones.

### Estudio
`02_scripts/02_estudio/`

- `01 PISA25_NE_Preprocesamiento.R`: construcción del objeto analítico.
- `02 PISA25_NE_Análisis.R`: estimación de modelos y generación de resultados.
- `03 Gráficas.R`: generación de las visualizaciones.

### Datos procesados
`data/processed/`

Contiene los objetos necesarios para reproducir los cálculos y visualizaciones sin distribuir los archivos originales completos de microdatos.

`PISA2025_analisis.rds` es una versión reducida del objeto analítico que conserva los componentes necesarios para reproducir las estimaciones publicadas.

## Reproducibilidad

Los scripts de preprocesamiento documentan la construcción de los objetos analíticos y requieren los microdatos originales de PISA 2025.

Los scripts de análisis y visualización pueden ejecutarse desde la raíz de este directorio utilizando los archivos incluidos en `data/processed`.

Los generadores de informes, archivos PDF y otros productos editoriales no se incluyen.

## Fuente

OECD · Programme for International Student Assessment (PISA) 2025.

## Autor

Antonio Villalpando Acuña  
Laboratorio de Investigación Social Avanzada (LISA)  
Ciudad de México · 2026
