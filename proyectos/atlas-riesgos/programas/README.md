# Programas de cálculo

Este directorio reúne los programas utilizados para construir los modelos y productos cuantitativos del **Atlas de Riesgos para la Nutrición de la Niñez en México**.

El código se publica con fines de **transparencia, trazabilidad y revisión metodológica**. Los scripts originales permitieron integrar información procedente de distintas fuentes oficiales, estimar modelos predictivos mediante **XGBoost**, identificar factores relevantes y producir mapas de riesgo por entidad federativa y grupo de edad.

## Nota metodológica

La principal limitación del ejercicio proviene de la heterogeneidad de las fuentes utilizadas. Algunas variables corresponden a observaciones individuales, mientras que otras son promedios, tasas o indicadores agregados por entidad federativa. Por tanto, las bases originales no comparten una misma unidad de análisis.

La integración permitió realizar un ejercicio exploratorio de identificación de patrones y tendencias territoriales, pero no construir una base plenamente armonizada a nivel individual, longitudinal o multinivel. En consecuencia, los resultados deben interpretarse como **señales globales de riesgo**, no como estimaciones causales ni como probabilidades individuales calibradas.

Esta limitación no puede resolverse únicamente mediante modificaciones al código. Para superarla se requiere información de mayor calidad, proveniente de una misma fuente o construida con unidades de observación compatibles.

## Organización del código

El repositorio se organizará en tres componentes:

### 1. Código original

Scripts utilizados para producir los resultados publicados en el Atlas. Se conservan sin modificar su lógica sustantiva, con el propósito de mantener la trazabilidad del estudio.

[**Consulta los scripts originales →**](codigo_original/)

### 2. Versión depurada y reproducible

Versión reorganizada y documentada del flujo de trabajo original. Su propósito será facilitar la lectura, ejecución y auditoría del código sin alterar deliberadamente las decisiones metodológicas del estudio publicado.

### 3. Auditoría y análisis de sensibilidad

Ejercicios destinados a examinar la estabilidad de los resultados ante cambios en la imputación, la selección de variables, la partición de las muestras y otras decisiones de modelización.

## Alcance

La publicación de estos materiales no constituye una nueva estimación del Atlas. Las modificaciones posteriores al código serán identificadas claramente y se mantendrán separadas de los programas originales.

La apertura del código se refiere exclusivamente a los materiales de cálculo aquí compartidos y no modifica los derechos de reproducción y representación del producto institucional publicado.

## Derechos y atribución

El código fuente se comparte como información abierta por un compromiso con la transparencia, la trazabilidad y la ética científica. Su publicación busca facilitar la revisión, el aprendizaje y la auditoría metodológica del trabajo realizado.

La disponibilidad del código no implica la cesión de los derechos asociados al producto publicado. Todos los derechos de reproducción, difusión y representación del **Atlas de Riesgos para la Nutrición de la Niñez en México** y de sus materiales institucionales permanecen atribuidos a **CEIDON†** y **Save the Children México**.

La reutilización o modificación del código deberá reconocer su procedencia y no podrá presentarse como una versión oficial del Atlas ni como un producto elaborado o avalado por dichas organizaciones.
