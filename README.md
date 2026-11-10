# Incidencia de dengue por región rectora en Costa Rica: factores asociados y comparación de 2026

Datos, código y figuras del estudio "Incidencia de dengue por región rectora en Costa Rica: factores asociados y comparación de 2026" (Víctor Brizuela, Kianny Rodríguez y Kendall Martínez; Universidad de Costa Rica, Escuela de Estadística).

El modelo final es un modelo lineal generalizado mixto binomial negativo (NB2, `glmmTMB`) para los casos de dengue notificados de la semana epidemiológica (SE) 1 a la 40 por región rectora, con efecto fijo de región, offset log(población) y efecto aleatorio de año, ajustado con 2015–2025. Sus predictores son el **arrastre** (incidencia de la SE 41 al cierre del año anterior) y la **aptitud térmica** (enero–agosto). Con ese modelo (REML) se compara lo observado en 2026 con lo esperado en cada región.

## Estructura

```
modelo_dengue.R         ejecutor de los scripts de R; al inicio dice cuáles reproducen el modelo final
datos/
  DICCIONARIO.md        definición, unidad, fuente y script de cada columna de la base final
  procesados/           base_region_anio.csv (base final del modelo: 108 filas) y las bases de las que sale
  intermedios/          texto de los PDF, tablas detectadas, D.pkl y panel de la primera etapa
  fuentes/              datos originales descargados:
    dengue_semanal_CR_2015-2026.xlsx  serie semanal nacional (OPS) y semanas regionales (ASIS)
    asis/               Excel del Análisis de Situación de Salud, 2012–2020
    pdfs/               boletines epidemiológicos del Ministerio de Salud, 2021–2026 (unos 750 MB)
    links_boletines_dengue_2021-2026.csv  enlace y fecha de cada boletín
    ops_cuadros_dengue/, extras/, inciensa/   cuadros de la OPS, RONI, boletín 2020 e informe de serotipos
    era5land_cds/       temperatura (ERA5-Land, Copernicus CDS); era5land_openmeteo/ no se usa (script 20)
    chirps_v3_iri/      lluvia (CHIRPS v3); chirps_v3_cr/ no se usa (script 18)
    espacial/           límites (GADM 4.1) y población (WorldPop 2020)
    mordecai2017/       parámetros de Mordecai et al. 2017 para la aptitud térmica
  regiones_rectoras.gpkg, mapeo_canton_region.csv, vecindad_regiones.csv, serotipos_anual.csv
modelo/resultados/      tablas del modelo final (s13_*: selección de variables; s14_*: calibración y 2026;
                        s15_*: descriptivos y diagnóstico), de la base final (s55_*) y de la construcción
                        de los predictores
figuras/                figura 1 (tasas por región y año) y figura 2 (2026 frente a lo esperado)
semanal/resultados/     análisis exploratorio por períodos de 4 semanas (scripts 49 y 50)
scripts/                scripts numerados (Python 01–10, R 11–55); scripts/README.md dice qué hace cada uno
```

## Cómo reproducir el modelo final

Desde la raíz del repositorio:

1. **Datos de casos** (Python): `01` a `06c`, en orden. Dejan la respuesta (`bloques_region_anio.csv`), el arrastre y el RONI previo (`predictores_region_anio.csv`). Los PDF y los Excel ya están en `datos/fuentes/`, así que no se vuelven a descargar.
2. **Predictores y modelo** (R): `Rscript modelo_dengue.R`. Corre los scripts de R en orden (no 18 ni 20) y deja el registro en `modelo/logs/`. Los que reproducen el modelo final son:
   - datos: 14–17, 19, 21–24, 29, 30, 34 y 35;
   - funciones del modelo: 12, 25, 36 y 39;
   - modelo final: 41 y 42 (selección de variables), 43 (REML), 44 (evaluación de 2026) y 46 (descriptivos, figura 1 y diagnóstico).
   Los demás son de etapas exploratorias y se conservan como registro.
3. **Base final**: `Rscript scripts/55_base_final.R` arma `datos/procesados/base_region_anio.csv` y comprueba que reproduce los coeficientes del modelo final (diferencia máxima 2e-11).

Para reajustar solo el modelo final basta `base_region_anio.csv`: `glmmTMB(casos_escalados ~ arrastre_z + aptitud_termica_pob_z + region + (1 | anio_f) + offset(log(poblacion)), family = nbinom2)` con las filas `conjunto == "ajuste"` (ML; para 2026 se usa `REML = TRUE`).

**Versiones** (`sessionInfo()`, `modelo/resultados/s15_software.csv`): R 4.6.1; glmmTMB 1.1.15.2, DHARMa 0.5.1, MASS 7.3.66, scoringRules 1.1.3, SPEI 1.8.1, terra 1.9.50, sf 1.1.3, exactextractr 0.10.1, ecmwfr 2.0.3, ggplot2 4.0.3, httr2 1.3.0, readxl 1.5.0.1, xml2 1.6.0 y jsonlite 2.0.0. Python 3.12.3 con pandas 3.1.1, openpyxl 3.1.5, xlrd 2.0.2 y pdfplumber 0.11.10; pdftotext 24.02.0 (poppler-utils).

La descarga de ERA5-Land (script 21) requiere una cuenta del Copernicus Climate Data Store. La clave se guarda con `ecmwfr::wf_set_key()` y no está en ningún archivo.

## Lo que no está en el repositorio

- **Archivos de Mordecai et al. 2017 de más de 100 MB** (`Rsave_files.zip`, 901 MB, y `Aegypti_DENV_model_outputs-informative.Rsave`, 160 MB): superan el límite de GitHub. Se obtienen del material suplementario de Mordecai EA et al., PLoS Negl Trop Dis. 2017;11(4):e0005568, doi:10.1371/journal.pntd.0005568. Sin ellos no corre el script 19, pero su salida (`modelo/resultados/mordecai_curva_r0.rds`) sí está.
- **Literatura en PDF** (`datos/fuentes/literatura/`), por derechos de autor. Los scripts 28, 32, 40 y 50 la descargan.
- **Salidas de las etapas exploratorias** (salvo `semanal/resultados/`): se regeneran con sus scripts.
- **Logs de ejecución** (`modelo/logs/`).

## Licencia

Licencia: [PENDIENTE: decisión de los autores]

Los datos de terceros conservan sus propias condiciones de uso: Ministerio de Salud de Costa Rica (ASIS y boletines epidemiológicos), Organización Panamericana de la Salud (portal ARBO), INCIENSA, Copernicus/ECMWF (ERA5-Land), Climate Hazards Center (CHIRPS v3), WorldPop, GADM, NOAA CPC (RONI) y Mordecai et al. 2017.
