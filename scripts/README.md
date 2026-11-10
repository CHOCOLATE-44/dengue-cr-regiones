# Scripts

Se corren desde la raíz del repositorio. La numeración es la del registro de trabajo del proyecto y no se cambió: se conservan los scripts de etapas exploratorias. Faltan los números 09, 45, 47, 48 y 51–54, que no son de datos ni de modelo.

**Flujo:** **modelo final** = produce un dato, una función o un resultado del modelo final; **dependencia** = el script en sí es de una etapa superada, pero el flujo del modelo final usa una de sus salidas o funciones; **exploratorio/superado** = etapa exploratoria; sus salidas no están en el repositorio (salvo `semanal/resultados/`) y el modelo final no las usa; **no se usa** = reemplazado y conservado como registro.

| Script | Qué hace | Lee | Escribe | Flujo |
|---|---|---|---|---|
| `01_descargar.py` | Descarga los boletines epidemiológicos (PDF) y los Excel ASIS | `datos/fuentes/links_boletines_dengue_2021-2026.csv` | `datos/fuentes/pdfs/`, `datos/fuentes/asis/`, `datos/intermedios/manifest.json` | modelo final |
| `02_extraer_texto.py` | Extrae el texto de los PDF (`pdftotext`) | `datos/fuentes/pdfs/`, `manifest.json` | `datos/intermedios/txt/` | modelo final |
| `03_detectar_tablas.py` | Detecta las tablas de dengue por región en el texto | `datos/intermedios/txt/`, `manifest.json` | `datos/intermedios/blocks.json` | modelo final |
| `04_transcripcion_visual.py` | Transcripción visual de las tablas que el texto no resuelve | — | `datos/intermedios/visual.json` | modelo final |
| `05_construir_base.py` | Base de lecturas de los boletines, con chequeos de corte, población y diferencias con la OPS | `blocks.json` (por `comun.py`), `visual.json`, `datos/intermedios/txt/`, `datos/fuentes/dengue_semanal_CR_2015-2026.xlsx` | `datos/intermedios/D.pkl`, `datos/procesados/boletines_regiones_2021-2026.csv` | modelo final |
| `06_construir_bloques.py` | Casos escalados por región, año y bloque (SE 1–18 y 19–40) y población: **la respuesta** | `D.pkl`, Excel ASIS | `datos/procesados/bloques_region_anio.csv` | modelo final |
| `06b_descargar_predictores.py` | Descarga el RONI (NOAA CPC), el Excel ASIS 2014 y el boletín N° 1 de 2020 | — | `datos/fuentes/extras/`, `datos/fuentes/asis/` | modelo final |
| `06c_construir_predictores.py` | Arrastre (SE 41–cierre del año anterior), RONI previo e inmunidad | `D.pkl`, `bloques_region_anio.csv`, ASIS 2014, `RONI.ascii.txt` | `datos/procesados/predictores_region_anio.csv`, `datos/intermedios/panel_modelo.csv` | modelo final |
| `07_generar_excel.py` | Excel original con hojas de boletines agregadas | `D.pkl`, CSV de `datos/procesados/` | `datos/procesados/dengue_CR_2015-2026_con_boletines.xlsx`, `log_boletines.csv` | exploratorio/superado (consulta) |
| `08_verificar.py` | Compara ese Excel con el original y recoteja cifras con `pdfplumber` | `D.pkl`, los dos Excel, PDF | — (informe en pantalla) | exploratorio/superado (consulta) |
| `10_generar_final.py` | `final.xlsx` de la primera etapa (arrastre + RONI) | `panel_modelo.csv` | `datos/procesados/final.xlsx` | exploratorio/superado |
| `11_predictores_nuevos.R` | `prop_reemergente` y `roni_mismo` (PASO 1 y 2) | `datos/serotipos_anual.csv` | `prop_reemergente_anual.csv`, `correlacion_predictores.csv` | exploratorio/superado |
| `12_cv_loyo.R` | Funciones de datos y validación: `preparar_datos()`, `ajustar()`, `simular()`, métricas LOYO | `bloques_region_anio.csv`, `predictores_region_anio.csv` | — | modelo final (funciones) |
| `13_comparar_modelos.R` | Compara M0, M1, M3 y M4 de la primera etapa | — (usa 12) | `tabla1_*`, `tabla2_*`, `coeficientes_modelos`, `cv_loyo_predicciones` | exploratorio/superado |
| `14_descargar_espacial.R` | Descarga GADM 4.1 y WorldPop 2020 | — | `datos/fuentes/espacial/` | modelo final |
| `15_mapeo_regiones.R` | Mapeo cantón → región, polígonos de las regiones y grilla de población de 0,1° | `datos/fuentes/espacial/`, `bloques_region_anio.csv` | `datos/mapeo_canton_region.csv`, `datos/regiones_rectoras.gpkg`, `poblacion_grilla_0p1.tif`, `validacion_poblacion_regiones.csv`, `gadm_cantones_nuevos.csv` | modelo final |
| `16_descargar_mordecai.R` | Descarga Mordecai et al. 2017 y su material suplementario | — | `datos/fuentes/mordecai2017/` | modelo final |
| `17_reemergencia.R` | Reemergencia por año (serotipo ausente 5 años o más) | `datos/serotipos_anual.csv` | `reemergencia_anual.csv` | modelo final (candidato de la selección) |
| `18_chirps.R` | CHIRPS v3 desde el servidor de CHC (sin respuesta) | grilla de población | `chirps_v3_0p1_mensual.tif` (reemplazado por 22) | no se usa |
| `19_curva_r0.R` | Curva R0(T) relativa de *Ae. aegypti* (Mordecai et al. 2017) | `datos/fuentes/mordecai2017/` | `mordecai_curva_r0.rds`, `mordecai_parametros_aegypti.csv`, `validacion_curva_r0.csv`, `figuras/fig_validacion_r0.png` | modelo final |
| `20_era5_openmeteo.R` | Temperatura ERA5-Land por Open-Meteo | grilla de población | `plan_descarga_era5.csv` (reemplazado por 21) | no se usa |
| `21_era5land_cds.R` | Temperatura media mensual ERA5-Land desde el CDS (la clave se lee del llavero con `ecmwfr::wf_get_key()`) | grilla de población | `datos/procesados/era5land_t2m_0p1_mensual.tif` | modelo final |
| `22_chirps_iri.R` | Lluvia CHIRPS v3 mensual desde la IRI | grilla de población | `datos/procesados/chirps_v3_0p1_mensual.tif`, `chirps_iri_vs_chc_1991.csv` | modelo final |
| `23_serotipos_ops_2009_2013.R` | Serotipos circulantes 2009–2013 (cuadros de la OPS) | `datos/serotipos_anual.csv` | `datos/serotipos_anual.csv` | modelo final |
| `24_predictores_bio.R` | Aptitud térmica, temperatura, sequía (−SPEI-4) y reemergencia por región y año | rásteres de `datos/procesados/`, `regiones_rectoras.gpkg`, `mordecai_curva_r0.rds`, `reemergencia_anual.csv` | `datos/procesados/predictores_bio.csv`, `cobertura_clima_regiones.csv` | modelo final |
| `25_parte_c_predictores.R` | Define `datos_bio()`; variación, colinealidad y coherencia de los predictores | `predictores_bio.csv` | `c1_*`, `c2_*`, `c3_*`, `figuras/fig_c4_*` | modelo final (función `datos_bio()`) |
| `26_parte_d_e_modelos.R` | Modelos con predictores biológicos y diagnósticos (sección 9) | — (usa 12 y 25) | `d_*`, `e*_*`, `tabla_d_modelos_bio`, `figuras/fig_e3_*` | exploratorio/superado |
| `27_pronostico_se18.R` | Pronóstico desde la SE 18 (sección 10) | `bloques_region_anio.csv` | `p_*`, `tabla_p_pronostico_se18` | exploratorio/superado |
| `28_literatura_pronostico.R` | Descarga la literatura del pronóstico | — | `datos/fuentes/literatura/` (no está en el repositorio) | exploratorio/superado |
| `29_descargar_asis_2012_2013.R` | Descarga los Excel ASIS 2012 y 2013 | — | `datos/fuentes/asis/` | modelo final (para `inmunidad3`) |
| `30_predictores_regionales.R` | Predictores regionales H1–H4 (`inmunidad3`, vecindad, indicadoras `caribe` y `chorotega`) | `bloques_region_anio.csv`, `boletines_regiones_2021-2026.csv`, `regiones_rectoras.gpkg` | `datos/procesados/predictores_regionales.csv`, `datos/vecindad_regiones.csv`, `h1_chequeo_totales_anuales.csv` | modelo final (lo leen 41 y 42) |
| `31_hipotesis_regionales.R` | Hipótesis regionales H1–H4 (sección 11) | `bloques_region_anio.csv`, `predictores_regionales.csv` | `h_*`, `tabla_h_hipotesis_regionales` | exploratorio/superado |
| `32_literatura_hipotesis.R` | Descarga las fuentes de H1–H4 | — | `datos/fuentes/literatura/` (no está en el repositorio) | exploratorio/superado |
| `33_seco_tiempo_real.R` | Bloque seco en tiempo real (corrección 1b–1c) | `bloques_region_anio.csv`, boletines, Excel original | `datos/procesados/v2_seco_tiempo_real.csv`, `v2_1b_*` | dependencia (lo lee 36) |
| `34_serotipos_importado.R` | Marca el caso importado de DENV-3 de 2022 | `datos/serotipos_anual.csv` | `datos/serotipos_anual.csv` | modelo final |
| `35_inmunidad3_v2.R` | `inmunidad3_v2` con una sola fuente de población | `casos_anuales_region_2012-2024.csv`, Excel original | `datos/procesados/v2_inmunidad3.csv`, `v2_inmunidad3_totales_vs_ops.csv` | modelo final (candidato de la selección) |
| `36_validacion_v2.R` | Validación v2 (LOYO y hacia adelante, sorteos comunes); define `formula_v2()`, `paso()` y otras funciones | `bloques_region_anio.csv`, `predictores_regionales.csv`, `v2_inmunidad3.csv`, `v2_seco_tiempo_real.csv` | `v2_predicciones_loyo_y_adelante.csv`, `v2_tabla_metricas_*` | dependencia (funciones; 42 y 43 leen sus predicciones) |
| `37_a_h3_diagnostico.R` | Diagnóstico de A_H3, que no se adopta | `predictores_regionales.csv` | `v2_a_h3_*` | exploratorio/superado |
| `38_literatura_correcciones.R` | Metadatos de Crossref de las citas corregidas | — | `v2_citas_metadatos.csv` | exploratorio/superado |
| `39_comparaciones_pareadas.R` | Comparaciones pareadas con IC por bootstrap de años; define `comparar()` y `r2_dentro()` | `v2_predicciones_loyo_y_adelante.csv` | `v2_comparaciones_pareadas.csv` | dependencia (funciones que usa 43) |
| `40_literatura_seleccion.R` | Descarga Roback y Legler (2021) e ISLR (2021) | — | `datos/fuentes/literatura/seleccion/` (no está en el repositorio) | modelo final (fuentes de la selección) |
| `41_seleccion_explicacion.R` | Selección de variables para explicación: llega a S = arrastre + aptitud térmica | `predictores_bio.csv` (por `datos_bio()`), `predictores_regionales.csv`, `v2_inmunidad3.csv` | `s13_explicacion_*` | **modelo final** |
| `42_seleccion_prediccion.R` | Selección de variables para predicción (ISLR, paso hacia adelante) | los mismos, `v2_predicciones_loyo_y_adelante.csv` | `s13_prediccion_*` | **modelo final** |
| `43_calibracion_reml.R` | Calibración ML frente a REML; elige REML para 2026 (`version_2026`); RR de la aptitud térmica | `predictores_bio.csv`, `v2_predicciones_loyo_y_adelante.csv` | `s14_calibracion_*`, `s14_aptitud_*` | **modelo final** |
| `44_evaluacion_2026.R` | Evaluación de 2026 con S, M1 y M0 (20 000 sorteos, Holm) | `predictores_bio.csv`, `bloques_region_anio.csv`, boletines, Excel original | `s14_2026_*`, `figuras/s14_prediccion_2026.png` | **modelo final** |
| `46_articulo_insumos.R` | Descriptivos, figura 1, θ y DHARMa del modelo S y números del texto | `modelo/resultados/s13_*`, `s14_*` | `s15_*`, `figuras/s15_tasas_region_anio.png` | **modelo final** |
| `49_semanal_series.R` | Casos por período de 4 semanas y criterio de parada (sección 16, fase A) | boletines, Excel original | `semanal/resultados/s45_calidad.csv`, `s49_*` | exploratorio/superado |
| `50_semanal_referencias.R` | Verifica Finch et al. 2025 | — | `semanal/resultados/s50_referencias_metadatos.csv`, `datos/fuentes/literatura/` (no está en el repositorio) | exploratorio/superado |
| `55_base_final.R` | Base final de 108 filas y verificación de los coeficientes de S | `bloques_region_anio.csv`, `predictores_region_anio.csv`, `predictores_bio.csv`, `v2_inmunidad3.csv`, `s13_explicacion_coeficientes.csv` | `datos/procesados/base_region_anio.csv`, `s55_verificacion_base.csv` | **modelo final** (base) |
| `funciones_r0.R` | Funciones de la curva R0(T) | — | — | modelo final (lo usan 19 y 24) |
| `comun.py` | Funciones comunes de los scripts de Python (selección de las tablas de dengue) | `datos/intermedios/blocks.json` | — | modelo final |

Salvo que se indique otra carpeta, los CSV de las columnas "Lee" y "Escribe" están en `modelo/resultados/`.
