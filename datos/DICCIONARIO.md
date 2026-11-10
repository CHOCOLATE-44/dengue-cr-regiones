# Diccionario de datos

## `procesados/base_region_anio.csv`: base final del artículo

Es la base exacta con que se ajustó el modelo final y se evaluó 2026: 108 filas (9 regiones rectoras × 2015–2026), una por región y año. La arma `scripts/55_base_final.R` con las mismas funciones que los scripts del modelo (`preparar_datos()` de `12_cv_loyo.R` y `datos_bio()` de `25_parte_c_predictores.R`). Al reajustar el modelo S con las 99 filas de 2015–2025 se reproducen los coeficientes de `modelo/resultados/s13_explicacion_coeficientes.csv` con una diferencia máxima de 2e-11 (`modelo/resultados/s55_verificacion_base.csv`). Los valores de 2026 coinciden con los de `s14_2026_extrapolacion.csv` y `s14_2026_evaluacion.csv`.

| Columna | Definición | Unidad | Fuente | Script que la genera |
|---|---|---|---|---|
| `region` | Región rectora del Ministerio de Salud (nombre corto: `C.Este` = Central Este, `H. Caribe` = Huetar Caribe, `H.Norte` = Huetar Norte) | — | Ministerio de Salud | `06_construir_bloques.py` |
| `anio` | Año epidemiológico | año | — | `06_construir_bloques.py` |
| `casos_escalados` | **Respuesta.** Casos notificados de dengue acumulados de la SE 1 a la 40, escalados al total nacional de la OPS: proporción regional a la SE 40 × acumulado nacional de la OPS a la SE 40 (suma de los bloques SE 1–18 y 19–40). En 2026, Boletín N° 39 con corte en la SE 40 | casos | Excel ASIS (2015–2020) y boletines epidemiológicos (2021–2026) del Ministerio de Salud; serie semanal revisada de la OPS (portal ARBO) | `06_construir_bloques.py` → `bloques_region_anio.csv` |
| `casos_crudos` | Casos regionales SE 1–40 tal como los publica la fuente, sin escalar | casos | igual que la anterior | `06_construir_bloques.py` |
| `poblacion` | Población de la región: mediana de la población implícita en los boletines del año (casos / tasa × 100 000). Occidente 2018 se interpola entre 2017 y 2019. Se usa como offset, log(población) | personas | boletines del Ministerio de Salud y Excel ASIS | `06_construir_bloques.py`; interpolación en `preparar_datos()` (`12_cv_loyo.R`) |
| `arrastre` | log(1 + tasa por 100 000) de la SE 41 al cierre del año anterior; la proporción regional de ese tramo se aplica al total de la OPS del tramo, y los tramos negativos se llevan a 0 | log(1 + casos por 100 000) | igual que la respuesta | `06c_construir_predictores.py` → `predictores_region_anio.csv` |
| `aptitud_termica_pob` | R0 relativo de la transmisión por *Aedes aegypti* (curva de Mordecai et al. 2017) aplicado a la temperatura de cada celda de 0,1° y cada mes de enero a agosto, promediado y ponderado por población | 0 a 1 | ERA5-Land mensual (Copernicus), Mordecai et al. 2017, WorldPop 2020 | `19_curva_r0.R`, `21_era5land_cds.R`, `24_predictores_bio.R` → `predictores_bio.csv` |
| `sequia_pob` | −SPEI-4 de abril (lluvia − evapotranspiración de Thornthwaite, referencia 1991–2020), ponderado por población; valores altos = más seco | adimensional | CHIRPS v3, ERA5-Land, WorldPop 2020 | `22_chirps_iri.R`, `24_predictores_bio.R` |
| `roni_previo` | RONI (índice relativo de El Niño, ERSSTv6) promedio de las estaciones JAS a NDJ del año anterior; igual para todas las regiones. En los scripts se llama `oni` | °C (anomalía) | NOAA CPC | `06c_construir_predictores.py` |
| `reemergencia` | 1 si en el año anterior se detectó un serotipo ausente los 5 años previos (sin contar casos importados); igual para todas las regiones. Definición post hoc | 0/1 | OPS, INCIENSA y Ministerio de Salud (serotipos) | `17_reemergencia.R`, `24_predictores_bio.R` |
| `inmunidad3_v2` | log(1 + suma de las tasas anuales por 100 000 de la región en los años a−3, a−2 y a−1), con una sola fuente de población. **No existe para 2026** (NA): no se usa en la evaluación de 2026 | log(1 + casos por 100 000) | `casos_anuales_region_2012-2024.csv` | `35_inmunidad3_v2.R` → `v2_inmunidad3.csv` |
| `conjunto` | `ajuste` (2015–2025, 99 filas) o `evaluacion_2026` (9 filas) | — | — | `55_base_final.R` |
| `*_z` | Versión estandarizada de cada predictor continuo: (x − media) / DE, con la media y la DE de las 99 filas de 2015–2025 (también para 2026) | DE | — | `55_base_final.R` (igual que `41` y `44`) |

El modelo final S usa `casos_escalados`, `region`, `poblacion`, `arrastre_z` y `aptitud_termica_pob_z`, más un efecto aleatorio por año. `sequia_pob`, `roni_previo`, `inmunidad3_v2` y `reemergencia` son los candidatos que no entraron en la selección (sección 13); `roni_previo` está además en el modelo de sensibilidad M1.

## Archivos intermedios y de consulta

| Archivo | Qué es | Script | ¿Lo usa el modelo final? |
|---|---|---|---|
| `intermedios/D.pkl` | Tabla de pandas con todas las lecturas de los boletines (3 320 filas, 26 columnas). Es la misma tabla que `procesados/boletines_regiones_2021-2026.csv`, más la columna `pdf` | `05_construir_base.py` | **Sí, de forma indirecta.** La leen `06_construir_bloques.py` (de donde sale la respuesta), `06c_construir_predictores.py` (arrastre y RONI previo), `07_generar_excel.py` y `08_verificar.py`. Por eso se deja en su lugar. Es un pickle: solo debe abrirse si se confía en el origen; el CSV tiene el mismo contenido |
| `intermedios/panel_modelo.csv` | Panel región-año del modelo de la primera etapa (casos, población, inmunidad, arrastre y RONI previo) | `06c_construir_predictores.py` | No. Solo lo lee `10_generar_final.py` |
| `procesados/final.xlsx` | Excel con la respuesta y los predictores de la primera etapa (arrastre + RONI previo), con notas por celda | `10_generar_final.py` | No. Es de consulta y quedó superado por `base_region_anio.csv` |
| `procesados/dengue_CR_2015-2026_con_boletines.xlsx` | Copia del Excel original con hojas agregadas para los boletines 2021–2026 | `07_generar_excel.py` (verificado por `08_verificar.py`) | No. Es de consulta; los números del modelo salen de los CSV |
