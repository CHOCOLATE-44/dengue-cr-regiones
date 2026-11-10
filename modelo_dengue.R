# Corre la selección de modelos de dengue (ajuste 2015–2025; 2026 queda reservado).
# Uso, desde la raíz del proyecto:  Rscript modelo_dengue.R
# Deja el registro en modelo/logs/modelo_dengue.log.
#
# QUÉ REPRODUCE EL MODELO FINAL (detalle en scripts/README.md):
#   - Datos: Python 01–06c, que se corren aparte y antes (respuesta, arrastre y RONI previo), y R 14–17, 19, 21–24, 29, 30, 34
#     y 35 (grilla de población, aptitud térmica, sequía, reemergencia, inmunidad3_v2 e indicadoras regionales).
#   - Funciones que usa el modelo final: 12 (preparar_datos, ajustar), 25 (datos_bio), 36 (formula_v2, paso) y
#     39 (comparar); 36 y 33 también dejan las predicciones v2 que leen 42 y 43.
#   - Modelo final: 41 y 42 (selección de variables), 43 y 44 (REML y evaluación de 2026); 46 (descriptivos,
#     figura 1, θ y DHARMa).
#   - Base final (se corre aparte): 55_base_final.R.
#   El resto (11, 13, 18, 20, 26–28, 31, 32, 37, 38, 49 y 50) es de etapas exploratorias; sus salidas no están en el
#   repositorio, salvo semanal/resultados/ (49 y 50).

dir.create("modelo/logs", showWarnings = FALSE, recursive = TRUE)
log_con <- file("modelo/logs/modelo_dengue.log", open = "wt")
sink(log_con, split = TRUE); sink(log_con, type = "message")
cat("Inicio:", format(Sys.time()), "\n"); print(sessionInfo())

source("scripts/11_predictores_nuevos.R", echo = TRUE, max.deparse.length = Inf)  # PASOS 1–2
source("scripts/12_cv_loyo.R",            echo = TRUE, max.deparse.length = Inf)  # PASO 4
source("scripts/13_comparar_modelos.R",   echo = TRUE, max.deparse.length = Inf)  # PASO 5

# Predictores biológicos: PARTE A (infraestructura espacial)
source("scripts/14_descargar_espacial.R", echo = TRUE, max.deparse.length = Inf)
source("scripts/15_mapeo_regiones.R",     echo = TRUE, max.deparse.length = Inf)

# PARTE B: predictores
source("scripts/16_descargar_mordecai.R", echo = TRUE, max.deparse.length = Inf)  # artículo, suplementos, MCMC
source("scripts/23_serotipos_ops_2009_2013.R", echo = TRUE, max.deparse.length = Inf)  # B4, serotipos OPS 2009–2012
source("scripts/34_serotipos_importado.R",     echo = TRUE, max.deparse.length = Inf)  # columna importado (6b)
source("scripts/17_reemergencia.R",       echo = TRUE, max.deparse.length = Inf)  # B4
source("scripts/19_curva_r0.R",           echo = TRUE, max.deparse.length = Inf)  # B2, curva R0(T)
source("scripts/21_era5land_cds.R",       echo = TRUE, max.deparse.length = Inf)  # B1, ERA5-Land (CDS)
source("scripts/22_chirps_iri.R",         echo = TRUE, max.deparse.length = Inf)  # B3, lluvia (espejo IRI)
source("scripts/24_predictores_bio.R",     echo = TRUE, max.deparse.length = Inf)  # B1–B4 por región -> predictores_bio.csv

# PARTES C, D y E
source("scripts/25_parte_c_predictores.R", echo = TRUE, max.deparse.length = Inf)  # C1–C4
source("scripts/26_parte_d_e_modelos.R",    echo = TRUE, max.deparse.length = Inf)  # tabla D y diagnósticos E1–E6

# Etapa 10: pronóstico desde la SE 18 (lista cerrada, evaluada una vez)
source("scripts/27_pronostico_se18.R",      echo = TRUE, max.deparse.length = Inf)  # P00, P0, P1, P1min, TechoP
source("scripts/28_literatura_pronostico.R", echo = TRUE, max.deparse.length = Inf)  # Cuong 2013, Lauer 2018 (PMC)

# Etapa 11: hipótesis regionales H1–H4 (lista cerrada)
source("scripts/29_descargar_asis_2012_2013.R", echo = TRUE, max.deparse.length = Inf)  # ASIS 2012 y 2013
source("scripts/30_predictores_regionales.R",   echo = TRUE, max.deparse.length = Inf)  # inmunidad3, vecindad, vecinos
source("scripts/31_hipotesis_regionales.R",     echo = TRUE, max.deparse.length = Inf)  # tabla H
source("scripts/32_literatura_hipotesis.R",     echo = TRUE, max.deparse.length = Inf)  # fuentes citadas

# Correcciones tras revisión externa (resultados con sufijo v2; los anteriores no se borran)
source("scripts/33_seco_tiempo_real.R",         echo = TRUE, max.deparse.length = Inf)  # 1b depuración, 1c tasa_seco_rt
source("scripts/35_inmunidad3_v2.R",            echo = TRUE, max.deparse.length = Inf)  # 6a y 6c
source("scripts/36_validacion_v2.R",            echo = TRUE, max.deparse.length = Inf)  # 1c y 2: LOYO y hacia adelante (20 000 sorteos comunes)
source("scripts/39_comparaciones_pareadas.R",   echo = TRUE, max.deparse.length = Inf)  # 3: pareadas con IC por bootstrap de años
source("scripts/37_a_h3_diagnostico.R",         echo = TRUE, max.deparse.length = Inf)  # 4: VIF, LRT y efecto neto de A_H3
source("scripts/38_literatura_correcciones.R",  echo = TRUE, max.deparse.length = Inf)  # 7: metadatos de citas

# Sección 13: selección de variables para explicación (Roback y Legler) y para predicción (ISLR)
source("scripts/40_literatura_seleccion.R",     echo = TRUE, max.deparse.length = Inf)  # libro y ISLR (caps. 1, 4, 8–11; 2, 5, 6)
source("scripts/41_seleccion_explicacion.R",    echo = TRUE, max.deparse.length = Inf)  # A: pasos a–i, LRT y bootstrap paramétrico
source("scripts/42_seleccion_prediccion.R",     echo = TRUE, max.deparse.length = Inf)  # B: forward + LOYO interno + 1 EE, hacia adelante

# Sección 14: calibración (ML vs REML) y evaluación de 2026
source("scripts/43_calibracion_reml.R",         echo = TRUE, max.deparse.length = Inf)  # 1b y 1c; define version_2026
source("scripts/44_evaluacion_2026.R",          echo = TRUE, max.deparse.length = Inf)  # 2a–2f y figuras/s14_prediccion_2026.png

# Sección 15: descriptivos y diagnóstico del modelo final
source("scripts/46_articulo_insumos.R",         echo = TRUE, max.deparse.length = Inf)  # descriptivos, figura 1, θ, DHARMa, cifras del texto

# Sección 16: modelo por períodos de 4 semanas (exploratorio)
source("scripts/49_semanal_series.R",           echo = TRUE, max.deparse.length = Inf)  # fase A: series, calidad y criterio de parada
source("scripts/50_semanal_referencias.R",      echo = TRUE, max.deparse.length = Inf)  # verificación de Finch et al. 2025


cat("Fin:", format(Sys.time()), "\n")
sink(type = "message"); sink(); close(log_con)
