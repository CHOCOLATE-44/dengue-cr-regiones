# Sección 13 B: selección de variables para PREDICCIÓN, según ISLR 2.ª ed. (James et al. 2021):
#   Algoritmo 6.2 (forward stepwise, pp. 229–230), ordenado por deviance (p. 229); tamaño elegido por validación
#   cruzada (6.1.3, p. 236) con la regla de un error estándar (p. 238); toda la selección solo con datos de
#   entrenamiento (6.5.1, p. 272).
# La selección se hace DENTRO de cada paso de la validación hacia adelante: para a = 2019…2025, con 2015…a−1:
#   1. Camino forward: desde M0 (region + offset + (1|anio_f)), en cada paso se agrega el término candidato que deja
#      la menor deviance (−2 logLik) en el entrenamiento. Las interacciones solo pueden entrar si su término principal
#      ya entró. Un predictor sin variación en el entrenamiento no es candidato (reemergencia antes de a = 2024).
#   2. Para cada modelo del camino (M0, M1, …, Mp): LOYO interno dentro de 2015…a−1; error = MSE de log(1 + tasa)
#      por año dejado fuera (media de las 9 regiones; predicción = media de 2000 sorteos de la predictiva). CV = media
#      de los MSE por año; EE = DE de los MSE por año / √K.
#   3. Regla de 1 EE: el modelo más chico cuyo CV es ≤ CV mínimo + EE del mínimo.
#   4. Ese modelo se reajusta con 2015…a−1 y predice a con los MISMOS 20 000 sorteos comunes de 36_validacion_v2.R
#      (misma semilla por año), así que se compara pareado con M0 y M1 hacia adelante (CRPS e IC como en 12.5).
# Candidatos (lista cerrada): arrastre, aptitud_termica_pob (ene–ago), sequia_pob (−SPEI-4 de abril), inmunidad3_v2,
#   oni (RONI previo), reemergencia, oni:caribe, sequia_pob:caribe, sequia_pob:chorotega. Respuesta SE 1–40.
#   Aptitud (ene–ago) y sequía (abril) son del mismo año: este pronóstico solo se podría hacer en setiembre.
# Requiere 12_cv_loyo.R (ajustar, simular, sigma_a, metricas_loyo), 25_parte_c_predictores.R (datos_bio),
# 36_validacion_v2.R (paso, continuas, termino_z, formula_v2) y 39_comparaciones_pareadas.R (comparar, r2_dentro).
# No se predice 2026.
library(glmmTMB)
library(parallel)
library(scoringRules)
library(MASS)   # mvrnorm, usado por simular()

B <- 20000; B_INTERNO <- 2000; REPS <- 2000
indicadoras <- c("caribe", "chorotega")
NUCLEOS <- max(1, detectCores() - 2)

reg <- read.csv("datos/procesados/predictores_regionales.csv", fileEncoding = "UTF-8")
inm <- read.csv("datos/procesados/v2_inmunidad3.csv", fileEncoding = "UTF-8")[, c("anio", "region", "inmunidad3_v2")]
anual <- merge(merge(datos_bio(), reg[, c("anio", "region", "caribe", "chorotega")], by = c("anio", "region")), inm,
               by = c("anio", "region"))
anual$region <- factor(anual$region); anual$anio_f <- factor(anual$anio); anual$casos <- anual$casos_escalados
anual <- anual[order(anual$anio, anual$region), ]
stopifnot(nrow(anual) == 99, !anyNA(anual))

candidatos <- c("arrastre", "aptitud_termica_pob", "sequia_pob", "inmunidad3_v2", "oni", "reemergencia",
                "oni:caribe", "sequia_pob:caribe", "sequia_pob:chorotega")
principal <- c("oni:caribe" = "oni", "sequia_pob:caribe" = "sequia_pob", "sequia_pob:chorotega" = "sequia_pob")

# Estandariza con el entrenamiento y quita términos cuyo predictor no varía en él
preparar <- function(ent, pru, t) {
  quitados <- character()
  for (v in continuas(t)) {
    m <- mean(ent[[v]]); s <- sd(ent[[v]])
    if (s == 0) { quitados <- c(quitados, v); next }
    ent[[paste0(v, "_z")]] <- (ent[[v]] - m) / s; if (!is.null(pru)) pru[[paste0(v, "_z")]] <- (pru[[v]] - m) / s
  }
  t <- t[!sapply(strsplit(t, ":"), function(p) any(p %in% quitados))]
  ent <- droplevels(ent); ent$region <- factor(ent$region, levels = levels(anual$region))
  list(ent = ent, pru = pru, t = t, quitados = quitados)
}

seleccionar <- function(a) {
  T_anios <- 2015:(a - 1); ent0 <- subset(anual, anio %in% T_anios)
  # 1. Camino forward ordenado por deviance
  disponibles <- candidatos[sapply(candidatos, function(t) all(sapply(continuas(t), function(v) sd(ent0[[v]]) > 0)))]
  actual <- character(); camino <- list(character()); dev_camino <- NA; reg_cam <- list()
  repeat {
    eleg <- setdiff(disponibles, actual)
    eleg <- eleg[is.na(principal[eleg]) | principal[eleg] %in% actual]
    if (!length(eleg)) break
    devs <- sapply(eleg, function(t) { z <- preparar(ent0, NULL, c(actual, t)); m <- ajustar(formula_v2(z$t), z$ent)
                                       if (is.na(logLik(m))) Inf else -2 * as.numeric(logLik(m)) })
    reg_cam[[length(reg_cam) + 1]] <- data.frame(anio_prueba = a, k = length(actual) + 1, candidato = eleg, deviance = devs)
    mejor <- eleg[which.min(devs)]; actual <- c(actual, mejor)
    camino[[length(camino) + 1]] <- actual; dev_camino <- c(dev_camino, min(devs))
  }
  # 2. LOYO interno para cada modelo del camino (MSE de log(1 + tasa))
  cv <- do.call(rbind, lapply(seq_along(camino), function(j) {
    t <- camino[[j]]
    mse <- unlist(mclapply(T_anios, function(ti) {
      z <- preparar(subset(ent0, anio != ti), subset(ent0, anio == ti), t)
      m <- ajustar(formula_v2(z$t), z$ent)
      set.seed(a * 1000 + ti)
      sl <- log1p(1e5 * simular(m, z$pru, B_INTERNO) / z$pru$poblacion)
      mean((z$pru$y_log - rowMeans(sl))^2)
    }, mc.cores = NUCLEOS))
    stopifnot(is.numeric(mse), length(mse) == length(T_anios), !anyNA(mse))
    data.frame(anio_prueba = a, k = j - 1, terminos = paste(c("M0", t), collapse = " + "),
               deviance_entrenamiento = dev_camino[j], cv_mse = mean(mse), ee = sd(mse) / sqrt(length(mse)), anios_internos = length(mse))
  }))
  # 3. Regla de 1 EE
  kmin <- which.min(cv$cv_mse); umbral <- cv$cv_mse[kmin] + cv$ee[kmin]
  kel <- min(which(cv$cv_mse <= umbral))
  cv$minimo <- seq_len(nrow(cv)) == kmin; cv$elegido_1ee <- seq_len(nrow(cv)) == kel
  list(cv = cv, camino = do.call(rbind, reg_cam), elegido = camino[[kel]])
}

sel <- lapply(2019:2025, function(a) { cat("Selección, año de prueba", a, "\n"); seleccionar(a) })
cv_tab <- do.call(rbind, lapply(sel, `[[`, "cv"))
cam_tab <- do.call(rbind, lapply(sel, `[[`, "camino"))
eleg <- data.frame(anio_prueba = 2019:2025, entrenamiento = paste0("2015–", 2018:2024),
                   terminos_elegidos = sapply(sel, function(s) paste(c("M0", s$elegido), collapse = " + ")))
for (t in candidatos) eleg[[t]] <- sapply(sel, function(s) t %in% s$elegido)

# 4. Predicción del año de prueba con los sorteos comunes de 36 (mismo `paso`, misma semilla)
pr_sel <- do.call(rbind, lapply(seq_along(sel), function(i) {
  a <- 2018 + i
  x <- paso(anual, list(SEL = sel[[i]]$elegido, M1_control = c("arrastre", "oni")), 2015:(a - 1), a, semilla = 2026 * 100 + a)
  cbind(esquema = "hacia_adelante", respuesta = "anual", x)
}))
# Control: M1 reproducido aquí debe dar las mismas predicciones que en v2_predicciones_loyo_y_adelante.csv
pv2 <- read.csv("modelo/resultados/v2_predicciones_loyo_y_adelante.csv", fileEncoding = "UTF-8")
pv2 <- subset(pv2, esquema == "hacia_adelante" & respuesta == "anual" & modelo %in% c("M0", "M1"))
ctrl <- merge(subset(pr_sel, modelo == "M1_control")[, c("anio", "region", "media_pred", "crps")],
              subset(pv2, modelo == "M1")[, c("anio", "region", "media_pred", "crps")], by = c("anio", "region"))
dif_ctrl <- max(abs(ctrl$media_pred.x - ctrl$media_pred.y), abs(ctrl$crps.x - ctrl$crps.y))
cat("Control de sorteos comunes (M1 aquí vs archivo v2), diferencia máxima:", dif_ctrl, "\n")
stopifnot(nrow(ctrl) == 63, dif_ctrl < 1e-8)
pr_sel <- subset(pr_sel, modelo == "SEL"); pr_sel$modelo <- "Seleccion_ISLR"

pred <- rbind(pv2[, names(pr_sel)], pr_sel)
met <- do.call(rbind, lapply(split(pred, pred$modelo), function(x) cbind(modelo = x$modelo[1], anios = "2019–2025", metricas_loyo(x))))
comp <- rbind(comparar("hacia_adelante", "anual", "Seleccion_ISLR", "M0"), comparar("hacia_adelante", "anual", "Seleccion_ISLR", "M1"))

options(width = 220)
print(eleg[, 1:3]); print(cv_tab, digits = 3); print(met, digits = 3)
print(comp[, c("modelo", "referencia", "dif_crps", "ic_crps_inf", "ic_crps_sup", "dif_r2_dentro", "ic_r2_inf", "ic_r2_sup",
               "anios_mejor_crps", "n_anios", "veredicto")], digits = 3)
write.csv(cv_tab, "modelo/resultados/s13_prediccion_cv_interno.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(cam_tab, "modelo/resultados/s13_prediccion_camino_forward.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(eleg, "modelo/resultados/s13_prediccion_seleccion_por_anio.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(pr_sel, "modelo/resultados/s13_prediccion_predicciones.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(met, "modelo/resultados/s13_prediccion_metricas.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(comp, "modelo/resultados/s13_prediccion_comparaciones.csv", row.names = FALSE, fileEncoding = "UTF-8")
