# Sección 14, PARTE 1: calibración de los intervalos, ML frente a REML (sin tocar 2026).
#   Roback y Legler, 8.5.4: ML sesga los componentes de varianza (usa los efectos fijos estimados); REML los corrige.
#   (11.6: en GLMM se usa típicamente ML; glmmTMB permite REML = TRUE también con NB2.)
# Modelos: S = arrastre + aptitud_termica_pob (sección 13), M1 = arrastre + oni, M0. Respuesta SE 1–40, 99 filas.
# Esquemas: LOYO 2015–2025 y hacia adelante 2019–2025, con los MISMOS sorteos comunes de 36_validacion_v2.R
#   (misma semilla y orden de sorteos por año de prueba): ML reproduce exactamente las predicciones v2.
# Métricas: σ_a, cobertura IP95, IP80 e IP50, CRPS, R² total y dentro de región; pareadas REML − ML (IC por
#   bootstrap de años, 39_comparaciones_pareadas.R).
# Regla fijada por el usuario antes de correr: para 2026 se usa REML si mejora la cobertura IP95 hacia adelante
#   (más cerca de 0,95) sin empeorar el CRPS (el IC de la diferencia REML − ML no queda entero por encima de 0).
#   Se aplica con S, el modelo principal, y la versión elegida se usa en los tres modelos de 2026.
# PARTE 1c: RR de la aptitud térmica por 1 DE total y por 1 DE dentro de región (aptitud centrada por región),
#   con IC por perfil en ML y de Wald en REML (glmmTMB no perfila REML), ajuste completo de 99 filas.
# Requiere 12_cv_loyo.R, 25_parte_c_predictores.R, 36_validacion_v2.R (continuas, termino_z, formula_v2) y
# 39_comparaciones_pareadas.R (comparar, r2_dentro). No se usa 2026.
library(glmmTMB)
library(scoringRules)

B <- 20000; REPS <- 2000
indicadoras <- c("caribe", "chorotega")
anual <- datos_bio(); anual$casos <- anual$casos_escalados
stopifnot(nrow(anual) == 99, max(anual$anio) == 2025)
modelos <- list(S = c("arrastre", "aptitud_termica_pob"), M1 = c("arrastre", "oni"), M0 = character())

# Igual que paso() de 36_validacion_v2.R (mismos sorteos), con REML opcional y más cuantiles
paso_reml <- function(datos, lista, entrenar, probar, semilla, reml) {
  ent0 <- subset(datos, anio %in% entrenar); pru0 <- subset(datos, anio %in% probar)
  set.seed(semilla)
  Zb <- matrix(rnorm(B * 40), B, 40); Zu <- rnorm(B); U <- matrix(runif(nrow(pru0) * B), nrow(pru0), B)
  do.call(rbind, lapply(names(lista), function(k) {
    t <- lista[[k]]; ent <- ent0; pru <- pru0
    for (v in continuas(t)) { m <- mean(ent[[v]]); s <- sd(ent[[v]])
      ent[[paste0(v, "_z")]] <- (ent[[v]] - m) / s; pru[[paste0(v, "_z")]] <- (pru[[v]] - m) / s }
    ent <- droplevels(ent); ent$region <- factor(ent$region, levels = levels(datos$region))
    w <- character()
    mod <- withCallingHandlers(glmmTMB(formula_v2(t), family = nbinom2, data = ent, REML = reml),
                               warning = function(e) { w <<- c(w, conditionMessage(e)); invokeRestart("muffleWarning") })
    bh <- fixef(mod)$cond; V <- vcov(mod)$cond
    X <- model.matrix(delete.response(terms(formula(mod, fixed.only = TRUE))), pru)[, names(bh), drop = FALSE]
    L <- t(chol(V + diag(1e-12, nrow(V))))
    beta <- sweep(Zb[, seq_along(bh), drop = FALSE] %*% t(L), 2, bh, "+")
    eta <- X %*% t(beta) + matrix(Zu * sigma_a(mod), nrow(X), B, byrow = TRUE) + log(pru$poblacion)
    y <- matrix(qnbinom(U, mu = exp(eta), size = sigma(mod)), nrow(X), B)
    sl <- log1p(1e5 * y / pru$poblacion)
    q <- t(apply(sl, 1, quantile, c(0.025, 0.975, 0.10, 0.90, 0.25, 0.75)))
    data.frame(modelo = paste0(k, "_", if (reml) "REML" else "ML"), anio = pru$anio, region = pru$region, y_log = pru$y_log,
               media_pred = rowMeans(sl), ip_inf = q[, 1], ip_sup = q[, 2], ip80_inf = q[, 3], ip80_sup = q[, 4],
               ip50_inf = q[, 5], ip50_sup = q[, 6], crps = crps_sample(pru$y_log, sl), se = (pru$y_log - rowMeans(sl))^2,
               sigma_a = sigma_a(mod), pdHess = isTRUE(mod$sdr$pdHess), avisos = paste(unique(w), collapse = " | "))
  }))
}
esquemas <- list(LOYO = lapply(2015:2025, function(a) list(ent = setdiff(2015:2025, a), pru = a)),
                 hacia_adelante = lapply(2019:2025, function(a) list(ent = 2015:(a - 1), pru = a)))
pred <- do.call(rbind, lapply(names(esquemas), function(e) do.call(rbind, lapply(esquemas[[e]], function(p)
  do.call(rbind, lapply(c(FALSE, TRUE), function(r)
    cbind(esquema = e, respuesta = "anual", paso_reml(anual, modelos, p$ent, p$pru, 2026 * 100 + p$pru, r))))))))

# Control: ML reproduce las predicciones v2 de M0 y M1
pv2 <- read.csv("modelo/resultados/v2_predicciones_loyo_y_adelante.csv", fileEncoding = "UTF-8")
pv2 <- subset(pv2, respuesta == "anual" & modelo %in% c("M0", "M1"))
pv2$modelo <- paste0(pv2$modelo, "_ML")
ctrl <- merge(pred, pv2[, c("esquema", "modelo", "anio", "region", "media_pred", "crps")], by = c("esquema", "modelo", "anio", "region"))
dif_ctrl <- max(abs(ctrl$media_pred.x - ctrl$media_pred.y), abs(ctrl$crps.x - ctrl$crps.y))
cat("Control ML vs v2 (filas", nrow(ctrl), "), diferencia máxima:", dif_ctrl, "\n")
stopifnot(nrow(ctrl) == 2 * (99 + 63), dif_ctrl < 1e-8)

cal <- do.call(rbind, lapply(split(pred, list(pred$esquema, pred$modelo), drop = TRUE), function(x)
  cbind(esquema = x$esquema[1], modelo = x$modelo[1], metricas_loyo(x),
        cobertura_ip80 = mean(x$y_log >= x$ip80_inf & x$y_log <= x$ip80_sup),
        cobertura_ip50 = mean(x$y_log >= x$ip50_inf & x$y_log <= x$ip50_sup),
        sigma_a_mediana_pasos = median(tapply(x$sigma_a, x$anio, `[`, 1)),
        sigma_a_min_pasos = min(x$sigma_a), pasos_sin_pdHess = length(unique(x$anio[!x$pdHess])),
        pasos_con_avisos = length(unique(x$anio[nzchar(x$avisos)])))))
rownames(cal) <- NULL
comp <- do.call(rbind, lapply(c("LOYO", "hacia_adelante"), function(e) do.call(rbind, lapply(names(modelos), function(k)
  comparar(e, "anual", paste0(k, "_REML"), paste0(k, "_ML"))))))

# Decisión (regla del usuario), con S hacia adelante
cS <- subset(cal, esquema == "hacia_adelante")
cob_ml <- cS$cobertura_ip95[cS$modelo == "S_ML"]; cob_reml <- cS$cobertura_ip95[cS$modelo == "S_REML"]
cc <- subset(comp, esquema == "hacia_adelante" & modelo == "S_REML")
mejora_cob <- abs(cob_reml - 0.95) < abs(cob_ml - 0.95)
empeora_crps <- cc$ic_crps_inf > 0
version <- if (mejora_cob && !empeora_crps) "REML" else "ML"
decision <- data.frame(modelo = "S", cobertura_ip95_ML = cob_ml, cobertura_ip95_REML = cob_reml, mejora_cobertura = mejora_cob,
                       dif_crps_REML_menos_ML = cc$dif_crps, ic_inf = cc$ic_crps_inf, ic_sup = cc$ic_crps_sup,
                       empeora_crps = empeora_crps, version_para_2026 = version)
# Misma regla aplicada a M1 y M0, solo informativa
for (k in c("M1", "M0")) {
  cm <- subset(comp, esquema == "hacia_adelante" & modelo == paste0(k, "_REML"))
  a <- cS$cobertura_ip95[cS$modelo == paste0(k, "_ML")]; r <- cS$cobertura_ip95[cS$modelo == paste0(k, "_REML")]
  decision <- rbind(decision, data.frame(modelo = paste(k, "(informativo)"), cobertura_ip95_ML = a, cobertura_ip95_REML = r,
    mejora_cobertura = abs(r - 0.95) < abs(a - 0.95), dif_crps_REML_menos_ML = cm$dif_crps, ic_inf = cm$ic_crps_inf,
    ic_sup = cm$ic_crps_sup, empeora_crps = cm$ic_crps_inf > 0,
    version_para_2026 = if (abs(r - 0.95) < abs(a - 0.95) && !(cm$ic_crps_inf > 0)) "REML" else "ML"))
}

# 1c. Aptitud térmica: RR por 1 DE total y por 1 DE dentro de región
apt <- anual$aptitud_termica_pob
de_total <- sd(apt); de_dentro <- sd(apt - ave(apt, anual$region))
completo <- anual
for (v in c("arrastre", "aptitud_termica_pob")) completo[[paste0(v, "_z")]] <- (completo[[v]] - mean(completo[[v]])) / sd(completo[[v]])
rr <- do.call(rbind, lapply(c(FALSE, TRUE), function(r) {
  m <- glmmTMB(formula_v2(modelos$S), family = nbinom2, data = completo, REML = r)
  b <- fixef(m)$cond["aptitud_termica_pob_z"]
  metodo <- if (r) "wald" else "profile"   # glmmTMB no calcula perfiles con REML
  ci <- confint(m, method = metodo, parm = "aptitud_termica_pob_z")
  data.frame(ajuste = if (r) "REML" else "ML", metodo_ic = metodo, beta_por_DE_total = b, de_total = de_total, de_dentro_region = de_dentro,
             razon_de = de_dentro / de_total,
             RR_por_DE_total = exp(b), IC_total_inf = exp(ci[1, 1]), IC_total_sup = exp(ci[1, 2]),
             RR_por_DE_dentro = exp(b * de_dentro / de_total), IC_dentro_inf = exp(ci[1, 1] * de_dentro / de_total),
             IC_dentro_sup = exp(ci[1, 2] * de_dentro / de_total),
             RR_por_001_unidades = exp(b / de_total * 0.01),
             varianza_dentro_sobre_total = var(apt - ave(apt, anual$region)) / var(apt), row.names = NULL)
}))
rango_apt <- aggregate(aptitud_termica_pob ~ region, anual, function(x) c(min = min(x), max = max(x), rango = diff(range(x))))

options(width = 220)
print(cal, digits = 3); print(comp[, c("esquema", "modelo", "referencia", "dif_crps", "ic_crps_inf", "ic_crps_sup",
                                       "dif_r2_dentro", "ic_r2_inf", "ic_r2_sup", "veredicto")], digits = 3)
print(decision, digits = 3); print(rr, digits = 3); print(rango_apt, digits = 3)
write.csv(pred, "modelo/resultados/s14_calibracion_predicciones.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(cal, "modelo/resultados/s14_calibracion_tabla.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(comp, "modelo/resultados/s14_calibracion_pareadas.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(decision, "modelo/resultados/s14_calibracion_decision.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(rr, "modelo/resultados/s14_aptitud_rr.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(do.call(data.frame, rango_apt), "modelo/resultados/s14_aptitud_rango_por_region.csv", row.names = FALSE, fileEncoding = "UTF-8")
version_2026 <- version
